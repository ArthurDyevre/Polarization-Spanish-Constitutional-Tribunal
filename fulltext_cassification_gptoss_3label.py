# -------------------------
# Dependencies
# -------------------------
from vllm import LLM, SamplingParams
from transformers import AutoTokenizer
import pandas as pd
import json
import re

# -------------------------
# Config
# -------------------------
MODEL_NAME  = "gpt-oss-120b"
LOCAL_DIR   = "" + MODEL_NAME

INPUT_PATH  = "tc_resolutions_fulltext_data_11May26.csv"
OUTPUT_PATH = "tc_resolutions_fulltext_data_27May26_output_gptoss.csv"
VALIDATION_PATH = "random_sample_handcode_100.csv"

BATCH_SIZE  = 4   

MODEL_MAX_CONTEXT = 131_000
OUTPUT_RESERVED   = 1024
PROMPT_OVERHEAD   = 200   

# -------------------------
# Model + tokenizer
# -------------------------
llm = None
tokenizer = None
sampling_params = None

_TERR_BUDGET = None
_POL_BUDGET  = None
_DOC_BUDGET  = None


def init_model():
    """Build the model, tokenizer, sampling params, and token budgets.

    Called exactly once, from the real main process only.
    """
    global llm, tokenizer, sampling_params
    global _TERR_BUDGET, _POL_BUDGET, _DOC_BUDGET

    llm = LLM(
        model=LOCAL_DIR,
        dtype="auto",
        tensor_parallel_size=2,
        max_model_len=MODEL_MAX_CONTEXT,
        gpu_memory_utilization=0.85,  
    )

    tokenizer = AutoTokenizer.from_pretrained(LOCAL_DIR)

    sampling_params = SamplingParams(
        temperature=0.0,
        max_tokens=OUTPUT_RESERVED,
    )


    _TERR_BUDGET = _system_token_budget(TERRITORIAL_SYSTEM)
    _POL_BUDGET  = _system_token_budget(POLITIZATION_SYSTEM)
    _DOC_BUDGET  = min(_TERR_BUDGET, _POL_BUDGET)

# -------------------------
# System prompts  (3 LABEL)
# -------------------------

TERRITORIAL_SYSTEM = """You are a legal expert on Spanish constitutional law.
Your task is to classify whether a ruling of the Spanish Constitutional Court (Tribunal Constitucional) 
concerns a TERRITORIAL dispute — that is, a conflict over the constitutional distribution of powers 
between the central State and the Autonomous Communities (or local entities).

A case is TERRITORIAL if its core legal question involves:
- Which level of government (State vs. Autonomous Community vs. local entity) has competence over a matter
- The constitutional allocation of legislative, executive, or financial powers between government levels
- The interpretation of the bloque de constitucionalidad in competence terms
- Conflicts over basic legislation (legislación básica) and regional development
- Fiscal or financial autonomy of regions (financiación autonómica, concierto económico)
- Language co-officiality as a competence question
- Autonomy statutes and their constitutional limits

A case is NOT territorial if the competence question is merely incidental and the core dispute 
concerns individual rights, criminal law, labor law, or procedural guarantees 
without a genuine center-periphery power conflict.

Classify the ruling using EXACTLY one of these three labels:
- CENTRAL: the territorial dispute is the primary or substantial issue
- PERIPHERAL: territorial issues appear but are secondary to another constitutional question  
- ABSENT: no meaningful territorial dimension

Return ONLY a JSON object with this exact structure, nothing else:
{"territorial": "CENTRAL|PERIPHERAL|ABSENT", "reason": "one sentence explanation"}"""


POLITIZATION_SYSTEM = """You are a legal expert on Spanish constitutional law.
Your task is to classify whether a ruling of the Spanish Constitutional Court (Tribunal Constitucional) 
concerns a POLITICALLY SALIENT dispute — that is, a case where the outcome is plausibly 
influenced by the ideological or political preferences of the judges, 
beyond what any competent jurist would decide on purely legal grounds.

A case is POLITICAL if it involves:
- Challenges to major legislative reforms with clear partisan valence (labor reform, abortion law, 
  amnesty, state of alarm, emergency decrees)
- Party politics, electoral law, or party illegalization
- Terrorism legislation or criminal prosecution of political actors
- Referendums or self-determination claims
- Budget laws contested as major policy instruments
- Cases with high media salience where different ideological camps have clearly opposing positions

A case is ROUTINE if it involves:
- Individual criminal cases (theft, fraud, assault, drug offenses) where the constitutional 
  question is purely procedural (due process, presumption of innocence, proportionality of sentence)
- Private law disputes (contracts, inheritance, divorce, tenancy) raising standard rights questions
- Individual labor disputes (dismissal, pension, unemployment benefit) with no policy dimension
- Routine administrative disputes (civil service access, transfers, leave) about individual situations
- Prison law questions about individual inmates with no broader policy dimension

Classify the ruling using EXACTLY one of these three labels:
- POLITICAL: the case is embedded in a politically salient dispute
- AMBIGUOUS: some political context but the core question is technical or individual
- ROUTINE: the case concerns an individual legal dispute with no plausible political salience

Return ONLY a JSON object with this exact structure, nothing else:
{"politization": "POLITICAL|AMBIGUOUS|ROUTINE", "reason": "one sentence explanation"}"""


# -------------------------
# Token counting helpers
# -------------------------

def count_tokens(text: str) -> int:
    """Return the number of tokens in *text* using the model's own tokenizer."""
    return len(tokenizer.encode(text, add_special_tokens=False))


def _system_token_budget(system_prompt: str) -> int:
    """
    Compute how many tokens remain for the document body after reserving space
    for the system prompt, the fixed user-message wrapper, and the output.
    """
    system_tokens   = count_tokens(system_prompt)
    available       = (MODEL_MAX_CONTEXT
                       - system_tokens
                       - OUTPUT_RESERVED
                       - PROMPT_OVERHEAD)
    return max(available, 0)


_USER_FRAME_TOKENS = 30


def select_text(full_text: str, tipo_resolucion: str) -> tuple[str, str]:
    """
    Choose the text to send to the model and return (text, source_label).

    We use full_text only. To include as much text as possible:
      1. full_text            — if it fits within _DOC_BUDGET, send it whole
      2. full_text_truncated  — otherwise keep the HEAD up to the budget
         (the antecedentes and the statement of the challenged norm, i.e. the
         strongest territorial/political signal, sit at the front of a ruling)

    The source_label records which branch was taken so you can audit it later.
    """
    frame_tokens  = count_tokens(f"Process type: {tipo_resolucion}") + _USER_FRAME_TOKENS
    available     = max(_DOC_BUDGET - frame_tokens, 0)

    # ── Branch 1: full text fits ──────────────────────────────────────────
    ft_tokens = count_tokens(full_text)
    if ft_tokens <= available:
        return full_text, "full_text"

    # ── Branch 2: truncate full_text to the budget (keep the head) ────────
    ids       = tokenizer.encode(full_text, add_special_tokens=False)
    truncated = tokenizer.decode(ids[:available], skip_special_tokens=True) if available > 0 else ""
    return truncated, "full_text_truncated"


# -------------------------
# Prompt builder
# -------------------------

def build_prompt(text: str, source_label: str, system_prompt: str, tipo_resolucion: str) -> str:
    """
    Apply the gpt-oss chat template.
    source_label tells the model which section of the ruling it is reading.
    Includes a safety-net trim if the assembled prompt still exceeds the budget
    (guards against BPE boundary effects and chat-template overhead drift).
    """
    label_map = {
        "full_text":           "Full ruling text",
        "full_text_truncated": "Full ruling text (truncated due to length)",
    }
    section_header = label_map.get(source_label, "Ruling text")

    def _assemble(t: str) -> str:
        user_content = (
            f"Process type: {tipo_resolucion}\n\n"
            f"{section_header}:\n\"\"\"\n{t}\n\"\"\""
        )
        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user",   "content": user_content},
        ]
        return tokenizer.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=True,
            reasoning_effort="low",
        )

    prompt = _assemble(text)

    # Safety net: verify the final prompt length and trim if still over budget.
    # This catches BPE boundary effects and any chat-template overhead not
    # captured by PROMPT_OVERHEAD.
    max_input_tokens = MODEL_MAX_CONTEXT - OUTPUT_RESERVED
    prompt_ids = tokenizer.encode(prompt, add_special_tokens=False)
    if len(prompt_ids) > max_input_tokens:
        excess   = len(prompt_ids) - max_input_tokens + 20  # 20-token safety margin
        text_ids = tokenizer.encode(text, add_special_tokens=False)
        keep     = max(len(text_ids) - excess, 0)
        trimmed  = tokenizer.decode(text_ids[:keep], skip_special_tokens=True) if keep > 0 else ""
        prompt   = _assemble(trimmed)

    return prompt


# -------------------------
# Output parsers  (3 LABEL)
# -------------------------

def _extract_json(text: str) -> dict:
    m = re.findall(r"\{[^{}]*\}", text, re.DOTALL)
    return json.loads(m[-1]) if m else {}

def parse_territorial(text: str) -> dict:
    try:
        data  = _extract_json(text)
        label = data.get("territorial", "PARSE_ERROR")
        if label not in ("CENTRAL", "PERIPHERAL", "ABSENT"):
            label = "PARSE_ERROR"
        return {"territorial": label, "territorial_reason": data.get("reason", "")}
    except Exception:
        return {"territorial": "PARSE_ERROR", "territorial_reason": text[:200]}


def parse_politization(text: str) -> dict:
    try:
        data  = _extract_json(text)
        label = data.get("politization", "PARSE_ERROR")
        if label not in ("POLITICAL", "AMBIGUOUS", "ROUTINE"):
            label = "PARSE_ERROR"
        return {"politization": label, "politization_reason": data.get("reason", "")}
    except Exception:
        return {"politization": "PARSE_ERROR", "politization_reason": text[:200]}


# -------------------------
# Batch inference
# -------------------------

def run_batch(prompts: list[str]) -> list[str]:
    outputs = llm.generate(prompts, sampling_params)
    return [out.outputs[0].text.strip() for out in outputs]


# -------------------------
# Main pipeline
# -------------------------

def parse_id_pat(id_pat: str) -> tuple[str, str, str]:
    """
    Parse 'AUTO 10/1996' → ('AUTO', '10', '1996').
    Falls back gracefully if the format is unexpected.
    """
    try:
        tipo, rest   = id_pat.strip().split(" ", 1)   # 'AUTO', '10/1996'
        numero, anno = rest.split("/", 1)              # '10', '1996'
        return tipo.strip(), numero.strip(), anno.strip()
    except ValueError:
        return id_pat.strip(), "", ""


def main():
    # THIS IS ONLY FOR DEBUG AND VALIDATION
    #validation = pd.read_csv(VALIDATION_PATH, usecols=["ID_PAT"], sep=";")
    #fulltext   = pd.read_csv(INPUT_PATH,
    #                         usecols=["ID_PAT", "numeric_id", "full_text", "antecedentes"],
    #                         sep=",")
    #df = validation.merge(fulltext, on="ID_PAT", how="inner")

    df = pd.read_csv(INPUT_PATH)

    # Sanity check: an empty merge means the ID_PAT values didn't line up
    # (encoding / whitespace mismatch between the two files).
    print(f"Merged rows: {len(df)}")
    assert len(df) > 0, "Merge produced 0 rows — check ID_PAT formatting/encoding in both CSVs."

    required = {"ID_PAT", "numeric_id", "full_text", "antecedentes"}
    missing  = required - set(df.columns)
    assert not missing, f"Missing columns: {missing}"

    df["full_text"] = df["full_text"].fillna("").str.strip()

    # Derive tipo / numero / anno from the ID_PAT string (e.g. 'AUTO 10/1996')
    parsed = df["ID_PAT"].fillna("Unknown").apply(parse_id_pat)
    df["tipo_resolucion"]   = [p[0] for p in parsed]
    df["numero_resolucion"] = [p[1] for p in parsed]
    df["anno_resolucion"]   = [p[2] for p in parsed]

    print(f"Loaded {len(df):,} rows.")
    print(f"Document token budget per prompt: {_DOC_BUDGET:,} tokens "
          f"(context {MODEL_MAX_CONTEXT:,} − system − output − overhead)\n")

    all_results = []
    n_batches   = (len(df) + BATCH_SIZE - 1) // BATCH_SIZE

    for i in range(0, len(df), BATCH_SIZE):
        batch     = df.iloc[i:i + BATCH_SIZE]
        batch_num = i // BATCH_SIZE + 1

        # ── Text selection (done once per row, shared across both passes) ──
        selections = [
            select_text(row.full_text, row.tipo_resolucion)
            for row in batch.itertuples()
        ]
        texts, source_labels = zip(*selections)

        # ── Pass 1: territorial ───────────────────────────────────────────
        terr_prompts = [
            build_prompt(text, label, TERRITORIAL_SYSTEM, row.tipo_resolucion)
            for text, label, row in zip(texts, source_labels, batch.itertuples())
        ]
        terr_raw = run_batch(terr_prompts)

        # ── Pass 2: politization ──────────────────────────────────────────
        pol_prompts = [
            build_prompt(text, label, POLITIZATION_SYSTEM, row.tipo_resolucion)
            for text, label, row in zip(texts, source_labels, batch.itertuples())
        ]
        pol_raw = run_batch(pol_prompts)

        # ── Collect results ───────────────────────────────────────────────
        for row, label, t_raw, p_raw in zip(
            batch.itertuples(), source_labels, terr_raw, pol_raw
        ):
            t = parse_territorial(t_raw)
            p = parse_politization(p_raw)
            all_results.append({
                "ID_PAT":               row.ID_PAT,
                "numeric_id":           row.numeric_id,
                "tipo_resolucion":      row.tipo_resolucion,
                "numero_resolucion":    row.numero_resolucion,
                "anno_resolucion":      row.anno_resolucion,
                "text_source":          label,
                "territorial":          t["territorial"],
                "territorial_reason":   t["territorial_reason"],
                "politization":         p["politization"],
                "politization_reason":  p["politization_reason"],
            })

        print(f"Batch {batch_num}/{n_batches} — "
              f"processed {min(i + BATCH_SIZE, len(df)):,} / {len(df):,}")

    out_df = pd.DataFrame(all_results)
    out_df.to_csv(OUTPUT_PATH, index=False)
    print(f"\nDone. Results saved to {OUTPUT_PATH}")

    # ── Audit: text source distribution ───────────────────────────────────
    # If 'full_text_truncated' is ~0, you are sending whole rulings (good).
    print("\nText source distribution:")
    print(out_df["text_source"].value_counts().to_string())

    # ── Parse error report ────────────────────────────────────────────────
    n_terr_errors = (out_df["territorial"] == "PARSE_ERROR").sum()
    n_pol_errors  = (out_df["politization"] == "PARSE_ERROR").sum()
    print(f"\nParse errors — territorial: {n_terr_errors}, politization: {n_pol_errors}")

    # ── Label distributions ───────────────────────────────────────────────
    print("\nTerritorial distribution:")
    print(out_df["territorial"].value_counts().to_string())
    print("\nPolitization distribution:")
    print(out_df["politization"].value_counts().to_string())


# -------------------------
# Run
# -------------------------
if __name__ == "__main__":
    init_model()
    main()