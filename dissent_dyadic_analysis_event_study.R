library(dataverse)
library(tidyverse)
library(ggplot2)
library(dplyr)

# import data

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")


data <- read.csv("complete_data_set7May2024.csv", sep = ",", header = T)


# percentage nonunanimous cases abstract review


percentage_nonunaninmous <- data %>% 
  filter(procedure == "Abstract") %>%
  group_by(year_r, ID_PAT) %>%
  summarise(dissent = max(dissent_dummy), .groups = "drop") %>%  # Determine if there was dissent in each case
  group_by(year_r) %>%  # Group by year
  summarise(percentage_dissent = mean(dissent)*100, total_cases = n(), .groups = "drop") 


ggplot(percentage_nonunaninmous, aes(x = year_r, y = percentage_dissent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Share of Nonunanimous Abstract Review Decisions",
       x = "Year",
       y = "Share Nonunanimous (%)")




# percentage of nonunanimous cases over year all cases

percentage_nonunaninmous <- data %>% 
  group_by(year_r, ID_PAT) %>%
  summarise(dissent = max(dissent_dummy), .groups = "drop") %>%  # Determine if there was dissent in each case
  group_by(year_r) %>%  # Group by year
  summarise(percentage_dissent = mean(dissent)*100, total_cases = n(), .groups = "drop") 



pdf("Nonunanimous_share.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(percentage_nonunaninmous) +
        geom_bar(aes(x = year_r, y =total_cases
        ), 
                 stat = "identity", fill = "steelblue") +
        geom_line(aes(x = year_r, y = percentage_dissent*100), 
                  stat = "identity", color = "red", size = 1) +
  theme_minimal() +
  labs(x = "", y = "Number of Cases") + 
  scale_y_continuous(sec.axis=sec_axis(~.*0.01, name="Nonunanimous (%)")))
dev.off()

# probability of nonunimous decision conditional on number of cases

case_level_data <- data %>%
  group_by(ID_PAT) %>%  # Group by case ID
  summarise(
    dissent = max(dissent_dummy),    # Whether there was a dissent in the case
    year_r = first(year_r),          # Year the decision was rendered
    procedure = first(procedure)     # Procedure type
  ) %>%
  ungroup()


# Compute the total number of cases per year
cases_per_year <- case_level_data %>%
  group_by(year_r) %>%
  summarise(total_cases = n(), .groups = "drop")

# View the total number of cases per year
print(cases_per_year)

# Merge the total cases data back to the case-level dataset
case_level_data <- case_level_data %>%
  left_join(cases_per_year, by = "year_r")


# 4. Estimate the probability of dissent at the case level using logistic regression
dissent_m1 <- glm(
  dissent ~ year_r, 
  data = case_level_data, 
  family = binomial(link = "logit")
)

# Summarize the model results
summary(dissent_m1)

# controlling for total cases
dissent_m2 <- glm(
  dissent ~ year_r + total_cases, 
  data = case_level_data, 
  family = binomial(link = "logit")
)

# Summarize the model results
summary(dissent_m2)


# adding procedure
dissent_m3 <- glm(
  dissent ~ year_r + total_cases + procedure, 
  data = case_level_data, 
  family = binomial(link = "logit")
)

summary(dissent_m3)

library(stargazer)
stargazer(dissent_m1,dissent_m2,dissent_m3,
          type = "latex", digits = 4, digits.extra = 3,
          title="",
          model.names = F,covariate.labels=c("Year",
                                             "Total Cases",
                                             "Concrete Review"),
          dep.var.labels="Decision (1 = nonunanimous, 0 = unanimous)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          #add.lines = list(
            #c("Year FE","No", "No","Yes","Yes","Yes"),
            #c("Case FE","No", "No","No","Yes","Yes"),
          style="default")



# 4. Estimate the probability of dissent at the case level using linear regression
dissent_ma <- lm(
  dissent ~ log(year_r-1979), 
  data = case_level_data
)

# Summarize the model results
summary(dissent_ma)

# controlling for total cases
dissent_mb <- lm(
  dissent ~ year_r + total_cases + as.factor(year_r), 
  data = case_level_data
)

# Summarize the model results
summary(dissent_mb)

a = 0
b = 0

# adding procedure
dissent_mc <- lm(
  dissent ~ year_r + total_cases + procedure, 
  data = case_level_data
)

summary(dissent_mc)

library(stargazer)
stargazer(dissent_ma,dissent_mb,dissent_mc,
          type = "latex", digits = 4, digits.extra = 3,
          title="",
          model.names = F,covariate.labels=c("Year",
                                             "Total Cases",
                                             "Concrete Review"),
          dep.var.labels="Decision (1 = nonunanimous, 0 = unanimous)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          #add.lines = list(
          #c("Year FE","No", "No","Yes","Yes","Yes"),
          #c("Case FE","No", "No","No","Yes","Yes"),
          style="default")


#### Compute relation between minority size and propensity to dissent

data_unique <- data %>%
  distinct(ID_PAT, id_juez, .keep_all = TRUE)

# Compute number of judges by ideology per case
ideology_counts <- data_unique %>%
  group_by(ID_PAT) %>%
  summarise(
    num_judges = n(),
    num_progressive = sum(Ideology == "P", na.rm = TRUE),
    num_conservative = sum(Ideology == "C", na.rm = TRUE)
  )

# Join these counts back to the original dataset
data <- data %>%
  left_join(ideology_counts, by = "ID_PAT")


data_unique <- data %>%
  distinct(ID_PAT, id_juez, .keep_all = TRUE)

# Step 2: Compute the majority ideology per case
majority_ideology <- data_unique %>%
  filter(Ideology %in% c("P", "C")) %>%  # Only include known ideologies
  group_by(ID_PAT) %>%
  summarise(
    majority = names(sort(table(Ideology), decreasing = TRUE))[1],
    .groups = "drop"
  )

# Step 3: Join back to assign majority ideology to each judge
data_unique <- data_unique %>%
  left_join(majority_ideology, by = "ID_PAT")

# Step 4: Mark minority judges
data_unique <- data_unique %>%
  mutate(minority_judge = ifelse(Ideology %in% c("P", "C") & Ideology != majority, 1, 0))

# Step 5: Compute fraction of minority judges per case

minority_fraction <- data_unique %>%
  group_by(ID_PAT) %>%
  summarise(
    num_minority = sum(minority_judge, na.rm = TRUE),
    num_judges = first(num_judges),
    frac_minority = 10 * num_minority / num_judges,
    .groups = "drop"
  )

minority_fraction <- minority_fraction %>%
  distinct(ID_PAT, .keep_all = TRUE)

# Step 6: Join result back to the original full dataset
data <- data %>%
  left_join(minority_fraction, by = "ID_PAT")

# add caseload to main data set

data <- data %>%
  left_join(cases_per_year, by = "year_r")


data_panel <- data 
  #%>%
  #filter(num_judges >= 8)

data_panel$year_factor <- as.factor(data_panel$year_r)

dissent_minority <- glm(
  dissent_dummy ~ frac_minority + year_r + procedure + total_cases, 
  data = data_panel, 
  family = binomial(link = "logit")
)

summary(dissent_minority)

# linear probability
dissent_minority <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r + procedure + total_cases, 
  data = data_panel
)

summary(dissent_minority)

d1 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority, 
  data = data_panel
)

summary(d1)

d2 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r, 
  data = data_panel
)

summary(d2)


d3 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r + total_cases, 
  data = data_panel
)

summary(d3)

d4 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r + total_cases + procedure, 
  data = data_panel
)

summary(d4)

d5 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r +
    total_cases + procedure + year_factor, 
  data = data_panel
)

summary(d5)

d6 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r +
    total_cases + procedure +  year_factor, 
  data = data_panel
)

summary(d6)


library(stargazer)
stargazer(d1,d2,d3,d4,d5,
          type = "latex", digits = 4, digits.extra = 3,
          title="",
          model.names = F,
          covariate.labels=c("Minority Size",
                                            "Time",
                                            "Caseload",
                                            "Concrete Review"),
          dep.var.labels="Vote (1 = dissent, 0 = majority)",
          omit = "year_factor",
          add.lines = list(
          c("Year FE","No", "No", "No", "No", "Yes")
          ),
          style="default")

stargazer(d1, d2, d3, d4, d5,
          type = "text",
          digits = 4,
          digits.extra = 3,
          title = "",
          model.names = FALSE,
          covariate.labels = c("Minority Size", "Time", "Caseload", "Concrete Review"),
          dep.var.labels = "Vote (1 = dissent, 0 = majority)",
          omit = "year_r",
          add.lines = list(
            c("Year FE", "No", "No", "No", "No", "Yes")
          ),
          style = "default")

# Polarization index, association with dissent frequency

PI <- read.csv("db_polarindex.csv") %>% select(year,polarindex)

case_level_data <- merge(case_level_data, PI, by.x = "year_r", by.y = "year", all.x = T)

dissent_m4 <- glm(
  dissent ~ year_r + total_cases + procedure + polarindex, 
  data = case_level_data, 
  family = binomial(link = "logit")
)

summary(dissent_m4)


# polarindex with lags

lags <- 1:3  # Specify the lags you want
case_level_data <- case_level_data %>%
  arrange(year_r) %>%
  mutate(across(
    .cols = polarindex,
    .fns = list(
      lag1 = ~ lag(., n = 1),
      lag2 = ~ lag(., n = 2),
      lag3 = ~ lag(., n = 3)
    )
  ))

dissent_m5 <- glm(
  dissent ~ year_r + total_cases + procedure + polarindex, 
  data = case_level_data, 
  family = binomial(link = "logit")
)

summary(dissent_m5)

# case parameters over time

data3 <- read.csv("item_parameters_full.csv")

data3$year <- as.numeric(sub(".*/", "", data3$item))

m1 <- lm(abs(beta) ~ year, data = data3, na.action = na.omit)
summary(m1)
cor.test(abs(data3$beta), data3$year)
plot(data3$year, abs(data3$beta))


ggplot(data3, aes(x = year, y = abs(beta))) +
  geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6, color = "blue") +
  labs(y = "Absolute Value Discrimination Parameter", x = "")

pdf("Beta_over_time_full.pdf", width = 6.5, height = 6.5)
print(ggplot(data3, aes(x = year, y = abs(beta))) +
  geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Adding a regression line
  labs(y = "Absolute Value Discrimination Parameter", x = "")
)
dev.off()

# case parameters by procedure

cases <- distinct(data2, ID_PAT, .keep_all = T)
data4 <- left_join(data3,cases[,c("ID_PAT", "procedure")],
                   by = join_by("item" == "ID_PAT"))

data4$procedure_bin <- as.numeric(factor(data4$procedure)) - 1

cor.test(data4$beta, data4$procedure_bin, use = "complete.obs")

## case parameters over time abstract review
data4 <- data4 %>% filter(procedure == "Abstract")
m2 <- lm(abs(beta) ~ year, data = data4, na.action = na.omit)
summary(m2)
cor.test(abs(data4$beta), data4$year)
plot(data4$year, abs(data4$beta))

ggplot(data4, aes(x = year, y = abs(beta))) +
  geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6, color = "blue") +
  labs(y = "Absolute Beta", x = "Year") +
  ggtitle("Scatter Plot of Absolute Beta over Years with Jitter")

pdf("Beta_over_time_abstract.pdf", width = 6.5, height = 6.5)
print(ggplot(data4, aes(x = year, y = abs(beta))) +
  geom_jitter(width = 0.2, height = 0.2, size = 3, alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Adding a regression line
  labs(y = "Absolute Value Discrimination Parameter", x = "Year")
)
dev.off()


# add party manifesto

data2 <- merge(data2, judge_ideo, by = "id_juez")
m2 <- lm(data2$Party_manifesto ~ Ideology, data = data2)
summary(m2)

# convert to dyadic format

dyads <- data %>%
  group_by(ID_PAT) %>%
  do({
    judges <- .$id_juez
    Vote <- .$Type_Vote
    positions <- .$Party_manifesto
    combn_data <- combn(judges, 2)
    combn_df <- data.frame(judge1 = combn_data[1, ], judge2 = combn_data[2, ])
    combn_df %>%
      rowwise() %>%
      mutate(
        agreement = as.integer(Vote[match(judge1, judges)] == Vote[match(judge2, judges)]),
        abs_distance = abs(positions[match(judge1, judges)] - positions[match(judge2, judges)])
      )
  }) %>%
  unnest(cols = c(judge1, judge2, agreement, abs_distance))

write.csv(dyads,"dyad_full.csv")

# add covariates

dyads <- read.csv("dyad_full.csv")

# remove incorrect rows for SENTENCIA 12/1986
dyads <- dyads %>%
  filter(ID_PAT != "SENTENCIA 12/1986")

covariates <- data %>% select(ID_PAT, year_r, procedure, frac_minority) %>% 
  distinct(., ID_PAT, .keep_all = T)

dyads <- left_join(dyads, covariates, by = "ID_PAT")

dyads <- left_join(dyads, cases_per_year, by = "year_r")

# Judge ideology by judge
judge_ideology <- data %>%
  select(Juez_Voto, id_juez, Ideology) %>%
  distinct()

# Join ideology for judge1 and 2
dyads <- dyads %>%
  left_join(
    judge_ideology %>%
      rename(
        ideology1 = Ideology,
        name1 = Juez_Voto
      ),
    by = c("judge1" = "id_juez")
  ) %>%
  left_join(
    judge_ideology %>%
      rename(
        ideology2 = Ideology,
        name2 = Juez_Voto
      ),
    by = c("judge2" = "id_juez")
  ) %>%
  mutate(
    pair_ideology = case_when(
      ideology1 == ideology2 & ideology1 %in% c("P", "C") ~ "Convergent",
      ideology1 != ideology2 & ideology1 %in% c("P", "C") & ideology2 %in% c("P", "C") ~ "Divergent",
      TRUE ~ "NA"
    ),
    post_2015 = as.integer(year_r >= 2015)
  )

# normalize ideological distance
library(scales)

dyads$N_abs_distance <- rescale(dyads$abs_distance)

# regression OLS
library(fixest)
library(plm)
set.seed(6764)

m1 <- feols(agreement ~ pair_ideology*year_r +
              frac_minority, 
         data = dyads,
         vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(agreement ~ pair_ideology*year_r + 
              frac_minority | as.factor(year_r), 
         data = dyads,
         vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(agreement ~ pair_ideology*year_r + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT, 
            data = dyads,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_pair_time_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:year_r" = "Mixed $\\times$ Time",
    "pair_ideologyConvergent:year_r" = "In-group $\\times$ Time",
    "pair_ideologyNA:year_r" = "Ambiguous $\\times$ Time"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:year_r",
    "pair_ideologyConvergent:year_r",
    "pair_ideologyNA:year_r" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)

m6 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              frac_minority + total_cases + procedure|as.factor(year_r), 
            data = dyads,
            vcov = ~ judge1 + judge2)
summary (m6)

m7 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              frac_minority + total_cases + procedure|as.factor(year_r) + ID_PAT, 
            data = dyads,
            vcov = ~ judge1 + judge2)
summary (m7)


m8 <- feols(agreement ~ pair_ideology*year_r + year_r + 
              frac_minority + 
              procedure|as.factor(year_r) + ID_PAT + judge1 + judge2, 
            data = dyads,
            vcov = ~ judge1 + judge2)
summary (m8)






stargazer(m2, m3, m4, m5, m6, m7, m8,
          type = "latex",
          title = "Regression Results: Predictors of Agreement",
          align = TRUE,
          keep.stat = c("n", "rsq"),
          dep.var.labels = "Agreement",
          column.labels = paste("Model", 1:7),
          covariate.labels = c("Normalized Distance", "Year", "Minority Fraction", 
                               "Total Cases", "Procedure", 
                               "Distance × Year"),
          notes.align = "l",
          notes = "Standard errors in parentheses. * p<0.1; ** p<0.05; *** p<0.01")


etable(m1, m2, m3, m4, m5, m6, m7,
       tex = TRUE,
       digits = 3,
       title = "Dyadic disagreement and ideological distance",
       dict = c(N_abs_distance = "Distance Appointing Party", year_r = "Time",
                frac_minority = "Minority Fraction", total_cases = "Caseload",
                procedure = "Concrete Review", "N_abs_distance::year_r" = "Distance Appointing Party × Time"))

etable(m1, m2, m3, m4, m5, m6,
       tex = TRUE,
       digits = 3,
       title = "Dyadic disagreement and minority size",
       dict = c(year_r = "Time",
                frac_minority = "Minority Fraction", total_cases = "Caseload",
                procedure = "Concrete Review"))


# Dyadic disagreement pre/post-2015

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 2015
# --------------------------------------------------


judge_span2015 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_2015 = if_else(start_year <= 2016 & end_year >= 2016, 1L, 0L)
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 2015 and observations for 2013-18 window
# --------------------------------------------------


window_start <- 2013
window_end <- 2018

dyads_span2015 <- dyads %>%
  left_join(
    judge_span2015 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_2015_1 = spans_2015
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span2015 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_2015_2 = spans_2015
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_2015_1 == 1,
    spans_2015_2 == 1,
    dplyr::between(year_r, window_start, window_end)
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    post_2015 = as.integer(year_r >= 2015)
  )

m_0 <- feols(
  agreement ~ pair_ideology * post_2015 +
    procedure + total_cases,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 +ID_PAT
)

summary(m_0)

# mean disagreement by ideology and period for judges spanning 2015

library(dplyr)
library(ggplot2)

year_window <- dyads_span2015 %>%
  summarise(
    min_year = min(year_r = 2013, na.rm = TRUE),
    max_year = max(year_r = 2018, na.rm = TRUE)
  )

plot_data <- dyads_span2015 %>%
  filter(year_r >= year_window$min_year,
         year_r <= year_window$max_year) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(1 - agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

pdf("proportion_disagreement_pair_post_2015.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(plot_data, aes(x = year_r, y = mean_disagreement,
                      colour = pair_ideology, group = pair_ideology)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = 2015.98, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    limits = range(plot_data$year_r, na.rm = TRUE),
    breaks = scales::pretty_breaks()
  ) +
  scale_y_continuous(
    #labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.5)
  ) +
  labs(
    x = "",
    y = "Proportion Disagreement",
    colour = "Pair composition",
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c("Convergent" = "steelblue", 
                                "Divergent" = "firebrick", "NA" = "grey"),
                     labels = c("Convergent" = "In-group (PP-PP, \nPSOE-PSOE)", 
                                "Divergent" = "Mixed (PSOE-PP, \nPP-PSOE)",
                                "NA" = "Ambiguous"),)
)
dev.off()

# Model estimation


m_0 <- feols(
  agreement ~ pair_ideology | ID_PAT,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_0)


m_0 <- feols(
  agreement ~ pair_ideology | ID_PAT + factor(year_r),
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_0)

m_a <- feols(
  agreement ~ pair_ideology * post_2015,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)

m_b <- feols(
  agreement ~ pair_ideology * post_2015 +
  total_cases + as.factor(procedure),
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_b)

m_c <- feols(
  agreement ~ pair_ideology * post_2015 +
    total_cases + as.factor(procedure) | as.factor(year_r),
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_c)

m_d <- feols(
  agreement ~ pair_ideology * post_2015 +
    total_cases + factor(procedure)
    | ID_PAT + as.factor(year_r),
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_d)

library(fixest)
etable(
  m_a, m_b, m_c, m_d,
  tex = TRUE,
  file = "regression_table_post2015_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D"),
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous",
    "post_2015" = "Post-2015",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_2015" = "Mixed $\\times$ Post-2015",
    "pair_ideologyConvergent:post_2015" = "Convergent $\\times$ Post-2015",
    "pair_ideologyNA:post_2015" = "Ambiguous $\\times$ Post-2015"
  ),
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "post_2015",
    "total_cases",
    "pair_ideologyDivergent:post_2015",
    "pair_ideologyConvergent:post_2015",
    "Constant"
  ),
  fixef.group = list("Case FE" = "ID_PAT")
)


etable(
  m_a, m_b, m_c, m_d,
  tex = TRUE,
  file = "regression_table.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  headers = c("Model A", "Model B", "Model C", "Model D"),
  
  dict = c(
    "pair_ideologyDivergent" = "Divergent",
    "pair_ideologyConvergent" = "Convergent",
    "post_2015" = "Post-2015",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_2015" = "Divergent $\\times$ Post-2015",
    "pair_ideologyConvergent:post_2015" = "Convergent $\\times$ Post-2015"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "post_2015",
    "total_cases",
    "pair_ideologyDivergent:post_2015",
    "pair_ideologyConvergent:post_2015",
    "Constant"
  ),
  
  fixef.group = list("Case fixed effects" = "ID_PAT")
)


# Dynamic interaction model
# Dyadic disagreement dynamic event-window analysis, 2015 election

library(dplyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(broom)

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans the 2015/2016 event window
# --------------------------------------------------

judge_span2015 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    
    # For the 2015 election, I recommend treating 2016 as the first post-election year.
    # This means 2015 is the final pre-treatment/reference year.
    spans_2015 = as.integer(start_year <= 2016 & end_year >= 2016)
  )

# --------------------------------------------------
# 2. Keep dyads where both judges span the event and observations are in 2013-2018
# --------------------------------------------------

window_start <- 2013
window_end <- 2018
event_year <- 2016

dyads_span2015 <- dyads %>%
  mutate(year_r = as.integer(year_r)) %>%
  left_join(
    judge_span2015 %>%
      select(id_juez, start_year, end_year, spans_2015) %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_2015_1 = spans_2015
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span2015 %>%
      select(id_juez, start_year, end_year, spans_2015) %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_2015_2 = spans_2015
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_2015_1 == 1,
    spans_2015_2 == 1,
    year_r >= window_start,
    year_r <= window_end
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    event_time = year_r - event_year,
    mixed_pair = as.integer(pair_ideology == "Divergent"),
    ingroup_pair = as.integer(pair_ideology == "Convergent"),
    ambiguous_pair = as.integer(is.na(pair_ideology) | pair_ideology == "NA"),
    pair_id = paste(pmin(judge1, judge2), pmax(judge1, judge2), sep = "_")
  )


# --------------------------------------------------
# 3. Main dynamic interaction model
# --------------------------------------------------
# Reference period: event_time = -1, i.e. 2015.
# Positive coefficients mean that mixed-pair disagreement is higher than in 2015,
# relative to in-group pairs.

m_dyn_2015 <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) |
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2015)

m_dyn_2015_b <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2015_b)

m_dyn_2015_c <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id + factor(year_r),
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2015_c)


m_dyn_2015_d <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
  total_cases + factor(procedure) + frac_minority|
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2015_d)

# --------------------------------------------------
# 4. Plot dynamic coefficients
# --------------------------------------------------

iplot(
  m_dyn_2015_d,
  xlab = "Years relative to first post-election year, 2016",
  ylab = "Mixed-pair disagreement relative to 2015",
  main = "Dynamic mixed-pair disagreement around the 2015 election"
)

library(ggtext)


coef_dyn_2015 <- broom::tidy(m_dyn_2015_d, conf.int = TRUE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    event_time = as.integer(gsub(".*event_time::(-?\\d+):mixed_pair", "\\1", term))
  )

ggplot(coef_dyn_2015, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = -0.04, linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(
    breaks = sort(unique(coef_dyn_2015$event_time))
  ) +
  labs(
    x = "Event time",
    y = "Mixed-pair disagreement relative to *t* = -1 (2015)",
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown()
    
  )

ggsave(
  filename = "dynamic_mixed_pair_disagreement_2015.pdf",
  width = 6.5,
  height = 3.8
)

etable(
  m_dyn_2015_b,
  m_dyn_2015_c,
  m_dyn_2015_d,
  tex = TRUE,
  file = "regression_table_dynamic_2015_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  dict = c(
    "total_cases" = "Caseload",
    "factor(procedure)" = "Procedure",
    "event_time::-3:mixed_pair" = "Mixed $\\times$ Event time -3",
    "event_time::-2:mixed_pair" = "Mixed $\\times$ Event time -2",
    "event_time::0:mixed_pair" = "Mixed $\\times$ Event time 0",
    "event_time::1:mixed_pair" = "Mixed $\\times$ Event time +1",
    "event_time::2:mixed_pair" = "Mixed $\\times$ Event time +2"
  ),
  order = c(
    "event_time::-3:mixed_pair",
    "event_time::-2:mixed_pair",
    "event_time::0:mixed_pair",
    "event_time::1:mixed_pair",
    "event_time::2:mixed_pair",
    "total_cases"
  ),
  fixef.group = list(
    "Judge-pair FE" = "pair_id",
    "Case FE" = "ID_PAT",
    "Year FE" = "year_r"
  )
)

# --------------------------------------------------
# 5. Descriptive plot: mean disagreement by pair type and year
# --------------------------------------------------

plot_data <- dyads_span2015 %>%
  filter(
    year_r >= window_start,
    year_r <= window_end,
    !is.na(pair_ideology)
  ) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(disagreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

p_mean_2015 <- ggplot(
  plot_data,
  aes(
    x = year_r,
    y = mean_disagreement,
    colour = pair_ideology,
    group = pair_ideology
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = 2016, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    breaks = window_start:window_end
  ) +
  scale_y_continuous(
    limits = c(0, 0.5)
  ) +
  labs(
    x = "",
    y = "Proportion disagreement",
    colour = "Pair composition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "Convergent" = "steelblue",
      "Divergent" = "firebrick",
      "NA" = "grey"
    ),
    labels = c(
      "Convergent" = "In-group (PP-PP,\nPSOE-PSOE)",
      "Divergent" = "Mixed (PSOE-PP,\nPP-PSOE)",
      "NA" = "Ambiguous"
    )
  )

ggsave(
  filename = "proportion_disagreement_pair_dynamic_2015.pdf",
  plot = p_mean_2015,
  width = 6.5,
  height = 3.5
)

etable(
  m_1, m_2, m_3, m_a, m_b,
  tex = TRUE,
  file = "regression_table_2015_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Divergent",
    "pair_ideologyConvergent" = "Convergent",
    "pair_ideologyNA" = "Ambivalent",
    "N_abs_distance" = "Distance Appointing Parties",
    "post_2015" = "Post-2015",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:post_2015" = "Divergent $\\times$ Post-2015",
    "pair_ideologyConvergent:post_2015" = "Convergent $\\times$ Post-2015",
    "pair_ideologyNA:post_2015" = "Ambivalent $\\times$ Post-2015"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:post_2015",
    "pair_ideologyConvergent:post_2015",
    "pair_ideologyNA:post_2015" = "Ambivalent $\\times$ Post-2015",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "factor(year_r)")
)



# Dyadic disagreement minority size quasi-experiment

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 1998
# --------------------------------------------------
judge_span1998 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_1998 = if_else(start_year < 1998 & end_year >= 1998, 1L, 0L)
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 1996
# --------------------------------------------------

window_start <- 1995
window_end <- 2000

dyads_span1998 <- dyads %>%
  left_join(
    judge_span1998 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_1998_1 = spans_1998
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1998 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_1998_2 = spans_1998
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_1998_1 == 1,
    spans_1998_2 == 1,
    dplyr::between(year_r, window_start, window_end)
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    post_1998 = as.integer(year_r >= 1998)
  )



m_a <- feols(
  agreement ~ pair_ideology*post_1998,
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)

m_b <- feols(
  agreement ~ pair_ideology*post_1998 +
    total_cases + as.factor(procedure),
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_b)

m_c <- feols(
  agreement ~ pair_ideology*post_1998 +
    total_cases + as.factor(procedure) | as.factor(year_r),
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_c)

m_d <- feols(
  agreement ~ pair_ideology*post_1998 +
    total_cases + as.factor(procedure)|as.factor(year_r) + ID_PAT,
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_d)

etable(
  m_a, m_b, m_c, m_d,
  tex = TRUE,
  file = "regression_table_minority1998.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "Convergent Pair",
    "post_1998" = "Post-1998",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_1998" = "Mixed $\\times$ Post-1998",
    "pair_ideologyConvergent:post_1998" = "In-group $\\times$ Post-1998"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "post_1998",
    "total_cases",
    "pair_ideologyDivergent:post_1998",
    "pair_ideologyConvergent:post_1998",
    "Constant"
  ),
  
  fixef.group = list("Case fixed effects" = "ID_PAT")
)


etable(
  m_1, m_2, m_3, m_4,
  tex = TRUE,
  file = "regression_table_minority1998.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  headers = c("Model A", "Model B", "Model C", "Model D"),
  
  dict = c(
    "pair_ideologyDivergent" = "Divergent Pair",
    "pair_ideologyConvergent" = "Convergent Pair",
    "post_1998" = "Post-1998",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_1998" = "Divergent $\\times$ Post-1998",
    "pair_ideologyConvergent:post_1998" = "Convergent $\\times$ Post-1998"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "post_1998",
    "total_cases",
    "pair_ideologyDivergent:post_1998",
    "pair_ideologyConvergent:post_1998",
    "Constant"
  ),
  
  fixef.group = list("Case fixed effects" = "ID_PAT")
)

# Dynamic Event Window Analysis: 1998 Composition Shift

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 1998
# --------------------------------------------------

judge_span1998 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_1998 = as.integer(start_year <= 1998 & end_year >= 1998)
  ) %>%
  group_by(id_juez) %>%
  summarise(
    Juez_Voto = first(Juez_Voto),
    start_year = min(start_year, na.rm = TRUE),
    end_year = max(end_year, na.rm = TRUE),
    spans_1998 = as.integer(any(spans_1998 == 1)),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep dyads where both judges span 1998 and observations are in 1993-2002
# --------------------------------------------------

window_start <- 1993
window_end <- 2002
event_year <- 1998

dyads_span1998 <- dyads %>%
  mutate(year_r = as.integer(year_r)) %>%
  left_join(
    judge_span1998 %>%
      select(id_juez, start_year, end_year, spans_1998) %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_1998_1 = spans_1998
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1998 %>%
      select(id_juez, start_year, end_year, spans_1998) %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_1998_2 = spans_1998
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_1998_1 == 1,
    spans_1998_2 == 1,
    year_r >= window_start,
    year_r <= window_end
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    event_time = year_r - event_year,
    mixed_pair = as.integer(pair_ideology == "Divergent"),
    ingroup_pair = as.integer(pair_ideology == "Convergent"),
    ambiguous_pair = as.integer(is.na(pair_ideology) | pair_ideology == "NA"),
    pair_id = paste(pmin(judge1, judge2), pmax(judge1, judge2), sep = "_")
  )

# --------------------------------------------------
# 3. Dynamic interaction models
# --------------------------------------------------

m_dyn_1998_b <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id,
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1998_b)

m_dyn_1998_c <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id + factor(year_r),
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1998_c)

m_dyn_1998_d <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id + factor(year_r),
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1998_d)

m_dyn_1998_e <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span1998,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1998_e)

# --------------------------------------------------
# 4. Dynamic coefficient plot
# --------------------------------------------------

coef_dyn_1998 <- broom::tidy(m_dyn_1998_e, conf.int = TRUE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    event_time = as.integer(
      gsub(".*event_time::(-?\\d+):mixed_pair", "\\1", term)
    )
  )

p_dyn_1998 <- ggplot(coef_dyn_1998, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = -0.02, linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(
    breaks = sort(unique(coef_dyn_1998$event_time))
  ) +
  labs(
    x = "Event time",
    y = "Mixed-pair disagreement relative to *t* = -1 (1997)",
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown()
  )

print(p_dyn_1998)

ggsave(
  filename = "dynamic_mixed_pair_disagreement_1998.pdf",
  plot = p_dyn_1998,
  width = 6.5,
  height = 3.8
)

# --------------------------------------------------
# 5. Raw disagreement plot
# --------------------------------------------------

plot_data_1998 <- dyads_span1998 %>%
  filter(!is.na(pair_ideology)) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(disagreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

p_raw_1998 <- ggplot(
  plot_data_1998,
  aes(
    x = year_r,
    y = mean_disagreement,
    colour = pair_ideology,
    group = pair_ideology
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = event_year, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    breaks = window_start:window_end
  ) +
  scale_y_continuous(
    limits = c(0, max(0.5, max(plot_data_1998$mean_disagreement, na.rm = TRUE)))
  ) +
  labs(
    x = "",
    y = "Proportion disagreement",
    colour = "Pair composition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "Convergent" = "steelblue",
      "Divergent" = "firebrick",
      "NA" = "grey"
    ),
    labels = c(
      "Convergent" = "In-group (PP-PP,\nPSOE-PSOE)",
      "Divergent" = "Mixed (PSOE-PP,\nPP-PSOE)",
      "NA" = "Ambiguous"
    )
  )

print(p_raw_1998)

ggsave(
  filename = "proportion_disagreement_pair_1998.pdf",
  plot = p_raw_1998,
  width = 6.5,
  height = 3.5
)

# --------------------------------------------------
# 6. Pre-trend Wald test
# --------------------------------------------------

wald(
  m_dyn_1998_d,
  keep = "event_time::\\-3|event_time::\\-2"
)

# Optional wider-window pre-trend test
wald(
  m_dyn_1998_d,
  keep = "event_time::\\-5|event_time::\\-4|event_time::\\-3|event_time::\\-2"
)

# --------------------------------------------------
# 7. Regression table
# --------------------------------------------------

etable(
  m_dyn_1998_b,
  m_dyn_1998_c,
  m_dyn_1998_d,
  m_dyn_1998_e,
  tex = TRUE,
  file = "regression_table_dynamic_1998_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  dict = c(
    "total_cases" = "Caseload",
    "frac_minority" = "Minority share",
    "factor(procedure)" = "Procedure",
    "event_time::-5:mixed_pair" = "Mixed $\\times$ Event time -5",
    "event_time::-4:mixed_pair" = "Mixed $\\times$ Event time -4",
    "event_time::-3:mixed_pair" = "Mixed $\\times$ Event time -3",
    "event_time::-2:mixed_pair" = "Mixed $\\times$ Event time -2",
    "event_time::0:mixed_pair" = "Mixed $\\times$ Event time 0",
    "event_time::1:mixed_pair" = "Mixed $\\times$ Event time +1",
    "event_time::2:mixed_pair" = "Mixed $\\times$ Event time +2",
    "event_time::3:mixed_pair" = "Mixed $\\times$ Event time +3",
    "event_time::4:mixed_pair" = "Mixed $\\times$ Event time +4"
  ),
  order = c(
    "event_time::-5:mixed_pair",
    "event_time::-4:mixed_pair",
    "event_time::-3:mixed_pair",
    "event_time::-2:mixed_pair",
    "event_time::0:mixed_pair",
    "event_time::1:mixed_pair",
    "event_time::2:mixed_pair",
    "event_time::3:mixed_pair",
    "event_time::4:mixed_pair",
    "total_cases",
    "frac_minority"
  ),
  fixef.group = list(
    "Judge-pair FE" = "pair_id",
    "Case FE" = "ID_PAT",
    "Year FE" = "year_r"
  )
)

# Dyadic disagreement minority size pre/post1986

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 1986
# --------------------------------------------------
judge_span1986 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_1986 = if_else(start_year < 1986 & end_year >= 1986, 1L, 0L)
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 1996
# --------------------------------------------------

window_start <- 1983
window_end <- 1988

dyads_span1986 <- dyads %>%
  left_join(
    judge_span1986 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_1986_1 = spans_1986
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1986 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_1986_2 = spans_1986
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_1986_1 == 1,
    spans_1986_2 == 1,
    dplyr::between(year_r, window_start, window_end)
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    post_1986 = as.integer(year_r >= 1986)
  )



m_a <- feols(
  agreement ~ pair_ideology*post_1986,
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)

m_b <- feols(
  agreement ~ pair_ideology*post_1986 +
    total_cases + as.factor(procedure),
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_b)

m_c <- feols(
  agreement ~ pair_ideology*post_1986 +
    total_cases + as.factor(procedure) | as.factor(year_r),
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_c)

m_d <- feols(
  agreement ~ pair_ideology*post_1986 +
    total_cases + as.factor(procedure)|as.factor(year_r) + ID_PAT,
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_d)

etable(
  m_a, m_b, m_c, m_d,
  tex = TRUE,
  file = "regression_table_minority1986.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "Convergent Pair",
    "post_1986" = "Post-1986",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_1986" = "Mixed $\\times$ Post-1986",
    "pair_ideologyConvergent:post_1986" = "In-group $\\times$ Post-1986"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "post_1986",
    "total_cases",
    "pair_ideologyDivergent:post_1986",
    "pair_ideologyConvergent:post_1986",
    "Constant"
  ),
  
  fixef.group = list("Case fixed effects" = "ID_PAT")
)

# Dynamic Event Window Analysis

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 1986
# --------------------------------------------------

judge_span1986 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_1986 = as.integer(start_year <= 1986 & end_year >= 1986)
  ) %>%
  group_by(id_juez) %>%
  summarise(
    Juez_Voto = first(Juez_Voto),
    start_year = min(start_year, na.rm = TRUE),
    end_year = max(end_year, na.rm = TRUE),
    spans_1986 = as.integer(any(spans_1986 == 1)),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep dyads where both judges span 1986 and observations are in 1983-1988
# --------------------------------------------------

window_start <- 1981
window_end <- 1990
event_year <- 1986

dyads_span1986 <- dyads %>%
  mutate(year_r = as.integer(year_r)) %>%
  left_join(
    judge_span1986 %>%
      select(id_juez, start_year, end_year, spans_1986) %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_1986_1 = spans_1986
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1986 %>%
      select(id_juez, start_year, end_year, spans_1986) %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_1986_2 = spans_1986
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_1986_1 == 1,
    spans_1986_2 == 1,
    year_r >= window_start,
    year_r <= window_end
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    event_time = year_r - event_year,
    mixed_pair = as.integer(pair_ideology == "Divergent"),
    ingroup_pair = as.integer(pair_ideology == "Convergent"),
    ambiguous_pair = as.integer(is.na(pair_ideology) | pair_ideology == "NA"),
    pair_id = paste(pmin(judge1, judge2), pmax(judge1, judge2), sep = "_")
  )

# --------------------------------------------------
# 3. Dynamic interaction models
# --------------------------------------------------

m_dyn_1986_b <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id,
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1986_b)

m_dyn_1986_c <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id + factor(year_r),
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1986_c)

m_dyn_1986_d <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id + factor(year_r),
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1986_d)

m_dyn_1986_e <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) |
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span1986,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_1986_e)

# --------------------------------------------------
# 4. Dynamic coefficient plot
# --------------------------------------------------

coef_dyn_1986 <- broom::tidy(m_dyn_1986_e, conf.int = TRUE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    event_time = as.integer(
      gsub(".*event_time::(-?\\d+):mixed_pair", "\\1", term)
    )
  )

p_dyn_1986 <- ggplot(coef_dyn_1986, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = -0.02, linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(
    breaks = sort(unique(coef_dyn_1986$event_time))
  ) +
  labs(
    x = "Event time",
    y = "Mixed-pair disagreement relative to *t* = -1 (1985)",
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown()
  )

print(p_dyn_1986)

ggsave(
  filename = "dynamic_mixed_pair_disagreement_1986.pdf",
  plot = p_dyn_1986,
  width = 6.5,
  height = 3.8
)

# --------------------------------------------------
# 5. Raw disagreement plot
# --------------------------------------------------

plot_data_1986 <- dyads_span1986 %>%
  filter(!is.na(pair_ideology)) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(disagreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

p_raw_1986 <- ggplot(
  plot_data_1986,
  aes(
    x = year_r,
    y = mean_disagreement,
    colour = pair_ideology,
    group = pair_ideology
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_vline(xintercept = event_year, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(
    breaks = window_start:window_end
  ) +
  scale_y_continuous(
    limits = c(0, max(0.5, max(plot_data_1986$mean_disagreement, na.rm = TRUE)))
  ) +
  labs(
    x = "",
    y = "Proportion disagreement",
    colour = "Pair composition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "Convergent" = "steelblue",
      "Divergent" = "firebrick",
      "NA" = "grey"
    ),
    labels = c(
      "Convergent" = "In-group (Con-Con,\nPro-Pro)",
      "Divergent" = "Mixed (Pro-Con,\nCon-Pro)",
      "NA" = "Ambiguous"
    )
  )

print(p_raw_1986)

ggsave(
  filename = "proportion_disagreement_pair_1986.pdf",
  plot = p_raw_1986,
  width = 6.5,
  height = 3.5
)

# --------------------------------------------------
# 6. Pre-trend Wald test
# --------------------------------------------------

wald(
  m_dyn_1986_d,
  keep = "event_time::\\-3|event_time::\\-2"
)

# --------------------------------------------------
# 7. Regression table
# --------------------------------------------------

etable(
  m_dyn_1986_b,
  m_dyn_1986_c,
  m_dyn_1986_d,
  m_dyn_1986_e,
  tex = TRUE,
  file = "regression_table_dynamic_1986_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  dict = c(
    "total_cases" = "Caseload",
    "frac_minority" = "Minority share",
    "factor(procedure)" = "Procedure",
    "event_time::-5:mixed_pair" = "Mixed $\\times$ Event time -5",
    "event_time::-4:mixed_pair" = "Mixed $\\times$ Event time -4",
    "event_time::-3:mixed_pair" = "Mixed $\\times$ Event time -3",
    "event_time::-2:mixed_pair" = "Mixed $\\times$ Event time -2",
    "event_time::0:mixed_pair" = "Mixed $\\times$ Event time 0",
    "event_time::1:mixed_pair" = "Mixed $\\times$ Event time +1",
    "event_time::2:mixed_pair" = "Mixed $\\times$ Event time +2",
    "event_time::3:mixed_pair" = "Mixed $\\times$ Event time +3",
    "event_time::4:mixed_pair" = "Mixed $\\times$ Event time +4"
  ),
  order = c(
    "event_time::-3:mixed_pair",
    "event_time::-2:mixed_pair",
    "event_time::0:mixed_pair",
    "event_time::1:mixed_pair",
    "event_time::2:mixed_pair",
    "total_cases",
    "frac_minority"
  ),
  fixef.group = list(
    "Judge-pair FE" = "pair_id",
    "Case FE" = "ID_PAT",
    "Year FE" = "year_r"
  )
)





# Pre-post 2010 Interaction Judges with tenure spanning 2010 -----------------------------------------------------------

library(dplyr)
library(ggplot2)

judge_span2010 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_2010 = if_else(start_year <= 2009 & end_year >= 2010, 1L, 0L)
  )

window_start <- 2007
window_end <- 2012

dyads_span2010 <- dyads %>%
  left_join(
    judge_span2010 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_2010_1 = spans_2010
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span2010 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_2010_2 = spans_2010
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_2010_1 == 1,
    spans_2010_2 == 1,
    dplyr::between(year_r, window_start, window_end)
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    post_2010 = as.integer(year_r >= 2010)
  )


plot_data <- dyads_span2010 %>%
  filter(year_r >= window_start,
         year_r <= window_end) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(1 - agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

pdf("proportion_disagreement_pair_post_2010.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(plot_data, aes(x = year_r, y = mean_disagreement,
                            colour = pair_ideology, group = pair_ideology)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.8) +
        geom_vline(xintercept = 2009.98, linetype = "dashed", linewidth = 0.6) +
        scale_x_continuous(
          limits = range(plot_data$year_r, na.rm = TRUE),
          breaks = scales::pretty_breaks()
        ) +
        scale_y_continuous(
          #labels = scales::percent_format(accuracy = 1),
          limits = c(0, 0.4)
        ) +
        labs(
          x = "",
          y = "Proportion Disagreement",
          colour = "Pair composition",
          title = "",
          subtitle = ""
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        ) +
        scale_color_manual(values = c("Convergent" = "steelblue", 
                                      "Divergent" = "firebrick", "NA" = "grey"),
                           labels = c("Convergent" = "In-group (PP-PP, \nPSOE-PSOE)", 
                                      "Divergent" = "Mixed (PSOE-PP, \nPP-PSOE)",
                                      "NA" = "Ambiguous"),)
)
dev.off()

# Model estimation


m_0 <- feols(
  agreement ~ pair_ideology*post_2010,
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_0)


m_1 <- feols(
  agreement ~ pair_ideology*post_2010 +
    total_cases + as.factor(procedure),
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_1)


m_2 <- feols(
  agreement ~ pair_ideology*post_2010+
    total_cases + as.factor(procedure)
  | factor(year_r),
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_2)

m_3 <- feols(
  agreement ~ pair_ideology*post_2010+
    total_cases + as.factor(procedure)
  | ID_PAT + factor(year_r),
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_3)




library(fixest)
etable(
  m_0, m_1, m_2, m_3,
  tex = TRUE,
  file = "regression_post2009_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "post_2010" = "Post-2009",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_2010" = "Mixed $\\times$ Post-2009",
    "pair_ideologyConvergent:post_2010" = "In-group $\\times$ Post-2009"
  ),
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)
# Re-estimation with logit

m_2 <- feglm(
  agreement ~ N_abs_distance*post_2010 | ID_PAT + factor(year_r),
  data = dyads_span2010, family = binomial(link = "logit"),
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_2)

# pre-post 2010 with judge FE

dyads_10 <- dyads %>%
  filter(year_r >= 207, year_r <= 2012) %>%
  mutate(
    agreement = agreement,
    post_2010 = if_else(year_r >= 2010, 1L, 0L),
    pair_ideology = factor(pair_ideology,
                           levels = c("Convergent", "Divergent", "NA"))
  )

plot_data <- dyads_10 %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    n = n(),
    mean_disagreement = mean(1 - agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_r, pair_ideology)

pdf("proportion_disagreement_pair_post_2010.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(plot_data, aes(x = year_r, y = mean_disagreement,
                            colour = pair_ideology, group = pair_ideology)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.8) +
        geom_vline(xintercept = 2009.98, linetype = "dashed", linewidth = 0.6) +
        scale_x_continuous(
          limits = range(plot_data$year_r, na.rm = TRUE),
          breaks = scales::pretty_breaks()
        ) +
        scale_y_continuous(
          #labels = scales::percent_format(accuracy = 1),
          limits = c(0, 0.4)
        ) +
        labs(
          x = "",
          y = "Proportion Disagreement",
          colour = "Pair composition",
          title = "",
          subtitle = ""
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        ) +
        scale_color_manual(values = c("Convergent" = "steelblue", 
                                      "Divergent" = "firebrick", "NA" = "grey"),
                           labels = c("Convergent" = "In-group (PP-PP, \nPSOE-PSOE)", 
                                      "Divergent" = "Mixed (PSOE-PP, \nPP-PSOE)",
                                      "NA" = "Ambiguous"),)
)
dev.off()

m_a <- feols(
  agreement ~ pair_ideology*post_2010 + as.factor(procedure)
  |judge1 + judge2 + ID_PAT + as.factor(year_r),
  data = dyads_10,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)

m_a <- feols(
  agreement ~ pair_ideology*post_2010
  |judge1 + judge2 + ID_PAT + as.factor(year_r),
  data = dyads_10,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)


# Dyadic disagreement dynamic event-window analysis, 2010 Catalonia ruling

library(dplyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(broom)

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 2010
# --------------------------------------------------

judge_span2010 <- data %>%
  select(Juez_Voto, id_juez, Appointment_date, End_in_court) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    End_in_court = dmy(End_in_court),
    start_year = year(Appointment_date),
    end_year = if_else(is.na(End_in_court), 9999L, year(End_in_court)),
    spans_2010 = as.integer(start_year <= 2010 & end_year >= 2010)
  ) %>%
  group_by(id_juez) %>%
  summarise(
    Juez_Voto = first(Juez_Voto),
    start_year = min(start_year, na.rm = TRUE),
    end_year = max(end_year, na.rm = TRUE),
    spans_2010 = as.integer(any(spans_2010 == 1)),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep dyads where both judges span 2010 and observations are in 2007-2012
# --------------------------------------------------

window_start <- 2007
window_end <- 2012
event_year <- 2010

dyads_span2010 <- dyads %>%
  mutate(year_r = as.integer(year_r)) %>%
  left_join(
    judge_span2010 %>%
      select(id_juez, start_year, end_year, spans_2010) %>%
      rename(
        judge1 = id_juez,
        first_year_1 = start_year,
        last_year_1 = end_year,
        spans_2010_1 = spans_2010
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span2010 %>%
      select(id_juez, start_year, end_year, spans_2010) %>%
      rename(
        judge2 = id_juez,
        first_year_2 = start_year,
        last_year_2 = end_year,
        spans_2010_2 = spans_2010
      ),
    by = "judge2"
  ) %>%
  filter(
    spans_2010_1 == 1,
    spans_2010_2 == 1,
    year_r >= window_start,
    year_r <= window_end
  ) %>%
  mutate(
    disagreement = 1 - agreement,
    event_time = year_r - event_year,
    mixed_pair = as.integer(pair_ideology == "Divergent"),
    ingroup_pair = as.integer(pair_ideology == "Convergent"),
    ambiguous_pair = as.integer(is.na(pair_ideology) | pair_ideology == "NA"),
    pair_id = paste(pmin(judge1, judge2), pmax(judge1, judge2), sep = "_")
  )

# --------------------------------------------------
# 3. Main dynamic interaction model
# --------------------------------------------------
# Reference period: event_time = -1, i.e. 2009.
# Positive coefficients mean mixed-pair disagreement is higher than in 2009,
# relative to in-group pairs.

m_dyn_2010 <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) |
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2010)

m_dyn_2010_b <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id,
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2010_b)

m_dyn_2010_c <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1) +
    total_cases + factor(procedure) + frac_minority |
    pair_id + factor(year_r),
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2010_c)

m_dyn_2010_d <- feols(
  disagreement ~ i(event_time, mixed_pair, ref = -1)+
    total_cases + factor(procedure) + frac_minority |
    pair_id + factor(year_r) + ID_PAT,
  data = dyads_span2010,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_dyn_2010_d)

iplot(
  m_dyn_2010_d,
  xlab = "Years relative to 2010 ruling",
  ylab = "Mixed-pair disagreement relative to 2009",
  main = "Dynamic mixed-pair disagreement around the 2010 Catalonia ruling"
)

library(ggtext)

coef_dyn_2010 <- broom::tidy(m_dyn_2010_d, conf.int = TRUE) %>%
  filter(grepl("event_time::", term)) %>%
  mutate(
    event_time = as.integer(gsub(".*event_time::(-?\\d+):mixed_pair", "\\1", term))
  )

p_dyn_2010 <- ggplot(coef_dyn_2010, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = -0.02, linetype = "dashed", linewidth = 0.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(
    breaks = sort(unique(coef_dyn_2010$event_time))
  ) +
  labs(
    x = "Event time",
    y = "Mixed-pair disagreement relative to *t* = -1 (2009)",
    title = "",
    subtitle = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_markdown()
  )

print(p_dyn_2010)

ggsave(
  filename = "dynamic_mixed_pair_disagreement_2010.pdf",
  plot = p_dyn_2010,
  width = 6.5,
  height = 3.8
)

etable(
  m_dyn_2010_b,
  m_dyn_2010_c,
  m_dyn_2010_d,
  tex = TRUE,
  file = "regression_table_dynamic_2010_same_judges.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  dict = c(
    "total_cases" = "Caseload",
    "factor(procedure)" = "Procedure",
    "event_time::-3:mixed_pair" = "Mixed $\\times$ Event time -3",
    "event_time::-2:mixed_pair" = "Mixed $\\times$ Event time -2",
    "event_time::0:mixed_pair" = "Mixed $\\times$ Event time 0",
    "event_time::1:mixed_pair" = "Mixed $\\times$ Event time +1",
    "event_time::2:mixed_pair" = "Mixed $\\times$ Event time +2"
  ),
  order = c(
    "event_time::-3:mixed_pair",
    "event_time::-2:mixed_pair",
    "event_time::0:mixed_pair",
    "event_time::1:mixed_pair",
    "event_time::2:mixed_pair",
    "total_cases"
  ),
  fixef.group = list(
    "Judge-pair FE" = "pair_id",
    "Case FE" = "ID_PAT",
    "Year FE" = "year_r"
  )
)

# mean disagreement by ideology and period

dyads_span2015 %>%
  group_by(post_2015, pair_ideology) %>%
  summarise(
    n = n(),
    mean_agreement = mean(agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(post_2015, pair_ideology)

# Stronger comparison: only pairs observed on both sides of 2015

dyads_span2015 <- dyads_span2015 %>%
  mutate(
    dyad_id = if_else(
      judge1 <= judge2,
      paste(judge1, judge2, sep = "_"),
      paste(judge2, judge1, sep = "_")
    )
  )

dyads_both_periods <- dyads_span2015 %>%
  group_by(dyad_id) %>%
  mutate(
    appears_pre = as.integer(any(year_r < 2015)),
    appears_post = as.integer(any(year_r >= 2015))
  ) %>%
  ungroup() %>%
  filter(appears_pre == 1, appears_post == 1)

m_0 <- feols(
  agreement ~ pair_ideology | ID_PAT,
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_0)


m_0 <- feols(
  agreement ~ pair_ideology | ID_PAT + factor(year_r),
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_0)

m_a <- feols(
  agreement ~ pair_ideology * post_2015 | factor(year_r),
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_a)

m_b <- feols(
  agreement ~ pair_ideology * post_2015 +
    total_cases,
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_b)

m_c <- feols(
  agreement ~ pair_ideology * post_2015 +
    total_cases + factor(procedure),
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_c)

m_d <- feols(
  agreement ~ pair_ideology * post_2015
  | ID_PAT,
  data = dyads_both_periods,
  vcov = ~ judge1 + judge2
)

summary(m_d)

library(fixest)
etable(
  m_0, m_a, m_b, m_c, m_d,
  tex = TRUE,
  file = "regression_post2015.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
  fitstat = ~ n + r2 + ar2,
  headers = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  dict = c(
    "pair_ideologyDivergent" = "Divergent",
    "pair_ideologyConvergent" = "Convergent",
    "pair_ideologyNA" = "Ambiguous",
    "post_2015" = "Post-2015",
    "total_cases" = "Caseload",
    "pair_ideologyDivergent:post_2015" = "Divergent $\\times$ Post-2015",
    "pair_ideologyConvergent:post_2015" = "Convergent $\\times$ Post-2015"
  ),
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "factor(year_r)")
)



# Polarization index, association with pairwise agreement

PI <- read.csv("db_polarindex.csv") %>% select(year,polarindex)

dyads2 <- merge(dyads, PI, by.x = "year_r", by.y = "year", all.x = T)


m1a <- feols(agreement ~ polarindex, 
            data = dyads2)
summary (m1a)


# Polarization index, association with dissent


data4 <- merge(data, PI, by.x = "year_r", by.y = "year", all.x = T)
   
m1a <- feols(Type_Vote ~ polarindex, 
             data = data4)
summary (m1a)

m1a <- feglm(Type_Vote ~ polarindex, 
             data = data4,
             family = binomial(link = "logit"))
summary (m1a)


# tables
# Define your dictionary without the 'drop' key, as it's not applicable here
dict <- c("(Intercept)" = "Constant", 
          N_abs_distance = "Distance App' Parties", 
          ID_PAT = "Case FE", agreement = "Pairwise Agreement",
          year_r = "Year", "as.factor(year_r)" = "Year FE", 
          judge1 = "Judge FE",
          note1 = dsb("*Notes*: This is a note that illustrates how to access notes ",
                      "from the dictionary."))

setFixest_dict(dict)

# Define the style for etable, assuming 'style.tex("qje")' meets your needs besides size
my_style = style.tex("base", model.format = "(1)")
setFixest_etable(style.tex = my_style)

# Generate etable, explicitly dropping 'judge2'
etable(list(m1, m2, m3, m4, m5, m6, m7), dict = dict, tex = TRUE, drop = "judge2", 
         page.width = "13cm")


ma1 <- glm(agreement ~ N_abs_distance + year_r, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma1)


ma2 <- glm(agreement ~ N_abs_distance + year_r + total_cases, 
            data = dyads, family = binomial(link = "logit"
            ))
summary (ma2)

ma3 <- glm(agreement ~ N_abs_distance + year_r + 
             total_cases + procedure, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma3)

ma4 <- glm(agreement ~ N_abs_distance + year_r + 
             total_cases + procedure + N_abs_distance*year_r, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma4)

ma5 <- glm(agreement ~ N_abs_distance + year_r + 
             total_cases + procedure + as.factor(year_r) + 
             N_abs_distance*year_r, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma5)

ma6 <- glm(agreement ~ N_abs_distance + year_r + 
             total_cases + procedure + as.factor(year_r) + 
             judge1 + judge2 +
             N_abs_distance*year_r, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma6)

ma7 <- feglm(agreement ~ N_abs_distance + year_r + 
             total_cases + procedure +
             ID_PAT +
             N_abs_distance*year_r, 
           data = dyads, family = binomial(link = "logit"
           ))
summary (ma7)


# tables
library(stargazer)
stargazer(ma1,ma2,ma3, ma4, ma5, ma6,
  type = "latex", digits = 4, digits.extra = 3,
          omit=c("as\\.factor\\(year_r\\)", "judge1", "judge2"), title="",
          model.names = F,covariate.labels=c("Distance Appointing Party",
                                             "Year",
                                             "Caseload",
                                             "Concrete Review",
                                             "Distance Appointing Party*Year"),
          dep.var.labels="Agreement (1 = Yes, 0 = No)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          add.lines = list(
                           c("Year FE","No", "No","No","No","Yes", "Yes"),
                           c("Judge FE","No", "No","No","No","No", "Yes")),
          style="default")



##### plot proportion conservative and progressive judges

data_unique <- data %>%
  distinct(ID_PAT, id_juez, year_r, Ideology) %>%
  filter(Ideology %in% c("P", "C"))  # Keep only progressive and conservative

# Step 2: Compute yearly proportions
ideology_by_year <- data_unique %>%
  group_by(year_r, Ideology) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year_r) %>%
  mutate(prop = n / sum(n))

# Step 3: Create scatter plot with trend line
ggplot(ideology_by_year, aes(x = year_r, y = prop, color = Ideology)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # fitting curve
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Progressive and Conservative Judges Over Time",
    x = "Year",
    y = "Proportion of Judges",
    color = "Ideology"
  ) +
  theme_minimal()


data_unique <- data %>%
  distinct(ID_PAT, id_juez, year_r, Ideology) %>%
  filter(Ideology %in% c("P", "C"))  # Keep only progressive and conservative

# Step 2: Compute yearly proportions
ideology_by_panel <- data_unique %>%
  group_by(ID_PAT, Ideology) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ID_PAT) %>%
  mutate(prop = n / sum(n))

# Step 3: Create scatter plot with trend line
ggplot(ideology_by_panel, aes(x = year_r, y = prop, color = Ideology)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # fitting curve
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Progressive and Conservative Judges Over Time",
    x = "Year",
    y = "Proportion of Judges",
    color = "Ideology"
  ) +
  theme_minimal()


# Step 1: One row per judge per case (panel), keep only C and P
panel_data <- data %>%
  distinct(ID_PAT, id_juez, Ideology, year_r) %>%
  filter(Ideology %in% c("P", "C"))

# Step 2: Count number of C and P judges per panel
panel_ideology <- panel_data %>%
  group_by(ID_PAT, year_r, Ideology) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(ID_PAT, year_r) %>%
  mutate(panel_prop = n / sum(n)) %>%
  ungroup()

# Step 3: Plot one point per panel
pdf("panel_ideology.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(panel_ideology, aes(x = year_r, y = panel_prop, color = Ideology)) +
  #geom_point(alpha = 0.4, size = 1.8) +  # data points per panel
  geom_jitter(width = 0.2, height = 0.01, alpha = 0.4, size = 1.8) +  # jittered data points
  geom_smooth(method = "loess", se = FALSE, span = 0.4) +  # smooth trend
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
      values = c("P" = "#1f77b4", "C" = "#d62728"),  # Optional: custom colors
      labels = c("P" = "Progressive", "C" = "Conservative"),
      name = "Ideology"
    )+
  labs(
    title = "",
    x = "",
    y = "Proportion on Panel",
    color = "Ideology"
  ) +
  theme_minimal()
)
dev.off()


#### Ideology and Cohort Comparison

judge_cohorts <- data %>%
  select(id_juez, Appointment_date) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    app_year = year(Appointment_date),
    cohort_group = case_when(
      app_year < 1983 ~ "pre1983",
      app_year >= 1983 & app_year < 1996 ~ "1983-1995",
      app_year >= 1996 & app_year < 2005 ~ "1996-2004",
      app_year >= 2005 & app_year < 2012 ~ "2005-2011",
      app_year >= 2012 & app_year < 2018 ~ "2012-2017",
      app_year >= 2018 ~ "post2018",
      TRUE ~ NA_character_
    )
  )

dyads_cohorts <- dyads %>%
  left_join(
    judge_cohorts %>% rename(judge1 = id_juez, cohort_1 = cohort_group),
    by = "judge1"
  ) %>%
  left_join(
    judge_cohorts %>% rename(judge2 = id_juez, cohort_2 = cohort_group),
    by = "judge2"
  ) %>%
  mutate(
    agreement = agreement,
    cohort_pair = if_else(
      cohort_1 <= cohort_2,
      paste(cohort_1, cohort_2, sep = "_"),
      paste(cohort_2, cohort_1, sep = "_")
    )
  )

broad_cohort_summary <- dyads_cohorts %>%
  group_by(cohort_pair) %>%
  summarise(
    n_dyads = n(),
    mean_agreement = mean(agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_agreement))

broad_cohort_summary


# with confidence intervals

dyads_cohorts <- dyads %>%
  left_join(
    judge_cohorts %>% rename(judge1 = id_juez, cohort_1 = cohort_group),
    by = "judge1"
  ) %>%
  left_join(
    judge_cohorts %>% rename(judge2 = id_juez, cohort_2 = cohort_group),
    by = "judge2"
  ) %>%
  mutate(
    cohort_pair = if_else(
      cohort_1 <= cohort_2,
      paste(cohort_1, cohort_2, sep = "_"),
      paste(cohort_2, cohort_1, sep = "_")
    ),
    same_cohort = if_else(cohort_1 == cohort_2, "Within cohort", "Between cohorts")
  )

broad_cohort_summary <- dyads_cohorts %>%
  group_by(cohort_pair) %>%
  summarise(
    n_dyads = sum(!is.na(agreement)),
    mean_agreement = mean(agreement, na.rm = TRUE),
    se = sqrt(mean_agreement * (1 - mean_agreement) / n_dyads),
    ci_low = mean_agreement - 1.96 * se,
    ci_high = mean_agreement + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ci_low = pmax(0, ci_low),
    ci_high = pmin(1, ci_high)
  ) %>%
  arrange(desc(mean_agreement))

broad_cohort_summary

# confidence intervals for within and between cohorts
within_between_summary <- dyads_cohorts %>%
  group_by(same_cohort) %>%
  summarise(
    n_dyads = sum(!is.na(agreement)),
    mean_agreement = mean(agreement, na.rm = TRUE),
    se = sqrt(mean_agreement * (1 - mean_agreement) / n_dyads),
    ci_low = mean_agreement - 1.96 * se,
    ci_high = mean_agreement + 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(
    ci_low = pmax(0, ci_low),
    ci_high = pmin(1, ci_high)
  )

within_between_summary

# With Wilson intervals
library(binom)

broad_cohort_summary <- dyads_cohorts %>%
  group_by(cohort_pair) %>%
  summarise(
    successes = sum(agreement == 1, na.rm = TRUE),
    n_dyads = sum(!is.na(agreement)),
    mean_agreement = successes / n_dyads,
    .groups = "drop"
  ) %>%
  bind_cols(
    binom.confint(
      x = .$successes,
      n = .$n_dyads,
      methods = "wilson"
    ) %>%
      select(lower, upper)
  ) %>%
  rename(
    ci_low = lower,
    ci_high = upper
  ) %>%
  arrange(desc(mean_agreement))

broad_cohort_summary

library(knitr)
library(kableExtra)
library(dplyr)

broad_cohort_latex <- broad_cohort_summary %>%
  mutate(
    mean_agreement = round(mean_agreement, 3),
    ci = paste0("[", round(ci_low, 3), ", ", round(ci_high, 3), "]")
  ) %>%
  select(cohort_pair, n_dyads, mean_agreement, ci)

kable(
  broad_cohort_latex,
  format = "latex",
  booktabs = TRUE,
  caption = "Agreement across judicial cohort pairings",
  col.names = c("Cohort pair", "N dyads", "Mean agreement", "95\\% CI"),
  align = c("l", "r", "r", "l")
) %>%
  kable_styling(latex_options = "hold_position")

library(dplyr)
library(lubridate)
library(stringr)
library(fixest)

# --------------------------------------------------
# 1. Judge-level metadata: appointment year + broad cohort
# --------------------------------------------------
judge_info <- data %>%
  select(id_juez, Appointment_date) %>%
  distinct() %>%
  mutate(
    Appointment_date = dmy(Appointment_date),
    app_year = year(Appointment_date),
    cohort_group = case_when(
      app_year < 2012 ~ "pre2011",
      #app_year >= 1996 & app_year < 2005 ~ "1996-2004",
     # app_year >= 2005 & app_year < 2015 ~ "2005-2014",
      app_year >= 2012 ~ "post2011",
      TRUE ~ NA_character_
    )
  )

# --------------------------------------------------
# 2. Recover each judge's observed service window from the data
#    (first and last year observed in the case-level data)
# --------------------------------------------------
judge_service <- data %>%
  select(id_juez, year_r) %>%
  distinct() %>%
  group_by(id_juez) %>%
  summarise(
    first_year = min(year_r, na.rm = TRUE),
    last_year  = max(year_r, na.rm = TRUE),
    .groups = "drop"
  )

judge_info <- judge_info %>%
  left_join(judge_service, by = "id_juez")

# --------------------------------------------------
# 3. Join cohort/service info to dyads for both judges
# --------------------------------------------------
dyads2 <- dyads %>%
  left_join(
    judge_info %>%
      rename(
        judge1 = id_juez,
        app_year_1 = app_year,
        cohort_1 = cohort_group,
        first_year_1 = first_year,
        last_year_1 = last_year
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_info %>%
      rename(
        judge2 = id_juez,
        app_year_2 = app_year,
        cohort_2 = cohort_group,
        first_year_2 = first_year,
        last_year_2 = last_year
      ),
    by = "judge2"
  )

# --------------------------------------------------
# 4. Keep only years when both judges are serving
#    and create overlap/cohort variables
# --------------------------------------------------
dyads2 <- dyads2 %>%
  mutate(
    overlap_start = pmax(first_year_1, first_year_2, na.rm = TRUE),
    overlap_end   = pmin(last_year_1,  last_year_2,  na.rm = TRUE),
    overlap_pair  = if_else(!is.na(overlap_start) & !is.na(overlap_end) &
                              overlap_start <= overlap_end, 1L, 0L),
    observed_in_overlap = if_else(overlap_pair == 1L &
                                    year_r >= overlap_start &
                                    year_r <= overlap_end, 1L, 0L)
  ) %>%
  filter(observed_in_overlap == 1L)

# --------------------------------------------------
# 5. Create order-invariant broad cohort pair
# --------------------------------------------------
dyads2 <- dyads2 %>%
  mutate(
    cohort_pair = if_else(
      cohort_1 <= cohort_2,
      paste(cohort_1, cohort_2, sep = "_"),
      paste(cohort_2, cohort_1, sep = "_")
    ),
    same_cohort = if_else(cohort_1 == cohort_2, 1L, 0L),
    cohort_gap = abs(app_year_1 - app_year_2),
    agreement = agreement
  )


cohort_summary <- dyads2 %>%
  group_by(cohort_pair) %>%
  summarise(
    n = n(),
    mean_agreement = mean(agreement, na.rm = TRUE),
    mean_abs_distance = mean(N_abs_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_agreement))

cohort_summary

#Model A: ideological distance × cohort pairing

dyads2 <- dyads2 %>%
  mutate(
    pair_ideology = relevel(factor(pair_ideology), ref = "Convergent"),
    cohort_pair   = relevel(factor(cohort_pair), ref = "pre2011_pre2011")
  )

m1 <- feols(
  agreement ~ pair_ideology * +
    frac_minority + factor(procedure) |
    year_r,
  data = dyads2,
  vcov = ~ judge1 + judge2
)

summary(m1)

m2 <- feols(
  agreement ~ N_abs_distance * same_cohort +
    frac_minority + factor(procedure) + post_2015 |
    year_r,
  data = dyads2,
  vcov = ~ judge1 + judge2
)

summary(m2)


# plot mean dyadic disagreement over time



dyads_plot <- dyads %>%
  filter(pair_ideology %in% c("Convergent", "Divergent")) %>%
  mutate(
    disagreement = 1 - agreement
  ) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    mean_disagreement = mean(disagreement, na.rm = TRUE),
    n = sum(!is.na(disagreement)),
    .groups = "drop"
  )

ggplot(dyads_plot, aes(x = year_r, y = mean_disagreement, linetype = pair_ideology)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    x = "",
    y = "Mean dyadic disagreement",
    linetype = "Pair ideology"
  ) +
  scale_x_continuous(
    breaks = seq(min(dyads_plot$year_r), max(dyads_plot$year_r), by = 5)
  ) +
  theme_minimal()


library(dplyr)
library(ggplot2)

dyads_plot <- dyads %>%
  filter(pair_ideology %in% c("Convergent", "Divergent")) %>%
  mutate(
    disagreement = 1 - agreement
  ) %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    mean_disagreement = mean(disagreement, na.rm = TRUE),
    n = sum(!is.na(disagreement)),
    sd_disagreement = sd(disagreement, na.rm = TRUE),
    se = sd_disagreement / sqrt(n),
    ci_low = mean_disagreement - 1.96 * se,
    ci_high = mean_disagreement + 1.96 * se,
    .groups = "drop"
  )


pdf("Proportion_dyadic_disagreement.pdf", 
    width = 6.5, height = 3.5)
print(ggplot(dyads_plot, aes(x = year_r, y = mean_disagreement, color = pair_ideology)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(
    x = "",
    y = "Proportion dyadic disagreement",
    color = "Pair Partisan \nComposition: "
  ) +
  scale_x_continuous(
    breaks = seq(min(dyads_plot$year_r), max(dyads_plot$year_r), by = 10)
  ) +
  scale_color_manual(values = c("Convergent" = "steelblue", "Divergent" = "firebrick"),
                     labels = c("Convergent" = "In-group (Con-Con, \nPro-Pro)", 
                                "Divergent" = "Mixed (Pro-Con, \nCon-Pro)")) +
  theme_minimal())
dev.off()

# Proportion dyadic disagreement and Dalton's Polarization Index


merged_data <- dyads_plot %>%
  select(year_r, pair_ideology, mean_disagreement) %>%
  left_join(
    PI %>% select(year, polarindex),
    by = c("year_r" = "year")
  )

# Check merged dataset
head(merged_data)

# Run correlation test by ideology group
results <- merged_data %>%
  group_by(pair_ideology) %>%
  group_modify(~{
    
    test <- cor.test(
      ~ mean_disagreement + polarindex,
      data = .x,
      use = "complete.obs"
    )
    
    tibble(
      correlation = unname(test$estimate),
      p_value = test$p.value,
      ci_lower = test$conf.int[1],
      ci_upper = test$conf.int[2]
    )
  })

results

#correlation Vdem data

library(dplyr)

vdem <- read.csv("V-Dem-CY-Full+Others-v16.csv")  # adjust filename

vdem_spain <- vdem %>%
  filter(country_name == "Spain") %>%
  select(year, v2cacamps)

dyads_clean <- dyads_plot %>%
  group_by(year_r, pair_ideology) %>%
  summarise(
    mean_disagreement = mean(mean_disagreement, na.rm = TRUE),
    .groups = "drop"
  )

merged_data <- dyads_clean %>%
  left_join(vdem_spain, by = c("year_r" = "year"))

results <- merged_data %>%
  group_by(pair_ideology) %>%
  group_modify(~{
    
    test <- cor.test(
      ~ mean_disagreement + v2cacamps,
      data = .x,
      use = "complete.obs"
    )
    
    tibble(
      correlation = unname(test$estimate),
      p_value = test$p.value,
      ci_lower = test$conf.int[1],
      ci_upper = test$conf.int[2]
    )
  })

results

# correlation V-Dem political polarization and share nonuanimous decisions

merged_data <- percentage_nonunaninmous %>%
  select(year_r, percentage_dissent) %>%
  left_join(vdem_spain, by = c("year_r" = "year"))

cor_test <- cor.test(
  merged_data$percentage_dissent,
  merged_data$v2cacamps,
  use = "complete.obs"
)

cor_test


# correlation Dalton political polarization and share nonuanimous decisions

merged_data <- percentage_nonunaninmous %>%
  select(year_r, percentage_dissent) %>%
  left_join(PI, by = c("year_r" = "year"))

cor_test <- cor.test(
  merged_data$percentage_dissent,
  merged_data$polarindex,
  use = "complete.obs"
)

cor_test



#### INTERACTION MIXED PAIR X POLORISATION INDICATORS

# Keep only Spain polarization values for 1980–2023
vdem_subset <- vdem_spain %>%
  select(year, v2cacamps) %>%
  filter(year >= 1980, year <= 2023) %>%
  distinct(year, .keep_all = TRUE)

# Merge into full dyads data
dyads1 <- dyads %>%
  left_join(vdem_subset, by = c("year_r" = "year"))

dyads1$Nv2cacamps <- scales::rescale(dyads1$v2cacamps, to = c(0, 1))

# add Dalton's Index
dyads1 <- dyads1 %>%
  left_join(PI, by = c("year_r" = "year"))

dyads1$Npolarindex <- scales::rescale(dyads1$polarindex, to = c(0, 1))

dyads1$disagreement <- 1-dyads$agreement

# estimate models with V-Dem polarization index

set.seed(6764)

m1 <- feols(disagreement ~ pair_ideology*Nv2cacamps +
              frac_minority, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m6)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_pair_vdem_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "Nv2cacamps" = "Mass Polarization",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:Nv2cacamps" = "Mixed $\\times$ Mass Polar.",
    "pair_ideologyConvergent:Nv2cacamps" = "In-group $\\times$ Mass Polar.",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Mass Polar."
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "Nv2cacamps",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:Nv2cacamps",
    "pair_ideologyConvergent:Nv2cacamps",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)

# Re-estimate controlling for appointment year FE

library(dplyr)
library(lubridate)
library(fixest)

library(dplyr)
library(lubridate)
library(fixest)

# --------------------------------------------------
# 1. Prepare judge-level appointment information
# --------------------------------------------------

library(dplyr)
library(fixest)

# --------------------------------------------------
# 1. Prepare judge-level appointment-year information
# --------------------------------------------------

judge_appt <- judge_info %>%
  select(id_juez, app_year) %>%
  distinct() %>%
  group_by(id_juez) %>%
  summarise(
    app_year = min(app_year, na.rm = TRUE),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Merge appointment-year information into dyadic data
# --------------------------------------------------

dyads1_appt <- dyads1 %>%
  left_join(
    judge_appt %>%
      rename(
        judge1 = id_juez,
        app_year_1 = app_year
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_appt %>%
      rename(
        judge2 = id_juez,
        app_year_2 = app_year
      ),
    by = "judge2"
  ) %>%
  mutate(
    mean_app_year = (app_year_1 + app_year_2) / 2,
    app_year_distance = abs(app_year_1 - app_year_2)
  )

# estimate with appointment year FE

m1_appt <- feols(
  disagreement ~ pair_ideology * Nv2cacamps +
    frac_minority + total_cases + as.factor(procedure)
  |app_year_1 + app_year_2,
  data = dyads1_appt,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m1_appt)

m2_appt <- feols(
  disagreement ~ pair_ideology * Nv2cacamps +
    frac_minority + as.factor(procedure)|
    as.factor(year_r) + app_year_1 + app_year_2,
  data = dyads1_appt,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m2_appt)

m3_appt <- feols(
  disagreement ~ pair_ideology * Nv2cacamps +
    frac_minority +
    as.factor(procedure) |
    as.factor(year_r) + app_year_1 + app_year_2 + ID_PAT,
  data = dyads1_appt,
  vcov = ~ judge1 + judge2 + ID_PAT
)
summary(m3_appt)


etable(
  m1_appt,
  m2_appt,
  m3_appt,
  tex = TRUE,
  file = "regression_table_pair_vdem_appointment_year_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "Nv2cacamps" = "Mass Polarization",
    "frac_minority" = "Minority Share",
    "total_cases" = "Caseload",
    "as.factor(procedure)Concrete" = "Concrete Review",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:Nv2cacamps" = "Mixed $\\times$ Mass Polar.",
    "pair_ideologyConvergent:Nv2cacamps" = "In-group $\\times$ Mass Polar.",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Mass Polar."
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "Nv2cacamps",
    "frac_minority",
    "total_cases",
    "as.factor(procedure)",
    "factor(procedure)",
    "pair_ideologyDivergent:Nv2cacamps",
    "pair_ideologyConvergent:Nv2cacamps",
    "pair_ideologyNA:Nv2cacamps",
    "Constant"
  ),
  
  fixef.group = list(
    "Judge 1 Appointment-Year FE" = "app_year_1",
    "Judge 2 Appointment-Year FE" = "app_year_2",
    "Year FE" = "year_r",
    "Case FE" = "ID_PAT"
  )
)


# Estimate models with Dalton's indicator


m1 <- feols(disagreement ~ pair_ideology*Npolarindex +
              frac_minority, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(disagreement ~ pair_ideology*Npolarindex + 
              frac_minority | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(disagreement ~ pair_ideology*Npolarindex + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(disagreement ~ pair_ideology*Npolarindex + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feols(disagreement ~ pair_ideology*Npolarindex + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feols(disagreement ~ pair_ideology*Npolarindex + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m6)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_pair_dalton_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "Npolarindex" = "Party Polarization",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:Npolarindex" = "Mixed $\\times$ Mass Polar.",
    "pair_ideologyConvergent:Npolarindex" = "In-group $\\times$ Mass Polar.",
    "pair_ideologyNA:Npolarindex" = "Ambiguous $\\times$ Mass Polar."
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "Npolarindex",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:Npolarindex",
    "pair_ideologyConvergent:Npolarindex",
    "pair_ideologyNA:Npolarindex" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)

# Appointing party distance X polarization indicator

m1 <- feols(disagreement ~ N_abs_distance*Nv2cacamps +
              frac_minority, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(disagreement ~ N_abs_distance*Nv2cacamps + 
              frac_minority | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(disagreement ~ N_abs_distance*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(disagreement ~ N_abs_distance*Nv2cacamps + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feols(disagreement ~ N_abs_distance*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feols(disagreement ~ N_abs_distance*Nv2cacamps + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m6)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_distance_vdem_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "N_abs_distance" = "Party Ideological Distance",
    "year_r" = "Time",
    "Nv2cacamps" = "Mass Polarization",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "N_abs_distance:Nv2cacamps" = "Party Distance $\\times$ Mass Polar."
  ),
  
  order = c(
    "N_abs_distance",
    "Nv2cacamps",
    "total_cases",
    "factor(procedure)",
    "N_abs_distanceDivergent:Nv2cacamps",
    "N_abs_distanceConvergent:Nv2cacamps",
    "N_abs_distanceNA:Nv2cacamps" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)

# Re-estimate Mixed*V-Dem with logistic regression

m1 <- feglm(disagreement ~ pair_ideology*Nv2cacamps +
              frac_minority, 
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feglm(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority | as.factor(year_r), 
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feglm(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feglm(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feglm(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT,
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feglm(disagreement ~ pair_ideology*Nv2cacamps + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            family = binomial("logit"),
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m6)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_pair_vdem_FE_logistic.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "Nv2cacamps" = "Mass Polarization",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:Nv2cacamps" = "Mixed $\\times$ Mass Polar.",
    "pair_ideologyConvergent:Nv2cacamps" = "In-group $\\times$ Mass Polar.",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Mass Polar."
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "Nv2cacamps",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:Nv2cacamps",
    "pair_ideologyConvergent:Nv2cacamps",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)


# estimate models mixed pair interaction with time

set.seed(6764)

m1 <- feols(disagreement ~ pair_ideology*year_r +
              frac_minority, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(disagreement ~ pair_ideology*year_r + 
              frac_minority | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(disagreement ~ pair_ideology*year_r + 
              frac_minority + 
              procedure | as.factor(year_r), 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(disagreement ~ pair_ideology*year_r + 
              frac_minority +
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)


m5 <- feols(disagreement ~ pair_ideology*year_r + 
              frac_minority + 
              procedure | as.factor(year_r) + ID_PAT, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m5)




m6 <- feols(disagreement ~ pair_ideology*year_r + 
              procedure | as.factor(year_r) + judge1 + judge2, 
            data = dyads1,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m6)


etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "regression_table_pair_time_FE.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "Nv2cacamps" = "Mass Polarization",
    "total_cases" = "Caseload",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:year_r" = "Mixed $\\times$ Time",
    "pair_ideologyConvergent:year_r" = "In-group $\\times$ Time",
    "pair_ideologyNA:year_r" = "Ambiguous $\\times$ Time"
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "year_r",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:year_r",
    "pair_ideologyConvergent:year_r",
    "pair_ideologyNA:year_r" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)


# Territorial Issues ------------------------------------------------------



### Dyadic Disagreement and Territorial Issues

library(dplyr)
library(ggplot2)

data_topic <- read.csv(
  "territorial_data_mistral.csv",
  sep = ";",
  header = TRUE,
  row.names = NULL
)

# clean
data_topic_clean <- data_topic %>%
  filter(
    !is.na(ID_PAT), ID_PAT != "",
    !is.na(territorial), territorial != "",
    !is.na(year_r)
  )

# convert territorial topic to dummy, keeping year_r
topic_covariates <- data_topic_clean %>%
  transmute(
    ID_PAT,
    year_r,
    territorial_dummy = case_when(
      territorial == "CENTRAL" ~ 1,
      territorial %in% c("ABSENT", "PERIPHERAL", "PHERIPHERAL") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(ID_PAT, year_r) %>%
  summarise(
    territorial_dummy = max(territorial_dummy, na.rm = TRUE),
    .groups = "drop"
  )



# plot over time

plot_territorial_years <- topic_covariates %>%
  filter(territorial_dummy == 1) %>%
  count(year_r, name = "n") %>%
  ggplot(aes(x = year_r, y = n)) +
  geom_col(
    width = 0.8,
    fill = "grey30"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(topic_covariates$year_r, na.rm = TRUE),
      max(topic_covariates$year_r, na.rm = TRUE),
      by = 5
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "",
    y = "Cases involving territorial issues"
  ) +
  theme_classic(base_size = 13) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank()
  )

plot_territorial_years

library(Cairo)

ggsave(
  "territorial_cases_by_year_mistral.pdf",
  plot = plot_territorial_years,
  device = cairo_pdf,
  width = 8,
  height = 5,
  units = "in"
)

# territorial issues by procedure and over time

procedure_covariate <- dyads1 %>%
  select(ID_PAT, procedure) %>%
  filter(!is.na(ID_PAT), ID_PAT != "") %>%
  distinct(ID_PAT, .keep_all = TRUE)

topic_covariates <- topic_covariates %>%
  left_join(
    procedure_covariate,
    by = "ID_PAT"
  )

library(scales)

territorial_prop_time <- topic_covariates %>%
  mutate(
    procedure_clean = tolower(trimws(procedure))
  ) %>%
  filter(
    procedure_clean %in% c("abstract", "concrete"),
    !is.na(year_r),
    !is.na(territorial_dummy)
  ) %>%
  group_by(year_r, procedure_clean) %>%
  summarise(
    n_cases = n(),
    n_territorial = sum(territorial_dummy == 1),
    prop_territorial = mean(territorial_dummy == 1),
    .groups = "drop"
  )

territorial_prop_time

plot_territorial_prop_time <- ggplot(
  territorial_prop_time,
  aes(
    x = year_r,
    y = prop_territorial,
    linetype = procedure_clean,
    shape = procedure_clean
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_x_continuous(
    breaks = seq(
      min(territorial_prop_time$year_r, na.rm = TRUE),
      max(territorial_prop_time$year_r, na.rm = TRUE),
      by = 5
    )
  ) +
  labs(
    x = "",
    y = "Proportion of cases involving territorial issues",
    linetype = "Procedure",
    shape = "Procedure"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

plot_territorial_prop_time


ggsave(
  "territorial_proportion_by_procedure_over_time_mistral.pdf",
  plot_territorial_prop_time,
  device = cairo_pdf,
  width = 7,
  height = 4.5,
  units = "in"
)

# Re-estimate partisan identity*mass polarization interaction with territorial dummy

dyads_topic <- dyads1 %>%
  left_join(topic_covariates, by = "ID_PAT")


m_a <- feols(
  disagreement ~ pair_ideology*Nv2cacamps + as.factor(territorial_dummy) 
  | factor(year_r.x) + ID_PAT,
  data = dyads_topic,
  vcov = ~ judge1 + judge2 + ID_PAT
)

summary(m_a)


m1 <- feols(disagreement ~ pair_ideology*Nv2cacamps +
              frac_minority + as.factor(territorial_dummy), 
            data = dyads_topic,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m1)

m2 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + as.factor(territorial_dummy)
            | as.factor(year_r.x), 
            data = dyads_topic,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m2)

m3 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + as.factor(territorial_dummy) +
              procedure | as.factor(year_r.x), 
            data = dyads_topic,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m3)


m4 <- feols(disagreement ~ pair_ideology*Nv2cacamps + 
              frac_minority + as.factor(territorial_dummy) +
              procedure | as.factor(year_r.x) + judge1 + judge2, 
            data = dyads_topic,
            vcov = ~ judge1 + judge2 + ID_PAT)
summary (m4)









etable(
  m1, m2, m3, m4,
  tex = TRUE,
  file = "regression_table_pair_vdem_territorial_dummy_mistral.tex",
  replace = TRUE,
  digits = 3,
  se.below = TRUE,
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1),
  fitstat = ~ n + r2 + ar2,
  #headers = c("Model A", "Model B", "Model C", "Model D", "Model E"),
  
  dict = c(
    "pair_ideologyDivergent" = "Mixed Pair",
    "pair_ideologyConvergent" = "In-group Pair",
    "pair_ideologyNA" = "Ambiguous Pair",
    "year_r" = "Time",
    "Nv2cacamps" = "Mass Polarization",
    "total_cases" = "Caseload",
    "as.factor(territorial_dummy)1" = "Territorial",
    "factor(procedure)Concrete" = "Concrete Review",
    "pair_ideologyDivergent:Nv2cacamps" = "Mixed $\\times$ Mass Polar.",
    "pair_ideologyConvergent:Nv2cacamps" = "In-group $\\times$ Mass Polar.",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Mass Polar."
  ),
  
  order = c(
    "pair_ideologyDivergent",
    "pair_ideologyConvergent",
    "pair_ideologyNA",
    "Nv2cacamps",
    "N_abs_distance",
    "post_2015",
    "total_cases",
    "factor(procedure)",
    "pair_ideologyDivergent:Nv2cacamps",
    "pair_ideologyConvergent:Nv2cacamps",
    "pair_ideologyNA:Nv2cacamps" = "Ambiguous $\\times$ Time",
    "Constant"
  ),
  
  fixef.group = list("Case FE" = "ID_PAT", "Year FE" = "as.factor(year_r)")
)


### Correlation territorial beta parameter values

item_parameters <- read.csv("item_parameters_full.csv")

data_beta <- topic_covariates %>%
  left_join(
    item_parameters %>% select(item, beta),
    by = c("ID_PAT" = "item")
  ) %>%
  mutate(
    territorial_num = territorial_dummy
  ) %>%
  filter(!is.na(territorial_num), !is.na(beta))

cor.test(data_beta$territorial_num, abs(data_beta$beta), method = "pearson")




### Party Manifesto policy gap

library(dplyr)
library(lubridate)
library(tidyr)
library(manifestoR)

# Set your Manifesto Project API key once per session
mp_setapikey("manifesto_apikey.txt")

# Download the current Main Dataset
mpds <- mp_maindataset()

spain_lr <- mpds %>%
  filter(countryname == "Spain") %>%
  mutate(
    year = year(edate),
    party_label = case_when(
      grepl("^PP$", partyabbrev) ~ "PP",
      grepl("PSOE", partyabbrev) ~ "PSOE",
      grepl("People'?s Party|Popular Party", partyname, ignore.case = TRUE) ~ "PP",
      grepl("Spanish Socialist Workers'? Party|PSOE", partyname, ignore.case = TRUE) ~ "PSOE",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(party_label), !is.na(rile)) %>%
  group_by(year, party_label) %>%
  summarise(
    rile = mean(as.numeric(rile), na.rm = TRUE),
    .groups = "drop"
  )

pp_psoe_yearly <- spain_lr %>%
  pivot_wider(names_from = party_label, values_from = rile) %>%
  complete(year = 1980:2023) %>%
  arrange(year) %>%
  mutate(
    PP = zoo::na.approx(PP, x = year, na.rm = FALSE),
    PSOE = zoo::na.approx(PSOE, x = year, na.rm = FALSE),
    lr_gap_pp_psoe = abs(PP - PSOE)
  )

# correlation RILE with share nonunanimous rulings

percentage_nonunanimous2 <- percentage_nonunaninmous %>%
  left_join(
    pp_psoe_yearly %>% select(year, lr_gap_pp_psoe),
    by = c("year_r" = "year")
  )

cor.test(
  percentage_nonunanimous2$percentage_dissent,
  percentage_nonunanimous2$lr_gap_pp_psoe,
  use = "complete.obs"
)

# correlation RILE federalism decentralisation

library(broom)
library(purrr)

spain_lr_territorial <- mpds %>%
  filter(countryname == "Spain") %>%
  mutate(
    year = year(edate),
    party_label = case_when(
      grepl("^PP$", partyabbrev) ~ "PP",
      grepl("PSOE", partyabbrev) ~ "PSOE",
      grepl("People'?s Party|Popular Party", partyname, ignore.case = TRUE) ~ "PP",
      grepl("Spanish Socialist Workers'? Party|PSOE", partyname, ignore.case = TRUE) ~ "PSOE",
      TRUE ~ NA_character_
    ),
    rile = as.numeric(rile),
    per301 = as.numeric(per301),  # Federalism: positive
    per302 = as.numeric(per302)   # Centralisation: positive
  ) %>%
  filter(!is.na(party_label), !is.na(rile)) %>%
  group_by(year, party_label) %>%
  summarise(
    rile = mean(rile, na.rm = TRUE),
    per301 = mean(per301, na.rm = TRUE),
    per302 = mean(per302, na.rm = TRUE),
    federal_minus_central = per301 - per302,
    .groups = "drop"
  )

spain_pp_psoe <- spain_lr_territorial %>%
  filter(party_label %in% c("PP", "PSOE")) %>%
  filter(!is.na(rile), !is.na(per302))

cor_test_rile_territorial <- cor.test(
  spain_pp_psoe$rile,
  -1*(spain_pp_psoe$federal_minus_central),
  use = "complete.obs"
  ) %>%
  broom::tidy() %>%
  mutate(
    parties = "PP + PSOE",
    n = nrow(spain_pp_psoe)
  ) %>%
  select(
    parties,
    n,
    estimate,
    statistic,
    p.value,
    conf.low,
    conf.high,
    method,
    alternative
  )

cor_test_rile_territorial

library(ggplot2)

ggplot(spain_lr_territorial, aes(x = rile, y = federal_minus_central, label = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(nudge_y = 0.2, size = 3) +
  facet_wrap(~ party_label) +
  labs(
    x = "RILE score",
    y = "Federal minus Central",
    title = ""
  )

ggplot(spain_lr_territorial, aes(x = rile, y = federal_minus_central, label = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(nudge_y = 0.2, size = 3) +
  facet_wrap(~ party_label) +
  labs(
    x = "RILE score",
    y = "Federalism minus Centralisation",
    title = "RILE and centralisation emphasis in Spanish PP and PSOE manifestos"
  )

# add other central parties to the correlation

mpds %>%
  filter(countryname == "Spain") %>%
  select(year = edate, party, partyabbrev, partyname, rile, per301, per302) %>%
  mutate(year = year(year)) %>%
  filter(year %in% c(1979, 1982)) %>%
  arrange(year, partyabbrev)

library(stringr)

spain_parties_territorial <- mpds %>%
  filter(countryname == "Spain") %>%
  mutate(
    year = year(edate),
    party_label = case_when(
      # PP
      str_detect(partyabbrev, regex("^PP$", ignore_case = TRUE)) ~ "PP",
      str_detect(partyname, regex("Partido Popular|Popular Party|People'?s Party", ignore_case = TRUE)) ~ "PP",
      
      # PSOE
      str_detect(partyabbrev, regex("^PSOE$", ignore_case = TRUE)) ~ "PSOE",
      str_detect(partyname, regex("Spanish Socialist Workers'? Party|Partido Socialista Obrero Español|PSOE", ignore_case = TRUE)) ~ "PSOE",
      
      # Podemos / Unidas Podemos
      str_detect(partyabbrev, regex("^Podemos$|^UP$|Unidas Podemos", ignore_case = TRUE)) ~ "Podemos",
      str_detect(partyname, regex("Podemos|Unidas Podemos", ignore_case = TRUE)) ~ "Podemos",
      
      # Izquierda Unida: use exact/bounded matches, not grepl("IU")
      str_detect(partyabbrev, regex("^IU$", ignore_case = TRUE)) ~ "Izquierda Unida",
      str_detect(partyname, regex("\\bIzquierda Unida\\b|\\bUnited Left\\b", ignore_case = TRUE)) ~ "Izquierda Unida",
      
      # Ciudadanos
      str_detect(partyabbrev, regex("^Cs$|^C'?s$|^Ciudadanos$", ignore_case = TRUE)) ~ "Ciudadanos",
      str_detect(partyname, regex("\\bCiudadanos\\b|\\bCitizens\\b", ignore_case = TRUE)) ~ "Ciudadanos",
      
      # Vox
      str_detect(partyabbrev, regex("^Vox$", ignore_case = TRUE)) ~ "Vox",
      str_detect(partyname, regex("\\bVox\\b", ignore_case = TRUE)) ~ "Vox",
      
      TRUE ~ NA_character_
    ),
    rile = as.numeric(rile),
    per301 = as.numeric(per301),
    per302 = as.numeric(per302)
  ) %>%
  filter(
    !is.na(party_label),
    !is.na(rile),
    !is.na(per301),
    !is.na(per302)
  ) %>%
  group_by(year, party_label) %>%
  summarise(
    rile = mean(rile, na.rm = TRUE),
    per301 = mean(per301, na.rm = TRUE),
    per302 = mean(per302, na.rm = TRUE),
    federal_minus_central = per301 - per302,
    .groups = "drop"
  )

cor.test(
  spain_parties_territorial$rile,
  -1*(spain_parties_territorial$federal_minus_central),
  use = "complete.obs"
)
