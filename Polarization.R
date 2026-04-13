library(dataverse)
library(tidyverse)
library(ggplot2)

# import data

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")

data1<-read.csv("DB_ConcreteReview_FullMatrix.csv", sep=",", header = T) %>% 
  select(id_row,id_row2, ID_PAT, voto_text, Vote_Dummy, Cases_with_dissents_dummy,
         Fecha.de.resolución, Órgano, Tipo.y.número.de.registro,
         year_r, Court_Panel, id_juez, CONSERVADOR.PROGRESISTA)


concrete_2002 <- read.csv("DB_ConcreteReview_FullMatrix_2002.csv", sep = ",",
                          header = T) %>% 
  select(id_row,id_row2, ID_PAT, voto_text, Vote_Dummy, Cases_with_dissents_dummy,
         Fecha.de.resolución, Órgano, Tipo.y.número.de.registro,
         year_r, Court_Panel, id_juez, CONSERVADOR.PROGRESISTA)

data1 <- rbind(data1, concrete_2002)



data1<-data1 %>% 
  select(id_row, ID_PAT, id_juez, Vote_Dummy, CONSERVADOR.PROGRESISTA, 
         Cases_with_dissents_dummy, year_r)%>% 
  rename(ID = id_row, Type_Vote = Vote_Dummy, Ideology = CONSERVADOR.PROGRESISTA, 
         dissent_dummy = Cases_with_dissents_dummy)

# add procedure variable

data1$procedure <- "Concrete"

# recode ID-PAT
data1$ID_PAT <- gsub("^\\s+|(.),.*", "\\1", data1$ID_PAT)

# import abstract review cases

data<-read.csv("AbstractReviewAllVotes.csv", sep=";", header = T)

judge_ideo <- data %>% distinct(id_juez, Party_manifesto)



# Recode votes ------------------------------------------------------------

data<-data %>% mutate(Type_Vote=recode(Type_Vote,
                                       "V.PARTICULAR"="1",
                                       "V.CONCURRENTE"="0"))

data$Type_Vote <-as.numeric(data$Type_Vote)

# create row ID

data <- data %>% 
  mutate(ID = row_number())

data <-data %>% 
  select(ID, ID_PAT, id_juez, Type_Vote, Vote_Dummy, CONSERVADOR_PROGRESISTA, 
         year_r)%>% 
  rename(Ideology = CONSERVADOR_PROGRESISTA)


#create dissent dummy


dissent_dummy <- data %>%
  group_by(ID_PAT) %>% summarise(dissent_dummy = ifelse(any(Type_Vote != 0), 1, 0)) %>%
  ungroup()

data <- merge(data, dissent_dummy, by = "ID_PAT")


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


#### Compute relation between minority size and propensity to disent

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
    frac_minority = 10*num_minority / num_judges
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

# case parameters over time abstract review

cases <- distinct(data2, ID_PAT, .keep_all = T)
data4 <- left_join(data3,cases[,c("ID_PAT", "procedure")],
                   by = join_by("item" == "ID_PAT"))

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

covariates <- data %>% select(ID_PAT, year_r, procedure, frac_minority) %>% 
  distinct(., ID_PAT, .keep_all = T)

dyads <- left_join(dyads, covariates, by = "ID_PAT")

dyads <- left_join(dyads, cases_per_year, by = "year_r")

# Judge ideology by case
judge_ideology <- data %>%
  select(ID_PAT, id_juez, Ideology) %>%
  distinct()

# Join ideology for judge1
dyads <- dyads %>%
  left_join(
    judge_ideology %>% rename(judge1 = id_juez, ideology1 = Ideology),
    by = c("ID_PAT", "judge1")
  ) %>%
  left_join(
    judge_ideology %>% rename(judge2 = id_juez, ideology2 = Ideology),
    by = c("ID_PAT", "judge2")
  ) %>%
  mutate(
    pair_ideology = case_when(
      ideology1 == "P" & ideology2 == "P" ~ "Convergent",
      ideology1 == "C" & ideology2 == "C" ~ "Convergent",
      (ideology1 == "P" & ideology2 == "C") |
        (ideology1 == "C" & ideology2 == "P") ~ "Divergent",
      TRUE ~ "NA"
    ),
    post_2015 = if_else(year_r >= 2015, 1L, 0L)
  )


# normalize ideological distance
library(scales)

dyads$N_abs_distance <- rescale(dyads$abs_distance)

# regression OLS
library(fixest)
library(plm)
set.seed(6764)

m1 <- feols(agreement ~ pair_ideology*post_2015, 
         data = dyads)
summary (m1)

m2 <- feols(agreement ~ post_2015 + year_r + frac_minority
            + total_cases + pair_ideology, 
         data = dyads)
summary (m2)

m3 <- feols(agreement ~ year_r + 
              frac_minority + total_cases + 
              procedure + pair_ideology, 
            data = dyads)
summary (m3)

m4 <- feols(agreement ~ year_r + 
              frac_minority + total_cases + 
              procedure + year_r*pair_ideology, 
            data = dyads)
summary (m4)

m5 <- feols(agreement ~ year_r + 
              frac_minority + total_cases + year_r*pair_ideology +
              procedure|as.factor(year_r), 
            data = dyads)
summary(m5)


m6 <- feols(agreement ~ year_r +
              frac_minority + total_cases + 
              year_r*pair_ideology +
              procedure |as.factor(year_r) + 
              judge1 + judge2,
            data = dyads)
summary(m6)

m7 <- feols(agreement ~ 
              frac_minority*post_2015, 
            data = dyads)
summary(m7)

m8 <- feols(agreement ~
              frac_minority*pair_ideology, 
            data = dyads)
summary (m8)


stargazer(m1, m2, m3, m4, m5, m6, m7,
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
  select(id_juez, year_r) %>%
  distinct() %>%
  group_by(id_juez) %>%
  summarise(
    first_year = min(year_r, na.rm = TRUE),
    last_year  = max(year_r, na.rm = TRUE),
    spans_2015 = if_else(first_year < 2015 & last_year >= 2015, 1L, 0L),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 2015
# --------------------------------------------------
dyads_span2015 <- dyads %>%
  left_join(
    judge_span2015 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = first_year,
        last_year_1 = last_year,
        spans_2015_1 = spans_2015
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span2015 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = first_year,
        last_year_2 = last_year,
        spans_2015_2 = spans_2015
      ),
    by = "judge2"
  ) %>%
  filter(spans_2015_1 == 1, spans_2015_2 == 1) %>%
  mutate(
    disagreement = 1 - agreement,
    post_2015 = if_else(year_r >= 2015, 1L, 0L)
  )


m_dist <- feols(
  agreement ~ pair_ideology * post_2015 +
    factor(procedure) + total_cases,
  data = dyads_span2015,
  vcov = ~ judge1 + judge2
)

summary(m_dist)

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
  fixef.group = list("Case fixed effects" = "ID_PAT")
)

# Dyadic disagreement placebo 1

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 1996
# --------------------------------------------------
judge_span1996 <- data %>%
  select(id_juez, year_r) %>%
  distinct() %>%
  group_by(id_juez) %>%
  summarise(
    first_year = min(year_r, na.rm = TRUE),
    last_year  = max(year_r, na.rm = TRUE),
    spans_1996 = if_else(first_year < 1996 & last_year >= 1996, 1L, 0L),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 1996
# --------------------------------------------------
dyads_span1996 <- dyads %>%
  left_join(
    judge_span1996 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = first_year,
        last_year_1 = last_year,
        spans_1996_1 = spans_1996
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1996 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = first_year,
        last_year_2 = last_year,
        spans_1996_2 = spans_1996
      ),
    by = "judge2"
  ) %>%
  filter(spans_1996_1 == 1, spans_1996_2 == 1) %>%
  mutate(
    disagreement = 1 - agreement,
    post_1996 = if_else(year_r >= 1996, 1L, 0L)
  )


m_dist <- feols(
  agreement ~ pair_ideology +
    factor(procedure) + total_cases,
  data = dyads_span1996,
  vcov = ~ judge1 + judge2
)

summary(m_dist)

# Dyadic disagreement placebo 2

# --------------------------------------------------
# 1. Identify judges whose observed tenure spans 2004
# --------------------------------------------------
judge_span2004 <- data %>%
  select(id_juez, year_r) %>%
  distinct() %>%
  group_by(id_juez) %>%
  summarise(
    first_year = min(year_r, na.rm = TRUE),
    last_year  = max(year_r, na.rm = TRUE),
    spans_2004 = if_else(first_year < 1996 & last_year >= 1996, 1L, 0L),
    .groups = "drop"
  )

# --------------------------------------------------
# 2. Keep only dyads where both judges span 1996
# --------------------------------------------------
dyads_span1996 <- dyads %>%
  left_join(
    judge_span1996 %>%
      rename(
        judge1 = id_juez,
        first_year_1 = first_year,
        last_year_1 = last_year,
        spans_1996_1 = spans_1996
      ),
    by = "judge1"
  ) %>%
  left_join(
    judge_span1996 %>%
      rename(
        judge2 = id_juez,
        first_year_2 = first_year,
        last_year_2 = last_year,
        spans_1996_2 = spans_1996
      ),
    by = "judge2"
  ) %>%
  filter(spans_1996_1 == 1, spans_1996_2 == 1) %>%
  mutate(
    disagreement = 1 - agreement,
    post_1996 = if_else(year_r >= 1996, 1L, 0L)
  )


m_dist <- feols(
  agreement ~ pair_ideology +
    factor(procedure) + total_cases,
  data = dyads_span1996,
  vcov = ~ judge1 + judge2
)

summary(m_dist)

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
  fixef.group = list("Case fixed effects" = "ID_PAT")
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
  agreement ~ pair_ideology * cohort_pair +
    frac_minority + factor(procedure) + post_2015 |
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



ggplot(dyads_plot, aes(x = year_r, y = mean_disagreement, color = pair_ideology)) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(
    x = "",
    y = "Mean dyadic disagreement",
    color = "Pair appointing \nparty: "
  ) +
  scale_x_continuous(
    breaks = seq(min(dyads_plot$year_r), max(dyads_plot$year_r), by = 10)
  ) +
  scale_color_manual(values = c("Convergent" = "steelblue", "Divergent" = "firebrick")) +
  theme_minimal()
