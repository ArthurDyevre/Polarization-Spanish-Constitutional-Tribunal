library(dataverse)
library(tidyverse)
library(ggplot2)

# import data

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")

data <- read.csv("complete_data_set7May2024.csv", sep = ",", header = T)

data_topic <- read.csv("data_april_2026.csv", sep = ";", header = T, row.names = NULL)

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

### Add topic (territorial issue)

#clean
data_topic_clean <- data_topic %>%
  filter(
    !is.na(ID_PAT), ID_PAT != "",
    !is.na(territorial), territorial != ""
  )


# convert issue to dummy

topic_covariates <- data_topic_clean %>%
  transmute(
    ID_PAT,
    territorial_dummy = case_when(
      territorial == "CENTRAL" ~ "CENTRAL",
      territorial %in% c("ABSENT", "PHERIPHERAL") ~ "NON_CENTRAL",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(ID_PAT) %>%
  summarise(
    territorial_dummy = first(territorial_dummy),
    .groups = "drop"
  )

data <- data %>%
  left_join(topic_covariates, by = "ID_PAT")

d7 <- lm(
  as.numeric(dissent_dummy) ~ frac_minority + year_r +
    total_cases + procedure + territorial_dummy, 
  data = data
)

summary(d7)
