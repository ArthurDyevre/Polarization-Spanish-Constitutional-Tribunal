library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")

data1 <- read.csv("complete_data_set7May2024.csv", sep=",", header = T)

data2 <- read.csv("id_jueces_fuzzyjoin.csv", sep = ",", header = T) %>% select(Juez_Voto, id_juez)

theta <-read.csv("indifference_estimates_full.csv", sep=",", header = T)

theta <- left_join(theta, data2, by = "id_juez")



alpha_beta <- read.csv("item_parameters_full.csv", sep = ",", header = T)

#normalise difficulty parameter
alpha_beta$alphas <- (alpha_beta$alpha - mean(alpha_beta$alpha))/sd(alpha_beta$alpha)
alpha_beta$ID_PAT <- alpha_beta$item

## 	SENTENCIA 46/2001

item <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "SENTENCIA 46/2001")


print(item)
print(theta)

item <- inner_join(item, theta, by = "id_juez", keep = F)

# Custom function to calculate ICC
calculate_icc <- function(theta_r, a, b) {
  prob <- (exp(-a +b * (theta_r))) / (1 + exp(-a + b * (theta_r)))
  return(prob)
}

# Parameter values

a <- alpha_beta %>%
  semi_join(item, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-2, 2, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("SENTENCIA_46_2001.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()

#### 

item2 <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "SENTENCIA 110/2010")


print(item)
print(theta)

item2 <- inner_join(item2, theta, by = "id_juez", keep = F)

print(item2)

a <- alpha_beta %>%
  semi_join(item2, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item2, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("SENTENCIA_110_2010.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item2, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item2, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()


#### 

item3 <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "AUTO 26/2007")


print(item3)
print(theta)

item3 <- inner_join(item3, theta, by = "id_juez", keep = F)

print(item3)

a <- alpha_beta %>%
  semi_join(item3, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item3, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("AUTO_26_2007.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item3, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item3, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()

#### 

item4 <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "AUTO 387/2007")


print(item4)
print(theta)

item4 <- inner_join(item4, theta, by = "id_juez", keep = F)

print(item4)

a <- alpha_beta %>%
  semi_join(item4, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item4, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("AUTO_387_2007.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item4, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item4, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()

# SENTENCIA 112/2010

item5 <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "SENTENCIA 112/2010")


print(item5)
print(theta)

item5 <- inner_join(item5, theta, by = "id_juez", keep = F)

print(item5)

a <- alpha_beta %>%
  semi_join(item5, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item5, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("SENTENCIA_112_2010.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item5, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item5, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()


# AUTO 31/2010

item6 <- data1 %>% select(id_juez, ID_PAT, Ideology, Type_Vote) %>% 
  filter(ID_PAT == "AUTO 117/2022")


print(item6)
print(theta)

item6 <- inner_join(item6, theta, by = "id_juez", keep = F)

print(item6)

a <- alpha_beta %>%
  semi_join(item6, by = "ID_PAT") %>%
  pull(alpha)
b <- alpha_beta %>%
  semi_join(item6, by = "ID_PAT") %>%
  pull(beta)


# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent Ideological Indifference Point", 
       y = "Probability of dissenting") +
  theme_classic()

pdf("SENTENCIA_31_2010.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item6, aes(x = ideal, y = Type_Vote)) + 
        geom_text_repel(data = item6, aes(label = Juez_Voto, x = ideal, y = Type_Vote),
                        size = 2, 
                        max.overlaps = 35, 
                        #position=position_jitter(width=0.03,height=0.04), 
                        colour = "black"))

dev.off()


