library(dataverse)
library(tidyverse)
library(ggplot2)

# import data

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/Tribunal Constitucional/R scripts")

data1<-read.csv("DB_ConcreteReview_FullMatrix.csv", sep=",", header = T) %>%
  filter(Cases_with_dissents_dummy == 1) %>% 
  select(id_row,id_row2, ID_PAT, voto_text, Vote_Dummy, Cases_with_dissents_dummy,
         Fecha.de.resolución, Órgano, Tipo.y.número.de.registro,
         year_r, Court_Panel, id_juez, CONSERVADOR.PROGRESISTA)
                                                    

concrete_2002 <- read.csv("DB_ConcreteReview_FullMatrix_2002.csv", sep = ",",
                          header = T) %>%
  filter(Cases_with_dissents_dummy == 1) %>% 
  select(id_row,id_row2, ID_PAT, voto_text, Vote_Dummy, Cases_with_dissents_dummy,
  Fecha.de.resolución, Órgano, Tipo.y.número.de.registro,
  year_r, Court_Panel, id_juez, CONSERVADOR.PROGRESISTA)

data1 <- rbind(data1, concrete_2002)

data1$ID_PAT <- gsub("^\\s+|(.),.*", "\\1", data1$ID_PAT)


data<-read.csv("Votes_TC_beta.csv", sep=";", header = T)




# Recode votes ------------------------------------------------------------

data<-data %>% mutate(Type_Vote=recode(Type_Vote,
                                       "V.PARTICULAR"="1",
                                       "V.CONCURRENTE"="0"))

data$Type_Vote <-as.numeric(data$Type_Vote)



data$Ideology <-as.factor(data$Ideology)


write.csv(data, "data_tc_full_nonunanimous.csv")

### voting matrix  ------------------------------------------------


# select variables

tc_df<-data %>% select(id_juez, ID_PAT, Type_Vote)

tc_df2<-data1 %>% select(id_juez, ID_PAT, Vote_Dummy)%>% 
  rename(Type_Vote = Vote_Dummy)

tc_df <- rbind(tc_df, tc_df2)

tc_df <- tc_df[order(tc_df$id_juez),]

RollCallList = unique(tc_df$ID_PAT)
JudgeList = unique(tc_df$id_juez)

# Generate vote matrix
VoteMatrix = matrix(NA, nrow = length(JudgeList), ncol = length(RollCallList))

## dim(VoteMatrix)    ## Check dimensions of matrix	length(RollCallList)
colnames(VoteMatrix) = RollCallList
rownames(VoteMatrix) = JudgeList

# fill in matrix

rows <-nrow(tc_df)
for (ii in 1:rows){
  VoteMatrix[tc_df$id_juez[ii],tc_df$ID_PAT[ii]] <- tc_df$Type_Vote[ii]
}





#### IRT estimation with pscl package ----------------------------------------

library(pscl)
library(plyr)
library(plotly)


# indifference points individual judges ------------------------------------------


# prepare data

VoteMatrix <- as.data.frame(VoteMatrix)

sapply(VoteMatrix, function(col) length(unique(col)))
VoteMatrix <- VoteMatrix[rowSums(is.na(VoteMatrix)) != ncol(VoteMatrix), 
                         sapply(VoteMatrix, function(col) length(unique(col))) > 2]

vote_rc <- rollcall(VoteMatrix, 
                    legis.names = rownames(VoteMatrix), 
                    vote.names = colnames(VoteMatrix))

# run estimation
set.seed(342789)
irt_pscl <- ideal(vote_rc, maxiter = 10000000, burnin = 10000, 
                  thin = 2500, store.item =  T, verbose = T)


# postprocess to normalize

irt_pscl <- postProcess(irt_pscl, "normalize")

# summarize results

summary(irt_pscl)


# plot ideal points and uncertainty

plot.ideal(irt_pscl, showAllNames = T)


# predict model

preds <- predict(irt_pscl)
str(preds)
preds$pred.probs
write.csv(preds$legis.percent, "predicted_votes_judges_full.csv")
write.csv(preds$pred.probs, "pre_prob_judges_full.csv")

# save ideal points judges

theta_judges <- cbind(summary(irt_pscl)$xm,summary(irt_pscl)$xsd) %>% 
  as.data.frame() %>%
  rename(ideal = 1, SD = 2)

info_judge <- data %>% 
  select(id_juez, Ideology)%>% group_by(Ideology,id_juez)%>% distinct()

theta_judges <- merge(theta_judges,info_judge, by.x = 0, by.y = "id_juez") %>%
  rename(id_juez = Row.names)

write.csv(theta_judges, "indifference_estimates_full.csv")

m1<-lm(ideal ~ Ideology,
       weights = 1/SD,
       data = theta_judges)
summary(m1)

# save item parameters

beta <-irt_pscl$betabar[,"Discrimination D1"]
alpha <- irt_pscl$betabar[,"Difficulty"]

item_parameters <- as.data.frame(cbind(alpha, beta))

item_parameters <- item_parameters %>% rownames_to_column(var = "item")

write.csv(item_parameters, "item_parameters_full.csv")
