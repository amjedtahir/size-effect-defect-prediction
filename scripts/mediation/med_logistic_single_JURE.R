# author: amjed tahir (a.tahir@massey.ac.nz)

# mediation analysis using individual metrics (univeriant) and binary dependent variable (bug-proneness)

# see the associated paper “Does class size matter? 
#An in-depth assessment of the effect of class size in software defect prediction”  for more details of this analysis

library(mediation)
library(stargazer)
library(tidyverse)
library(magrittr)
library(dplyr)


setwd("/cloud/project/data/JURE/")
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory




for (k in 1:length(listcsv)){
  
  
  ldf[[k]] <- read.csv(listcsv[k] , header = TRUE, stringsAsFactors = FALSE)
 
  filename <- sub("csv", "txt", listcsv[[k]])
  
  name = paste("/cloud/project/output/mediation/JURE/logistic/",filename)
  sink(file = name)
  
  
  df <- ldf[[k]] %>% select(c(4,7,8,9,10,11,14,25)) %>% mutate(LOC=log(loc+1))
  
  
  header <- sub(".csv", "", listcsv[[k]])
  header <- paste("****",header, "****\n\n\n")
  
  cat(header)
  
  
  cat("\n----------------------------------------\n")
  
  cat("RFC\n")
  
  model_rfc1 <- lm( data = df ,LOC ~ rfc)
  model_rfc2 <- glm( data = df ,buggy ~ rfc + LOC, family = binomial)
  fitMod_rfc <- mediate(model_rfc1 , model_rfc2 , treat = "rfc" ,  mediator = "LOC", boot = TRUE, sims = 5000)
  
  # summary(fitMod_rfc)
  # capture.output( summary(fitMod_rfc), file, append=TRUE)
  
  print( summary(fitMod_rfc))
  

  


  cat("----------------------------------------")

  cat("WMC\n")

  modelwmc1 <- lm( data = df ,LOC ~ wmc)
  modelwmc2 <- glm( data = df ,buggy ~ wmc + LOC, family = binomial)
  fitModwmc <- mediate(modelwmc1 , modelwmc2 , treat = "wmc" , mediator = "LOC", boot = TRUE, sims = 5000)
  print(summary(fitModwmc))


  cat("----------------------------------------")

  cat("CBO\n")

  model_cbo1 <- lm( data = df ,LOC ~ cbo)
  model_cbo2 <- glm( data = df ,buggy ~ cbo + LOC, family = binomial)
  fitMod_cbo <- mediate(model_cbo1 , model_cbo2 , treat = "cbo" , mediator = "LOC", boot = TRUE, sims = 5000)
  print(summary(fitMod_cbo))

  # cat("----------------------------------------")

  # cat("DIT")
  #
  # model_dit1 <- glm( data = df ,loc ~ dit, family = binomial)
  # model_dit2 <- glm( data = df ,buggy ~ dit + loc, family = binomial)
  # fitMod_dit <- mediate(model_dit1 , model_dit2 , treat = "dit" , mediator = "loc", boot = TRUE, sims = 50000)
  # summary(fitMod_dit)

  cat("----------------------------------------")

  cat("LCOM\n")

  model_lcom1 <- lm( data = df ,LOC ~ lcom)
  model_lcom2 <- glm( data = df ,buggy ~ lcom + LOC, family = binomial)
  fitMod_lcom <- mediate(model_lcom1 , model_lcom2 , treat = "lcom" , mediator = "LOC", boot = TRUE, sims = 5000)
  print(summary(fitMod_lcom))


  cat("----------------------------------------")

  cat("FANIN\n")

  model_fin1 <- lm( data = df ,LOC ~ fanin)
  model_fin2 <- glm( data = df ,buggy ~ fanin + LOC, family = binomial)
  fitMod_fin <- mediate(model_fin1 , model_fin2 , treat = "fanin" , mediator = "LOC", boot = TRUE, sims = 5000)
  print(summary(fitMod_fin))


  cat("----------------------------------------")

  cat("FANOUT\n")

  model_fout1 <- lm( data = df ,LOC ~ fanout)
  model_fout2 <- glm( data = df ,buggy ~ fanout + LOC, family = binomial)
  fitMod_fout <- mediate(model_fout1 , model_fout2, treat = "fanout" , mediator = "LOC", boot = TRUE, sims = 5000)
  print(summary(fitMod_fout))

  
  sink(file = NULL)
  
  
}



