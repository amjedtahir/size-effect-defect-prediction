# author: amjed tahir (a.tahir@massey.ac.nz)

# moderation analysis using individual metrics (univeriant) and count dependent variable (number of bugs)

# see the associated paper “Does class size matter? 
#An in-depth assessment of the effect of class size in software defect prediction”  for more details of this analysis

library(rockchalk)
library(stargazer)
library(dplyr)



setwd("/cloud/project/data/JURE/")
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory


for (k in 1:length(listcsv)){
  
  
  ldf[[k]] <- read.csv(listcsv[k] , header = TRUE, stringsAsFactors = FALSE)
  
  filename <- sub("csv", "txt", listcsv[[k]])
  filegraph <- sub(".csv", "", listcsv[[k]])
  
  name = paste("/cloud/project/output/moderation/JURE/count/",filename)
  print(name) 
  sink(file = name)
  
  # cat(filename) #check, to make sure the above is working
  
  # bug <- ldf[[k]]$bug
  # loc <- ldf[[k]]$loc
  # wmc <- ldf[[k]]$wmc
  # cbo <- ldf[[k]]$cbo
  # rfc <- ldf[[k]]$rfc
  # lcom <- ldf[[k]]$lcom
  # fanin <- ldf[[k]]$fanin
  # fanout <- ldf[[k]]$fanout
  
  
  df <- ldf[[k]] %>% select(c(4,7,8,9,10,11,14,24)) %>% mutate(LOC=log(loc+1))
  

  graph_path <- "/cloud/project/output/moderation/JURE/graphs/count/"
  cat("***RFC***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data = df, bug ~ LOC + rfc + rfc*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","rfc",".png", sep = "")
  
  # png(filename=filegraph_name, width = 750, height = 400)
  png(filename=filegraph_name)
  
  plotSlopes(model, plotx="rfc", modx="LOC", xlab = "RFC", ylab = "Number of defects", modxVals = "std.dev")
  
  print(summary(model))
  stargazer(model,type="text")
  dev.off()
  
  cat("END\n----------------------------------------\n\n\n")
  
  
  cat("***WMC***\n")
  cat("----------------------------------------\n")

  model <- glm( data = df, bug ~ LOC + wmc + wmc*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","wmc",".png", sep = "")

  png(filename=filegraph_name)
  plotSlopes(model, plotx="wmc", modx="LOC", xlab = "WMC", ylab = "Number of defects", modxVals = "std.dev")

  print(summary(model))
  stargazer(model,type="text")

  dev.off()

  cat("END\n----------------------------------------\n")
  

  cat("***CBO***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data = df, bug ~ LOC + cbo + cbo*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","cbo",".png", sep = "")
  
  png(filename=filegraph_name)
  plotSlopes(model, plotx="cbo", modx="LOC", xlab = "CBO", ylab = "Number of defects", modxVals = "std.dev")
  
  print(summary(model))
  stargazer(model,type="text")
  
  dev.off()
  
  cat("END\n----------------------------------------\n")

 
  cat("***LCOM***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data = df, bug ~ LOC + lcom + lcom*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","lcom",".png", sep = "")
  
  png(filename=filegraph_name)
  plotSlopes(model, plotx="lcom", modx="LOC", xlab = "LCOM", ylab = "Number of defects", modxVals = "std.dev")
  
  print(summary(model))
  stargazer(model,type="text")
  
  dev.off()
  
  cat("END\n----------------------------------------\n")
  
  cat("***FANIN***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data = df, bug ~ LOC + fanin + fanin*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","fanin",".png", sep = "")
  
  png(filename=filegraph_name)
  plotSlopes(model, plotx="fanin", modx="LOC", xlab = "Fan In", ylab = "Number of defects", modxVals = "std.dev")
  
  print(summary(model))
  stargazer(model,type="text")
  
  dev.off()
  
  cat("END\n----------------------------------------\n")
  
  cat("***FANOUT***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data = df, bug ~ LOC + fanout + fanout*LOC ,family = "poisson")
  filegraph_name = paste(graph_path,filegraph,"_","fanout",".png", sep = "")
  
  png(filename=filegraph_name)
  plotSlopes(model, plotx="fanout", modx="LOC", xlab = "Fan Out", ylab = "Number of defects", modxVals = "std.dev")
  
  print(summary(model))
  stargazer(model,type="text")
  
  dev.off()
  
  cat("END\n----------------------------------------\n")
  
  sink(file = NULL)
  
  
}



