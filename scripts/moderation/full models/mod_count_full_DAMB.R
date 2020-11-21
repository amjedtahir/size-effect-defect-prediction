library(rockchalk)
library(stargazer)

#moderation analysis using individual metrics (univeriant) with poisson regression 

setwd("/cloud/project/data/DAMB/")
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory


for (k in 1:length(listcsv)){
  
  
  ldf[[k]] <- read.csv(listcsv[k] , header = TRUE, stringsAsFactors = FALSE)
  
  filename <- sub("csv", "txt", listcsv[[k]])
  filegraph <- sub(".csv", "", listcsv[[k]])
  
  name = paste("/cloud/project/output/moderation/DAMB/full/count/",filename)
  print(name) 
  sink(file = name)
  
  cat(paste(filegraph,"\n\n"))
  

  
  df <- ldf[[k]] %>% select(c(2,3,4,5,7,8,9,10,23)) %>% mutate(LOC=log(loc+1))
  
  cat("***Moderation Continuous Full Model***\n")
  cat("----------------------------------------\n")
  
  model <- glm( data= df , bug ~ (rfc + wmc + cbo + lcom + fanin + fanout)*LOC ,family = poisson)
  # filegraph_name = paste("/cloud/project/graphs/moderation/DAMB/full/",filegraph,"_","rfc",".png", sep = "")

  print(summary(model))
  stargazer(model,type="text")
  # dev.off()
  
  cat("END\n----------------------------------------\n\n\n")
  
  
  sink(file = NULL)
  
  
}



