
data <- read.csv("/Users/Janet/Downloads/overwatch-diary.csv", header=TRUE)
library(tidyverse)
library(xfun)
glimpse(data)
data <- data%>%
  select(my_team_sr,enemy_team_sr,map,result,charcter_1)
data$my_team_sr <- as.numeric(data$my_team_sr)
data$enemy_team_sr <- as.numeric(data$enemy_team_sr)
data <- na.omit(data)
data$diff_sr = data$my_team_sr - data$enemy_team_sr
colSums(is.na(data))
summary(data$my_team_sr)
summary(data$enemy_team_sr)
summary(data$diff_sr)

data$map <- as.factor(data$map)
levels(data$map)
data$map = ifelse(data$map == "Dorado"|data$map == "Junkertown"|data$map == "junkerTown"|data$map == "Eichenwade"|data$map == "Eichenwalde"|data$map == "EichenWalde"|data$map == "Gibraltar"|data$map == "Gilbraltar"|data$map == "Hollywood"|data$map == "JunkerTown"|data$map == "King's Row"|data$map == "king's Row"|data$map == "Numbani"|data$map == "king's Row"|data$map == "Route 66","Escort",ifelse(data$map == "Hanamura"|data$map == "Hoirzon"|data$map == "Horizon"|data$map == "ilios"|data$map == "Ilios"|data$map == "Illios"|data$map == "Gibraltar"|data$map == "Gilbraltar"|data$map == "Lijang"|data$map == "lijang Tower"|data$map == "Lijang Tower"|data$map == "LIjang Tower"|data$map == "lijiang Tower"|data$map == "Lijiang Tower"|data$map == "Nepal"|data$map == "Oasis"|data$map == "Temple of Anubis"|data$map == "TEmple of Anubis"|data$map == "Volkaya Industries"|data$map == "Volskaya"|data$map == "Volskaya Industries","Capture",NA))
data <- na.omit(data)
colSums(is.na(data))
data$map <- as.factor(data$map)
summary(data$map)

data$result <- as.factor(data$result)
levels(data$result)
summary(data$result)
data$result = ifelse(data$result == "L"|data$result == "Loss","0",ifelse(data$result == "W"|data$result == "Win","1",NA))
data <- na.omit(data)
colSums(is.na(data))
data$result <- as.factor(data$result)
summary(data$result)

data$charcter_1 <- as.factor(data$charcter_1)
summary(data$charcter_1)
data$charcter_1 = ifelse(data$charcter_1 == "Ana"|data$charcter_1 == "ANa","Ana",ifelse(data$charcter_1 == "Lucio"|data$charcter_1 == "LUcio"|data$charcter_1 == "Luico","Lucio",ifelse(data$charcter_1 == "mercy"|data$charcter_1 == "Mercy"|data$charcter_1 == "Mery","Mercy",ifelse(data$charcter_1 == "Moira","Moira", ifelse(data$charcter_1 == "Symmetra","Symmetra",ifelse(data$charcter_1 == "zen"|data$charcter_1 == "Zen","Zen",NA))))))
data <- na.omit(data)
colSums(is.na(data))
data$charcter_1 <- as.factor(data$charcter_1)
summary(data$charcter_1)                                                                     




library(janitor)
library(tidyverse)
library(survey)
data$Rank <- ifelse(data$my_team_sr<2000,"Silver",
                    ifelse(data$my_team_sr>=3000,"Diamond",
                           ifelse(data$my_team_sr<3000&data$my_team_sr>=2500,"Platinum",
                                  ifelse(data$my_team_sr<2500&data$my_team_sr>=2000,"Gold",NA))))
data$Rank <- as.factor(data$Rank)
summary(data$Rank)  
35000000*0.21
35000000*0.32
35000000*0.25
35000000*0.10

data$fpc<-ifelse(data$Rank=="Silver",7350000,
                 ifelse(data$Rank=="Gold",11200000,
                        ifelse(data$Rank=="Platinum",8750000,
                               ifelse(data$Rank=="Diamond",3500000,0))))
clean_data <- data
glimpse(clean_data)
clean_data$character=clean_data$charcter_1
clean_data<- clean_data%>%select(-charcter_1)
prv.design.strs <- svydesign(id=~1,strata=~Rank, data=clean_data,fpc=~fpc)
svyglm.strs.logit<-svyglm(result ~ my_team_sr+enemy_team_sr+map+character, prv.design.strs, family="binomial")
summary(svyglm.strs.logit)

