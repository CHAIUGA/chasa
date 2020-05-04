#* @apiTitle Plumber Example API
if(!require("jsonlite")) install.packages("jsonlite")
if(!require(dplyr)) install.packages("dplyr")
library(jsonlite)
library(plumber)
library(dplyr)

model <- readRDS("disease-prediction-model.rds")


#* Return the predicted diseases

#* @param diseases The diseases to predict
#* @param user_located The location name
#* @param user_month The month to predict

#* @post /predict_disease_incident
function(diseases,user_located, user_month) {
  
  originalData <- read.csv(url("https://ndownloader.figshare.com/files/22519493"))
  df  <- originalData
  change2<- function(vec){
    tb <- c( "1" = "Nakasongola", "2" = "Entebbe","3" = "Butambala", "4" = "Gulu", "5" = "Kampala", "6" ="Sembabule", "7" = "Soroti", "8" = "Kitgum", "9"="Nakaseke")
    unname(tb[vec])
  }
  
  df$loca<-lapply(df$location,change2)
  
  df$located<-as.character(df$loca)
  drops <- c("loca")
  df<-df[ , !(names(df) %in% drops)]
  df<-df[,-16]
  originalData <- df
  originalData #copy of original data with location names added
  #corelations
  x<-df[3:11]
  cor(x, use="complete.obs", method="pearson")## note mean temp, humidity and max temp
  #scaling
  colMeans(x)
  y<-scale(x)
  colMeans(y)
  cor(y, use="complete.obs", method="pearson")
  say<-df[-c(3:11)]
  dfs<-cbind(say,y)
  
  sel<-as.data.frame(dfs)
  
  sel$dised2 <- as.integer(as.factor(sel$disease))
  sel<-sel[!(sel$disease=="Yellow_fever"), ]# only two values found these are removed
  sel<-sel[!(sel$disease=="Guinea_worm"), ]# only one observation found makes randomisation difficult
  sel$disease <- factor(sel$disease)
  sel$month2<-as.numeric(sel$month)
  ##clean up
  rm(x,y,dfs,say,df,change2)

  t1<-predict(model, newdata = sel, allow_new_levels=TRUE)
  results<-cbind(sel,t1)
  
  estimatesDF <- results %>% select(disease, located, month, Estimate) %>% filter(disease %in% c(diseases)) %>% filter(located %in% c(user_located)) %>% filter(month %in% c(user_month)) %>% select(Estimate)
  response <- originalData %>% select(preasure,rain,sun,humidity,mean_temp,max_temp,min_temp, wind_gust,mean_wind_spd, disease, located, month) %>% filter(disease %in% c(diseases)) %>% filter(located %in% c(user_located)) %>% filter(month %in% c(user_month))
  
  response <- cbind(response,estimatesDF)
  
  response
}

