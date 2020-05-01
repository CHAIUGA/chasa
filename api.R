#* @apiTitle Plumber Example API
if(!require("jsonlite")) install.packages("jsonlite")
library(jsonlite)
library(plumber)

model <- readRDS("disease-prediction-model.rds")


#* Return the predicted diseases
#* @param pressure integer The pressure value
#* @param rain The rain value
#* @param sun The sun value
#* @param humidity The humidity value
#* @param meanTemp The meanTemp value
#* @param maxTemp The maxTemp value
#* @param minTemp The minTemp value
#* @param windGust The windGust value
#* @param minWindSpeed The minWindSpeed value

#* @param diseases The diseases to predict
#* @param location The location id
#* @param located The location name
#* @param month The month to predict

#* @post /predict_disease_incident
function(pressure, rain, sun, humidity, meanTemp, 
         maxTemp, minTemp, windGust, minWindSpeed,
         diseases, 
         location, located, month) {
  
  df <- read.csv("Disease-Weather-finaldb.csv")
  mask<-sample(1:436,60,replace=F)
  initDF = df[df$ID %in% mask, ] # used the original dataframe to pick 60 records at random to play with.
  
  #
  #
  #
  #
  #
  #DATA SENT FROM END USER
  #
  userDiseases <- c(diseases)
  userWeather <- data.frame(preasure=c(as.numeric(pressure)), 
                            rain=c(as.numeric(rain)),
                            sun=c(as.numeric(sun)), 
                            humidity=c(as.numeric(humidity)), 
                            mean_temp=c(as.numeric(meanTemp)), 
                            max_temp=c(as.numeric(maxTemp)), 
                            min_temp=c(as.numeric(minTemp)), 
                            wind_gust=c(as.numeric(windGust)), 
                            mean_wind_spd=c(as.numeric(minWindSpeed)))
  
  userMetadata <- data.frame(location = c(as.numeric(location)), 
                             month=c(month), 
                             located=c(located))
  
  
  tempDF <- data.frame(matrix(ncol = 15, nrow = 0))
  
  for (i in userDiseases) {
    row <- data.frame(location = userMetadata[1,1], total = 0, month = userMetadata[1,2], disease = i, located = userMetadata[1,3], ID = 1, preasure=c(userWeather[1, c("preasure")]), rain=c(userWeather[1, c("rain")]),sun=c(userWeather[1, c("sun")]), humidity=c(userWeather[1, c("humidity")]), mean_temp=c(userWeather[1, c("mean_temp")]), max_temp=c(userWeather[1, c("max_temp")]), min_temp=c(userWeather[1, c("min_temp")]), wind_gust=c(userWeather[1, c("wind_gust")]), mean_wind_spd=c(userWeather[1, c("mean_wind_spd")]))
    
    tempDF <- rbind(tempDF, row)  
  }
  #
  #END DATA MANIPULATION
  #
  #
  #
  
  initDF <- rbind(initDF, tempDF)  
  
  
  initDF$dised2 <- as.integer(as.factor(initDF$disease))
  initDF<-initDF[!(initDF$disease=="Yellow_fever"), ]# only two values found these are removed
  initDF<-initDF[!(initDF$disease=="Guinea_worm"), ]# only one observation found makes randomisation difficult
  initDF$disease <- factor(initDF$disease)
  initDF$month2<-as.numeric(initDF$month)
  x<-initDF[3:11]
  y<-scale(x)
  py<-initDF[,-c(3:11)]
  paramsDF<-cbind(py,y)
  
  model <- readRDS("disease-prediction-model.rds")
  t1<-predict(model, newdata = paramsDF, allow_new_levels=TRUE)
  
  outcomeDF<-cbind(paramsDF,t1)
  disease <- tail(outcomeDF, n=length(userDiseases))#select the appended data at the bottom
  predictions <- disease[,c("disease","Estimate")] #select the disease and estimates columns

  
  predictions #return disease and estimates
}

