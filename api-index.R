if(!require("plumber")) install.packages("plumber")
if(!require("jsonlite")) install.packages("jsonlite")
library(plumber)
library(jsonlite)

getwd()

pr <- plumb("api.R")

swaggerFile <- pr$swaggerFile()
swaggerFile$info$title <- "Disease cases prediction api"
swaggerFile$info$description <- "Returns the diseases predicted incidences" 
swaggerFile$info$version <-"1.0.0"
swagger <- toJSON(swaggerFile, pretty = TRUE, auto_unbox = TRUE)
cat(swagger, file = "plumber-swagger.json", append = FALSE)
pr$run(port=8000)
#http://127.0.0.1:8000/swagger.json

