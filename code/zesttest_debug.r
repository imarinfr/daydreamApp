setwd("/Users/ivanmarin-franch/06.optocom/03.software/daydreamApp/code/")
library(OPI)
library(deldir)
library(sp)
library(rgeos)
debugSource("zesttests.r")
type <- "size"
eye  <- "R"
size <- 1.72
lum  <- 400
val  <- 1.5 # stop value
load("dayParams.rda")
load("../config/grids.rda")
locs <- grids$practice
testGrid <- "Practice"

opts <- list(size = size, lum = lum, color = dayParams$color, bg = dayParams$bg, minlum = dayParams$minlum, maxlum = dayParams$maxlum,
             minarea = dayParams$minarea, maxarea = dayParams$maxarea,
             presTime = dayParams$presTime, respWindow = dayParams$respWindow, respWinPed = dayParams$respWinPed,
             respTimesLength = dayParams$respTimesLength, minISI = dayParams$minISI)

chooseOpi("SimNo")
opiParams <- opiGetParams("opiInitialize")

opiParams$ip <- "192.168.43.208"
do.call(opiInitialize, opiParams)
do.call(opiSetBackground, list(lum = dayParams$bg, eye = eye))
zestTest(type = type, eye = eye, locs, opts, port = 6011)
opiClose()