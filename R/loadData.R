# Load Libraries
library(tidyverse)
library(vroom) # For faster CSV reading

# Load Data
kansasExpoDat <- vroom("kansasExpoDat.csv")
uniqueExposures <- unique(kansasExpoDat$exposureName)

everDxDat <- vroom("everDxDat.csv")
uniqueDx <- unique(everDxDat$condition)
dxClass <- unique(everDxDat$conditionClass)

balloonDat <- vroom("balloonDat.csv")

contingencyChartDat <- vroom("contingencyChartDat.csv")

demoDat <- vroom("demoDat.csv") %>% filter(age < 105 & age > 15)

ageGenderDat <- vroom("ageGenderDat.csv") %>% 
  filter(age < 105 & age > 15)

contingencyChartDemoInfoBoxDat <- vroom("contingencyChartDemoInfoBoxDat.csv")

expoOnlyDat <- balloonDat %>% select(id, exposureName, exposureValue)

#fullDemoDat <- vroom("fullDemoDat.csv")

#milInfoBoxDat <- vroom("milInfoBoxDat.csv")

avgExpoDat <- vroom("avgExpoDat.csv")
