
#Loading in the data
data <- read.csv("RW18_28_stdyPLOTlong.csv", header = T)

#install.packages("readxl")
library(readxl)

#Trying to combine all the data into one with relevant info
treegrowth <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "treegrowth")
treatments <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "treatments")
plotinfo <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "plotinfo")

plotinfo_main <- plotinfo[, c(1:6, 8)]
treatments_main <- treatments[, c(2:7)]


#Merging
library(dplyr)
library(stringr)

treatments_main <- treatments_main %>%
  mutate(TRT_CODE = str_pad(as.character(TRT_CODE), width = 3, pad = "0"))

main_plotinfo <- left_join(plotinfo_main, treatments_main, by = c("TRT_CODE", "TRT_VARIATION"))


#Yay, looks like main_plotinfo has the proper data now!
