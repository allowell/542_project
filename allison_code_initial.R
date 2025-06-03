
#Loading in the data
#data <- read.csv("RW18_28_stdyPLOTlong.csv", header = T)

#install.packages("readxl")
library(readxl)

#Trying to combine all the data into one with relevant info
treegrowth <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "treegrowth")
treatments <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "treatments")
plotinfo <- read_excel("RW28 data_foliar_and_soil_nutrients.xlsx", sheet = "plotinfo")

maindata <- treegrowth[,c(1,4,5,7)]


#Merging
library(dplyr)
library(stringr)

plotinfo_subset <- plotinfo %>%
  select(STDY, PLOT, TRT_CODE, TRT_VARIATION, SIZE_TREAT)

first_combined <- maindata %>%
  left_join(plotinfo_subset, by = c("STDY", "PLOT"))

treatments <- treatments %>%
  mutate(TRT_CODE = str_pad(as.character(TRT_CODE), width = 3, pad = "0")) %>%
  select(TRT_CODE, TRT_VARIATION, TRT_DESCRIP, N_RATE, P_RATE, K_RATE)

main_combine <- first_combined %>%
  left_join(treatments, by = c("TRT_CODE", "TRT_VARIATION"))


#Yay, looks like main_combine has the proper data now!

#Reading in the shape files
library(sf)
plot_shapes <- st_read("Regionwide28_18plots.shp", options = "ENCODING=UTF-8")

library(ggplot2)
ggplot(plot_shapes[1:40,]) +
  geom_sf() +
  theme_minimal()
