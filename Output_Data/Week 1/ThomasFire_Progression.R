#Alli Shields
#EEMB 144L
#Thomas Fire Progression


##### Packages #####

#install.packages("lubridate")
library(lubridate)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#install.packages("dplyr")
library(dplyr)

excel_sheets("Input_Data/week1/Thomas_Fire_Progression.xlsx")

Thomasfire.data <- read_excel("Input_Data/week1/Thomas_Fire_Progression.xlsx", sheet = "Data")

#Thomasfire.data <- read_excel("Input_Data/week1/Thomas_Fire_Progression.xlsx", sheet = "Metadata")

##### Data Exploration #####

#names(Thomasfire.data)
#head(Thomasfire.data)
#tail(Thomasfire.data)
#glimpse(Thomasfire.data)

ThomasFire <- Thomasfire.data %>% 
  arrange(desc(Acres_Burned)) %>%
  mutate_at(vars( "PM10"), replace_na, 0) %>%
  mutate(dur = as.duration(interv),
         days = as.numeric(dur, "days")) %>% 

##### ggplot #####

ggplot(ThomasFire, aes(x = Date, y = Acres_Burned)) +
 geom_point() %>%
  


