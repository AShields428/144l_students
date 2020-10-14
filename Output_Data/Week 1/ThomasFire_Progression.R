#Alli Shields
#EEMB 144L
#Thomas Fire Progression


##### Packages #####

install.packages("lubridate")
library(lubridate)

install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

install.packages("dplyr")
library(dplyr)

excel_sheets("Input_Data/week1/Thomas_Fire_Progression.xlsx")

Thomasfire.data <- read_excel("Input_Data/week1/Thomas_Fire_Progression.xlsx", sheet = "Data")

Thomasfire.data <- read_excel("Input_Data/week1/Thomas_Fire_Progression.xlsx", sheet = "Metadata")
