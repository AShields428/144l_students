#####EEMB 144L Intro to R with CAL FIRE data#####
#Alli Shields
#10/5/2020

#####Load Packages#####

#tidyverse is a collection of many useful programs
#install.packages('tidyverse')
library(tidyverse)

#install.packages ("dplyr")
library(dplyr)

#readxl is used to read excel files
install.packages("readxl")
library(readxl)

#install.packages("praise")
library(praise) 

#####Load Dataset#####

excel_sheets("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx")

metadata <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet = "Metadata")

data <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet = "Data")

View(data)
View(metadata)

#To clear your environment you can type in the following: rm(list= ls()) or hit the broom icon

##### Initial Data Exploration #####
names(data)
dim(data)
class(data)
head(data)
tail(data)
str(data)
typeof(data$Total_Acres_Burned) #single columns can be referred to using '$'

max(data$Total_Acres_Burned)
max(data$Structures_Destroyed)
?max
max(data$Structures_Destroyed, na.rm = T)

summary(data)

##### Basic data wrangling (dplyr functions) #####

glimpse(data)

df1 <- select(data, County_Unit:Controlled_Date, Total_Acres_Burned, Cause:Structures_Damaged)
unique(df1$County_Unit)

df2 <- filter(df1, County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "SAN DIEGO", "ORANGE", "VENTURA/SANTA BARBARA") & Total_Acres_Burned >= 500)

# | = "or"
# == - "equals"/"matches, %in% c()
#& = "and"

df3 <- arrange(df2, desc(Total_Acres_Burned))

df4 <- mutate_at(df3, vars("Structures_Destroyed", "Structures_Damaged"), replace_na, 0)


df5<- mutate(df4, struc_impact = Structures_Damaged + Structures_Destroyed)

#mess with time
install.packages(("lubridate"))

library(lubridate)

df6 <- mutate(df5, interv = interval(Start_Date, Controlled_Date), 
              dur = as.duration(interv))
              days = as.numeric(dur, "days")
              
##### Introduction to Piping #####

socal.fires <- data %>% 
                #mac: cmd + shift+ m
                
  select(County_Unit:Controlled_Date, Total_Acres_Burned, Cause:Structures_Damaged) %>% 
  filter(County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "SAN DIEGO", "ORANGE", "VENTURA/SANTA BARBARA") & Total_Acres_Burned >= 500) %>% 
                arrange((desc(Total_Acres_Burned)) %>% 
                          mutate(struct_impact = Structures_Damaged + Structures_Destroyed),
                        interv = interval(Start_Date, Controlled_Date), 
                        dur = as.duration(interv))
                        days = as.numeric(dur, "days")



