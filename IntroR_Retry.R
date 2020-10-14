library(tidyverse)
library(readxl)

excel_sheets("Input_Data/week1/2013_2019_CalFire_Redbook.xlsx")
calfire.data <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet = "Data")

socal.fires <- calfire.data %>% 
  filter(County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "ORANGE", "SAN DIEGO") & Total_Acres_Burned >= 500 | Fire_Name == "THOMAS") %>% 
  mutate_at(vars(Structures_Destroyed:Civil_Fatalities), replace_na, 0) %>% 
  mutate(Fatalities = Fire_Fatalities + Civil_Fatalities,
         interv = interval(Start_Date, Controlled_Date),
         dur = as.duration(interv),
         days = as.numeric(dur, "days"),
         
         County_Unit = ifelse(County_Unit == "VENTURA/SANTA BARBARA", "VENTURA", County_Unit)
  ) %>% 
  arrange(desc(Start_Date), Total_Acres_Burned)

##### ggplot #####
socal.plot <- socal.fires %>% 
  rename(start = Start Date,
         acres = Total_Acres_Burned) %>% 
  ggplot(aes(x= start, y= acres)) +
  geom_point()

socal.plot <- socal.fires %>%
  rename(start = Start_Date,
         acres = Total_Acres_Burned,
         county = County_Unit) %>% 
  ggplot(aes(x = start, y = acres)) +
  geom_point(aes(color = county)) +
  ggtitle("California SoCal Major Fires 2013 - 2018") +
  xlab("Date") +
  ylab("Acres Burned") +
  theme(panel.grid.major = element_blank())
