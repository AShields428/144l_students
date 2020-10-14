library(tidyverse)
library(readxl)
library(dplyr)

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
ggplot(socal.fires, aes(x= Start_Date, y= Total_Acres_Burned)) +
  geom_point(aes(color = County_Unit)) + 
  ggtitle("CA South Coast Major Fires \n2013 - 2018") + 
  labs(x = "", y = "Total Acres Burned", color = "County") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_grid(rows = "County_Unit", scales = "free")

plot.data <- socal.fires %>% 
  rename(county = County_Unit,
         acres = Total_Acres_Burned,
         start = Start_Date,
         end = Controlled_Date) %>% 
  mutate(year = year(start),
         county = ifelse(county == "VENTURA/SANTA BARBARA", "VENTURA", county)) 

incidents <- plot.data %>% 
  group_by(county, year) %>% 
  tally() %>% 
  ungroup()
  
  incidents.plot <- incidents %>% 
    ggplot(aes(x = year, y = n)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    labs(title = "CA South Coast Major Fire Incidents \n 2013 -2018", x = "", y = "Incidents", color = "County") + 
    theme_bw() +
    facet_grid(rows = "county", scales = "free") 
  
  
  all_incidents <- plot.data %>% 
    group_by(year) %>% 
    tally() %>% 
    ungroup()
  
  all_incidents.plot <- all_incidents %>% 
    ggplot(aes(x = year, y = n)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    labs(title = "CA South Coast Major Fire Incidents \n 2013 -2018", x = "", y = "Incidents") + 
    theme_bw() 
  
  ##### Save Data and Plots ####
  
 saveRDS(socal.fires, file = "Output_Data/Week 1/socal_fires_data.rds")
  write_csv(socal.fires, "Output_Data/Week 1/socal_fires_data.csv")
  
  ggsave(filename = "Fire_Incidents", all_incidents.plot, device = "jpeg", "Output_Data/Week 1/")
         