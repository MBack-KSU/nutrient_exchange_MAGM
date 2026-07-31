##Author: Mike Back
##Organization: Kent State University
##Project: 2022-2023 Turtle Creek Bay (MAGM) Manuscript
##R version: 4.2.1
##Date updated:4/3/2024

library(tidyverse)

#####
##Import Data
#####

#I think this code wasw just used to look at the differences in surface water
# nutrient concentration prior to putting them all in the combined csv,
#it's not used now

skalar_data_ic_2022 = read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\raw_data\\20220728_gracesproj_intactcores_mb.csv", stringsAsFactors = T)
View(skalar_data_ic_2022)
data_amb_ic_2022 <- skalar_data_ic_2022 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:15, 26:30, 33:159)) %>% 
  mutate(NO3.NO2..Results = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 3.27, (3.27/2))) %>% 
  mutate(orthoP..Results = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 8.02, (8.02/2))) %>% 
  mutate(NH3..Results = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 4.66, (4.66/2))) %>% 
  mutate(across(c("NO3.NO2..Results", "orthoP..Results", "NH3..Results"), ~./1000)) %>% 
  select(c("SampleIdentity", "Comments", "NO3.NO2..Results", "orthoP..Results", "NH3..Results"))
View(data_amb_ic_2022)

skalar_data_ic_2023 = read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\raw_data\\20231031_H2Ohio_MAGM_waterchem_ICIandRB_mb.xls - FullNutrients.csv", stringsAsFactors = T)
View(skalar_data_ic_2023)
data_amb_ic_2023 <- skalar_data_ic_2023 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:16, 27:30, 37:60)) %>% 
  mutate(NO3.NO2..Results = replace(NO3.NO2..Results, NO3.NO2..Results < 12.3, (12.3/2))) %>% 
  mutate(orthoP..Results = replace(orthoP..Results, orthoP..Results < 7.97, (7.97/2))) %>% 
  mutate(NH3..Results = replace(NH3..Results, NH3..Results < 8.57, (8.57/2))) %>% 
  mutate(across(c("NO3.NO2..Results", "orthoP..Results", "NH3..Results"), ~./1000))
View(data_amb_ic_2023)


skalar_data_rb_2022 = read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\raw_data\\20221118_TRUC_MAGMresindeployment.xlsx - FullNutrients.csv", stringsAsFactors = T)
View(skalar_data_rb_2022)
data_amb_rb_2022 <- skalar_data_rb_2022 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:28, 34:37, 48:51, 55:59)) %>% 
  mutate(NO3.NO2..Results = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 7.34, (7.34/2))) %>% 
  mutate(orthoP..Results = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 9.42, (9.42/2))) %>% 
  mutate(NH3..Results = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 12.07, (12.07/2))) %>% 
  mutate(across(c("NO3.NO2..Results", "orthoP..Results", "NH3..Results"), ~./1000)) %>% 
  select(c("SampleIdentity", "Comments", "NO3.NO2..Results", "orthoP..Results", "NH3..Results"))
View(data_amb_rb_2022)

skalar_data_rb_2023_1 = read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\raw_data\\20231031_H2Ohio_MAGM_waterchem_ICIandRB_mb.xls - FullNutrients.csv", stringsAsFactors = T)
View(skalar_data_rb_2023_1)
data_amb_rb_2023_1 <- skalar_data_rb_2023_1 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:36, 41:44, 57:60)) %>% 
  mutate(NO3.NO2..Results = replace(NO3.NO2..Results, NO3.NO2..Results < 12.3, (12.3/2))) %>% 
  mutate(orthoP..Results = replace(orthoP..Results, orthoP..Results < 7.97, (7.97/2))) %>% 
  mutate(NH3..Results = replace(NH3..Results, NH3..Results < 8.57, (8.57/2))) %>% 
  mutate(across(c("NO3.NO2..Results", "orthoP..Results", "NH3..Results"), ~./1000))
View(data_amb_rb_2023_1)

skalar_data_rb_2023_2 = read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\raw_data\\20231205_H2Ohio_filtered_mb.xls - FullNutrients.csv", stringsAsFactors = T)
View(skalar_data_rb_2023_2)
data_amb_rb_2023_2 <- skalar_data_rb_2023_2 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:30, 39:74)) %>% 
  mutate(NO3.NO2..Results = replace(NO3.NO2..Results, NO3.NO2..Results < 4.62, (4.62/2))) %>% 
  mutate(orthoP..Results = replace(orthoP..Results, orthoP..Results < 4.05, (4.05/2))) %>% 
  mutate(NH3..Results = replace(NH3..Results, NH3..Results < 3.55, (3.55/2))) %>% 
  mutate(across(c("NO3.NO2..Results", "orthoP..Results", "NH3..Results"), ~./1000))
View(data_amb_rb_2023_2)

data_amb <- bind_rows(data_amb_ic_2022, data_amb_ic_2023, data_amb_rb_2022, data_amb_rb_2023_1, data_amb_rb_2023_2)

View(data_amb)
write.csv(data_amb, "C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\2022_2023_MAGM_ambient_concentrations.csv")
