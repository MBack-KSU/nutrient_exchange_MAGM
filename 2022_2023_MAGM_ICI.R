##Author: Mike Back
##Organization: Kent State University
##Project: 2022-2023 Magee Marsh Nutrient Flux
##R version: 4.2.1
##Date updated:9/27/2023


library(tidyverse)

#####
##Import Data
#Processing skalar data
#####

#2022 data

skalar_data_2022 = read.csv("data\\raw_data\\20220728_gracesproj_intactcores_mb.csv", stringsAsFactors = T)

data_2022 <- skalar_data_2022 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:95, 106:108, 119:121, 132:134, 145:147, 156:159)) %>% 
  mutate(NO3.NO2..Results.ug.L. = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 3.27, (3.27/2))) %>% 
  mutate(orthoP..Results.ug.L. = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 8.02, (8.02/2))) %>% 
  mutate(NH3..Results.ug.L. = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 4.66, (4.66/2)))
View(data_2022)
write.csv(data_2022, "data\\2022_MAGM_ICI_skalardata.csv")

#2023 data

skalar_data_2023 = read.csv("data\\raw_data\\20230815_H2Ohio_MAGM_ICI_mb.csv", stringsAsFactors = T)

data_2023 <- skalar_data_2023 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:15, 26:29, 40:43, 54:57, 68:71, 82:85, 96:102)) %>% 
  mutate(NO3.NO2..Results.ug.L. = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 10.29, (10.29/2))) %>% 
  mutate(orthoP..Results.ug.L. = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 3.89, (3.89/2))) %>% 
  mutate(NH3..Results.ug.L. = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 2.15, (2.15/2)))
View(skalar_data_2023)
write.csv(data_2023, "data\\2023_MAGM_ICI_skalardata.csv")

#below code from H2Ohio github
#####
##Import data
#Post-processing data combined into one file
#####

#"2022_2023_MAGM_ICI_skalar_combined.csv" file was manually combined from the above
#processed data files and combined with flow rate  and core surface area data

data <- read.csv("data\\2022_2023_MAGM_ICI_skalar_combined.csv", stringsAsFactors = T)
data$core_id <- as.factor(data$core_id)
data$year <- as.factor(data$year)
summary(data)
str(data)
head(data)
View(data)

#####
##Calulate Flux For Each Nutrient
#####

#calulate the flow rate for each core based off of the mL/min and core surface area recorded during incubation
data <- data %>% 
  mutate(data, flow_rate_L_hr_m2 = 
           ((flow_rate / 1000) * 60) / core_sa_m2)#divide by 1000 to convert mL to L and multiple by 60 to convert min to hours


##orthoP flux
oP_flux <- data %>% #this calulates flux of each core at each of the three time points
  arrange(habitat) %>% #so you can better see groups of the same patch types
  group_by(core_id, habitat, time) %>% #this line allows you to see all three of these variables in the final table
  summarize(oP_flux_mg_m2_d = (out_orthoP_ugL - in_orthoP_ugL) * flow_rate_L_hr_m2/ 1000 * 24)#convert from ug to mg and hr to day
View(oP_flux)

oP_flux_mean_time <- oP_flux %>% #average the three time points for each core
  summarize(oP_flux_mg_m2_d = mean(oP_flux_mg_m2_d))
View(oP_flux_mean_time)


oP_flux_habitat <- oP_flux %>% group_by(habitat) %>% #average each of the habitat(patch) flux from all three of the cores and all three of their time points
  summarize(SE = sd(oP_flux_mg_m2_d) / sqrt(9), oP_flux_mg_m2_d = mean(oP_flux_mg_m2_d))#N=9 because 3 cores X 3 time points
View(oP_flux_habitat)


##NO3 flux
NO3_flux <- data %>% arrange(habitat) %>% group_by(core_id, habitat, time) %>% #same code as above
  summarize(NO3_flux_mg_m2_d = (out_NO3_ugL - in_NO3_ugL) * flow_rate_L_hr_m2 / 1000 * 24)
View(NO3_flux)

NO3_flux_mean_time <- NO3_flux %>% summarize(NO3_flux_mg_m2_d = mean(NO3_flux_mg_m2_d))
View(NO3_flux_mean_time)


NO3_flux_habitat <- NO3_flux %>% group_by(habitat) %>% 
  summarize(SE = sd(NO3_flux_mg_m2_d) / sqrt(9), NO3_flux_mg_m2_d = mean(NO3_flux_mg_m2_d))
View(NO3_flux_habitat)


##NH3 flux
NH3_flux <- data %>% arrange(habitat) %>% group_by(core_id, habitat, time) %>% 
  summarize(NH3_flux_mg_m2_d = (out_NH3_ugL - in_NH3_ugL) * flow_rate_L_hr_m2 / 1000 * 24)
View(NH3_flux)

NH3_flux_mean_time <- NH3_flux %>% summarize(NH3_flux_mg_m2_d = mean(NH3_flux_mg_m2_d))
View(NH3_flux_mean_time)


NH3_flux_habitat <- NH3_flux %>% group_by(habitat) %>% 
  summarize(SE = sd(NH3_flux_mg_m2_d) / sqrt(9), NH3_flux_mg_m2_d = mean(NH3_flux_mg_m2_d))
View(NH3_flux_habitat)

##Combine all three nutrient flux data
flux_data_oPandNO3 <- right_join(oP_flux_mean_time, 
                                 NO3_flux_mean_time, 
                                 by = c("core_id", "habitat"))
flux_data <- right_join(flux_data_oPandNO3, 
                        NH3_flux_mean_time, 
                        by = c("core_id", "habitat"))
View(flux_data)

write.csv(flux_data, "data\\2022_2023_MAGM_ICI_fluxes.csv", row.names=FALSE)

#####
#Time series for inflow nutrient concentrations
#####
data %>% group_by(time, habitat) %>% 
  ggplot(aes(x = time, y = in_NO3_ugL)) + 
  geom_point() + 
  facet_grid(year~habitat)
data %>% group_by(time, habitat) %>% 
  ggplot(aes(x = time, y = in_orthoP_ugL)) + 
  geom_point() + 
  facet_grid(year~habitat)
data %>% group_by(time, habitat) %>% 
  ggplot(aes(x = time, y = in_NH3_ugL)) + 
  geom_point() + 
  facet_grid(year~habitat)


#####
#Time Series Graphs of fluxes
library(ggpubr)#loading this package may break the above code, fyi
#####
#orthoP
core_1 <- subset(oP_flux, core_id == "1")
core_2 <- subset(oP_flux, core_id == "2")
core_3 <- subset(oP_flux, core_id == "3")

core_4 <- subset(oP_flux, core_id == "4")
core_5 <- subset(oP_flux, core_id == "5")
core_6 <- subset(oP_flux, core_id == "6")

core_7 <- subset(oP_flux, core_id == "7")
core_8 <- subset(oP_flux, core_id == "8")
core_9 <- subset(oP_flux, core_id == "9")

core_10 <- subset(oP_flux, core_id == "10")
core_11 <- subset(oP_flux, core_id == "11")
core_12 <- subset(oP_flux, core_id == "12")

core_13 <- subset(oP_flux, core_id == "13")
core_14 <- subset(oP_flux, core_id == "14")
core_15 <- subset(oP_flux, core_id == "15")
core_16 <- subset(oP_flux, core_id == "16")

core_17 <- subset(oP_flux, core_id == "17")
core_18 <- subset(oP_flux, core_id == "18")
core_19 <- subset(oP_flux, core_id == "19")
core_20 <- subset(oP_flux, core_id == "20")

core_21 <- subset(oP_flux, core_id == "21")
core_22 <- subset(oP_flux, core_id == "22")
core_23 <- subset(oP_flux, core_id == "23")
core_24 <- subset(oP_flux, core_id == "24")

core_25 <- subset(oP_flux, core_id == "25")
core_26 <- subset(oP_flux, core_id == "26")
core_27 <- subset(oP_flux, core_id == "27")
core_28 <- subset(oP_flux, core_id == "28")

core_1_time_series <- ggplot(core_1, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_2_time_series <- ggplot(core_2, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_3_time_series <- ggplot(core_3, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_4_time_series <- ggplot(core_4, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_5_time_series <- ggplot(core_5, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_6_time_series <- ggplot(core_6, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_7_time_series <- ggplot(core_7, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_8_time_series <- ggplot(core_8, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_9_time_series <- ggplot(core_9, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_10_time_series <- ggplot(core_10, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_11_time_series <- ggplot(core_11, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_12_time_series <- ggplot(core_12, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,10)
core_13_time_series <- ggplot(core_13, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_14_time_series <- ggplot(core_14, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_15_time_series <- ggplot(core_15, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_16_time_series <- ggplot(core_16, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_17_time_series <- ggplot(core_17, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_18_time_series <- ggplot(core_18, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_19_time_series <- ggplot(core_19, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_20_time_series <- ggplot(core_20, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_21_time_series <- ggplot(core_21, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_22_time_series <- ggplot(core_22, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_23_time_series <- ggplot(core_23, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_24_time_series <- ggplot(core_24, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_25_time_series <- ggplot(core_25, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_26_time_series <- ggplot(core_26, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)
core_27_time_series <- ggplot(core_27, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20.4)
core_28_time_series <- ggplot(core_28, aes(x = time, y = oP_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-12.5,20)

##NO3
core_1 <- subset(NO3_flux, core_id == "1")
core_2 <- subset(NO3_flux, core_id == "2")
core_3 <- subset(NO3_flux, core_id == "3")

core_4 <- subset(NO3_flux, core_id == "4")
core_5 <- subset(NO3_flux, core_id == "5")
core_6 <- subset(NO3_flux, core_id == "6")

core_7 <- subset(NO3_flux, core_id == "7")
core_8 <- subset(NO3_flux, core_id == "8")
core_9 <- subset(NO3_flux, core_id == "9")

core_10 <- subset(NO3_flux, core_id == "10")
core_11 <- subset(NO3_flux, core_id == "11")
core_12 <- subset(NO3_flux, core_id == "12")

core_13 <- subset(NO3_flux, core_id == "13")
core_14 <- subset(NO3_flux, core_id == "14")
core_15 <- subset(NO3_flux, core_id == "15")
core_16 <- subset(NO3_flux, core_id == "16")

core_17 <- subset(NO3_flux, core_id == "17")
core_18 <- subset(NO3_flux, core_id == "18")
core_19 <- subset(NO3_flux, core_id == "19")
core_20 <- subset(NO3_flux, core_id == "20")

core_21 <- subset(NO3_flux, core_id == "21")
core_22 <- subset(NO3_flux, core_id == "22")
core_23 <- subset(NO3_flux, core_id == "23")
core_24 <- subset(NO3_flux, core_id == "24")

core_25 <- subset(NO3_flux, core_id == "25")
core_26 <- subset(NO3_flux, core_id == "26")
core_27 <- subset(NO3_flux, core_id == "27")
core_28 <- subset(NO3_flux, core_id == "28")

core_1_time_series <- ggplot(core_1, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_2_time_series <- ggplot(core_2, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_3_time_series <- ggplot(core_3, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_4_time_series <- ggplot(core_4, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_5_time_series <- ggplot(core_5, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_6_time_series <- ggplot(core_6, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_7_time_series <- ggplot(core_7, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_8_time_series <- ggplot(core_8, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_9_time_series <- ggplot(core_9, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_10_time_series <- ggplot(core_10, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_11_time_series <- ggplot(core_11, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_12_time_series <- ggplot(core_12, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_13_time_series <- ggplot(core_13, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_14_time_series <- ggplot(core_14, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_15_time_series <- ggplot(core_15, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_16_time_series <- ggplot(core_16, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_17_time_series <- ggplot(core_17, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_18_time_series <- ggplot(core_18, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_19_time_series <- ggplot(core_19, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_20_time_series <- ggplot(core_20, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_21_time_series <- ggplot(core_21, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_22_time_series <- ggplot(core_22, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_23_time_series <- ggplot(core_23, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_24_time_series <- ggplot(core_24, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_25_time_series <- ggplot(core_25, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_26_time_series <- ggplot(core_26, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_27_time_series <- ggplot(core_27, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)
core_28_time_series <- ggplot(core_28, aes(x = time, y = NO3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-7.5,11.5)


##NH3
core_1 <- subset(NH3_flux, core_id == "1")
core_2 <- subset(NH3_flux, core_id == "2")
core_3 <- subset(NH3_flux, core_id == "3")

core_4 <- subset(NH3_flux, core_id == "4")
core_5 <- subset(NH3_flux, core_id == "5")
core_6 <- subset(NH3_flux, core_id == "6")

core_7 <- subset(NH3_flux, core_id == "7")
core_8 <- subset(NH3_flux, core_id == "8")
core_9 <- subset(NH3_flux, core_id == "9")

core_10 <- subset(NH3_flux, core_id == "10")
core_11 <- subset(NH3_flux, core_id == "11")
core_12 <- subset(NH3_flux, core_id == "12")

core_13 <- subset(NH3_flux, core_id == "13")
core_14 <- subset(NH3_flux, core_id == "14")
core_15 <- subset(NH3_flux, core_id == "15")
core_16 <- subset(NH3_flux, core_id == "16")

core_17 <- subset(NH3_flux, core_id == "17")
core_18 <- subset(NH3_flux, core_id == "18")
core_19 <- subset(NH3_flux, core_id == "19")
core_20 <- subset(NH3_flux, core_id == "20")

core_21 <- subset(NH3_flux, core_id == "21")
core_22 <- subset(NH3_flux, core_id == "22")
core_23 <- subset(NH3_flux, core_id == "23")
core_24 <- subset(NH3_flux, core_id == "24")

core_25 <- subset(NH3_flux, core_id == "25")
core_26 <- subset(NH3_flux, core_id == "26")
core_27 <- subset(NH3_flux, core_id == "27")
core_28 <- subset(NH3_flux, core_id == "28")

core_1_time_series <- ggplot(core_1, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_2_time_series <- ggplot(core_2, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_3_time_series <- ggplot(core_3, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_4_time_series <- ggplot(core_4, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_5_time_series <- ggplot(core_5, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_6_time_series <- ggplot(core_6, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_7_time_series <- ggplot(core_7, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_8_time_series <- ggplot(core_8, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_9_time_series <- ggplot(core_9, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_10_time_series <- ggplot(core_10, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_11_time_series <- ggplot(core_11, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_12_time_series <- ggplot(core_12, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_13_time_series <- ggplot(core_13, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_14_time_series <- ggplot(core_14, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_15_time_series <- ggplot(core_15, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_16_time_series <- ggplot(core_16, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_17_time_series <- ggplot(core_17, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_18_time_series <- ggplot(core_18, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_19_time_series <- ggplot(core_19, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_20_time_series <- ggplot(core_20, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_21_time_series <- ggplot(core_21, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_22_time_series <- ggplot(core_22, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_23_time_series <- ggplot(core_23, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_24_time_series <- ggplot(core_24, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_25_time_series <- ggplot(core_25, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_26_time_series <- ggplot(core_26, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_27_time_series <- ggplot(core_27, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)
core_28_time_series <- ggplot(core_28, aes(x = time, y = NH3_flux_mg_m2_d, group = 1)) + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) + 
  geom_line() + 
  ylim(-10,238)

grasses_2022 <- ggarrange(core_1_time_series, core_2_time_series, core_3_time_series, labels = c("1","2","3"), ncol = 3)
sav_2022 <- ggarrange(core_4_time_series, core_5_time_series, core_6_time_series, labels = c("4","5","6"), ncol = 3)
hardwoods_2022 <- ggarrange(core_7_time_series, core_8_time_series, core_9_time_series, labels = c("7","8","9"), ncol = 3)
typha_2022 <- ggarrange(core_10_time_series, core_11_time_series, core_12_time_series, labels = c("10","11","12"), ncol = 3)
sav_2023 <- ggarrange(core_13_time_series, core_14_time_series, core_15_time_series, core_16_time_series, labels = c("13","14","15","16"), ncol = 4)
favA_2023 <- ggarrange(core_17_time_series, core_18_time_series, core_19_time_series, core_20_time_series, labels = c("17","18","19","20"), ncol = 4)
favB_2023 <- ggarrange(core_21_time_series, core_22_time_series, core_23_time_series, core_24_time_series, labels = c("21","22","23","24"), ncol = 4)
hardwoods_2023 <- ggarrange(core_25_time_series, core_26_time_series, core_27_time_series, core_28_time_series, labels = c("25","26","27","28"), ncol = 4)

ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\grasses_2022.jpeg", 
       plot=grasses_2022, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\sav_2022.jpeg", 
       plot=sav_2022, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\hardwoods_2022.jpeg", 
       plot=hardwoods_2022, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\typha_2022.jpeg", 
       plot=typha_2022, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\sav_2023.jpeg", 
       plot=sav_2023, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\favA_2023.jpeg", 
       plot=favA_2023, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\favB_2023.jpeg", 
       plot=favB_2023, height=10, width=20, units=c("cm"), dpi=600)
ggsave("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\ICI_time_series\\hardwoods_2023.jpeg", 
       plot=hardwoods_2023, height=10, width=20, units=c("cm"), dpi=600)


#####
##Calculate Flux for N2 and O2 dissolved gases
#####
#copy code from above nutrient flux calc but change variable names for N2 and O2
data_gas <- read.csv("data\\mims_data\\2022_2023_MAGM_ICI_gas_combined.csv", stringsAsFactors = T)
data_gas$core_id <- as.factor(data_gas$core_id)
data_gas$year <- as.factor(data_gas$year)
summary(data_gas)
str(data_gas)
head(data_gas)
View(data_gas)

#calulate the flow rate for each core based off of the mL/min and core surface area recorded during incubation
data_gas <- data_gas %>% 
  mutate(data_gas, flow_rate_L_hr_m2 = 
           ((flow_rate / 1000) * 60) / core_sa_m2)#divide by 1000 to convert mL to L and multiple by 60 to convert min to hours


##N2 flux
N2_flux <- data_gas %>% #this calulates flux of each core at each of the three time points
  arrange(habitat) %>% #so you can better see groups of the same patch types
  group_by(core_id, habitat, time) %>% #this line allows you to see all three of these variables in the final table
  summarize(N2_flux_mg_m2_d = (out_N2_mgL - in_N2_mgL) * flow_rate_L_hr_m2 * 24)#convert from hr to day
View(N2_flux)

N2_flux_mean_time <- N2_flux %>% #average the three time points for each core
  summarize(N2_flux_mg_m2_d = mean(N2_flux_mg_m2_d))
View(N2_flux_mean_time)

N2_flux_habitat <- N2_flux %>% group_by(habitat) %>% #average each of the habitat(patch) flux from all three of the cores and all three of their time points
  summarize(SE = sd(N2_flux_mg_m2_d) / sqrt(9), N2_flux_mg_m2_d = mean(N2_flux_mg_m2_d))#N=9 because 3 cores X 3 time points
View(N2_flux_habitat)


##O2 flux
O2_flux <- data_gas %>% #this calulates flux of each core at each of the three time points
  arrange(habitat) %>% #so you can better see groups of the same patch types
  group_by(core_id, habitat, time) %>% #this line allows you to see all three of these variables in the final table
  summarize(O2_flux_mg_m2_d = (out_O2_mgL - in_O2_mgL) * flow_rate_L_hr_m2 * 24)#convert from hr to day
View(O2_flux)

O2_flux_mean_time <- O2_flux %>% #average the three time points for each core
  summarize(O2_flux_mg_m2_d = mean(O2_flux_mg_m2_d))
View(O2_flux_mean_time)

O2_flux_habitat <- O2_flux %>% group_by(habitat) %>% #average each of the habitat(patch) flux from all three of the cores and all three of their time points
  summarize(SE = sd(O2_flux_mg_m2_d) / sqrt(9), O2_flux_mg_m2_d = mean(O2_flux_mg_m2_d))#N=9 because 3 cores X 3 time points
View(O2_flux_habitat)



##Combine both gas flux data
flux_data_gas <- right_join(N2_flux_mean_time, 
                                 O2_flux_mean_time, 
                                 by = c("core_id", "habitat"))
View(flux_data_gas)

write.csv(flux_data_gas, "C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\2022_2023_MAGM_ICI_gas_fluxes.csv", row.names=FALSE)
