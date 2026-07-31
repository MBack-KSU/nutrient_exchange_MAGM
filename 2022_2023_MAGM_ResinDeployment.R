##Author: Mike Back
##Organization: Kent State University
##Project: 2022-2023 Turtle Creek Bay (MAGM) Manuscript
##R version: 4.2.1
##Date updated: 2/18/2025

library(tidyverse)

#####
##Import and Process Data
#####

##2022 data

#raw data from the skalar (nutrient analyzer)
skalar_data_2022 = read.csv("data\\raw_data\\20221123_MAGM_resinextract_rerun_mb.csv", stringsAsFactors = T)

#data collected from the lab (resin wieghts, etc.)
lab_data_2022 = read.csv("data\\lab_data_20220816_MAGM_rb.csv", stringsAsFactors = T)
lab_data_2022$sample_point = as.factor(lab_data_2022$sample_point)
lab_data_2022$bag_id = as.factor(lab_data_2022$bag_id)

#processing raw skalar data
data_2022 <- skalar_data_2022 %>% 
  select(5,6,8,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:16, 26:29, 39:42, 52:55, 65:68, 78:81, 91:94, 104:107, 117:120, 130:149)) %>% #remove unnecessary rows (i.e. standards, QC checks, drifts, washes)
  separate_wider_position(SampleIdentity, c(sample_point = 1, core_id = 1, bag_id = 1)) %>%  #separate "SampleIdentity" col into three cols 
  mutate(NO3.NO2..Results.ug.L. = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 2.02, (2.02/2))) %>% 
  mutate(orthoP..Results.ug.L. = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 20.84, (20.84/2))) %>% 
  mutate(NH3..Results.ug.L. = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 7.69, (7.69/2)))

data_2022 <- right_join(lab_data_2022, data_2022, by = c("sample_point", "core_id", "bag_id"))

View(data_2022)
summary(data_2022)


##2023 data

skalar_data_2023_1 = read.csv("data\\raw_data\\2023_resin_extract_data\\20230913_MAGM_ResinExtraction1_mb.csv", stringsAsFactors = T)
skalar_data_2023_2 = read.csv("data\\raw_data\\2023_resin_extract_data\\20230914_MAGM_ResinExtraction2_mb.csv", stringsAsFactors = T)

lab_data_2023 = read.csv("data\\lab_data_20230829_MAGM_rb.csv", stringsAsFactors = T)
lab_data_2023$sample_point = as.factor(lab_data_2023$sample_point)
lab_data_2023$bag_id = as.factor(lab_data_2023$bag_id)

#first, skalar runs without NOx, cause that data was no good. see metadata
data_2023_1 <- skalar_data_2023_1 %>% 
  select(5,6,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:16, 27:30, 41:44)) %>% #remove unnecessary rows (i.e. standards, QC checks, drifts, washes)
  separate_wider_delim(SampleIdentity, "_", names =  c("sample_point", "bag_id")) %>% #separate "SampleIdentity" col into 2 columns for sample id and bag id. NOTE: this deployment only had one core per sample point.
  add_column(core_id = "A", .before = "bag_id") %>% #add column for core_id to match other code that has 3 cores per sample point. Just call them all core "A"
  mutate(orthoP..Results.ug.L. = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 11.04, (11.04/2))) %>% 
  mutate(NH3..Results.ug.L. = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 10.16, (10.16/2)))

data_2023_2 <- skalar_data_2023_2 %>% 
  select(5,6,12,16) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:16, 27:30, 41:44, 50:58)) %>% #remove unnecessary rows (i.e. standards, QC checks, drifts, washes)
  separate_wider_delim(SampleIdentity, "_", names =  c("sample_point", "bag_id")) %>% #separate "SampleIdentity" col into 2 columns for sample id and bag id. NOTE: this deployment only had one core per sample point.
  add_column(core_id = "A", .before = "bag_id") %>% #add column for core_id to match other code that has 3 cores per sample point. Just call them all core "A"
  mutate(orthoP..Results.ug.L. = replace(orthoP..Results.ug.L., orthoP..Results.ug.L. < 13.37, (13.37/2))) %>% 
  mutate(NH3..Results.ug.L. = replace(NH3..Results.ug.L., NH3..Results.ug.L. < 7.16, (7.16/2)))

data_2023_drp_nh3 <- full_join(data_2023_1, data_2023_2)

#Second, skalar runs with only NOx
skalar_data_2023_1_nox = read.csv("data\\raw_data\\2023_resin_extract_data\\20241204_MAGM_resinExt_2023rerun1_mb.csv", stringsAsFactors = T)
skalar_data_2023_2_nox = read.csv("data\\raw_data\\2023_resin_extract_data\\20241205_MAGM_resinExt_2023rerun2_mb.csv", stringsAsFactors = T)
View(skalar_data_2023_2_nox)

data_2023_1_nox <- skalar_data_2023_1_nox %>% rename(NO3.NO2..Results.ug.L. = NO3.NO2..Results) %>% 
  select(5,8) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:14, 25:29, 40:43)) %>% #remove unnecessary rows (i.e. standards, QC checks, drifts, washes)
  separate_wider_delim(SampleIdentity, "_", names =  c("sample_point", "bag_id")) %>% #separate "SampleIdentity" col into 2 columns for sample id and bag id. NOTE: this deployment only had one core per sample point.
  add_column(core_id = "A", .before = "bag_id") %>% #add column for core_id to match other code that has 3 cores per sample point. Just call them all core "A"
  mutate(NO3.NO2..Results.ug.L. = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 8.52, (8.52/2)))

data_2023_2_nox <- skalar_data_2023_2_nox %>% rename(NO3.NO2..Results.ug.L. = NO3.NO2..Results) %>% 
  select(5,8) %>% #remove unnecessary columns found in the skalar data frame
  filter(!row_number() %in% c(1:15, 26:30, 41:45, 51:60)) %>% #remove unnecessary rows (i.e. standards, QC checks, drifts, washes)
  separate_wider_delim(SampleIdentity, "_", names =  c("sample_point", "bag_id")) %>% #separate "SampleIdentity" col into 2 columns for sample id and bag id. NOTE: this deployment only had one core per sample point.
  add_column(core_id = "A", .before = "bag_id") %>% #add column for core_id to match other code that has 3 cores per sample point. Just call them all core "A"
  mutate(NO3.NO2..Results.ug.L. = replace(NO3.NO2..Results.ug.L., NO3.NO2..Results.ug.L. < 4.48, (4.48/2)))

View(data_2023_2_nox)

#combine the 2 NOx dataframes
data_2023_nox <- full_join(data_2023_1_nox, data_2023_2_nox)

#combine drp, nh3, with nox dataframe
data_2023 <- right_join(data_2023_drp_nh3, data_2023_nox, by = c("sample_point", "core_id", "bag_id"))

#combine nutrient data with lab data

data_2023 <- right_join(lab_data_2023, data_2023, by = c("sample_point", "core_id", "bag_id"))

View(data_2023)

#combine 2022 and 2023 data
data <- full_join(data_2022, data_2023)  #this includes repeated sample points from 2022 and 2023. I.e. we had sample point 1-9 both years

data$sample_point <- as.numeric(data$sample_point) #change sample_point to numeric to change sample points in 2023 (so that there aren't repeats)
data$sample_point <- ifelse(data$date_deployed > 20230101, data$sample_point + 12, data$sample_point) #add 9 to all 2023 sample points so that the data set is 1-25 (1-9 from 2022 and 10-25 from 2023)
data$sample_point <- as.factor(data$sample_point)

View(data)



#####
##Calculate Fluxes From Resin Extraction Concentrations
#####

##orthoP flux

#accumulation calculations
data <- data %>% 
  mutate(data, oP_ugg_resin = 
           (orthoP..Results.ug.L. * total_extractant_added_L) / 
           (extraction_subsample_ww * percent_dw)) %>% #convert extractant conc (ug/L) to ug/g of resin
  mutate(data, oP_ug_whole_bag = (oP_ugg_resin * resin_added * perc_dw_org)) %>% #find mass of nutrient in the bag of resin
  mutate(data, oP_ug_m2_d = oP_ug_whole_bag / core_sa_m2 / days_deployed) #do we need this step? this is used to calulate difference in accumlation rate

#see which cores are oversaturated (middle bag >30% of total accumulation)
oP_midbag_perc <- data %>% arrange(bag_id) %>% group_by(sample_point,core_id) %>% #assign the bag id with corresponding sample points and core id's
  summarize(oP_midbag_perc = (oP_ug_whole_bag[2] / #the 2 is refering to the second number in the group series. i.e. bag 2 in core A at sample point 1
                                (oP_ug_whole_bag[1] + oP_ug_whole_bag[2] + oP_ug_whole_bag[3])) *100) #calculate the percent of nutrient accumulated in the middle bag, if accumulated amount in the middle bag is over 25% of the accumulated amount in all 3 bags together, then the top or bottom bag was over saturated (i.e. core is useless:())

View(oP_midbag_perc)

#flux calculations
oP_flux_rb <- data %>% group_by(sample_point, core_id) %>% 
  summarize(oP_flux_mg_m2_d = ((oP_ug_whole_bag[3] - oP_ug_whole_bag[1]) / 1000) / 
              core_sa_m2 / days_deployed) %>% 
  summarize(oP_flux_mg_m2_d = mean(oP_flux_mg_m2_d))#calculate flux (mass of bottom bag - mass of top bag) / 1000(unit conversion ug to mg) / surface area of the core / days deployed
View(oP_flux_rb)#in mg/m2/day


##NO3 flux

#same as above code for DRP

data <- data %>% #repeat code for from orthoP
  mutate(data, NO3_ugg_resin = 
           (NO3.NO2..Results.ug.L. * total_extractant_added_L) / 
           (extraction_subsample_ww * percent_dw)) %>% 
  mutate(data, NO3_ug_whole_bag = (NO3_ugg_resin * resin_added * perc_dw_org)) %>% 
  mutate(data, NO3_ug_m2_d = NO3_ug_whole_bag / core_sa_m2 / days_deployed) ##do we need this step? this is used to calulate difference in accumlation rate

NO3_midbag_perc <- data %>% arrange(bag_id) %>% group_by(sample_point, core_id) %>% 
  summarize(NO3_midbag_perc = (NO3_ug_whole_bag[2] / 
                                 (NO3_ug_whole_bag[1] + NO3_ug_whole_bag[2] + NO3_ug_whole_bag[3])) * 100)
View(NO3_midbag_perc)

NO3_flux_rb <- data %>% arrange(bag_id) %>% group_by(sample_point, core_id) %>% 
  summarize(NO3_flux_mg_m2_d = ((NO3_ug_whole_bag[3] - NO3_ug_whole_bag[1]) / 1000) / 
              core_sa_m2 / days_deployed) %>% 
  summarize(NO3_flux_mg_m2_d = mean(NO3_flux_mg_m2_d))
View(NO3_flux_rb)


##NH3 flux

#same as above code for DRP

data <- data %>% 
  mutate(data, NH3_ugg_resin = 
           (NH3..Results.ug.L. * total_extractant_added_L) / 
           (extraction_subsample_ww * percent_dw)) %>% 
  mutate(data, NH3_ug_whole_bag = (NH3_ugg_resin * resin_added * perc_dw_org)) %>% 
  mutate(data, NH3_ug_m2_d = NH3_ug_whole_bag / core_sa_m2 / days_deployed) ##do we need this step? this is used to calulate difference in accumlation rate

NH3_midbag_perc <- data %>% arrange(bag_id) %>% group_by(sample_point, core_id) %>% 
  summarize(NH3_midbag_perc = (NH3_ug_whole_bag[2] / 
                                 (NH3_ug_whole_bag[1] + NH3_ug_whole_bag[2] + NH3_ug_whole_bag[3])) * 100)
View(NH3_midbag_perc)

NH3_flux_rb <- data %>% arrange(bag_id) %>% group_by(sample_point, core_id) %>% 
  summarize(NH3_flux_mg_m2_d = ((NH3_ug_whole_bag[3] - NH3_ug_whole_bag[1]) / 1000) / 
              core_sa_m2 / days_deployed) %>% 
  summarize(NH3_flux_mg_m2_d = mean(NH3_flux_mg_m2_d))
View(NH3_flux_rb)



#####
##Table for accumulation rate of each bag (FOR SUPPLEMENTARY INFO of manuscript)
#####
options(pillar.sigfig = 3)

accum_each_bag <- data %>% 
  select(sample_point, core_id, bag_id, oP_ug_m2_d, NO3_ug_m2_d, NH3_ug_m2_d) %>% 
  mutate(oP_mg_m2_d = oP_ug_m2_d/1000) %>% 
  mutate(NO3_mg_m2_d = NO3_ug_m2_d/1000) %>% 
  mutate(NH3_mg_m2_d = NH3_ug_m2_d/1000) %>% 
  select(sample_point, core_id, bag_id, oP_mg_m2_d, NO3_mg_m2_d, NH3_mg_m2_d) %>% 
  group_by(sample_point, bag_id) %>% 
  summarize(mean_drp_accum = mean(oP_mg_m2_d), 
            mean_nox_accum = mean(NO3_mg_m2_d), 
            mean_nh3_accum = mean(NH3_mg_m2_d)) %>% 
  mutate(across(where(is.numeric), round, 2))

View(accum_each_bag)

write.csv(accum_each_bag, "FOR_SUPP_TABLE_resin_accumulation_each_bag.csv", row.names=FALSE)



#####
##Data organization
#####

#combine fluxes of all three nutrients

flux_data_oP <- right_join(oP_flux_rb, 
                           oP_midbag_perc, 
                           by = c("sample_point", "core_id"))
flux_data_nox <- right_join(NO3_flux_rb, 
                            NO3_midbag_perc, 
                            by = c("sample_point", "core_id"))
flux_data_nh3 <- right_join(NH3_flux_rb, 
                            NH3_midbag_perc, 
                            by = c("sample_point", "core_id"))
flux_data_op_nox <- right_join(flux_data_oP, 
                               flux_data_nox, 
                               by = c("sample_point", "core_id"))
flux_data <- right_join(flux_data_op_nox, 
                        flux_data_nh3, 
                        by = c("sample_point", "core_id"))
View(flux_data)

#remove oversaturated core data, middle bag acculumation >30% of total core accumulation (sum of top, middle, bottom bags)

flux_data <- flux_data %>% 
  mutate(oP_flux_mg_m2_d = case_when(oP_midbag_perc > 30 ~ NA_real_, 
                                     TRUE ~ oP_flux_mg_m2_d), 
         NO3_flux_mg_m2_d = case_when(NO3_midbag_perc > 30 ~ NA_real_, 
                                     TRUE ~ NO3_flux_mg_m2_d), 
         NH3_flux_mg_m2_d = case_when(NH3_midbag_perc > 30 ~ NA_real_, 
                                     TRUE ~ NH3_flux_mg_m2_d)) %>% 
  select(sample_point, core_id, oP_flux_mg_m2_d, NO3_flux_mg_m2_d, NH3_flux_mg_m2_d)
  

#summarise flux of each nutrient by sample point (because 2022 cores were triplicates at each sample point), mean and standard deviation (sd)

flux_data_summarized <- flux_data %>% group_by(sample_point) %>% 
  summarize(mean_oP_flux_mg_m2_d = mean(oP_flux_mg_m2_d, na.rm = T), 
            sd_oP = sd(oP_flux_mg_m2_d), 
            mean_NO3_flux_mg_m2_d = mean(NO3_flux_mg_m2_d, na.rm = T), 
            sd_NO3 = sd(NO3_flux_mg_m2_d), 
            mean_NH3_flux_mg_m2_d = mean(NH3_flux_mg_m2_d, na.rm = T),
            sd_NH3 = sd(NH3_flux_mg_m2_d))
View(flux_data_summarized)

write.csv(flux_data_summarized, "data\\2022_2023_MAGM_RB_fluxes.csv", row.names=FALSE)
