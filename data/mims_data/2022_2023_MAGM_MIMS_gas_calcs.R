##Author: Mike Back, mback@kent.edu
##R version 4.2.1
##Script to analyze data from the MIMS for MAGM manuscript 2024

library(mimsy)
#data is copied from original MIMS document to fit the template for mimsy
#2022 ICI
data_2022 <- read.csv("data\\mims_data\\data_mimsy_2022.csv", 
                 header = TRUE, stringsAsFactors = FALSE)
summary(data_2022)
View(data_2022)

results_2022 <- mimsy(data_2022, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2022
mimsy.save(results_2022, "data\\mims_data\\mimsyCalcs_2022.xlsx")

#2023 ICI
data_2023_1 <- read.csv("data\\mims_data\\data_mimsy_2023_1.csv", 
                      header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_1)

results_2023_1 <- mimsy(data_2023_1, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_1
mimsy.save(results_2023_1, "data\\mims_data\\mimsyCalcs_2023_1.xlsx")

data_2023_2 <- read.csv("data\\mims_data\\data_mimsy_2023_2.csv", 
                        header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_2)
View(data_2023_2)

results_2023_2 <- mimsy(data_2023_2, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_2
mimsy.save(results_2023_2, "data\\mims_data\\mimsyCalcs_2023_2.xlsx")


#####
#below is data compiled after cutting the original file based on changes in drift slope
#####
#2022 ICI
#data_2022 <- read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\mims_data\\data_mimsy_2022.csv", 
#                      header = TRUE, stringsAsFactors = FALSE)
#summary(data_2022)
#View(data_2022)

#results_2022 <- mimsy(data_2022, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
#results_2022
#mimsy.save(results_2022, "C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\mims_data\\mimsyCalcs_2022.xlsx")

#2023 ICI
#Cut 1, Standards 1-3
data_2023_1_cut1 <- read.csv("data\\mims_data\\test_to_verify_mimsy\\data_mimsy_2023_1_cut1.csv", 
                        header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_1_cut1)
View(data_2023_1_cut1)

results_2023_1_cut1 <- mimsy(data_2023_1_cut1, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_1_cut1
mimsy.save(results_2023_1_cut1, "data\\mims_data\\test_to_verify_mimsy\\mimsyCalcs_2023_1_cut1.xlsx")

#Cut 2, standards 3-4, had to mannually change "Group" column in the CSV from 3 and 4 to 1 and 2 for calculations
data_2023_1_cut2 <- read.csv("data\\mims_data\\test_to_verify_mimsy\\data_mimsy_2023_1_cut2.csv", 
                             header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_1_cut2)

results_2023_1_cut2 <- mimsy(data_2023_1_cut2, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_1_cut2
mimsy.save(results_2023_1_cut2, "data\\mims_data\\test_to_verify_mimsy\\mimsyCalcs_2023_1_cut2.xlsx")

#Cut 3, standards 4-6, had to change group from 4-6 to 1-3 for calculations
data_2023_1_cut3 <- read.csv("data\\mims_data\\test_to_verify_mimsy\\data_mimsy_2023_1_cut3.csv", 
                             header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_1_cut3)

results_2023_1_cut3 <- mimsy(data_2023_1_cut3, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_1_cut3
mimsy.save(results_2023_1_cut3, "data\\mims_data\\test_to_verify_mimsy\\mimsyCalcs_2023_1_cut3.xlsx")


#Cut 4, standards 6-7,, had to change group from 6 and 7 to 1 and 2 for calculations
data_2023_1_cut4 <- read.csv("data\\mims_data\\test_to_verify_mimsy\\data_mimsy_2023_1_cut4_final.csv", 
                             header = TRUE, stringsAsFactors = FALSE)
summary(data_2023_1_cut4)

results_2023_1_cut4 <- mimsy(data_2023_1_cut4, baromet.press = 1.00, units = 'atm') #couldn't find barometric pressure for 10/27/2022, so I used barometric pressure of Kent Ohio on 7/22/2024 via the barometric pressure app (apple) of 30.03 inHg converted to atm
results_2023_1_cut4
mimsy.save(results_2023_1_cut4, "data\\mims_data\\test_to_verify_mimsy\\mimsyCalcs_2023_1_cut4.xlsx")

#comparison with WSU method for calculating gas concentration showed a
#consistently small difference against mimsy calcs, even when the files
#were cut to account for drift
#therefore, mimsy accounts for change in slope of the drift