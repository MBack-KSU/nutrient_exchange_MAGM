data <- read.csv("C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\Dissertation\\MAGM nutrient flux manuscript\\data\\mims_data\\2022_mimsy_trial\\data_mimsy.csv", 
                 header = TRUE, stringsAsFactors = FALSE)
summary(data)
library(mimsy)
results <- mimsy(data, baromet.press = 1.013, units = 'atm')
results
mimsy.save(results, "C:\\Users\\mikeb\\Documents\\Kinsman-Costello Lab\\H2Ohio\\Soil Scale Measurements\\Intact Core Incubations\\20220712_MAGM\\Gas data\\mimsyCalculations.xlsx")
