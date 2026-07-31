##Author: Mike Back
##Organization: Kent State University
##Project: 2022-2023 Turtle Creek Bay (MAGM) Manuscript
##R version: 4.2.1
##Date updated: 2/18/2025

library(tidyverse)
library(lme4) #for LMERs of resin bag data for sample points with 3 cores
library(emmeans)
library(afex)#to give p values to the LMERs
library(MASS)#for AIC model selection
library(MuMIn)#to get r2 for the LMERs
library(car)#for Levene's tests
library(lmtest) #for likelihood ratio tests

#####
##Import Data
#####

data = read.csv("data\\2022_2023_MAGM_fluxes_combined.csv", stringsAsFactors = T)
data$year <- as.factor(data$year)
data$sample_point <- as.factor(data$sample_point)
summary(data)
str(data)
View(data)



#####
##AMBIENT WATER/SOIL ANALYSIS
#####

#t test for if rb vs ic water samples were different

t.test(amb_nox_mgL~method, data  = data)
t.test(amb_drp_mgL~method, data  = data)
t.test(amb_nh3_mgL~method, data  = data)

#not significant;y different, so use the resin bag samples since that better
#relates to the ambient soil samples

rb_data <- subset(data, method == "rb")
summary(rb_data)
View(rb_data)

t.test(amb_nox_mgL~year, data  = rb_data)
t.test(amb_drp_mgL~year, data  = rb_data)
t.test(amb_nh3_mgL~year, data  = rb_data)

##water nutrient concentrations

amb.drp.hab.lm <- lm(amb_drp_mgL~habitat, data = rb_data)
summary(amb.drp.hab.lm)
plot(amb.drp.hab.lm, which=c(1,2))
emm1.amb = emmeans(amb.drp.hab.lm, ~habitat)
contrast(emm1.amb, method = "pairwise", adjust = "tukey")

leveneTest(amb.drp.hab.lm, center = mean) #to test unequal variance

amb.nox.hab.lm <- lm(amb_nox_mgL~habitat, data = rb_data)
summary(amb.nox.hab.lm)
plot(amb.nox.hab.lm, which=c(1,2))
emm2.amb = emmeans(amb.nox.hab.lm, ~habitat)
contrast(emm2.amb, method = "pairwise", adjust = "tukey")

amb.nh3.hab.lm <- lm(amb_nh3_mgL~habitat, data = rb_data)
summary(amb.nh3.hab.lm)
plot(amb.nh3.hab.lm, which=c(1,2))
emm3.amb = emmeans(amb.nh3.hab.lm, ~habitat)
contrast(emm3.amb, method = "pairwise", adjust = "tukey")

#for table of ambient conditions (water nutrient concentrations)

data_amb_water_sum <- rb_data %>% 
  group_by(habitat) %>% 
  summarize(mean_amb_nox = mean(amb_nox_mgL*1000), 
            sd_amb_nox = sd(amb_nox_mgL*1000), 
            mean_amb_drp = mean(amb_drp_mgL*1000), 
            sd_amb_drp = sd(amb_drp_mgL*1000), 
            mean_amb_nh3 = mean(amb_nh3_mgL*1000), 
            sd_amb_nh3 = sd(amb_nh3_mgL*1000), 
            mean_depth = mean(water_depth), 
            sd_depth = sd(water_depth), 
            mean_temp = mean(phys_wtemp_C), 
            sd_temp = sd(phys_wtemp_C), 
            mean_pH = mean(phys_pH), 
            sd_pH = sd(phys_pH), 
            mean_DO = mean(phys_do_mgL), 
            sd_DO = sd(phys_do_mgL), 
            mean_spc = mean(phys_spec_cond), 
            sd_spc = sd(phys_spec_cond), 
            mean_turb = mean(phys_turb_fnu), 
            sd_turb = sd(phys_turb_fnu))
View(data_amb_water_sum)
write.csv(data_amb_water_sum, "FOR_TABLE_ambient_water_summary.csv", row.names = F)

#for supplementary table seperating by year
data_amb_water_sum_years <- rb_data %>% 
  group_by(habitat, year) %>% 
  summarize(mean_amb_nox = mean(amb_nox_mgL*1000), 
            sd_amb_nox = sd(amb_nox_mgL*1000), 
            mean_amb_drp = mean(amb_drp_mgL*1000), 
            sd_amb_drp = sd(amb_drp_mgL*1000), 
            mean_amb_nh3 = mean(amb_nh3_mgL*1000), 
            sd_amb_nh3 = sd(amb_nh3_mgL*1000), 
            mean_depth = mean(water_depth), 
            sd_depth = sd(water_depth), 
            mean_temp = mean(phys_wtemp_C), 
            sd_temp = sd(phys_wtemp_C), 
            mean_pH = mean(phys_pH), 
            sd_pH = sd(phys_pH), 
            mean_DO = mean(phys_do_mgL), 
            sd_DO = sd(phys_do_mgL), 
            mean_spc = mean(phys_spec_cond), 
            sd_spc = sd(phys_spec_cond), 
            mean_turb = mean(phys_turb_fnu), 
            sd_turb = sd(phys_turb_fnu))
View(data_amb_water_sum_years)
write.csv(data_amb_water_sum_years, "FOR_SUPP_TABLE_ambient_water_summary_byYear.csv", row.names = F)


##soil nutrient concentrations

#TP

soilTP.hab.lm <- lm(soil_TP_mg_kg~habitat, data = rb_data)
summary(soilTP.hab.lm)
plot(soilTP.hab.lm, which=c(1,2))
emm1.soil = emmeans(soilTP.hab.lm, ~habitat)
contrast(emm1.soil, method = "pairwise", adjust = "tukey")

#TN

soilTN.hab.lm <- lm(soil_TN_mg_kg~habitat, data = rb_data)
summary(soilTN.hab.lm)
plot(soilTN.hab.lm, which=c(1,2))
emm2.soil = emmeans(soilTN.hab.lm, ~habitat)
contrast(emm2.soil, method = "pairwise", adjust = "tukey")


#to make the soil table

rb_data_no_na <- rb_data %>% 
  dplyr::select(1:13, 17:22, 24:37, 43)
rb_data_no_na <- na.omit(rb_data_no_na)
View(rb_data_no_na)

data_amb_soil_sum <- rb_data_no_na %>% 
  group_by(habitat) %>% 
  summarize(mean_TP = mean(soil_TP_mg_kg), 
            sd_TP = sd(soil_TP_mg_kg), 
            mean_TN = mean(soil_TN_mg_kg), 
            sd_TN = sd(soil_TN_mg_kg), 
            mean_BD = mean(bulk_density_g_cm3), 
            sd_BD = sd(bulk_density_g_cm3), 
            mean_SPSC = mean(soil_SPSC), 
            sd_SPSC = sd(soil_SPSC), 
            mean_P = mean(M3_P_mg_kg), 
            sd_P = sd(M3_P_mg_kg), 
            mean_Fe = mean(M3_Fe_mg_kg), 
            sd_Fe = sd(M3_Fe_mg_kg), 
            mean_Al = mean(M3_Al_mg_kg), 
            sd_Al = sd(M3_Al_mg_kg), 
            mean_Ca = mean(M3_Ca_mg_kg), 
            sd_Ca = sd(M3_Ca_mg_kg), 
            mean_Mg = mean(M3_Mg_mg_kg), 
            sd_Mg = sd(M3_Mg_mg_kg), 
            mean_K = mean(M3_K_mg_kg), 
            sd_K = sd(M3_K_mg_kg))
options(pillar.sigfig = 3)
paste(data_amb_soil_sum)
View(data_amb_soil_sum)
write.csv(data_amb_soil_sum, "FOR_TABLE_ambient_soil_summary.csv", row.names = F)

#for supplementary table of ICI soil characteristics
ic_data <- subset(data, method == "ic")
ic_data_no_na <- ic_data %>% 
  dplyr::select(1:13, 29:42, 46)
ic_data_no_na <- na.omit(ic_data_no_na)
View(ic_data_no_na)

data_amb_soil_sum_IC_only <- ic_data_no_na %>% 
  group_by(habitat) %>% 
  summarize(mean_TP = mean(soil_TP_mg_kg), 
            sd_TP = sd(soil_TP_mg_kg), 
            mean_TN = mean(soil_TN_mg_kg), 
            sd_TN = sd(soil_TN_mg_kg), 
            mean_SPSC = mean(soil_SPSC), 
            sd_SPSC = sd(soil_SPSC), 
            mean_P = mean(M3_P_mg_kg), 
            sd_P = sd(M3_P_mg_kg), 
            mean_Fe = mean(M3_Fe_mg_kg), 
            sd_Fe = sd(M3_Fe_mg_kg), 
            mean_Al = mean(M3_Al_mg_kg), 
            sd_Al = sd(M3_Al_mg_kg), 
            mean_Ca = mean(M3_Ca_mg_kg), 
            sd_Ca = sd(M3_Ca_mg_kg), 
            mean_Mg = mean(M3_Mg_mg_kg), 
            sd_Mg = sd(M3_Mg_mg_kg), 
            mean_K = mean(M3_K_mg_kg), 
            sd_K = sd(M3_K_mg_kg), 
            mean_WEDRP = mean(WE_srp_mg_kg), 
            sd_WEDRP = sd(WE_srp_mg_kg), 
            mean_WENOx = mean(WE_nox_mg_kg), 
            sd_WENOx = sd(WE_nox_mg_kg), 
            mean_WENH3 = mean(WE_nh3_mg_kg), 
            sd_WENH3 = sd(WE_nh3_mg_kg), 
            mean_pH = mean(soil_pH), 
            sd_pH = sd(soil_pH), 
            mean_EC = mean(ECw_uS_cm), 
            sd_EC = sd(ECw_uS_cm), 
            mean_TOC = mean(TOC_mg_kg), 
            sd_TOC = sd(TOC_mg_kg))
options(pillar.sigfig = 3)
paste(data_amb_soil_sum_IC_only)
View(data_amb_soil_sum_IC_only)
write.csv(data_amb_soil_sum_IC_only, "FOR_Supplement_ambient_soil_summary_IC_only.csv", row.names = F)


#bulk density
soilBD.hab.lm <- lm(bulk_density_g_cm3~habitat, data = rb_data)
summary(soilBD.hab.lm)
plot(soilBD.hab.lm, which=c(1,2))
emm3.soil = emmeans(soilBD.hab.lm, ~habitat)
contrast(emm3.soil, method = "pairwise", adjust = "tukey")

#SPSC
soilSPSC.hab.lm <- lm(soil_SPSC~habitat, data = rb_data)
summary(soilSPSC.hab.lm)
plot(soilSPSC.hab.lm, which=c(1,2))
emm4.soil = emmeans(soilSPSC.hab.lm, ~habitat)
contrast(emm4.soil, method = "pairwise", adjust = "tukey")

#M3 P
soilM3P.hab.lm <- lm(M3_P_mg_kg~habitat, data = rb_data)
summary(soilM3P.hab.lm)
plot(soilM3P.hab.lm, which=c(1,2))
emm5.soil = emmeans(soilM3P.hab.lm, ~habitat)
contrast(emm5.soil, method = "pairwise", adjust = "tukey")

#M3 Fe
soilM3Fe.hab.lm <- lm(M3_Fe_mg_kg~habitat, data = rb_data)
summary(soilM3Fe.hab.lm)
plot(soilM3Fe.hab.lm, which=c(1,2))
emm6.soil = emmeans(soilM3Fe.hab.lm, ~habitat)
contrast(emm6.soil, method = "pairwise", adjust = "tukey")

#M3 Al
soilM3Al.hab.lm <- lm(M3_Al_mg_kg~habitat, data = rb_data)
summary(soilM3Al.hab.lm)
plot(soilM3Al.hab.lm, which=c(1,2))
emm7.soil = emmeans(soilM3Al.hab.lm, ~habitat)
contrast(emm7.soil, method = "pairwise", adjust = "tukey")

#M3 Ca
soilM3Ca.hab.lm <- lm(M3_Ca_mg_kg~habitat, data = rb_data)
summary(soilM3Ca.hab.lm)
plot(soilM3Ca.hab.lm, which=c(1,2))
emm8.soil = emmeans(soilM3Ca.hab.lm, ~habitat)
contrast(emm8.soil, method = "pairwise", adjust = "tukey")

#M3 Mg
soilM3Mg.hab.lm <- lm(M3_Mg_mg_kg~habitat, data = rb_data)
summary(soilM3Mg.hab.lm)
plot(soilM3Mg.hab.lm, which=c(1,2))
emm9.soil = emmeans(soilM3Mg.hab.lm, ~habitat)
contrast(emm9.soil, method = "pairwise", adjust = "tukey")

#M3 K
soilM3K.hab.lm <- lm(M3_K_mg_kg~habitat, data = rb_data)
summary(soilM3K.hab.lm)
plot(soilM3K.hab.lm, which=c(1,2))
emm10.soil = emmeans(soilM3K.hab.lm, ~habitat)
contrast(emm10.soil, method = "pairwise", adjust = "tukey")

#TOC
ic_data <- subset(data, method == "ic")
soilTOC.hab.lm <- lm(TOC_mg_kg~habitat, data = ic_data)
summary(soilTOC.hab.lm)
plot(soilTOC.hab.lm, which=c(1,2))
emm11.soil = emmeans(soilTOC.hab.lm, ~habitat)
contrast(emm11.soil, method = "pairwise", adjust = "tukey")



#####
##START INTACT CORE ANALYSIS
#####

ic_data <- subset(data, method == "ic")#intact core data in the above csv is a mean of each time point during incubation
View(ic_data)
summary(ic_data)
hist(ic_data$flux_drp_mgm2d)
hist(ic_data$flux_nox_mgm2d)
hist(ic_data$flux_nh3_mgm2d)



#####
##Flux vs patch ANOVAs
#####

##DRP
#LM habitat predictor
drp.hab.lm <- lm(flux_drp_mgm2d ~ habitat, data = ic_data)
summary(drp.hab.lm)
plot(drp.hab.lm, which=c(1,2))
emm1=emmeans(drp.hab.lm, ~habitat)
contrast(emm1, method = "pairwise", adjust = "tukey")

##NOx
#LM habitat predictor
nox.hab.lm <- lm(flux_nox_mgm2d ~ habitat, data = ic_data)
summary(nox.hab.lm)
plot(nox.hab.lm, which=c(1,2))
emm2=emmeans(nox.hab.lm, ~habitat)
contrast(emm2, method = "pairwise", adjust = "tukey")

##NH3
#LM habitat predictor
nh3.hab.lm <- lm(flux_nh3_mgm2d ~ habitat, data = ic_data)
summary(nh3.hab.lm)
plot(nh3.hab.lm, which=c(1,2))
emm3=emmeans(nh3.hab.lm, ~habitat)
contrast(emm3, method = "pairwise", adjust = "tukey")

##N2 gas
#LM habitat predictor
n2.hab.lm <- lm(N2_flux_mg_m2_d ~ habitat, data = ic_data)
summary(n2.hab.lm)
plot(n2.hab.lm, which=c(1,2))
emm4=emmeans(n2.hab.lm, ~habitat)
contrast(emm4, method = "pairwise", adjust = "tukey")

##O2 gas
#LM habitat predictor
o2.hab.lm <- lm(O2_flux_mg_m2_d ~ habitat, data = ic_data)
summary(o2.hab.lm)
plot(o2.hab.lm, which=c(1,2))
emm5=emmeans(o2.hab.lm, ~habitat)
contrast(emm5, method = "pairwise", adjust = "tukey")


gas_summary <- ic_data %>% 
  group_by(habitat) %>% 
  summarize(mean_N2 = mean(N2_flux_mg_m2_d), 
            sd_N2 = sd(N2_flux_mg_m2_d), 
            mean_O2 = mean(O2_flux_mg_m2_d), 
            sd_O2 = sd(O2_flux_mg_m2_d))



#####
##Model selection for intact cores vs water parameters
#####

#remove NA's becasue stepAIC() doesn't allow for them
ic_data_no_na <- ic_data %>% 
  dplyr::select(1:19, 22, 24, 25)
ic_data_no_na <- na.omit(ic_data_no_na)
View(ic_data_no_na) #takes out sample points 4 and 10 due to missing SPC data


##SRP
fullmod <- lm(flux_drp_mgm2d~habitat+year+in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)
minmod <- lm(flux_drp_mgm2d~1,data=ic_data_no_na)

#without habitat and year
fullmod2 <- lm(flux_drp_mgm2d~in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)

stepAIC(fullmod2, direction = "backward") #lm(formula = flux_drp_mgm2d ~ habitat + year, data = ic_data_no_na)
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod2, lower = minmod))#lm(formula = flux_drp_mgm2d ~ in_nox_mgL + habitat + year, data = ic_data_no_na)
stepAIC(minmod, direction = "both", scope = list(upper = fullmod2, lower = minmod)) #lm(formula = flux_drp_mgm2d ~ habitat + year, data = ic_data_no_na)


##NOx
fullmod <- lm(flux_nox_mgm2d~habitat+year+in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)
minmod <- lm(flux_nox_mgm2d~1,data=ic_data_no_na)

#without habitat and year
fullmod2 <- lm(flux_nox_mgm2d~in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)

stepAIC(fullmod2, direction = "backward") #lm(formula = flux_nox_mgm2d ~ habitat + year + in_nox_mgL + phys_turb_fnu, data = ic_data_no_na)
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod2, lower = minmod))#lm(formula = flux_nox_mgm2d ~ in_nox_mgL + habitat + year + phys_turb_fnu, data = ic_data_no_na)
stepAIC(minmod, direction = "both", scope = list(upper = fullmod2, lower = minmod)) #lm(formula = flux_nox_mgm2d ~ in_nox_mgL + habitat + year + phys_turb_fnu, data = ic_data_no_na)


##NH3
fullmod <- lm(flux_nh3_mgm2d~habitat+year+in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)
minmod <- lm(flux_nh3_mgm2d~1,data=ic_data_no_na)

#without habitat and year
fullmod2 <- lm(flux_nh3_mgm2d~in_nox_mgL+in_drp_mgL+in_nh3_mgL+water_depth+phys_do_mgL+phys_spec_cond+phys_turb_fnu,data=ic_data_no_na)

stepAIC(fullmod, direction = "backward") #lm(formula = flux_nh3_mgm2d ~ habitat + year + in_nox_mgL + in_drp_mgL + water_depth, data = ic_data_no_na)
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))#lm(formula = flux_nh3_mgm2d ~ habitat + year + in_nox_mgL, data = ic_data_no_na)
stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod)) #lm(formula = flux_nh3_mgm2d ~ habitat + year + in_nox_mgL, data = ic_data_no_na)



#####
##Flux compared to inflow (ambient) nutrient concentrations
#####

##DRP flux
#LM drp inflow conc predictor

drp.inflow.lm <- lm(flux_drp_mgm2d~in_drp_mgL, data = ic_data)
summary(drp.inflow.lm)
plot(drp.inflow.lm, which=c(1,2))
plot(flux_drp_mgm2d~in_drp_mgL, data = ic_data)

#LMER inflow fixed and habitat random, not used
#drp.inflow.hab.lmer <- lmer(flux_drp_mgm2d ~ in_drp_mgL + (1|habitat), data = ic_data, REML = F)
#summary(drp.inflow.hab.lmer)
#emmeans(drp.inflow.hab.lmer, "in_drp_mgL")
#r.squaredGLMM(drp.inflow.hab.lmer)

#likelihood ratio test to compare models, lm habitat, lm inflow
anova(drp.hab.lm, drp.inflow.lm)

lrtest(drp.hab.lm, drp.inflow.lm)

#additional predictors of drp flux
#vs ambient NO3
cor.test(ic_data$flux_drp_mgm2d, ic_data$in_nox_mgL)
oP.flux.v.ambientNOx.lm <- lm(flux_drp_mgm2d~in_nox_mgL, data = ic_data)
summary(oP.flux.v.ambientNOx.lm)
plot(oP.flux.v.ambientNOx.lm, which=c(1,2))
plot(ic_data$flux_drp_mgm2d~ic_data$in_nox_mgL)

#vs ambient NH3
cor.test(ic_data$flux_drp_mgm2d, ic_data$in_nh3_mgL)
oP.flux.v.ambientNH3.lm <- lm(flux_drp_mgm2d~in_nh3_mgL, data = ic_data)
summary(oP.flux.v.ambientNH3.lm)
plot(oP.flux.v.ambientNH3.lm, which=c(1,2))
plot(ic_data$flux_drp_mgm2d~ic_data$in_nh3_mgL)

#vs water depth
cor.test(ic_data$flux_drp_mgm2d, ic_data$water_depth)
oP.flux.v.water_depth.lm <- lm(flux_drp_mgm2d~water_depth, data = ic_data)
summary(oP.flux.v.water_depth.lm)
plot(oP.flux.v.water_depth.lm, which=c(1,2))


##NOx flux
#LM nox inflow conc predictor
nox.inflow.lm <- lm(flux_nox_mgm2d~in_nox_mgL, data = ic_data)
summary(nox.inflow.lm)
plot(nox.inflow.lm, which=c(1,2))
plot(flux_nox_mgm2d~in_nox_mgL, data = ic_data)

#LMER inflow fixed and habitat random, not used
#nox.inflow.hab.lmer <- lmer(flux_nox_mgm2d ~ in_nox_mgL + (1|habitat), data = ic_data, REML = F)
#summary(nox.inflow.hab.lmer)
#r.squaredGLMM(nox.inflow.hab.lmer)

#likelihood ratio test to compare models, lm habitat, lm inflow
anova(nox.hab.lm, nox.inflow.lm)
lrtest(nox.hab.lm, nox.inflow.lm)

#additional predictors of nox flux
#vs ambient drp
NO3.flux.v.ambientdrp.lm <- lm(flux_nox_mgm2d~in_drp_mgL, data = ic_data)
summary(NO3.flux.v.ambientdrp.lm)
plot(NO3.flux.v.ambientdrp.lm, which=c(1,2))
plot(ic_data$flux_nox_mgm2d~ic_data$in_drp_mgL)

#vs ambient NH3
NO3.flux.v.ambientNH3.lm <- lm(flux_nox_mgm2d~in_nh3_mgL, data = ic_data)
summary(NO3.flux.v.ambientNH3.lm)
plot(NO3.flux.v.ambientNH3.lm, which=c(1,2))
plot(flux_nox_mgm2d~in_nh3_mgL, data = ic_data)

#vs water depth
NO3.flux.v.water_depth.lm <- lm(flux_nox_mgm2d~water_depth, data = ic_data)
summary(NO3.flux.v.water_depth.lm)
plot(NO3.flux.v.water_depth.lm, which=c(1,2))
plot(flux_nox_mgm2d~water_depth, data = ic_data)

#vs DO
NO3.flux.v.phys_do_mgL.lm <- lm(flux_nox_mgm2d~phys_do_mgL, data = ic_data)
summary(NO3.flux.v.phys_do_mgL.lm)
plot(NO3.flux.v.phys_do_mgL.lm, which=c(1,2))


##NH3
#LM nh3 inflow conc predictor
nh3.inflow.lm <- lm(flux_nh3_mgm2d~in_nh3_mgL, data = ic_data)
summary(nh3.inflow.lm)
plot(nh3.inflow.lm, which=c(1,2))

#LMER inflow fixed and habitat random, REMOVED
#nh3.inflow.hab.lmer <- lmer(flux_nh3_mgm2d ~ in_nh3_mgL + (1|habitat), data = ic_data, REML = F)
#summary(nh3.inflow.hab.lmer)
#r.squaredGLMM(nh3.inflow.hab.lmer)

#likelihood ratio test to compare models, lm habitat, lm inflow
anova(nh3.hab.lm, nh3.inflow.lm)
lrtest(nh3.hab.lm, nh3.inflow.lm)

#additional predictors of nh3 flux
#vs ambient SRP
NH3.flux.v.ambientSRP.lm <- lm(flux_nh3_mgm2d~in_drp_mgL, data = ic_data)
summary(NH3.flux.v.ambientSRP.lm)
plot(NH3.flux.v.ambientSRP.lm, which=c(1,2))
plot(flux_nh3_mgm2d~in_drp_mgL, data = ic_data)

#nh3 vs nox
NH3.flux.v.ambientnox.lm <- lm(flux_nh3_mgm2d~in_nox_mgL, data = ic_data)
summary(NH3.flux.v.ambientnox.lm)
plot(NH3.flux.v.ambientnox.lm, which=c(1,2))
plot(flux_nh3_mgm2d~in_nox_mgL, data = ic_data)

#vs water depth
NH3.flux.v.water_depth.lm <- lm(flux_nh3_mgm2d~water_depth, data = ic_data)
summary(NH3.flux.v.water_depth.lm)
plot(NH3.flux.v.water_depth.lm, which=c(1,2))
plot(flux_nh3_mgm2d~water_depth, data = ic_data)

#vs DO
NH3.flux.v.phys_do_mgL.lm <- lm(flux_nh3_mgm2d~phys_do_mgL, data = ic_data)
summary(NH3.flux.v.phys_do_mgL.lm)
plot(NH3.flux.v.phys_do_mgL.lm, which=c(1,2))
plot(flux_nh3_mgm2d~phys_do_mgL, data = ic_data)



#####
##IC Flux vs soil properties
#####
ic_data_soils_no_na <- ic_data %>% 
  dplyr::select(1:19, 29:43)
ic_data_soils_no_na <- na.omit(ic_data_soils_no_na)
View(ic_data_soils_no_na)
summary(ic_data_soils_no_na)
str(ic_data_soils_no_na)


##DRP
fullmod <- lm(flux_drp_mgm2d~amb_nox_mgL + 
                amb_drp_mgL+
                amb_nh3_mgL+
                soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                WE_srp_mg_kg+
                WE_nh3_mg_kg+
                WE_nox_mg_kg+
                soil_pH+
                ECw_uS_cm+
                bulk_density_g_cm3,
              data=ic_data_soils_no_na)
minmod <- lm(flux_drp_mgm2d~1,data=ic_data_soils_no_na)

stepAIC(fullmod, direction = "backward") 
#flux_drp_mgm2d ~ M3_P_mg_kg + M3_Fe_mg_kg + M3_Ca_mg_kg + WE_nox_mg_kg + soil_pH + ECw_uS_cm

stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))
#flux_drp_mgm2d ~ soil_TP_mg_kg + M3_P_mg_kg + WE_nh3_mg_kg

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#flux_drp_mgm2d ~ soil_TP_mg_kg + M3_P_mg_kg + WE_nh3_mg_kg


##linear models selected by AIC
#drp vs soil TP
oP.flux.v.TP.lm <- lm(flux_drp_mgm2d~soil_TP_mg_kg, data = ic_data)
summary(oP.flux.v.TP.lm)
plot(oP.flux.v.TP.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_TP_mg_kg, data = ic_data)

#drp vs M3 P
oP.flux.v.M3_P.lm <- lm(flux_drp_mgm2d~M3_P_mg_kg, data = ic_data)
summary(oP.flux.v.M3_P.lm)
plot(oP.flux.v.M3_P.lm, which=c(1,2))

#drp vs WE nh3, not significant
oP.flux.v.WEnh3.lm <- lm(flux_drp_mgm2d~WE_nh3_mg_kg, data = ic_data)
summary(oP.flux.v.WEnh3.lm)

#drp vs SPSC, not significant
oP.flux.v.SPSC.lm <- lm(flux_drp_mgm2d~soil_SPSC, data = ic_data)
summary(oP.flux.v.SPSC.lm)
plot(oP.flux.v.SPSC.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_SPSC, data = ic_data) 


##NOx
fullmod <- lm(flux_nox_mgm2d~soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                WE_srp_mg_kg+
                WE_nh3_mg_kg+
                WE_nox_mg_kg+
                soil_pH+
                ECw_uS_cm+
                bulk_density_g_cm3,
              data=ic_data_soils_no_na)
minmod <- lm(flux_nox_mgm2d~1,data=ic_data_soils_no_na)

stepAIC(fullmod, direction = "backward") #lm(formula = flux_drp_mgm2d ~ habitat + year, data = ic_data_no_na)
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))#lm(formula = flux_drp_mgm2d ~ in_nox_mgL + habitat + year, data = ic_data_no_na)
#flux_nox_mgm2d ~ WE_nox_mg_kg + M3_K_mg_kg + soil_TN_mg_kg

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#flux_nox_mgm2d ~ WE_nox_mg_kg + M3_K_mg_kg + soil_TN_mg_kg


#nox vs TN
nox.flux.v.TN.lm <- lm(flux_nox_mgm2d~soil_TN_mg_kg, data = ic_data)
summary(nox.flux.v.TN.lm)
plot(nox.flux.v.TN.lm, which=c(1,2))
plot(flux_nox_mgm2d~soil_TN_mg_kg, data = ic_data)

#nox vs WE nox
nox.flux.v.WEnox.lm <- lm(flux_nox_mgm2d~WE_nox_mg_kg, data = ic_data)
summary(nox.flux.v.WEnox.lm)
plot(nox.flux.v.WEnox.lm, which=c(1,2))
plot(flux_nox_mgm2d~WE_nox_mg_kg, data = ic_data)

#nox vs M3 K, not significant
nox.flux.v.M3K.lm <- lm(flux_nox_mgm2d~M3_K_mg_kg, data = ic_data)
summary(nox.flux.v.M3K.lm)
plot(nox.flux.v.M3K.lm, which=c(1,2))
plot(flux_nox_mgm2d~M3_K_mg_kg, data = ic_data)


##NH3
fullmod <- lm(flux_nh3_mgm2d~soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                WE_srp_mg_kg+
                WE_nh3_mg_kg+
                WE_nox_mg_kg+
                soil_pH+
                ECw_uS_cm+
                bulk_density_g_cm3,
              data=ic_data_soils_no_na)
minmod <- lm(flux_nh3_mgm2d~1,data=ic_data_soils_no_na)

stepAIC(fullmod, direction = "backward")
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))
#flux_nh3_mgm2d ~ M3_P_mg_kg + WE_srp_mg_kg

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#flux_nh3_mgm2d ~ M3_P_mg_kg + WE_srp_mg_kg

#nh3 vs TN
nh3.flux.v.TN.lm <- lm(flux_nh3_mgm2d~soil_TN_mg_kg, data = ic_data)
summary(nh3.flux.v.TN.lm)
plot(nh3.flux.v.TN.lm, which=c(1,2))
plot(flux_nh3_mgm2d~soil_TN_mg_kg, data = ic_data)

#nh3 vs WE nh3
nh3.flux.v.WEnh3.lm <- lm(flux_nh3_mgm2d~WE_nh3_mg_kg, data = ic_data)
summary(nh3.flux.v.WEnh3.lm)
plot(nh3.flux.v.WEnh3.lm, which=c(1,2))
plot(flux_nh3_mgm2d~soil_TN_mg_kg, data = ic_data)

#nh3 vs M3 P
nh3.flux.v.M3_P.lm <- lm(flux_nh3_mgm2d~M3_P_mg_kg, data = ic_data)
summary(nh3.flux.v.M3_P.lm)
plot(nh3.flux.v.M3_P.lm, which=c(1,2))
plot(flux_nh3_mgm2d~M3_P_mg_kg, data = ic_data)

#nh3 vs WE srp, not significant
nh3.flux.v.WEsrp.lm <- lm(flux_nh3_mgm2d~WE_srp_mg_kg, data = ic_data)
summary(nh3.flux.v.WEsrp.lm)
plot(nh3.flux.v.WEsrp.lm, which=c(1,2))
plot(flux_nh3_mgm2d~WE_srp_mg_kg, data = ic_data)



#####
##START RESIN BAG ANALYSIS
#####

#subset data

rb_data <- subset(data, method == "rb")
View(rb_data)
summary(rb_data)
str(rb_data)
hist(rb_data$flux_drp_mgm2d)
hist(rb_data$flux_nox_mgm2d)
hist(rb_data$flux_nh3_mgm2d)



#####
##Flux vs patches for RB
#####

#DRP
drp.hab.lm <- lm(flux_drp_mgm2d ~ habitat, data = rb_data)
summary(drp.hab.lm)
plot(drp.hab.lm, which=c(1,2))
emm1=emmeans(drp.hab.lm, ~habitat)
contrast(emm1, method = "pairwise", adjust = "tukey")
drp_hab_cld <- cld(emm1, 
                   adjust = "Tukey", 
                   Letters = letters, 
                   alpha = 0.05)

#remove sav due to unequal variance
rb_drp_no_sav <- subset(rb_data, habitat == "fav" | habitat == "grasses"|habitat == "hardwoods")

#LM without SAV data
drp.hab.lm <- lm(flux_drp_mgm2d ~ habitat, data = rb_drp_no_sav)
summary(drp.hab.lm)
plot(drp.hab.lm, which=c(1,2))
emm1=emmeans(drp.hab.lm, ~habitat)
contrast(emm1, method = "pairwise", adjust = "tukey")
drp_hab_cld <- cld(emm1, 
                   adjust = "Tukey", 
                   Letters = letters, 
                   alpha = 0.05)
#still not significant so add use sav data going forward

#LM drp inflow conc predictor
cor.test(rb_data$flux_drp_mgm2d, rb_data$in_drp_mgL)
drp.inflow.lm <- lm(flux_drp_mgm2d~in_drp_mgL, data = rb_data)
summary(drp.inflow.lm)
plot(drp.inflow.lm, which=c(1,2))

#LMER inflow fixed and habitat random
drp.inflow.hab.lmer <- lmer(flux_drp_mgm2d ~ in_drp_mgL + (1|habitat), 
                            data = rb_data, REML = F)
summary(drp.inflow.hab.lmer)

r.squaredGLMM(drp.inflow.hab.lmer)

#compare models
anova(drp.inflow.hab.lmer, drp.inflow.lm, drp.hab.lm)


##NOx
#LM habitat predictor
nox.hab.lm <- lm(flux_nox_mgm2d ~ habitat, data = ic_data)
summary(nox.hab.lm)
plot(nox.hab.lm, which=c(1,2))
emm2=emmeans(nox.hab.lm, ~habitat)
contrast(emm2, method = "pairwise", adjust = "tukey")
nox_hab_cld <- cld(emm2, 
                   adjust = "Tukey", 
                   Letters = letters, 
                   alpha = 0.05)

#LM nox inflow conc predictor
cor.test(ic_data$flux_nox_mgm2d, ic_data$in_nox_mgL)
nox.inflow.lm <- lm(flux_nox_mgm2d~in_nox_mgL, data = ic_data)
summary(nox.inflow.lm)
plot(nox.inflow.lm, which=c(1,2))

#LMER inflow fixed and habitat random
nox.inflow.hab.lmer <- lmer(flux_nox_mgm2d ~ in_nox_mgL + (1|habitat), 
                            data = ic_data, REML = F)
summary(nox.inflow.hab.lmer)

r.squaredGLMM(nox.inflow.hab.lmer)

#compare models
anova(nox.inflow.hab.lmer, nox.inflow.lm, nox.hab.lm)


##NH3
#LM habitat predictor
nh3.hab.lm <- lm(flux_nh3_mgm2d ~ habitat, data = ic_data)
summary(nh3.hab.lm)
plot(nh3.hab.lm, which=c(1,2))
emm3=emmeans(nh3.hab.lm, ~habitat)
contrast(emm3, method = "pairwise", adjust = "tukey")
nh3_hab_cld <- cld(emm3, 
                   adjust = "Tukey", 
                   Letters = letters, 
                   alpha = 0.05)

#LM nh3 inflow conc predictor
cor.test(ic_data$flux_nh3_mgm2d, ic_data$in_nh3_mgL)
nh3.inflow.lm <- lm(flux_nh3_mgm2d~in_nh3_mgL, data = ic_data)
summary(nh3.inflow.lm)
plot(nh3.inflow.lm, which=c(1,2))

#LMER inflow fixed and habitat random
nh3.inflow.hab.lmer <- lmer(flux_nh3_mgm2d ~ in_nh3_mgL + (1|habitat), 
                            data = ic_data, REML = F)
summary(nh3.inflow.hab.lmer)

r.squaredGLMM(nh3.inflow.hab.lmer)

#compare models
anova(nh3.inflow.hab.lmer, nh3.inflow.lm, nh3.hab.lm)



#####
##Model selection for rb vs water
#####

#remove NA's becasue stepAIC() doesn't allow for them
rb_data_no_na <- rb_data %>% 
  dplyr::select(sample_point, 
                habitat, 
                amb_nox_mgL, 
                amb_drp_mgL, 
                amb_nh3_mgL, 
                flux_drp_mgm2d, 
                flux_nox_mgm2d, 
                flux_nh3_mgm2d, 
                water_depth, 
                phys_wtemp_C, 
                phys_pH, 
                phys_do_mgL, 
                phys_spec_cond, 
                phys_turb_fnu, 
                soil_TP_mg_kg, 
                soil_TN_mg_kg, 
                M3_P_mg_kg, 
                M3_Fe_mg_kg, 
                M3_Al_mg_kg, 
                soil_SPSC, 
                M3_Ca_mg_kg, 
                M3_Mg_mg_kg, 
                M3_K_mg_kg, 
                bulk_density_g_cm3)
View(rb_data_no_na)
str(rb_data_no_na)
ncol(rb_data_no_na)


##DRP
rb_data_no_na_drp <- rb_data_no_na %>% 
  dplyr::select(sample_point, 
                habitat, 
                amb_nox_mgL, 
                amb_drp_mgL, 
                amb_nh3_mgL, 
                flux_drp_mgm2d, 
                water_depth, 
                phys_wtemp_C, 
                phys_pH, 
                phys_do_mgL, 
                phys_spec_cond, 
                phys_turb_fnu, 
                soil_TP_mg_kg, 
                soil_TN_mg_kg, 
                M3_P_mg_kg, 
                M3_Fe_mg_kg, 
                M3_Al_mg_kg, 
                soil_SPSC, 
                M3_Ca_mg_kg, 
                M3_Mg_mg_kg, 
                M3_K_mg_kg, 
                bulk_density_g_cm3)
rb_data_no_na_drp <- na.omit(rb_data_no_na_drp)
View(rb_data_no_na_drp)

minmod <- lm(flux_drp_mgm2d~1,data=rb_data_no_na_drp)

#without habitat and year
fullmod2 <- lm(flux_drp_mgm2d~amb_nox_mgL+
                 amb_drp_mgL+
                 amb_nh3_mgL+
                 water_depth+
                 phys_pH+
                 phys_do_mgL+
                 phys_spec_cond+
                 phys_turb_fnu, data=rb_data_no_na_drp)

stepAIC(fullmod2, direction = "backward")
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod2, lower = minmod))
stepAIC(minmod, direction = "both", scope = list(upper = fullmod2, lower = minmod))


##NOx
#remove drp flux column and water temp
rb_data_no_na_nox <- rb_data_no_na %>% 
  dplyr::select(sample_point, 
                habitat, 
                amb_nox_mgL, 
                amb_drp_mgL, 
                amb_nh3_mgL, 
                flux_nox_mgm2d, 
                flux_nh3_mgm2d, 
                water_depth, 
                phys_pH, 
                phys_do_mgL, 
                phys_spec_cond, 
                phys_turb_fnu, 
                soil_TP_mg_kg, 
                soil_TN_mg_kg, 
                M3_P_mg_kg, 
                M3_Fe_mg_kg, 
                M3_Al_mg_kg, 
                soil_SPSC, 
                M3_Ca_mg_kg, 
                M3_Mg_mg_kg, 
                M3_K_mg_kg, 
                bulk_density_g_cm3)
rb_data_no_na_nox <- na.omit(rb_data_no_na_nox)
View(rb_data_no_na_nox)

minmod <- lm(flux_nox_mgm2d~1,data=rb_data_no_na)

#without habitat and year
fullmod2 <- lm(flux_nox_mgm2d~amb_nox_mgL+
                 amb_drp_mgL+
                 amb_nh3_mgL+
                 water_depth+
                 phys_pH+
                 phys_do_mgL+
                 phys_spec_cond+
                 phys_turb_fnu, data=rb_data_no_na)

stepAIC(fullmod2, direction = "backward")
#flux_nox_mgm2d ~ in_nh3_mgL + water_depth + phys_pH + phys_spec_cond
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod2, lower = minmod))
stepAIC(minmod, direction = "both", scope = list(upper = fullmod2, lower = minmod))

##NH3
minmod <- lm(flux_nh3_mgm2d~1,data=rb_data_no_na)

#without habitat and year
fullmod2 <- lm(flux_nh3_mgm2d~in_nox_mgL+
                 in_drp_mgL+
                 in_nh3_mgL+
                 water_depth+
                 phys_pH+
                 phys_do_mgL+
                 phys_spec_cond+
                 phys_turb_fnu, data=rb_data_no_na)

stepAIC(fullmod2, direction = "backward")
stepAIC(minmod, direction = "forward", scope = list(upper = fullmod2, lower = minmod))
stepAIC(minmod, direction = "both", scope = list(upper = fullmod2, lower = minmod))



#####
##RB Flux vs ambient water
#####

##DRP flux
#LM drp inflow conc predictor
cor.test(rb_data$flux_drp_mgm2d, rb_data$amb_drp_mgL)
drp.amb.lm <- lm(flux_drp_mgm2d~amb_drp_mgL, data = rb_data)
summary(drp.amb.lm)
plot(drp.amb.lm, which=c(1,2))
plot(flux_drp_mgm2d~amb_drp_mgL, data = rb_data)

#vs amb nox
cor.test(rb_data$flux_drp_mgm2d, rb_data$amb_nox_mgL)
drp.amb.nox.lm <- lm(flux_drp_mgm2d~amb_nox_mgL, data = rb_data)
summary(drp.amb.nox.lm)
plot(drp.amb.nox.lm, which=c(1,2))
plot(flux_drp_mgm2d~amb_nox_mgL, data = rb_data)
#remove sample point 4
rb_data_no_point4 <- rb_data %>% filter(!row_number() %in% 4)
View(rb_data_no_point4)
drp.amb.nox.lm <- lm(flux_drp_mgm2d~amb_nox_mgL, data = rb_data_no_point4)
summary(drp.amb.nox.lm)
plot(drp.amb.nox.lm, which=c(1,2))
plot(flux_drp_mgm2d~amb_nox_mgL, data = rb_data_no_point4)

#vs amb nh3
cor.test(rb_data$flux_drp_mgm2d, rb_data$amb_nh3_mgL)
drp.amb.nh3.lm <- lm(flux_drp_mgm2d~amb_nh3_mgL, data = rb_data)
summary(drp.amb.nh3.lm)
plot(drp.amb.nh3.lm, which=c(1,2))
plot(flux_drp_mgm2d~amb_nh3_mgL, data = rb_data)


##NOx Flux
#LM nox inflow conc predictor
cor.test(rb_data$flux_nox_mgm2d, rb_data$amb_nox_mgL)
nox.amb.lm <- lm(flux_nox_mgm2d~amb_nox_mgL, data = rb_data)
summary(nox.amb.lm)
plot(nox.amb.lm, which=c(1,2))
plot(flux_nox_mgm2d~amb_nox_mgL, data = rb_data)

#vs amb drp
cor.test(rb_data$flux_nox_mgm2d, rb_data$amb_drp_mgL)
nox.amb.drp.lm <- lm(flux_nox_mgm2d~amb_drp_mgL, data = rb_data)
summary(nox.amb.drp.lm)
plot(nox.amb.drp.lm, which=c(1,2))
plot(flux_nox_mgm2d~amb_drp_mgL, data = rb_data)

#vs amb nh3
cor.test(rb_data$flux_nox_mgm2d, rb_data$amb_nh3_mgL)
nox.amb.nh3.lm <- lm(flux_nox_mgm2d~amb_nh3_mgL, data = rb_data)
summary(nox.amb.nh3.lm)
plot(nox.amb.nh3.lm, which=c(1,2))
plot(flux_nox_mgm2d~amb_nh3_mgL, data = rb_data)


##NH3 Flux
#LM nh3 inflow conc predictor
cor.test(rb_data$flux_nh3_mgm2d, rb_data$amb_nh3_mgL)
nh3.amb.lm <- lm(flux_nh3_mgm2d~amb_nh3_mgL, data = rb_data)
summary(nh3.amb.lm)
plot(nh3.amb.lm, which=c(1,2))
plot(flux_nh3_mgm2d~amb_nh3_mgL, data = rb_data)

#vs amb nox
cor.test(rb_data$flux_nh3_mgm2d, rb_data$amb_nox_mgL)
nh3.amb.nox.lm <- lm(flux_nh3_mgm2d~amb_nox_mgL, data = rb_data)
summary(nh3.amb.nox.lm)
plot(nh3.amb.nox.lm, which=c(1,2))
plot(flux_nh3_mgm2d~amb_nox_mgL, data = rb_data)

#vs amb drp
cor.test(rb_data$flux_nh3_mgm2d, rb_data$amb_drp_mgL)
nh3.amb.drp.lm <- lm(flux_nh3_mgm2d~amb_drp_mgL, data = rb_data)
summary(nh3.amb.drp.lm)
plot(nh3.amb.drp.lm, which=c(1,2))
plot(flux_nh3_mgm2d~amb_drp_mgL, data = rb_data)



#####
##model selection rb vs soil
#####

#below dataframe is only for the nh3 selection
rb_data_soils_no_na <- rb_data %>% 
  dplyr::select(sample_point, 
                habitat, 
                flux_nh3_mgm2d, 
                soil_TP_mg_kg, 
                soil_TN_mg_kg, 
                M3_P_mg_kg, 
                M3_Fe_mg_kg, 
                M3_Al_mg_kg, 
                soil_SPSC, 
                M3_Ca_mg_kg, 
                M3_Mg_mg_kg, 
                M3_K_mg_kg, 
                bulk_density_g_cm3)
rb_data_soils_no_na <- na.omit(rb_data_soils_no_na)
View(rb_data_soils_no_na)
summary(ic_data_soils_no_na)
str(ic_data_soils_no_na)


##DRP
fullmod <- lm(flux_drp_mgm2d~soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                bulk_density_g_cm3,
              data=rb_data_no_na_drp)
minmod <- lm(flux_drp_mgm2d~1,data=rb_data_no_na_drp)

stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))
#flux_drp_mgm2d ~ M3_Al_mg_kg + bulk_density_g_cm3 + soil_TN_mg_kg + 
#  M3_Mg_mg_kg + soil_SPSC

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#flux_drp_mgm2d ~ M3_Al_mg_kg + bulk_density_g_cm3 + soil_TN_mg_kg + 
#  M3_Mg_mg_kg + soil_SPSC


##linear models selected by AIC
#drp vs M3Al, marginally sig
oP.flux.v.M3Al.lm <- lm(flux_drp_mgm2d~M3_Al_mg_kg, data = rb_data)
summary(oP.flux.v.M3Al.lm)
plot(oP.flux.v.M3Al.lm, which=c(1,2))
plot(flux_drp_mgm2d~M3_Al_mg_kg, data = rb_data)

#drp vs BD, not sig
oP.flux.v.BD.lm <- lm(flux_drp_mgm2d~bulk_density_g_cm3, data = rb_data)
summary(oP.flux.v.BD.lm)
plot(oP.flux.v.BD.lm, which=c(1,2))
plot(flux_drp_mgm2d~bulk_density_g_cm3, data = rb_data)

#drp vs TN, not sig
drp.flux.v.TN.lm <- lm(flux_drp_mgm2d~soil_TN_mg_kg, data = rb_data)
summary(drp.flux.v.TN.lm)
plot(drp.flux.v.TN.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_TN_mg_kg, data = rb_data)

#drp vs M3Mg, not sig
oP.flux.v.M3Mg.lm <- lm(flux_drp_mgm2d~M3_Mg_mg_kg, data = rb_data)
summary(oP.flux.v.M3Mg.lm)
plot(oP.flux.v.M3Mg.lm, which=c(1,2))
plot(flux_drp_mgm2d~M3_Mg_mg_kg, data = rb_data)

#drp vs SPSC, not significant
oP.flux.v.SPSC.lm <- lm(flux_drp_mgm2d~soil_SPSC, data = rb_data)
summary(oP.flux.v.SPSC.lm)
plot(oP.flux.v.SPSC.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_SPSC, data = rb_data) 

#drp vs soil TP, not significant
oP.flux.v.TP.lm <- lm(flux_drp_mgm2d~soil_TP_mg_kg, data = rb_data)
summary(oP.flux.v.TP.lm)
plot(oP.flux.v.TP.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_TP_mg_kg, data = rb_data)

#drp vs M3 P, not sig
oP.flux.v.M3_P.lm <- lm(flux_drp_mgm2d~M3_P_mg_kg, data = rb_data)
summary(oP.flux.v.M3_P.lm)
plot(oP.flux.v.M3_P.lm, which=c(1,2))


##NOx
fullmod <- lm(flux_nox_mgm2d~soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                bulk_density_g_cm3,
              data=rb_data_no_na_nox)
minmod <- lm(flux_nox_mgm2d~1,data=rb_data_no_na_nox)

stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))#lm(formula = flux_drp_mgm2d ~ in_nox_mgL + habitat + year, data = ic_data_no_na)
#flux_nox_mgm2d ~ M3_Ca_mg_kg + M3_Al_mg_kg + M3_Fe_mg_kg + soil_TN_mg_kg

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#flux_nox_mgm2d ~ M3_Ca_mg_kg + M3_Al_mg_kg + M3_Fe_mg_kg + soil_TN_mg_kg


#nox vs M3 Ca, significant
nox.flux.v.M3Ca.lm <- lm(flux_nox_mgm2d~M3_Ca_mg_kg, data = rb_data)
summary(nox.flux.v.M3Ca.lm)
plot(nox.flux.v.M3Ca.lm, which=c(1,2))
plot(flux_nox_mgm2d~M3_Ca_mg_kg, data = rb_data)

#nox vs M3 Al, marginally significant
nox.flux.v.M3Al.lm <- lm(flux_nox_mgm2d~M3_Al_mg_kg, data = rb_data)
summary(nox.flux.v.M3Al.lm)
plot(nox.flux.v.M3Al.lm, which=c(1,2))
plot(flux_nox_mgm2d~M3_Al_mg_kg, data = rb_data)

#nox vs M3 Fe, not significant
nox.flux.v.M3Fe.lm <- lm(flux_nox_mgm2d~M3_Fe_mg_kg, data = rb_data)
summary(nox.flux.v.M3Fe.lm)
plot(nox.flux.v.M3Fe.lm, which=c(1,2))
plot(flux_nox_mgm2d~M3_Fe_mg_kg, data = rb_data)

#nox vs TN, not sig
nox.flux.v.TN.lm <- lm(flux_nox_mgm2d~soil_TN_mg_kg, data = rb_data)
summary(nox.flux.v.TN.lm)
plot(nox.flux.v.TN.lm, which=c(1,2))
plot(flux_nox_mgm2d~soil_TN_mg_kg, data = rb_data)

#nox vs TP, not sig
nox.flux.v.TP.lm <- lm(flux_nox_mgm2d~soil_TP_mg_kg, data = rb_data)
summary(nox.flux.v.TP.lm)
plot(nox.flux.v.TP.lm, which=c(1,2))
plot(flux_nox_mgm2d~soil_TP_mg_kg, data = rb_data)


##NH3
fullmod <- lm(flux_nh3_mgm2d~soil_TP_mg_kg+
                soil_TN_mg_kg+
                M3_P_mg_kg+
                M3_Fe_mg_kg+
                M3_Al_mg_kg+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                soil_SPSC+
                M3_Ca_mg_kg+
                M3_Mg_mg_kg+
                M3_K_mg_kg+
                bulk_density_g_cm3,
              data=rb_data_soils_no_na)
minmod <- lm(flux_nh3_mgm2d~1,data=rb_data_soils_no_na)

stepAIC(minmod, direction = "forward", scope = list(upper = fullmod, lower = minmod))
#nothing

stepAIC(minmod, direction = "both", scope = list(upper = fullmod, lower = minmod))
#nothing

#nh3 vs TN, not sig
nh3.flux.v.TN.lm <- lm(flux_nh3_mgm2d~soil_TN_mg_kg, data = rb_data)
summary(nh3.flux.v.TN.lm)
plot(nh3.flux.v.TN.lm, which=c(1,2))
plot(flux_nh3_mgm2d~soil_TN_mg_kg, data = rb_data)

nh3.flux.v.TP.lm <- lm(flux_nh3_mgm2d~soil_TP_mg_kg, data = rb_data)
summary(nh3.flux.v.TP.lm)
plot(nh3.flux.v.TP.lm, which=c(1,2))
plot(flux_nh3_mgm2d~soil_TP_mg_kg, data = rb_data)



#####
##RB Flux vs ambient soil
#####

##DRP flux
#LM drp TP soil conc predictor
cor.test(rb_data$flux_drp_mgm2d, rb_data$soil_TP_mg_kg)
drp.amb.lm <- lm(flux_drp_mgm2d~soil_TP_mg_kg, data = rb_data)
summary(drp.amb.lm)
plot(drp.amb.lm, which=c(1,2))
plot(flux_drp_mgm2d~soil_TP_mg_kg, data = rb_data)


##NOx Flux
#LM nox inflow conc predictor
cor.test(rb_data$flux_nox_mgm2d, rb_data$amb_nox_mgL)
nox.amb.lm <- lm(flux_nox_mgm2d~amb_nox_mgL, data = rb_data)
summary(nox.amb.lm)
plot(nox.amb.lm, which=c(1,2))
plot(flux_nox_mgm2d~soil_TN_mg_kg, data = rb_data)


##NH3 Flux
#LM nh3 inflow conc predictor
cor.test(rb_data$flux_nh3_mgm2d, rb_data$amb_nh3_mgL)
nh3.amb.lm <- lm(flux_nh3_mgm2d~amb_nh3_mgL, data = rb_data)
summary(nh3.amb.lm)
plot(nh3.amb.lm, which=c(1,2))
plot(flux_nh3_mgm2d~soil_TN_mg_kg, data = rb_data)



#####
##Table of fluxes averaged across patches
#for supplementary table 1
#####

avg_patch_fluxes <- data %>% 
  select(c("habitat", "method", "flux_drp_mgm2d", "flux_nox_mgm2d", "flux_nh3_mgm2d")) %>% 
  group_by(habitat, method) %>% 
  summarise(avg_drp_mgm2d = mean(flux_drp_mgm2d, na.rm = TRUE), 
            se_drp_mgm2d = sd(flux_drp_mgm2d, na.rm = TRUE)/sqrt(n()), 
            avg_nox_mgm2d = mean(flux_nox_mgm2d, na.rm = TRUE), 
            se_nox_mgm2d = sd(flux_nox_mgm2d, na.rm = TRUE)/sqrt(n()), 
            avg_nh3_mgm2d = mean(flux_nh3_mgm2d, na.rm = TRUE), 
            se_nh3_mgm2d = sd(flux_nh3_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  arrange(method)

View(avg_patch_fluxes)
write.csv(avg_patch_fluxes, "FOR_SUPP_TABLE_patch_fluxes.csv", row.names = F)





