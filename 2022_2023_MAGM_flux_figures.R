##Author: Mike Back
##Organization: Kent State University
##Project: 2022-2023 Turtle Creek Bay (MAGM) Manuscript
##R version: 4.2.1
##Date updated:2/18/2025

library(tidyverse)
library(patchwork)
library(ggpubr) #only used to make the methods comparison scatter plots, could be switched with patchwork now


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
##Figure 2
#Nutrient fluxes across patches for intact core incubations (ic) and resin bag cores (rb)
#####

#first make ICI three panel side

ic_data <- subset(data, method == "ic")

ic_fluxes_gathered <- gather(ic_data, "nutrient", "flux", 17:19) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."), 
         method = recode(method, 
                         'ic' = "bold(Intact~Core~Incubations)"), 
         nutrient = recode(nutrient, 
                           'flux_drp_mgm2d' = "bold(DRP)", 
                           'flux_nh3_mgm2d' = "bold(NH[3]-N)", #all becasue of the dumb subscript 3 and x in NH3 and NOx
                           'flux_nox_mgm2d' = "bold(NO[x]-N)"))
View(ic_fluxes_gathered)

#make each nutrient plot individually

#ic drp

ic_drp_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(DRP)")

ic_drp <- ic_drp_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
             scales = "free", 
             space = 'free_x', 
             labeller = label_parsed) + 
  scale_y_continuous(limits = c(-12, 20), breaks = seq(-12,20, by = 4)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_drp_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("DRP Exchange", paste("(mg/m"^" 2"*"/d)"))))) + #used to separate the two lines on the axis label
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(fill="white"), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_drp

#ic NOx

ic_nox_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(NO[x]-N)")

ic_nox <- ic_nox_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-4, 5), breaks = seq(-4,5, by = 1)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_nox_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("NO"[x]*"-N Exchange", paste("(mg/m"^" 2"*"/d)"))))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_nox

#ic NH3

library(ggbreak) #in case we want to figure out line breaks, but currently doesn't work with "patchwork"

ic_nh3_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(NH[3]-N)")

ic_nh3 <- ic_nh3_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-40, 160), breaks = seq(-40, 160, by = 20)) + 
#  scale_y_break(c(80,140)) + #for line break
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_nh3_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("NH"[3]*"-N Exchange", paste("(mg/m"^" 2"*"/d)"))))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
#        axis.text.y.right = element_blank(), #for line break
#        axis.line.y.right = element_blank(), #for line break
#        axis.ticks.y.right = element_blank(), #for line break
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_nh3


#now resin bag plots

data_resin_fig2 = read.csv("data\\FOR_FIG2_resin_fluxes_transposed.csv", stringsAsFactors = T)
data_resin_fig2$sample_point <- as.factor(data_resin_fig2$sample_point)
summary(data_resin_fig2)
View(data_resin_fig2)


data_resin_fig2 <- data_resin_fig2 %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."), 
         method = recode(method, 
                         'rb' = "bold(Stacked~Resin~Bag~Units)"), 
         nutrient = recode(nutrient, 
                           'drp' = "bold(DRP)", 
                           'nh3' = "bold(NH[3]-N)", #all becasue of the dumb subscript 3 and x in NH3 and NOx
                           'nox' = "bold(NO[x]-N)"), 
         flux_direction = fct_relevel(flux_direction, 'net', 'top', 'bottom'), 
         flux_direction = recode(flux_direction, 'net' = "Net Flux", 
                                 'top' = "Sediment Influx", 
                                 'bottom' = "Sediment Efflux")) %>% 
  filter(habitat == "Floating" | habitat == "Emergent" | habitat == "Hardwoods" | habitat == "Submerged")

#rb drp

rb_drp_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(DRP)")

rb_drp <- rb_drp_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
             scales = "free", 
             space = 'free_x', 
             labeller = label_parsed) + 
  scale_y_continuous(limits = c(-12, 20), breaks = seq(-12,20, by = 4)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_drp_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10),  
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(fill="white"), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_drp

#rb nox

rb_nox_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(NO[x]-N)")

rb_nox <- rb_nox_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-4, 5), breaks = seq(-4,5, by = 1)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_nox_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_nox


#rb nh3

rb_nh3_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(NH[3]-N)")

rb_nh3 <- rb_nh3_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-40, 160), breaks = seq(-40, 160, by = 20)) + 
#  scale_y_break(c(80,140)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_nh3_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = c(0.85, 0.8), 
        legend.key.size = unit(0.5, "cm"), 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
#        axis.text.y.right = element_blank(),
#        axis.line.y.right = element_blank(),
#        axis.ticks.y.right = element_blank(), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_nh3


#put together using "patchwork"

combined <- ic_drp + theme(plot.tag.position  = c(.35, .86)) + #plot.tag.position is to position the panel letter
  rb_drp + theme(plot.tag.position  = c(.03, .86)) + 
  ic_nox + theme(plot.tag.position  = c(.35, .95)) + 
  rb_nox + theme(plot.tag.position  = c(.03, .95)) + 
  ic_nh3 + theme(plot.tag.position  = c(.35, .95)) + 
  rb_nh3 + theme(plot.tag.position  = c(.03, .95)) + 
  plot_layout(widths = c(.3,.75,.3,.75,.3,.75), ncol = 2, axes = "collect") + #widths are changed to match the size of the bars in each plot
  plot_annotation(tag_levels = 'A') #to make the panel letters capital letters starting with "A"

combined

ggsave("flux_patches_2.png", 
       plot=combined, height=25, width=21, units=c("cm"), dpi=600)



#####
##Figure 3
#Nutrient flux vs inflow/soil concentrations for intact core incubations
#####

#subset intact core (ic) data
ic_data <- subset(data, method == "ic")
ic_data <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  mutate(in_drp_mgL = in_drp_mgL * 1000) %>% #turn conentrations in ug/L
  mutate(in_nox_mgL = in_nox_mgL * 1000) %>% 
  mutate(in_nh3_mgL = in_nh3_mgL * 1000)
View(ic_data)

#DRP inflow vs flux
drp_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_drp_mgL, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_drp_mgL, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 75, y = 9, parse = TRUE, label = paste("R^2", "==", 0.28)) + 
  annotate("text", x = 75, y = 7, label = "p = 0.004") + 
  annotate("text", x = 0, y = 10.75, label = "A", fontface = "bold", size = 6) + 
  labs(x = "Inflow DRP (ug DRP-P/L)", 
       y = expression("DRP Flux (mg DRP-P/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())


#NOx inflow vs flux
nox_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 40, y = 3, parse = TRUE, label = paste("R^2", "==", 0.49)) + 
  annotate("text", x = 40, y = 2, label = paste(" p < 0.001")) + 
  annotate("text", x = 0, y = 4.1, label = "C", fontface = "bold", size = 6) + 
  labs(x = expression("Inflow NO"[x]~"(ug NO"[x]~"-N/L)"), 
       y = expression("NO"[x]~" Flux (mg NO"[x]~"-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  ylim(-5, 4.25) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())


#NH3 inflow vs flux
nh3_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 70, y = 125, parse = TRUE, label = paste("R^2", "==", 0.002)) + 
  annotate("text", x = 70, y = 110, label = "p = 0.825") + 
  annotate("text", x = 0, y = 150, label = "E", fontface = "bold", size = 6) + 
  labs(x = expression("Inflow NH"[3]~"(ug NH"[3]~"-N/L)"), 
       y = expression("NH "[3]~"Flux (mg NH "[3]~"-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())



#DRP soil vs flux
drp_soilM3P <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = M3_P_mg_kg, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = M3_P_mg_kg, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 30, y = 9, parse = TRUE, label = paste("R^2", "==", 0.19)) + 
  annotate("text", x = 30, y = 7, label = " p = 0.021") + 
  annotate("text", x = 0, y = 10.75, label = "B", fontface = "bold", size = 6) + 
  labs(x = "Bioavailable P (mg P/kg soil)", 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())


#NOx soil vs flux
nox_soilWEnox <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = WE_nox_mg_kg, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = WE_nox_mg_kg, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 25, y = 3.5, parse = TRUE, label = paste("R^2", "==", 0.30)) + 
  annotate("text", x = 25, y = 2.5, label = paste("  p = 0.003")) + 
  annotate("text", x = 0, y = 4.1, label = "D", fontface = "bold", size = 6) + 
  labs(x = expression("Labile NO"[x]~"(mg NO"[x]~"-N/kg soil)"), 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  ylim(-5, 4.25) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())


#NH3 soil vs flux
nh3_soilWEnh3 <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = WE_nh3_mg_kg, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = WE_nh3_mg_kg, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 50, y = 125, parse = TRUE, label = paste("R^2", "==", 0.17)) + 
  annotate("text", x = 50, y = 110, label = " p = 0.027") + 
  annotate("text", x = 0, y = 150, label = "F", fontface = "bold", size = 6) + 
  labs(x = expression("Labile NH"[3]~"(mg NH"[3]~"-N/kg soil)"), 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 12), 
        panel.grid = element_blank())


##combine inflow and soil
flux_inflow_soil <- ggarrange(drp_inflow, 
                              drp_soilM3P, 
                              nox_inflow, 
                              nox_soilWEnox, 
                              nh3_inflow, 
                              nh3_soilWEnh3, 
                              nrow = 3, ncol = 2, 
                              common.legend = T, 
                              legend = "bottom", 
                              align = "v")
flux_inflow_soil

ggsave("flux_inflow_soil_no_gridlines.png", 
       plot=flux_inflow_soil, height=20, width=25, units=c("cm"), dpi=600)


#####
##Figure 4
#gas fluxes in patches
#####

gas_fluxes_gathered <- gather(ic_data, "gas", "flux", c("N2_flux_mg_m2_d", "O2_flux_mg_m2_d")) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  mutate(gas = recode(gas, 
                      'N2_flux_mg_m2_d' = "bold(N[2]~Flux~(mg/m^2/d))", 
                      'O2_flux_mg_m2_d' = "bold(O[2]~Flux~(mg/m^2/d))"))

summary(gas_fluxes_gathered)

gas_panel_letters <- data.frame(gas = c("bold(N[2]~Flux~(mg/m^2/d))", 
                                        "bold(O[2]~Flux~(mg/m^2/d))"), 
                                habitat = c("Floating Veg.", 
                                            "Floating Veg."), 
                                m = c(65, 
                                      130), 
                                label = c("A", 
                                          "B"))

gas_flux_patches <- gas_fluxes_gathered %>% 
  group_by(habitat, gas) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m)) + 
  geom_bar(aes(fill = m > 0), 
           stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_wrap(~gas, 
             scales = "free_y", 
             strip.position = "left", 
             labeller = label_parsed) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = gas_fluxes_gathered, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL) + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.spacing.x = unit(0.25, "lines"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        strip.text.x = element_text(size=11, face="bold"),
        strip.text.y = element_text(size=11, face="bold"), 
        strip.background = element_blank(),  
        strip.placement = "outside", 
        axis.title.x = element_text(size = 11, face = "bold"), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray", "gray")) + 
  geom_hline(yintercept = 0) + 
  geom_text(data = gas_panel_letters, label = gas_panel_letters$label, 
            size = 4, fontface = "bold", 
            position = position_nudge(-0.4))

gas_flux_patches

ggsave("gas_flux_patches_no_gridlines.png", 
       plot=gas_flux_patches, height=8, width=15, units=c("cm"), dpi=600)



#####
##Figure 5
#2022 and 2023 ICI vs RB fluxes for each sample point
#####

fluxes_gathered <- gather(data, "nutrient", "flux", 17:19)

fluxes_drp <- subset(fluxes_gathered, nutrient == "flux_drp_mgm2d")
fluxes_nox <- subset(fluxes_gathered, nutrient == "flux_nox_mgm2d")
fluxes_nh3 <- subset(fluxes_gathered, nutrient == "flux_nh3_mgm2d")
View(fluxes_drp)

ic_data <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp."))


##DRP rb vs ic fluxes
drp_methods <- fluxes_drp %>% dplyr::select(sample_point, method, flux, habitat) %>%  #use the dataframe from the first figure, called fig 3 because this is reused code, select for only columns of interest
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% #makes the two methods individual columns to allow for comparison
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = 7, y = 8.5, angle = 50, label = "1:1", size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  labs(title = NULL, 
      x = expression(atop("Intact Core Incubation", paste("DRP Flux (mg/m"^" 2"*"/d)"))), 
      y = expression(atop("Stacked Resin Bag Units", paste("DRP Flux (mg/m"^" 2"*"/d)"))), 
      shape = "Vegetation Patch") + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        legend.text = element_text(size = 6))

##NO3 rb vs ic fluxes
nox_methods <- fluxes_nox %>% dplyr::select(sample_point, method, flux, habitat) %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% 
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = -1, y = -0.6, angle = 75, label = "1:1", size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  labs(title = NULL, 
       x = expression(atop("Intact Core Incubation", paste("NO"[x]~"-N Flux (mg/m"^" 2"*"/d)"))), 
       y = expression(atop("Stacked Resin Bag Units", paste("NO"[x]~"-N Flux (mg/m"^" 2"*"/d)"))), 
       shape = "Vegetation Patch") + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        legend.text = element_text(size = 6))

##NH3 rb vs ic fluxes
nh3_methods <- fluxes_nh3 %>% dplyr::select(sample_point, method, flux, habitat) %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% na.omit() %>% 
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = 14, y = 6, angle = 75, label = "1:1", size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  labs(title = NULL, 
       x = expression(atop("Intact Core Incubation", paste("NH"[3]~"-N Flux (mg/m"^" 2"*"/d)"))), 
       y = expression(atop("Stacked Resin Bag Units", paste("NH"[3]~"-N Flux (mg/m"^" 2"*"/d)"))), 
       shape = "Vegetation Patch") + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10), 
        legend.text = element_text(size = 6))

fluxes_methods <- ggarrange(drp_methods, 
                            nox_methods, 
                            nh3_methods, 
                            nrow = 1, ncol = 3, 
                            common.legend = T, 
                            legend = "bottom", 
                            align = "h")
fluxes_methods #NEED TO FIX LEGEND

ggsave("fluxes_methods_revised.png", 
       plot=fluxes_methods, height=7, width=18, units=c("cm"), dpi=600)


#####
##Supplementary Fig. 1
#####
ic_data_gas <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."))

##N2 Gas flux in relation to NOx inflow conc
N2_nox <- ic_data_gas %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d), color = "black", method = lm) + 
  theme_bw() + 
  labs(x = expression(bold("Inflow NO"[x]*"-N Concentration (mg NO"[x]*"-N/L)")), 
       y = expression(bold("N"[2]*" Flux (mg N/m"^" 2"*"/d)")),
       shape = "Vegetation Patch") + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2))
N2_nox #more negative N2 flux (influx;n fixation) was with cores that had higher NOx concentration
ggsave("N2_vs_nox_in.png", 
       plot=N2_nox, height=9, width=18, units=c("cm"), dpi=600)





#####
## Extra, not in the Manuscript
#####

#Nutrient flux vs ambient/soil concentrations for Resin Bags

rb_data <- subset(data, method == "rb")
rb_data <- rb_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp."))
View(rb_data)


#DRP
drp_inflow <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = in_drp_mgL, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_drp_mgL, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 0.075, y = 9), label = "R^2 = 0.58
p < 0.001") + 
  labs(x = "Inflow DRP (mg DRP-P/L)", 
       y = expression("DRP Flux (mg DRP-P/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
drp_inflow


#NOx
nox_inflow <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 0.04, y = 3), label = "R^2 = 0.73
p < 0.001") + 
  labs(x = "Inflow NOx (mg NOx-N/L)", 
       y = expression("NOx Flux (mg NOx-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
nox_inflow


#NH3
nh3_inflow <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 0.07, y = 125), label = "R^2 = 0.33
p = 0.230") + 
  labs(x = "Inflow NH3 (mg NH3-N/L)", 
       y = expression("NH3 Flux (mg NH3-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
nh3_inflow


##Flux vs soil for ici

#DRP
drp_soilTP <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = soil_TP_mg_kg, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = soil_TP_mg_kg, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 500, y = 9), label = "R^2 = 0.22
p = 0.012 ") + 
  labs(x = "Sediment Total P (mg P/kg soil)", 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
drp_soilTP


#NOx
nox_soilTN <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = soil_TN_mg_kg, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = soil_TN_mg_kg, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 2500, y = 3), label = "R^2 = 0.26
p = 0.006") + 
  labs(x = "Sediment total N (mg N/kg soil)", 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
nox_soilTN


#NH3
nh3_soilTN <- rb_data %>% 
  ggplot() + 
  geom_point(aes(x = soil_TN_mg_kg, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = soil_TN_mg_kg, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw() + 
  geom_text(aes(x = 2500, y = 125), label = "R^2 = 0.07
p = 0.169") + 
  labs(x = "Sediment Total N (mg N/kg soil)", 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
nh3_soilTN


##combine inflow and soil
flux_rb_inflow_soil <- ggarrange(drp_inflow, drp_soilTP, nox_inflow, nox_soilTN, nh3_inflow, nh3_soilTN, nrow = 3, ncol = 2, common.legend = T, legend = "bottom")



###Extra gas flux figures

gas_fluxes_gathered <- gather(ic_data, "gas", "flux", 44:45) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp."))

##N2 Gas flux in relation to NOx inflow conc
N2_nox <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d), color = "black", method = lm) + 
  theme_bw() + 
  labs(x = "Inflow NOx (mg NOx-N/L)", 
       y = expression("N2 Flux (mg N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
N2_nox #more negative N2 flux (influx;n fixation) was with cores that had higher NOx concentration
ggsave("N2_vs_nox_in.png", 
       plot=N2_nox, height=9, width=18, units=c("cm"), dpi=600)

##N2 Gas flux in relation to TOC in sed
N2_TOC <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = TOC_mg_kg, y = N2_flux_mg_m2_d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = TOC_mg_kg, y = N2_flux_mg_m2_d), color = "black", method = lm) + 
  theme_bw() + 
  labs(x = "Sed TOC (mg C/kg sediment)", 
       y = expression("N2 Flux (mg N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
N2_TOC
ggsave("N2_vs_TOC.png", 
       plot=N2_TOC, height=9, width=18, units=c("cm"), dpi=600)

#N2 flux vs NOx flux
N2_nox_flux <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = flux_nox_mgm2d, y = N2_flux_mg_m2_d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = flux_nox_mgm2d, y = N2_flux_mg_m2_d), color = "black", method = lm) + 
  theme_bw() + 
  labs(x = "NOx flux", 
       y = expression("N2 Flux (mg N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_color_manual(values = c("#56B4E9", "#009E73", "#E69F00", "#0072B2", "#F0E442")) + 
  scale_shape_manual(values = c(12, 17, 15, 18, 16))
N2_nox_flux
ggsave("N2_vs_nox_flux.png", 
       plot=N2_nox_flux, height=9, width=18, units=c("cm"), dpi=600)


##Original figure two without separate resin bag flux

fluxes_gathered <- gather(data, "nutrient", "flux", 17:19) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp."), 
         method = recode(method, 
                         'ic' = "bold(Intact~Core~Incubations)", #Have to recode as an expression to get the facet_grid labels to work in the below figure
                         'rb' = "bold(Stacked~Resin~Bag~Cores)"), 
         nutrient = recode(nutrient, 
                           'flux_drp_mgm2d' = "bold(DRP)", 
                           'flux_nh3_mgm2d' = "bold(NH[3]-N)", #all becasue of the dumb subscript 3 and x in NH3 and NOx
                           'flux_nox_mgm2d' = "bold(NO[x]-N)"))
View(fluxes_gathered)

fig_text <- data.frame(nutrient = c("bold(DRP)", #need to add "No Data" labels in the resin bag graphs for typha patch
                                    "bold(NO[x]-N)", 
                                    "bold(NH[3]-N)"), 
                       m = c(1.75, 0.5, 12), 
                       habitat = c("Typha spp.", "Typha spp.", "Typha spp."), 
                       method = c("bold(Stacked~Resin~Bag~Cores)", 
                                  "bold(Stacked~Resin~Bag~Cores)", 
                                  "bold(Stacked~Resin~Bag~Cores)"), 
                       label = c("No Data", "No Data", "No Data"))

panel_letters <- data.frame(nutrient = c("bold(DRP)", #to add panel letters to figure 2
                                         "bold(NO[x]-N)", 
                                         "bold(NH[3]-N)", 
                                         "bold(DRP)", 
                                         "bold(NO[x]-N)", 
                                         "bold(NH[3]-N)"), 
                            m = c(12, 
                                  4, 
                                  148,
                                  12, 
                                  4, 
                                  148), 
                            habitat = c("Floating Veg.", 
                                        "Floating Veg.", 
                                        "Floating Veg.",
                                        "Floating Veg.",
                                        "Floating Veg.",
                                        "Floating Veg."), 
                            method = c("bold(Intact~Core~Incubations)", 
                                       "bold(Intact~Core~Incubations)", 
                                       "bold(Intact~Core~Incubations)", 
                                       "bold(Stacked~Resin~Bag~Cores)", 
                                       "bold(Stacked~Resin~Bag~Cores)", 
                                       "bold(Stacked~Resin~Bag~Cores)"), 
                            label = c("A", 
                                      "C", 
                                      "E", 
                                      "B", 
                                      "D", 
                                      "F"))

flux_patches_pos_neg <- fluxes_gathered %>%  
  group_by(method, habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m)) + 
  geom_bar(aes(fill = m>0), 
           stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid(fct_relevel(nutrient, 
                         'bold(DRP)', 
                         'bold(NO[x]-N)', 
                         'bold(NH[3]-N)') ~ method, 
             scales = "free", 
             space = 'free_x', 
             labeller = label_parsed) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = fluxes_gathered, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold("Nutrient Flux (mg/m"^" 2"*"/d)"))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.spacing.y = unit(0.75, "lines"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        strip.text.x = element_text(size=10, face="bold"),
        strip.text.y = element_text(size=10, face="bold"), 
        strip.background = element_rect(fill="white"), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12)) + 
  scale_fill_manual(values = c("#009E73", "#E69F00")) + 
  geom_hline(yintercept = 0) + 
  geom_text(data = fig_text, label = fig_text$label, size = 3) + 
  geom_text(data = panel_letters, label = panel_letters$label, 
            size = 4, fontface = "bold", 
            position = position_nudge(-0.4))

flux_patches_pos_neg
ggsave("flux_patches_pos_neg.png", 
       plot=flux_patches_pos_neg, height=20, width=15, units=c("cm"), dpi=600)



#####
##THE GREAT UNIT CHANGE OF 2025
#####

data <- data %>% 
  mutate(across(c(amb_nox_mgL, 
                  amb_nh3_mgL, 
                  in_nox_mgL, 
                  in_nh3_mgL, 
                  flux_nox_mgm2d, 
                  flux_nh3_mgm2d, 
                  WE_nox_mg_kg, 
                  WE_nh3_mg_kg, 
                  N2_flux_mg_m2_d, 
                  resin_accum_top_nox_mgm2d, 
                  resin_accum_bottom_nox_mgm2d, 
                  resin_accum_top_nh3_mgm2d, 
                  resin_accum_bottom_nh3_mgm2d), ~ .x * 71.43)) %>% 
  mutate(across(c(amb_drp_mgL, 
                  in_drp_mgL, 
                  flux_drp_mgm2d, 
                  M3_P_mg_kg, 
                  resin_accum_top_drp_mgm2d, 
                  resin_accum_bottom_drp_mgm2d), ~ .x * 32.29)) %>% 
  mutate(across(c(O2_flux_mg_m2_d), ~ .x * 62.5))
View(data)
summary(data)


#####
##Figure 2 MOLAR
#Nutrient fluxes across patches for intact core incubations (ic) and resin bag cores (rb)
#####

#first make ICI three panel side

ic_data <- subset(data, method == "ic")
summary(ic_data)

ic_fluxes_gathered <- gather(ic_data, "nutrient", "flux", 17:19) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."), 
         method = recode(method, 
                         'ic' = "bold(Intact~Core~Incubations)"), 
         nutrient = recode(nutrient, 
                           'flux_drp_mgm2d' = "bold(DRP)", 
                           'flux_nh3_mgm2d' = "bold(NH[3]-N)", #all becasue of the dumb subscript 3 and x in NH3 and NOx
                           'flux_nox_mgm2d' = "bold(NO[x]-N)"))
View(ic_fluxes_gathered)

#make each nutrient plot individually

#ic drp

ic_drp_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(DRP)")

ic_drp <- ic_drp_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-400, 650), breaks = seq(-400, 650, by = 200)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_drp_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("DRP Exchange", paste("(\u03BCmol/m"^" 2"*"/d)"))))) + #used to separate the two lines on the axis label
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(fill="white"), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_drp

#ic NOx

ic_nox_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(NO[x]-N)")

ic_nox <- ic_nox_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-300, 360), breaks = seq(-300, 360, by = 100)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_nox_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("NO"[x]*"-N Exchange", paste("(\u03BCmol/m"^" 2"*"/d)"))))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_nox

#ic NH3

library(ggbreak) #in case we want to figure out line breaks, but currently doesn't work with "patchwork"

ic_nh3_data <- ic_fluxes_gathered %>% 
  filter(nutrient == "bold(NH[3]-N)")

ic_nh3 <- ic_nh3_data %>% 
  group_by(habitat, nutrient) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = nutrient)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-3000, 11450), breaks = seq(-3000, 11450, by = 3000)) + 
  #  scale_y_break(c(80,140)) + #for line break
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = ic_nh3_data, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = .8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = expression(bold(atop("NH"[4]^ "+" *"-N Exchange", paste("(\u03BCmol/m"^" 2"*"/d)"))))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        #        axis.text.y.right = element_blank(), #for line break
        #        axis.line.y.right = element_blank(), #for line break
        #        axis.ticks.y.right = element_blank(), #for line break
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray")) + 
  geom_hline(yintercept = 0)
ic_nh3


#now resin bag plots

data_resin_fig2 = read.csv("data\\FOR_FIG2_resin_fluxes_transposed_molar.csv", stringsAsFactors = T)
data_resin_fig2$sample_point <- as.factor(data_resin_fig2$sample_point)
summary(data_resin_fig2)
View(data_resin_fig2)


data_resin_fig2 <- data_resin_fig2 %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."), 
         method = recode(method, 
                         'rb' = "bold(Stacked~Resin~Bag~Units)"), 
         nutrient = recode(nutrient, 
                           'drp' = "bold(DRP)", 
                           'nh3' = "bold(NH[3]-N)", #all becasue of the dumb subscript 3 and x in NH3 and NOx
                           'nox' = "bold(NO[x]-N)"), 
         flux_direction = fct_relevel(flux_direction, 'net', 'top', 'bottom'), 
         flux_direction = recode(flux_direction, 'net' = "Net Flux", 
                                 'top' = "Sediment Influx", 
                                 'bottom' = "Sediment Efflux")) %>% 
  filter(habitat == "Floating" | habitat == "Emergent" | habitat == "Hardwoods" | habitat == "Submerged")

#rb drp

rb_drp_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(DRP)")

rb_drp <- rb_drp_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-400, 650), breaks = seq(-400, 650, by = 200)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_drp_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10),  
        strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(fill="white"), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_drp

#rb nox

rb_nox_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(NO[x]-N)")

rb_nox <- rb_nox_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-300, 360), breaks = seq(-300, 360, by = 100)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_nox_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_nox


#rb nh3

rb_nh3_data <- data_resin_fig2 %>% 
  filter(nutrient == "bold(NH[3]-N)")

rb_nh3 <- rb_nh3_data %>% 
  group_by(habitat, flux_direction) %>% 
  summarise(m = mean(flux_rate_mgm2d, na.rm = TRUE), 
            se = sd(flux_rate_mgm2d, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m, fill = flux_direction)) + 
  geom_bar(stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_grid( ~ method, 
              scales = "free", 
              space = 'free_x', 
              labeller = label_parsed) + 
  scale_y_continuous(limits = c(-3000, 11450), breaks = seq(-3000, 11450, by = 3000)) + 
  #  scale_y_break(c(80,140)) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = rb_nh3_data, aes(x = habitat, y = flux_rate_mgm2d, fill = flux_direction), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0.8), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL, 
       fill = "Flux Direction") + 
  scale_fill_manual(values = c("gray", "gray25", "white")) + 
  theme_bw() + 
  theme(legend.position = c(0.85, 0.8), 
        legend.key.size = unit(0.5, "cm"), 
        axis.text.x = element_text(size = 10, angle = 30, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10), 
        strip.text.x = element_blank(), 
        strip.background = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 12), 
        #        axis.text.y.right = element_blank(),
        #        axis.line.y.right = element_blank(),
        #        axis.ticks.y.right = element_blank(), 
        panel.grid = element_blank()) + 
  geom_hline(yintercept = 0)
rb_nh3


#put together using "patchwork"

combined <- ic_drp + theme(plot.tag.position  = c(.4, .86)) + #plot.tag.position is to position the panel letter
  rb_drp + theme(plot.tag.position  = c(.03, .86)) + 
  ic_nox + theme(plot.tag.position  = c(.4, .95)) + 
  rb_nox + theme(plot.tag.position  = c(.03, .95)) + 
  ic_nh3 + theme(plot.tag.position  = c(.4, .95)) + 
  rb_nh3 + theme(plot.tag.position  = c(.03, .95)) + 
  plot_layout(widths = c(.3,.75,.3,.75,.3,.75), ncol = 2, axes = "collect") + #widths are changed to match the size of the bars in each plot
  plot_annotation(tag_levels = 'A') #to make the panel letters capital letters starting with "A"

combined

ggsave("flux_patches_2_molar.png", 
       plot=combined, height=25, width=21, units=c("cm"), dpi=600)



#####
##Figure 3 MOLAR
#Nutrient flux vs inflow/soil concentrations for intact core incubations
#####

#subset intact core (ic) data
ic_data <- subset(data, method == "ic")
View(ic_data)
ic_data <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) #%>% 
  #mutate(in_drp_mgL = in_drp_mgL * 1000) %>% #turn conentrations in ug/L
  #mutate(in_nox_mgL = in_nox_mgL * 1000) %>% 
  #mutate(in_nh3_mgL = in_nh3_mgL * 1000)      DONT NEED THESE SINCE UNIT CONVERSION
View(ic_data)

#DRP inflow vs flux
drp_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_drp_mgL, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_drp_mgL, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 2.4, y = 320, parse = TRUE, label = paste("R^2", "==", 0.28)) + 
  annotate("text", x = 2.4, y = 226, label = "p = 0.004") + 
  annotate("text", x = 0, y = 347, label = "A", fontface = "bold", size = 6) + 
  labs(x = "Inflow DRP (\u03BCmol DRP-P/L)", 
       y = expression("DRP Flux (\u03BCmol DRP-P/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-400, 400), breaks = seq(-400, 400, by = 200)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())


#NOx inflow vs flux
nox_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 2.84, y = 214, parse = TRUE, label = paste("R^2", "==", 0.49)) + 
  annotate("text", x = 2.84, y = 142, label = paste(" p < 0.001")) + 
  annotate("text", x = 0, y = 293, label = "C", fontface = "bold", size = 6) + 
  labs(x = expression("Inflow NO"[x]~"(\u03BCmol NO"[x]~"-N/L)"), 
       y = expression("NO"[x]~" Flux (\u03BCmol NO"[x]~"-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-350, 360), breaks = seq(-350, 360, by = 200)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())


#NH3 inflow vs flux
nh3_inflow <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nh3_mgL, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 4.97, y = 9178, parse = TRUE, label = paste("R^2", "==", 0.002)) + 
  annotate("text", x = 4.97, y = 7857, label = "p = 0.825") + 
  annotate("text", x = 0, y = 10714, label = "E", fontface = "bold", size = 6) + 
  labs(x = expression("Inflow NH"[4]^ "+" *" (\u03BCmol NH"[4]^ "+" *"-N/L)"), 
       y = expression("NH"[4]^ "+" *"Flux (\u03BCmol NH"[4]^ "+" *"-N/m"^" 2"*"/d)"),
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-3000, 11450), breaks = seq(-3000, 11450, by = 3000)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())



#DRP soil vs flux
drp_soilM3P <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = M3_P_mg_kg, y = flux_drp_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = M3_P_mg_kg, y = flux_drp_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 500, y = 320, parse = TRUE, label = paste("R^2", "==", 0.19)) + 
  annotate("text", x = 500, y = 226, label = " p = 0.021") + 
  annotate("text", x = 0, y = 347, label = "B", fontface = "bold", size = 6) + 
  labs(x = "Bioavailable P (\u03BCmol P/kg soil)", 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-400, 400), breaks = seq(-400, 400, by = 200)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())


#NOx soil vs flux
nox_soilWEnox <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = WE_nox_mg_kg, y = flux_nox_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = WE_nox_mg_kg, y = flux_nox_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 3000, y = -100, parse = TRUE, label = paste("R^2", "==", 0.30)) + 
  annotate("text", x = 3000, y = -178, label = paste("  p = 0.003")) + 
  annotate("text", x = 0, y = 293, label = "D", fontface = "bold", size = 6) + 
  labs(x = expression("Labile NO"[x]~"(\u03BCmol NO"[x]~"-N/kg soil)"), 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-350, 360), breaks = seq(-350, 360, by = 200)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())


#NH3 soil vs flux
nh3_soilWEnh3 <- ic_data %>% 
  ggplot() + 
  geom_point(aes(x = WE_nh3_mg_kg, y = flux_nh3_mgm2d, color = habitat, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = WE_nh3_mg_kg, y = flux_nh3_mgm2d), color = "black", method = lm) + 
  theme_bw(base_size = 13) + 
  annotate("text", x = 1250, y = 9178, parse = TRUE, label = paste("R^2", "==", 0.17)) + 
  annotate("text", x = 1250, y = 7857, label = " p = 0.027") + 
  annotate("text", x = 0, y = 10714, label = "F", fontface = "bold", size = 6) + 
  labs(x = expression("Labile NH"[4]^ "+" *" (\u03BCmol NH"[4]^ "+" *"-N/kg soil)"), 
       y = NULL,
       shape = "Vegetation Patch", 
       color = "Vegetation Patch") + 
  scale_y_continuous(limits = c(-3000, 11450), breaks = seq(-3000, 11450, by = 3000)) + 
  scale_color_manual(values = c("black", "black", "black", "black", "black")) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(axis.title = element_text(size = 11), 
        panel.grid = element_blank())


##combine inflow and soil
flux_inflow_soil <- ggarrange(drp_inflow, 
                              drp_soilM3P, 
                              nox_inflow, 
                              nox_soilWEnox, 
                              nh3_inflow, 
                              nh3_soilWEnh3, 
                              nrow = 3, ncol = 2, 
                              common.legend = T, 
                              legend = "bottom", 
                              align = "v")
flux_inflow_soil

ggsave("flux_inflow_soil_no_gridlines_molar.png", 
       plot=flux_inflow_soil, height=20, width=25, units=c("cm"), dpi=600)


#####
##Figure 4 MOLAR
#gas fluxes in patches
#####

gas_fluxes_gathered <- gather(ic_data, "gas", "flux", c("N2_flux_mg_m2_d", "O2_flux_mg_m2_d")) %>% #gather data to have one column of nutrient type and one column of flux values
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  mutate(gas = recode(gas, 
                      'N2_flux_mg_m2_d' = "bold(N[2]~Flux~(\u03BCmol/m^2/d))", 
                      'O2_flux_mg_m2_d' = "bold(O[2]~Flux~(\u03BCmol/m^2/d))"))

summary(gas_fluxes_gathered)

gas_panel_letters <- data.frame(gas = c("bold(N[2]~Flux~(\u03BCmol/m^2/d))", 
                                        "bold(O[2]~Flux~(\u03BCmol/m^2/d))"), 
                                habitat = c("Floating Veg.", 
                                            "Floating Veg."), 
                                m = c(4643, 
                                      8125), 
                                label = c("A", 
                                          "B"))

gas_flux_patches <- gas_fluxes_gathered %>% 
  group_by(habitat, gas) %>% 
  summarize(m = mean(flux, na.rm = TRUE),
            se = sd(flux, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(aes(x = habitat, y = m)) + 
  geom_bar(aes(fill = m > 0), 
           stat = "identity", 
           color = "black", 
           position = "dodge", 
           width = 0.8) + 
  facet_wrap(~gas, 
             scales = "free_y", 
             strip.position = "left", 
             labeller = label_parsed) + 
  geom_errorbar(aes(ymin=m-se, ymax=m+se), 
                width = .2,
                position = position_dodge(.8)) + 
  geom_point(data = gas_fluxes_gathered, aes(x = habitat, y = flux), #call back to old dataframe that hasn't been summarized
             position = position_dodge(width = 0), 
             size = 2) + 
  labs(x = "Vegetation Patch", 
       y = NULL) + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.spacing.x = unit(0.25, "lines"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), 
        strip.text.x = element_text(size=11, face="bold"),
        strip.text.y = element_text(size=11, face="bold"), 
        strip.background = element_blank(),  
        strip.placement = "outside", 
        axis.title.x = element_text(size = 11, face = "bold"), 
        panel.grid = element_blank()) + 
  scale_fill_manual(values = c("gray", "gray")) + 
  geom_hline(yintercept = 0) + 
  geom_text(data = gas_panel_letters, label = gas_panel_letters$label, 
            size = 4, fontface = "bold", 
            position = position_nudge(-0.4))

gas_flux_patches

ggsave("gas_flux_patches_no_gridlines_molar.png", 
       plot=gas_flux_patches, height=8, width=15, units=c("cm"), dpi=600)



#####
##Figure 5 MOLAR
#2022 and 2023 ICI vs RB fluxes for each sample point
#####

fluxes_gathered <- gather(data, "nutrient", "flux", 17:19)

fluxes_drp <- subset(fluxes_gathered, nutrient == "flux_drp_mgm2d")
fluxes_nox <- subset(fluxes_gathered, nutrient == "flux_nox_mgm2d")
fluxes_nh3 <- subset(fluxes_gathered, nutrient == "flux_nh3_mgm2d")
View(fluxes_drp)

ic_data <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp."))


##DRP rb vs ic fluxes
drp_methods <- fluxes_drp %>% dplyr::select(sample_point, method, flux, habitat) %>%  #use the dataframe from the first figure, called fig 3 because this is reused code, select for only columns of interest
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% #makes the two methods individual columns to allow for comparison
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = 226.03, y = 274.47, angle = 50, label = "1:1", size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  labs(title = NULL, 
       x = expression(atop("Intact Core Incubation", paste("DRP Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       y = expression(atop("Stacked Resin Bag Units", paste("DRP Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       shape = "Vegetation Patch") + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 8), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

##NO3 rb vs ic fluxes
nox_methods <- fluxes_nox %>% dplyr::select(sample_point, method, flux, habitat) %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% 
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = -71.43, y = -42.86, angle = 75, label = "1:1", size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  labs(title = NULL, 
       x = expression(atop("Intact Core Incubation", paste("NO"[x]~"-N Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       y = expression(atop("Stacked Resin Bag Units", paste("NO"[x]~"-N Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       shape = "Vegetation Patch") + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 8), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

##NH3 rb vs ic fluxes
nh3_methods <- fluxes_nh3 %>% dplyr::select(sample_point, method, flux, habitat) %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating Veg.", #recode habitat factor names
                          'grasses' = "Emergent Veg.", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged Veg.", 
                          'typha' = "Typha spp.")) %>% 
  pivot_wider(names_from = method, values_from = flux) %>% na.omit() %>% 
  ggplot(aes(x = ic, y = rb)) + 
  geom_point(aes(shape = habitat), size = 2) +
  geom_smooth(color = "black", method = lm, se = FALSE) + 
  geom_abline(slope = 1, linetype = "dashed") + 
  annotate("text", x = 1000.02, y = 428.58, angle = 75, label = "1:1", size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  labs(title = NULL, 
       x = expression(atop("Intact Core Incubation", paste("NH"[4]^ "+" *"-N Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       y = expression(atop("Stacked Resin Bag Units", paste("NH"[4]^ "+" *"-N Flux (\u03BCmol/m"^" 2"*"/d)"))), 
       shape = "Vegetation Patch") + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme_classic() + 
  theme(axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 8), 
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8), 
        axis.text = element_text(size = 6))

fluxes_methods <- ggarrange(drp_methods, 
                            nox_methods, 
                            nh3_methods, 
                            nrow = 1, ncol = 3, 
                            common.legend = T, 
                            legend = "bottom", 
                            align = "h")
fluxes_methods

ggsave("fluxes_methods_revised_molar.png", 
       plot=fluxes_methods, height=7, width=18, units=c("cm"), dpi=600)


#####
##Supplementary Fig. 1 MOLAR
#####
ic_data_gas <- ic_data %>% 
  mutate(habitat = recode(habitat, 'fav' = "Floating", #recode habitat factor names
                          'grasses' = "Emergent", 
                          'hardwoods' = "Hardwoods", 
                          'sav' = "Submerged", 
                          'typha' = "Typha spp."))

##N2 Gas flux in relation to NOx inflow conc
N2_nox <- ic_data_gas %>% 
  ggplot() + 
  geom_point(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d, shape = habitat), size = 3) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(aes(x = in_nox_mgL, y = N2_flux_mg_m2_d), color = "black", method = lm) + 
  theme_bw() + 
  labs(x = expression(bold("Inflow NO"[x]*"-N Concentration (\u03BCmol NO"[x]*"-N/L)")), 
       y = expression(bold("N"[2]*" Flux (\u03BCmol N/m"^" 2"*"/d)")),
       shape = "Vegetation Patch") + 
  ylim(-12000, 6000) + 
  scale_shape_manual(values = c(15, 16, 17, 0, 2)) + 
  theme(panel.grid = element_blank())

N2_nox #more negative N2 flux (influx;n fixation) was with cores that had higher NOx concentration
ggsave("N2_vs_nox_in_molar.png", 
       plot=N2_nox, height=9, width=18, units=c("cm"), dpi=600)


