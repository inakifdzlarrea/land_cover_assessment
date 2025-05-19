#In this script we are going to estimate the effect size of using  synthesis maps (CORINE, SIOSE and NFI) vs Supervised Classification. To assess this we are going to use ratios between the land cover proportions and its evolution. We are also going to assess if the size of the effect is dependency on the scale we use (Catchment, Riparian and Reach)
#February 2025

#load packages
library(tidyverse)

#load data:-----

#land cover proportion comparisons
comparison_SC_CORINE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_CORINE_evo.csv")
comparison_SC_SIOSE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_SIOSE_evo.csv")
comparison_SC_INV_FOR <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_INV_FOR_evo.csv")

#land cover evolution model slopes
model_slopes_CORINE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/evolution_models/model_slopes_CORINE.csv")
model_slopes_SIOSE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt//evolution_models/model_slopes_SIOSE.csv")
model_slopes_INV_FOR <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt//evolution_models/model_slopes_INV_FOR.csv")

#Differences in land cover proportions-----

##Land cover proportion comparison plots----
###CORINE-----
plot_SC_CORINE = comparison_SC_CORINE %>%group_by(land_use, Scale)%>%
  filter(Year_SC != 1997)%>%#no data for Corine in this year
  summarise(LU_prop_SC_mean = mean(LU_prop_SC), 
            LU_prop_SC_min = quantile(LU_prop_SC, 0.1),
            LU_prop_SC_max = quantile(LU_prop_SC, 0.9),
            LU_prop_CORINE_mean = mean(LU_prop_CORINE),
            LU_prop_CORINE_min = quantile(LU_prop_CORINE, 0.1),
            LU_prop_CORINE_max = quantile(LU_prop_CORINE, 0.9))%>%
  ggplot(aes(LU_prop_SC_mean, LU_prop_CORINE_mean, color = land_use, shape = Scale))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax = LU_prop_CORINE_max, ymin = LU_prop_CORINE_min))+
  geom_errorbarh(aes(xmax = LU_prop_SC_max, xmin = LU_prop_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean LU proportion in SC (Q10-Q90)", y ="Mean LU proportion in CORINE (Q10-Q90)")+
  theme_classic()

###SIOSE-----
plot_SC_SIOSE = comparison_SC_SIOSE %>%group_by(land_use, Scale)%>%
  summarise(LU_prop_SC_mean = mean(LU_prop_SC), 
            LU_prop_SC_min = quantile(LU_prop_SC, 0.1),
            LU_prop_SC_max = quantile(LU_prop_SC, 0.9),
            LU_prop_SIOSE_mean = mean(LU_prop_SIOSE),
            LU_prop_SIOSE_min = quantile(LU_prop_SIOSE, 0.1),
            LU_prop_SIOSE_max = quantile(LU_prop_SIOSE, 0.9))%>%
  ggplot(aes(LU_prop_SC_mean, LU_prop_SIOSE_mean, color = land_use, shape = Scale))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax = LU_prop_SIOSE_max, ymin = LU_prop_SIOSE_min))+
  geom_errorbarh(aes(xmax = LU_prop_SC_max, xmin = LU_prop_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean LU proportion in SC (Q10-Q90)", y ="Mean LU proportion in SIOSE (Q10-Q90)")+
  theme_classic()


###INV_FOR-----
plot_SC_INV_FOR = comparison_SC_INV_FOR %>%group_by(land_use, Scale)%>%
  summarise(LU_prop_SC_mean = mean(LU_prop_SC), 
            LU_prop_SC_min = quantile(LU_prop_SC, 0.1),
            LU_prop_SC_max = quantile(LU_prop_SC, 0.9),
            LU_prop_INV_FOR_mean = mean(LU_prop_INV_FOR),
            LU_prop_INV_FOR_min = quantile(LU_prop_INV_FOR, 0.1),
            LU_prop_INV_FOR_max = quantile(LU_prop_INV_FOR, 0.9))%>%
  ggplot(aes(LU_prop_SC_mean, LU_prop_INV_FOR_mean, color = land_use, shape = Scale))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax = LU_prop_INV_FOR_max, ymin = LU_prop_INV_FOR_min))+
  geom_errorbarh(aes(xmax = LU_prop_SC_max, xmin = LU_prop_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean LU proportion in SC (Q10-Q90)", y ="Mean LU proportion in INV_FOR (Q10-Q90)")+
  theme_classic()

##Comparison ratios----
#we are going to use Normalized difference of the land cover proportion between Supervised Classification and synthesis maps



###CORINE----
#differences on land use proportions and its log ratio
comparison_SC_CORINE = comparison_SC_CORINE %>% 
  filter(Year_SC != 1997)%>%#no data for Corine in this year
  mutate(DIFF = LU_prop_CORINE - LU_prop_SC, 
         NORM_DIFF = ( (LU_prop_CORINE - LU_prop_SC)/(LU_prop_CORINE + LU_prop_SC)),
         LOG_RATIO_SC_CORINE = log2(LU_prop_CORINE/LU_prop_SC))

#NaN values means 0/0 normalized differences, so we are substituting them for Os
comparison_SC_CORINE$NORM_DIFF[is.na(comparison_SC_CORINE$NORM_DIFF)] <- 0

#Now we are assessing normalized differences function to different factors:
#SCALE, LAND_USE and PARK
sum_of_diff_SC_CORINE_by_S_LU_P <- comparison_SC_CORINE %>% group_by(Scale, land_use, Park)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE and LAND USE
sum_of_diff_SC_CORINE_by_S_LU <- comparison_SC_CORINE %>% group_by(Scale, land_use)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE
sum_of_diff_SC_CORINE_by_S <- comparison_SC_CORINE %>% group_by(Scale)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            DIFF_CI = ((sd(abs(DIFF))/sqrt(length(DIFF)))*1.96),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            NORMDIFF_CI = ((sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF)))*1.96),
            n = length(DIFF))

sum_of_diff_SC_CORINE_by_S %>% ggplot(aes(Scale, NORMDIFF_mean))+
  geom_point()+
  geom_errorbar(aes(ymax = (NORMDIFF_mean + NORMDIFF_CI), ymin = (NORMDIFF_mean - NORMDIFF_CI)))+
  theme_classic()

#TOTAL: we are going to join the sums by scale and scale and land-use inn to a single table 
sum_of_diff_SC_CORINE_by_S = sum_of_diff_SC_CORINE_by_S %>% mutate(land_use = "Total")

sum_of_diff_SC_CORINE <- rbind(sum_of_diff_SC_CORINE_by_S_LU,sum_of_diff_SC_CORINE_by_S)
write_csv(sum_of_diff_SC_CORINE, "../RIMSEC_general_classification/SC_vs_synt//ratios/sum_differences_SC_CORINE.csv")



###SIOSE----
#differences on land use proportions and its log ratio
comparison_SC_SIOSE = comparison_SC_SIOSE %>%
  mutate(DIFF = LU_prop_SIOSE - LU_prop_SC, 
         NORM_DIFF = ( (LU_prop_SIOSE - LU_prop_SC)/(LU_prop_SIOSE + LU_prop_SC)),
         LOG_RATIO_SC_SIOSE = log2(LU_prop_SIOSE/LU_prop_SC))

#NaN values means 0/0 normalized differences, so we are substituting them for Os
comparison_SC_SIOSE$NORM_DIFF[is.na(comparison_SC_SIOSE$NORM_DIFF)] <- 0

#Now we are assessing normalized differences function to different factors:
#SCALE, LAND_USE and PARK
sum_of_diff_SC_SIOSE_by_S_LU_P <- comparison_SC_SIOSE %>% group_by(Scale, land_use, Park)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE and LAND USE
sum_of_diff_SC_SIOSE_by_S_LU <- comparison_SC_SIOSE %>% group_by(Scale, land_use)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE
sum_of_diff_SC_SIOSE_by_S <- comparison_SC_SIOSE %>% group_by(Scale)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            DIFF_CI = ((sd(abs(DIFF))/sqrt(length(DIFF)))*1.96),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            NORMDIFF_CI = ((sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF)))*1.96),
            n = length(DIFF))

sum_of_diff_SC_SIOSE_by_S %>% ggplot(aes(Scale, NORMDIFF_mean))+
  geom_point()+
  geom_errorbar(aes(ymax = (NORMDIFF_mean + NORMDIFF_CI), ymin = (NORMDIFF_mean - NORMDIFF_CI)))+
  theme_classic()

#TOTAL: we are going to join the sums by scale and scale and land-use inn to a single table 
sum_of_diff_SC_SIOSE_by_S = sum_of_diff_SC_SIOSE_by_S %>% mutate(land_use = "Total")

sum_of_diff_SC_SIOSE <- rbind(sum_of_diff_SC_SIOSE_by_S_LU,sum_of_diff_SC_SIOSE_by_S)
write_csv(sum_of_diff_SC_SIOSE, "../RIMSEC_general_classification/SC_vs_synt//ratios/sum_differences_SC_SIOSE.csv")



###INV_FOR----
#differences on land use proportions and its log ratio
comparison_SC_INV_FOR = comparison_SC_INV_FOR %>%
  mutate(DIFF = LU_prop_INV_FOR - LU_prop_SC, 
         NORM_DIFF = ( (LU_prop_INV_FOR - LU_prop_SC)/(LU_prop_INV_FOR + LU_prop_SC)),
         LOG_RATIO_SC_INV_FOR = log2(LU_prop_INV_FOR/LU_prop_SC))

#NaN values means 0/0 normalized differences, so we are substituting them for Os
comparison_SC_INV_FOR$NORM_DIFF[is.na(comparison_SC_INV_FOR$NORM_DIFF)] <- 0

#Now we are assessing normalized differences function to different factors:
#SCALE, LAND_USE and PARK
sum_of_diff_SC_INV_FOR_by_S_LU_P <- comparison_SC_INV_FOR %>% group_by(Scale, land_use, Park)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE and LAND USE
sum_of_diff_SC_INV_FOR_by_S_LU <- comparison_SC_INV_FOR %>% group_by(Scale, land_use)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            n = length(DIFF))


#SCALE
sum_of_diff_SC_INV_FOR_by_S <- comparison_SC_INV_FOR %>% group_by(Scale)%>%
  summarise(DIFF_mean = mean(abs(DIFF)),
            DIFF_sd = sd(abs(DIFF)),
            DIFF_SE = (sd(abs(DIFF))/sqrt(length(DIFF))),
            DIFF_CI = ((sd(abs(DIFF))/sqrt(length(DIFF)))*1.96),
            NORMDIFF_mean = mean(abs(NORM_DIFF)),
            NORMDIFF_sd = sd(abs(NORM_DIFF)),
            NORMDIFF_SE = (sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF))),
            NORMDIFF_CI = ((sd(abs(NORM_DIFF))/sqrt(length(NORM_DIFF)))*1.96),
            n = length(DIFF))

sum_of_diff_SC_INV_FOR_by_S %>% ggplot(aes(Scale, NORMDIFF_mean))+
  geom_point()+
  geom_errorbar(aes(ymax = (NORMDIFF_mean + NORMDIFF_CI), ymin = (NORMDIFF_mean - NORMDIFF_CI)))+
  theme_classic()

#TOTAL: we are going to join the sums by scale and scale and land-use inn to a single table 
sum_of_diff_SC_INV_FOR_by_S = sum_of_diff_SC_INV_FOR_by_S %>% mutate(land_use = "Total")

sum_of_diff_SC_INV_FOR <- rbind(sum_of_diff_SC_INV_FOR_by_S_LU,sum_of_diff_SC_INV_FOR_by_S)
write_csv(sum_of_diff_SC_INV_FOR, "../RIMSEC_general_classification/SC_vs_synt//ratios/sum_differences_SC_INV_FOR.csv")


#Differences in land cover evolution-----
##Calculate log-ratio of the slopes------
#log((0.1+SYNT_slope)/(0.1+SC_slope)). We sum 0.1 to the values to make them positive and avoid errors when calculating logarithms. Then we are grouping this values by the Scale.

###CORINE----
model_slopes_CORINE =  model_slopes_CORINE %>% mutate(RATIO = ((0.1+CORINE_slope)/(0.1+SC_slope)),
                                        LOG_RATIO = log((0.1+CORINE_slope)/(0.1+SC_slope)),
                                        ABS_LOG_RATIO = abs(log((0.1+CORINE_slope)/(0.1+SC_slope))))

#plot it
model_slopes_CORINE %>%  ggplot(aes(x = land_use, y = LOG_RATIO, colour = land_use))+
  geom_boxplot()+
  facet_grid(factor(Scale, levels = c("Catchment", "Riparian", "Reach"))~.)+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(strip.background = element_blank())+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = "", y = "Log ratio")

#Group LOG_RATIO values by Scale
model_slopes_by_scale <- model_slopes_CORINE %>%group_by(Scale)%>%
  summarise(LOG_RATIO_mean = mean(abs(LOG_RATIO)),
            LOG_RATIO_sd = sd(abs(LOG_RATIO)),
            LOG_RATIO_SE = (sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO))),
            LOG_RATIO_CI = ((sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO)))*1.96))

model_slopes_by_scale_CORINE = model_slopes_by_scale
model_slopes_by_scale_CORINE$Map <- "CORINE"

###SIOSE----
model_slopes_SIOSE =  model_slopes_SIOSE %>% mutate(RATIO = ((0.1+SIOSE_slope)/(0.1+SC_slope)),
                                        LOG_RATIO = log((0.1+SIOSE_slope)/(0.1+SC_slope)),
                                        ABS_LOG_RATIO = abs(log((0.1+SIOSE_slope)/(0.1+SC_slope))))

#plot it
model_slopes_SIOSE %>%  ggplot(aes(x = land_use, y = LOG_RATIO, colour = land_use))+
  geom_boxplot()+
  facet_grid(factor(Scale, levels = c("Catchment", "Riparian", "Reach"))~.)+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(strip.background = element_blank())+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = "", y = "Log ratio")

#Group LOG_RATIO values by Scale
model_slopes_by_scale <- model_slopes_SIOSE %>%group_by(Scale)%>%
  summarise(LOG_RATIO_mean = mean(abs(LOG_RATIO)),
            LOG_RATIO_sd = sd(abs(LOG_RATIO)),
            LOG_RATIO_SE = (sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO))),
            LOG_RATIO_CI = ((sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO)))*1.96))

model_slopes_by_scale_SIOSE = model_slopes_by_scale
model_slopes_by_scale_SIOSE$Map <- "SIOSE"

###INV_FOR----
model_slopes_INV_FOR =  model_slopes_INV_FOR %>% mutate(RATIO = ((0.1+INV_FOR_slope)/(0.1+SC_slope)),
                                        LOG_RATIO = log((0.1+INV_FOR_slope)/(0.1+SC_slope)),
                                        ABS_LOG_RATIO = abs(log((0.1+INV_FOR_slope)/(0.1+SC_slope))))

#plot it
model_slopes_INV_FOR %>%  ggplot(aes(x = land_use, y = LOG_RATIO, colour = land_use))+
  geom_boxplot()+
  facet_grid(factor(Scale, levels = c("Catchment", "Riparian", "Reach"))~.)+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red")+
  theme(strip.background = element_blank())+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = "", y = "Log ratio")

#Group LOG_RATIO values by Scale
model_slopes_by_scale <- model_slopes_INV_FOR %>%group_by(Scale)%>%
  summarise(LOG_RATIO_mean = mean(abs(LOG_RATIO)),
            LOG_RATIO_sd = sd(abs(LOG_RATIO)),
            LOG_RATIO_SE = (sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO))),
            LOG_RATIO_CI = ((sd(abs(LOG_RATIO))/sqrt(length(LOG_RATIO)))*1.96))

model_slopes_by_scale_INV_FOR = model_slopes_by_scale
model_slopes_by_scale_INV_FOR$Map <- "INV_FOR"


#Assess scale dependency----
##Land cover proportion----
#at each of the synthesis maps
#CORINE
anova_CORINE_prop <- aov(abs(NORM_DIFF) ~ Scale, data = comparison_SC_CORINE)
summary(anova_CORINE_prop)

TukeyHSD(anova_CORINE_prop)

tukey_results_CORINE_prop <- TukeyHSD(anova_CORINE_prop)$Scale
tukey_results_CORINE_prop <- as.data.frame(tukey_results_CORINE_prop)
tukey_results_CORINE_prop <- tukey_results_CORINE_prop %>% mutate(Map = "CORINE", 
                                                       Scale = rownames(tukey_results_CORINE_prop))

#SIOSE
anova_SIOSE_prop <- aov(abs(NORM_DIFF) ~ Scale, data = comparison_SC_SIOSE)
summary(anova_SIOSE_prop)

TukeyHSD(anova_SIOSE_prop)

tukey_results_SIOSE_prop <- TukeyHSD(anova_SIOSE_prop)$Scale
tukey_results_SIOSE_prop <- as.data.frame(tukey_results_SIOSE_prop)
tukey_results_SIOSE_prop <- tukey_results_SIOSE_prop %>% mutate(Map = "SIOSE", 
                                                                  Scale = rownames(tukey_results_SIOSE_prop))
#INV_FOR
anova_INV_FOR_prop <- aov(abs(NORM_DIFF) ~ Scale, data = comparison_SC_INV_FOR)
summary(anova_INV_FOR_prop)

TukeyHSD(anova_INV_FOR_prop)

tukey_results_INV_FOR_prop <- TukeyHSD(anova_INV_FOR_prop)$Scale
tukey_results_INV_FOR_prop <- as.data.frame(tukey_results_INV_FOR_prop)
tukey_results_INV_FOR_prop <- tukey_results_INV_FOR_prop %>% mutate(Map = "INV_FOR", 
                                                                  Scale = rownames(tukey_results_INV_FOR_prop))

tukey_results_total_prop <- rbind(tukey_results_CORINE_prop,tukey_results_INV_FOR_prop,tukey_results_SIOSE_prop)
write_csv(tukey_results_total_prop, file = "../RIMSEC_general_classification/scale_dependency/tukey_test_results_prop.csv")
#NO SCALE DEPENDENCY ON THE NORMALIZED DIFFERENCES OBSERVED BETWEEN LAND COVER PROPORTIONS

#join the normalized differences of all maps by Scale, to plot this result later
sum_of_diff_SC_CORINE_by_S$MAP <- "CORINE"
sum_of_diff_SC_CORINE_by_S$value <- "Normalized difference"

sum_of_diff_SC_SIOSE_by_S$MAP <- "SIOSE"
sum_of_diff_SC_SIOSE_by_S$value <- "Normalized difference"

sum_of_diff_SC_INV_FOR_by_S$MAP <- "NFI"
sum_of_diff_SC_INV_FOR_by_S$value <- "Normalized difference"

sum_of_diff = rbind(sum_of_diff_SC_CORINE_by_S,sum_of_diff_SC_SIOSE_by_S,sum_of_diff_SC_INV_FOR_by_S)

##Land cover evolution----
#CORINE
anova_CORINE <- aov(ABS_LOG_RATIO ~ Scale, data = model_slopes_CORINE)
summary(anova_CORINE)

TukeyHSD(anova_CORINE)

tukey_results_CORINE <- TukeyHSD(anova_CORINE)$Scale
tukey_results_CORINE = as.data.frame(tukey_results_CORINE)
tukey_results_CORINE = tukey_results_CORINE %>% mutate(Map = "CORINE", 
                                                       Scale = rownames(tukey_results_CORINE))

#SIOSE
anova_SIOSE <- aov(ABS_LOG_RATIO ~ Scale, data = model_slopes_SIOSE)
summary(anova_SIOSE)

TukeyHSD(anova_SIOSE)

tukey_results_SIOSE <- TukeyHSD(anova_SIOSE)$Scale
tukey_results_SIOSE <- as.data.frame(tukey_results_SIOSE)
tukey_results_SIOSE <- tukey_results_SIOSE %>% mutate(Map = "SIOSE", 
                                                       Scale = rownames(tukey_results_SIOSE))
#INV_FOR
anova_INV_FOR <- aov(ABS_LOG_RATIO ~ Scale, data = model_slopes_INV_FOR)
summary(anova_INV_FOR)

TukeyHSD(anova_INV_FOR)

tukey_results_INV_FOR <- TukeyHSD(anova_INV_FOR)$Scale
tukey_results_INV_FOR = as.data.frame(tukey_results_INV_FOR)
tukey_results_INV_FOR = tukey_results_INV_FOR %>% mutate(Map = "INV_FOR", 
                                                       Scale = rownames(tukey_results_INV_FOR))

tukey_results_total <- rbind(tukey_results_CORINE, tukey_results_INV_FOR, tukey_results_SIOSE)
write_csv(tukey_results_total, file = "../RIMSEC_general_classification/scale_dependency/tukey_test_results.csv")

#join slope ratio together
slope_ratios <-  rbind(model_slopes_by_scale_CORINE, model_slopes_by_scale_SIOSE, model_slopes_by_scale_INV_FOR)

#Assess scale dependency + use of each map----

##Proportions-----
#join the dataframes
normdiff_CORINE <- comparison_SC_CORINE %>% 
  select(Scale, NORM_DIFF)%>%
  mutate(Map = "CORINE")
        
normdiff_SIOSE <- comparison_SC_SIOSE %>% 
  select(Scale, NORM_DIFF)%>%
  mutate(Map = "SIOSE")

normdiff_INV_FOR <- comparison_SC_INV_FOR %>% 
  select(Scale, NORM_DIFF)%>%
  mutate(Map = "INV_FOR")

norm_diff_total <- rbind(normdiff_CORINE, normdiff_SIOSE, normdiff_INV_FOR)

#test ANOVA and Tukey
anova_total_prop <-aov(abs(NORM_DIFF) ~ Scale + Map + Scale*Map, data = norm_diff_total)
summary(anova_total_prop)

TukeyHSD(anova_total_prop)

##Evolution-----
logratio_CORINE <- model_slopes_CORINE %>% 
  select(Scale, ABS_LOG_RATIO)%>%
  mutate(Map = "CORINE")

logratio_SIOSE <- model_slopes_SIOSE %>% 
  select(Scale, ABS_LOG_RATIO)%>%
  mutate(Map = "SIOSE")

logratio_INV_FOR <- model_slopes_INV_FOR %>% 
  select(Scale, ABS_LOG_RATIO)%>%
  mutate(Map = "INV_FOR")

logratio_total <- rbind(logratio_CORINE,logratio_INV_FOR,logratio_SIOSE)

#test ANOVA and Tukey
anova_total_evo <-aov(ABS_LOG_RATIO ~ Scale + Map + Scale*Map, data = logratio_total)
summary(anova_total_evo)

TukeyHSD(anova_total_evo)

#Plots to show the effect-----
#In Land use proportion difference
plot_sum_of_diff <- sum_of_diff %>% mutate(MAP = ifelse(MAP == "CORINE", "a",
                                                 ifelse(MAP == "SIOSE", "b","c"))) %>% ggplot(aes(MAP, NORMDIFF_mean, color = Scale, shape = Scale))+
  geom_point(position =  position_dodge(0.75), size = 8)+
  geom_errorbar(aes(ymax = (NORMDIFF_mean + NORMDIFF_CI), ymin = (NORMDIFF_mean - NORMDIFF_CI)), position =  position_dodge(0.75), width = 0.5, linewidth = 2)+
  scale_y_continuous(name = "Mean normalized difference of LU proportion")+
  scale_x_discrete(labels = c("CORINE","SIOSE","NFI"))+
  scale_color_discrete(labels = c("Catchment", "Riparian", "Reach"))+
  guides(color = "none", shape = "none")+
  theme_classic()+
  theme(axis.text = element_text(size = 20))


#In Land use evolution slope
plot_slope_ratios <- slope_ratios %>% mutate(Map =  ifelse(Map == "CORINE", "a",
                                                    ifelse(Map == "SIOSE", "b","c")))%>%
  ggplot(aes(Map, LOG_RATIO_mean, color = Scale, shape = Scale))+
  geom_point(position =  position_dodge(0.75), size = 8)+
  geom_errorbar(aes(ymax = (LOG_RATIO_mean + LOG_RATIO_CI), ymin = (LOG_RATIO_mean - LOG_RATIO_CI)), position =  position_dodge(0.75), width = 0.5, linewidth = 2)+
  scale_y_continuous(name = "Mean log-ratio of land use slopes")+
  scale_x_discrete(labels = c("CORINE","SIOSE","NFI"))+
  scale_color_discrete(labels = c("Catchment", "Riparian", "Reach"))+
  guides(color = "none", shape = "none")+
  theme_classic()+
  theme(axis.text = element_text(size = 20))

ggpubr::ggarrange(plot_sum_of_diff, plot_slope_ratios, ncol = 2, nrow =1)


