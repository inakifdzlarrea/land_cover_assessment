#In this script we are going to compare land cover proportions between SC and the rest of synthesis maps (INV_FOR, SIOSE and NFI)
#February 2025

#load needed packages
library(tidyverse)
library(readxl)

#load data and homogenize data----
#Supervised classification
SC_LU_props <- read_csv(file = "../RIMSEC_general_classification/metrics/outlet/LU_prop.csv")
SC_LU_props <- SC_LU_props %>%
  mutate(Year = ifelse(Period == "1984_1990", 1987, 
                ifelse(Period == "1990_1995", 1992, 
                ifelse(Period == "1995_2000", 1997,
                ifelse(Period == "2000_2005", 2002,
                ifelse(Period == "2005_2010", 2007,
                ifelse(Period == "2010_2015", 2012,
                ifelse(Period == "2015_2019", 2017,
                ifelse(Period == "2019_2023", 2021, "")))))))),
         Map = "SC")%>%
  select(Park, Scale, Map, Year, Period, land_use, LU_prop)

#Synthesis maps
#load
synt_LU_props <- read_excel( "../comparacion/area_processed_maps/landuse_prop_processed_maps.xlsx")


#Homogenize
#At reach scale we have the info from each sampling point. So we need to calculate the mean land-use proportion of each catchment.
synt_LU_props <- synt_LU_props %>% 
  mutate(Park = substr(PARK, 1,4),
         Scale = str_to_lower(SCALE),
         land_use = str_to_sentence(LAND_USE),
         Map = MAP,
         Year = YEAR,
         Period = ifelse(1984<=YEAR & YEAR<=1989, "1984_1990",
                  ifelse(1990<=YEAR & YEAR<=1994, "1990_1995",
                  ifelse(1995<=YEAR & YEAR<=1999, "1995_2000",
                  ifelse(2000<=YEAR & YEAR<=2004, "2000_2005",
                  ifelse(2005<=YEAR & YEAR<=2009, "2005_2010",
                  ifelse(2010<=YEAR & YEAR<=2014, "2010_2015",
                  ifelse(2015<=YEAR & YEAR<=2018, "2015_2019",
                  ifelse(2019<=YEAR & YEAR<=2023, "2019_2023","")))))))))%>%
  group_by(Park, Scale, Map, Year,Period, land_use)%>%
  summarise(LU_prop = mean(PROP))

#Join them together
all_LU_props <- rbind(SC_LU_props, synt_LU_props)%>%
  filter(land_use != "Unknown" )
#save it
write_csv(all_LU_props, file = "../RIMSEC_general_classification/SC_vs_synt/all_classifications_LU_props.csv")

#make it wider
 comparison_LU_props <- all_LU_props %>%#there're 2 SIOSE available maps for the periods 2005-2010 and 2010-2015 that creates problems for comparison, so calculate LU_prop mean for these two periods.
   select(!Year)%>%
   group_by(Park, Scale, Period, land_use, Map)%>%
   summarise(LU_prop = mean(LU_prop))%>%
   pivot_wider(names_from = Map, values_from = LU_prop)
 
 #We are going to fill the NAs with 0 in the cases were classification MAP is available for that PERIOD but not that LAND_USE at that SCALE. LAND_USE and PERIODS in which the MAP is not available will remain as NAs.
 #Fill gaps in supervised classification
 comparison_LU_props$SC[is.na(comparison_LU_props$SC)]<- 0
 #Fill gaps in INV_FOR classification (only some periods are available and those are which we are going to correct: )
 comparison_LU_props$INV_FOR[is.na(comparison_LU_props$INV_FOR) & (comparison_LU_props$Period == "1990_1995"|
                                                                   comparison_LU_props$Period == "2000_2005"|
                                                                   comparison_LU_props$Period == "2005_2010"|
                                                                   comparison_LU_props$Period == "2010_2015"|
                                                                   comparison_LU_props$Period == "2015_2019")] <- 0
 #Fill gaps in SIOSE classification (only some periods are available and those are which we are going to correct)
 comparison_LU_props$SIOSE[is.na(comparison_LU_props$SIOSE) & 
                               (comparison_LU_props$Period == "2005_2010"|
                                  comparison_LU_props$Period == "2010_2015"|
                                  comparison_LU_props$Period == "2015_2019")] <- 0
 #Fill gaps in INVFOR classification (only some periods are available and those are which we are going to correct)
 comparison_LU_props$INV_FOR[is.na(comparison_LU_props$INV_FOR) & 
                                 (comparison_LU_props$Period == "2005_2010"|
                                  comparison_LU_props$Period == "2010_2015"|
                                  comparison_LU_props$Period == "2015_2019"|
                                  comparison_LU_props$Period == "2019_2023")] <- 0
 
#Save it
write_csv(comparison_LU_props, file = "../RIMSEC_general_classification/SC_vs_synt/LU_prop_comparison.csv")

#Comparison in land use proportions-----
comparison_LU_props <- read_csv( file = "../RIMSEC_general_classification/SC_vs_synt/LU_prop_comparison.csv")

#test deviance from the 1:1 slope-----
#define the function to test the difference

slope_comp <- function(model, null_model, datos) {
  SSR_fixed <- sum(residuals(null_model)^2)
  SSR_model <- sum(residuals(model)^2)
  n <- nrow(datos)
  p <- 2
  
  Slope <- model$coefficients[2]
  Intercept <- model$coefficients[1]
  F_stat <- ((SSR_fixed - SSR_model) / p) / (SSR_model / (n - p))
  p_valor <- pf(F_stat, df1 = p, df2 = n - p, lower.tail = FALSE)
  
  return(data.frame(Slope = Slope, Intercept = Intercept,F_stat = F_stat, p_valor = p_valor))
}

#apply the function to each map and scale
param_grid <- expand_grid(
  Scale = unique(comparison_LU_props$Scale),
  Map = c("CORINE", "SIOSE", "INV_FOR")
)

process_combination <- function(Scale, Map) {
  filtered_data <- comparison_LU_props %>%
    filter(!is.na(.data[[Map]]),
           land_use != "Unknown",
           Scale == !!Scale)  
  
  null_model <- lm(filtered_data[[Map]] ~ 0 + offset(SC), data = filtered_data)
  real_model <- lm(filtered_data[[Map]] ~ SC, data = filtered_data)
  
  slope_comp(real_model, null_model, filtered_data) %>% 
    mutate(Scale = Scale, Map = Map, .before = 1)
}

resultados <- param_grid %>% 
  pmap_dfr(process_combination)

#save a csv
write_csv(resultados, file = "../RIMSEC_general_classification/SC_vs_synt/LC_prop_F_stats.csv")


#PLOTS----
##Option 1 plots----
#here we grouped the points by Scale (that will be represented by shape) and Land cover (represented by colors). We calculated the mean values and sd (as error bars at both axis). Linear models were plotted in black and the linetype represent each scale.

####CORINE----
#plot
summary_data <- comparison_LU_props %>%
  filter(!(is.na(CORINE)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_CORINE = mean(CORINE*100, na.rm = TRUE),
    sd_CORINE = sd(CORINE*100, na.rm = TRUE)
  )

plot1_prop_SC_CORINE_lm <-comparison_LU_props %>%
  filter(!(is.na(CORINE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = CORINE*100)) +
  geom_smooth(aes(linetype = Scale), 
              method = "lm", alpha = 0.2, color = "black") +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = land_use),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE,
        ymin = mean_CORINE - sd_CORINE,
        ymax = mean_CORINE + sd_CORINE,
        color = land_use),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE, 
        shape = Scale, 
        color = land_use),
    size = 3
  ) +
  labs(x = "",
       y ="Relative Land Cover in SPM (mean ± SD)",
       title = "CORINE-SC") +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "a)",
           size = 10, hjust = -0.1, vjust = 1.1, )




####SIOSE----
summary_data <- comparison_LU_props %>%
  filter(!(is.na(SIOSE)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_SIOSE = mean(SIOSE*100, na.rm = TRUE),
    sd_SIOSE = sd(SIOSE*100, na.rm = TRUE)
  )

plot1_prop_SC_SIOSE_lm <-comparison_LU_props %>%
  filter(!(is.na(SIOSE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = SIOSE*100)) +
  geom_smooth(aes(linetype = Scale), 
              method = "lm", alpha = 0.2, color = "black") +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = land_use),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE,
        ymin = mean_SIOSE - sd_SIOSE,
        ymax = mean_SIOSE + sd_SIOSE,
        color = land_use),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE, 
        shape = Scale, 
        color = land_use),
    size = 3
  ) +
  labs(x = "",
       y ="",
       title = "SIOSE-SC") +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "b)",
           size = 10, hjust = -0.1, vjust = 1.1, )



####INV_FOR-----
summary_data <- comparison_LU_props %>%
  filter(!(is.na(INV_FOR)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_INV_FOR = mean(INV_FOR*100, na.rm = TRUE),
    sd_INV_FOR = sd(INV_FOR*100, na.rm = TRUE)
  )

plot1_prop_SC_INV_FOR_lm <-comparison_LU_props %>%
  filter(!(is.na(INV_FOR)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = INV_FOR*100)) +
  geom_smooth(aes(linetype = Scale), 
              method = "lm", alpha = 0.2, color = "black") +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = land_use),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR,
        ymin = mean_INV_FOR - sd_INV_FOR,
        ymax = mean_INV_FOR + sd_INV_FOR,
        color = land_use),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR, 
        shape = Scale, 
        color = land_use),
    size = 3
  ) +
  labs(x = "",
       y ="",
       title = "NFI-SC") +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "c)",
           size = 10, hjust = -0.1, vjust = 1.1, )




##
option1_plots_prop <- gridExtra::grid.arrange(plot1_prop_SC_CORINE_lm, plot1_prop_SC_SIOSE_lm, plot1_prop_SC_INV_FOR_lm, ncol = 3)

##Option 2 plots----
#here we grouped the points by Scale (that will be represented by shape and color) and Land cover (no representation). We calculated the mean values and sd (as error bars at both axis). Linear models were coloured by Scale.

####CORINE----
#plot
summary_data <- comparison_LU_props %>%
  filter(!(is.na(CORINE)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_CORINE = mean(CORINE*100, na.rm = TRUE),
    sd_CORINE = sd(CORINE*100, na.rm = TRUE)
  )

plot2_prop_SC_CORINE_lm <-comparison_LU_props %>%
  filter(!(is.na(CORINE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = CORINE*100)) +
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = Scale),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE,
        ymin = mean_CORINE - sd_CORINE,
        ymax = mean_CORINE + sd_CORINE,
        color = Scale),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_CORINE, 
        shape = Scale, 
        color = Scale),
    size = 3
  ) +
  labs(x = "",
       y ="Relative Land Cover in SPM (mean ± SD)",
       title = "CORINE-SC") +
  geom_abline (slope=1, linetype = "dashed", color="black")+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "a)",
           size = 10, hjust = -0.1, vjust = 1.1, )


####SIOSE----
summary_data <- comparison_LU_props %>%
  filter(!(is.na(SIOSE)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_SIOSE = mean(SIOSE*100, na.rm = TRUE),
    sd_SIOSE = sd(SIOSE*100, na.rm = TRUE)
  )

plot2_prop_SC_SIOSE_lm <-comparison_LU_props %>%
  filter(!(is.na(SIOSE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = SIOSE*100)) +
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = Scale),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE,
        ymin = mean_SIOSE - sd_SIOSE,
        ymax = mean_SIOSE + sd_SIOSE,
        color = Scale),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_SIOSE, 
        shape = Scale, 
        color = Scale),
    size = 3
  ) +
  labs(x = "",
       y ="",
       title = "SIOSE-SC") +
  geom_abline (slope=1, linetype = "dashed", color="black")+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "b)",
           size = 10, hjust = -0.1, vjust = 1.1, )


####INV_FOR-----
summary_data <- comparison_LU_props %>%
  filter(!(is.na(INV_FOR)))%>%
  filter(land_use != "Unknown")%>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC*100, na.rm = TRUE),
    sd_SC = sd(SC*100, na.rm = TRUE),
    mean_INV_FOR = mean(INV_FOR*100, na.rm = TRUE),
    sd_INV_FOR = sd(INV_FOR*100, na.rm = TRUE)
  )

plot2_prop_SC_INV_FOR_lm <-comparison_LU_props %>%
  filter(!(is.na(INV_FOR)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = INV_FOR*100)) +
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  geom_errorbarh(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR,
        xmin = mean_SC - sd_SC,
        xmax = mean_SC + sd_SC,
        color = Scale),
    height = 0
  ) +
  geom_errorbar(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR,
        ymin = mean_INV_FOR - sd_INV_FOR,
        ymax = mean_INV_FOR + sd_INV_FOR,
        color = Scale),
    width = 0
  ) +
  geom_point(
    data = summary_data,
    aes(x = mean_SC, y = mean_INV_FOR, 
        shape = Scale, 
        color = Scale),
    size = 3
  ) +
  labs(x = "",
       y ="",
       title = "NFI-SC") +
  geom_abline (slope=1, linetype = "dashed", color="black")+
  theme_bw()+
  xlim(0,100)+
  ylim(0,100 )+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "c)",
           size = 10, hjust = -0.1, vjust = 1.1, )

##
option2_plots_prop <- gridExtra::grid.arrange(plot2_prop_SC_CORINE_lm, plot2_prop_SC_SIOSE_lm, plot2_prop_SC_INV_FOR_lm, ncol = 3)

##Option 3 plots----
#here we didn´t group the points, so all points are plotted and coloured by Scale.Linear models were coloured by Scale.

####CORINE----
plot3_prop_SC_CORINE_lm <-comparison_LU_props %>%
  filter(!(is.na(CORINE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = CORINE*100)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  labs(x = "",
       y ="Relative Land Cover in SPM",
       title = "CORINE-SC") +
  theme_bw()+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "a)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####SIOSE----
plot3_prop_SC_SIOSE_lm <-comparison_LU_props %>%
  filter(!(is.na(SIOSE)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = SIOSE*100)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  labs(x = "",
       y ="",
       title = "SIOSE-SC") +
  theme_bw()+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "b)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####INV_FOR----
plot3_prop_SC_INV_FOR_lm <-comparison_LU_props %>%
  filter(!(is.na(INV_FOR)))%>%
  filter(land_use != "Unknown")%>%
  ggplot(aes(x = SC*100, y = INV_FOR*100)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  labs(x = "",
       y ="",
       title = "NFI-SC") +
  theme_bw()+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 15)
  )+
  annotate(geom = "text", x = -Inf, y = Inf, label = "c)",
           size = 10, hjust = -0.1, vjust = 1.1, )


##
option3_plots_prop <- gridExtra::grid.arrange(plot3_prop_SC_CORINE_lm, plot3_prop_SC_SIOSE_lm, plot3_prop_SC_INV_FOR_lm, ncol = 3)

