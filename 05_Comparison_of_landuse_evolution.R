#In this script we are going to compare land cover evolution between SC and the rest of synthesis maps (CORINE, SIOSE and NFI)
#February 2025

#load needed packages
library(tidyverse)
library(readxl)

#Load and prepare data----
#data of SC classification
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
         Map = "SC",
         Year_evo = as.numeric(Year) - min(as.numeric(Year)))%>%
  select(Park, Scale, Map, Year, Year_evo, Period, land_use, LU_prop)

#we need a column describing the year since the surveillance started
#create a dataframe with all possible combinations
c <- expand_grid(Park = unique(SC_LU_props$Park),
                 Year = unique(SC_LU_props$Year),
                 land_use = unique(SC_LU_props$land_use),
                 LU_prop = "",
                 Map = "", 
                 Scale = unique(SC_LU_props$Scale),
                 Year_evo = "")

SC_LU_props <- SC_LU_props %>% full_join(c, LU_props_SC, by = c("Park", "Year", "land_use", "Scale"))%>% 
  mutate(Year_evo = as.numeric(Year) -  min(as.numeric(Year)),
         LU_prop = ifelse(!is.na(LU_prop.x),LU_prop.x, 0 ))%>%
  select(Park, Year, Year_evo, land_use, Scale, LU_prop)

#load land cover proportions of all classifications for all periods
all_LU_props <- read_csv("../RIMSEC_general_classification/SC_vs_synt/all_classifications_LU_props.csv")

#Build the linear models for SC----
#estimate the trend of each land use a t each park as the slope of the linear model relating land use proportion and year since the start of the surveillance
LU_evo_models_SC<- list()

for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      SC_LU_props %>% filter(Scale == unique(SC_LU_props$Scale)[i])%>%
        filter(land_use == unique(SC_LU_props$land_use)[j])%>%
        filter(Park == unique(SC_LU_props$Park)[k])%>%
        lm(LU_prop ~ Year_evo,.) -> LU_evo_models_SC[[unique(SC_LU_props$Scale)[i]]][[unique(SC_LU_props$land_use)[j]]][[unique(SC_LU_props$Park)[k]]]
    }}}  

#write the slopes of the models in a table
#Then we are creating a data frame with the slopes of the models and its statistical significance (p-value), the most recent land use proportion

#define the function to extract p-values
overall_p <- function(my_model) {
  f <- summary(my_model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

LU_slopes_SC <- tibble(
  land_use = character(0),
  Scale = character(0),
  Park = character(0),
  slope = numeric(0),
  sign = numeric(0),
  prop_1984 = numeric(0),
  prop_2022 = numeric(0))


x <- tibble(
  land_use = character(1),
  Scale = character(1),
  Park = character(1),
  slope = numeric(1),
  sign = numeric(1),
  prop_1984 = numeric(1),
  prop_2022 = numeric(1)
)

#paste the values of the slopes into the empty tibble
for (i in 1:3){#for each Scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each Park
      x$Scale[] <- unique(SC_LU_props$Scale)[i]
      x$land_use[] <- unique(SC_LU_props$land_use)[j]
      x$Park[] <- unique(SC_LU_props$Park)[k]
      
      LU_evo_models_SC[[unique(SC_LU_props$Scale)[i]]][[unique(SC_LU_props$land_use)[j]]][[unique(SC_LU_props$Park)[k]]][["coefficients"]][[2]] -> x$slope[]
      
      overall_p(LU_evo_models_SC[[unique(SC_LU_props$Scale)[i]]][[unique(SC_LU_props$land_use)[j]]][[unique(SC_LU_props$Park)[k]]]) -> x$sign[]
      
      x$prop_1984[] <- SC_LU_props %>% filter(Scale == unique(SC_LU_props$Scale)[i] &
                                                land_use == unique(SC_LU_props$land_use)[j]&
                                                Park == unique(SC_LU_props$Park)[k]&
                                                Year == "1987")%>% select(LU_prop)%>%as.numeric()
      
      x$prop_2022[] <- SC_LU_props %>% filter(Scale == unique(SC_LU_props$Scale)[i] &
                                                land_use == unique(SC_LU_props$land_use)[j]&
                                                Park == unique(SC_LU_props$Park)[k]&
                                                Year == "2021")%>% select(LU_prop)%>%as.numeric()
      
      LU_slopes_SC <- rbind(LU_slopes_SC, x)
    }}}

LU_slopes_SC = LU_slopes_SC %>% mutate(tend = ifelse(sign > 0.05, "=", 
                                                     ifelse(slope > 0, "+","-")))
LU_slopes_SC$tend[is.na(LU_slopes_SC$tend)]<- "="

LU_slopes_SC = LU_slopes_SC %>% 
  mutate(prop_tend = paste0(as.character(round(prop_2022*100, digits = 2))," ", tend),
         sign_slope = as.numeric(ifelse(tend != "=", slope, "")),
         prop_84_22_slope = paste0(as.character(round(prop_1984*100, digits = 2)),"-", as.character(round(prop_2022*100, digits = 2))," ", as.character(round(sign_slope*100, digits = 5))))

write_csv(LU_slopes_SC, file = "../RIMSEC_general_classification/metrics/outlet/LU_slopes_SC.csv")

###Plot linear models----
SC_LU_props <- SC_LU_props %>%
  left_join(LU_slopes_SC %>% select(Scale,land_use, Park, sign, slope), by = c("Scale", "land_use", "Park"))%>% 
  mutate(signif = ifelse(sign < 0.05 & slope > 0, "Significantly positive",
                  ifelse(sign < 0.05 & slope < 0, "Significantly negative","Not significant")))%>%
  mutate(signif = ifelse(is.na(signif), "Not significant", signif))

ggplot(SC_LU_props, aes(Year_evo, LU_prop*100, group = Scale, colour = signif, shape = Scale)) +
  geom_point(size = 7) +
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 2, alpha = 0.3) + 
  scale_color_manual(values = c("Significantly positive" = "darkgreen","Significantly negative" = "red", "Not significant" = "grey")) +
  theme_classic()+
  theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.background = element_blank(),  # Eliminar fondo del panel
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(size = 25),  # Tamaño de fuente para las etiquetas del eje X
        axis.text.y = element_text(size = 25),
        strip.text.x = element_text(size = 30),  # Tiras horizontales (filas)
        strip.text.y = element_text(size = 30))+
  scale_x_continuous(
    breaks = c(0,10,20,30),
    labels = c("1987", "1997", "2007", "2017") 
  ) +
  facet_grid(rows = vars(land_use), cols = vars(Park), scales = "free_y")


#Comparison to synthesis maps-----
c <- expand_grid(Park = unique(all_LU_props$Park),
                 Period = unique(all_LU_props$Period),
                 land_use = unique(all_LU_props$land_use),
                 LU_prop = "",
                 Scale = unique(all_LU_props$Scale),
                 Map = unique(all_LU_props$Map))

all_LU_props <- all_LU_props %>% full_join(c, all_props_SC, by = c("Park", "Period", "land_use", "Scale", "Map"))%>% 
  mutate(LU_prop = ifelse(!is.na(LU_prop.x),LU_prop.x, 0 ))%>%
  select(Park, Period, Year, Map, land_use, Scale, LU_prop)

##Split data for each of synthesis maps-----
###SC_vs_CORINE----
x <- all_LU_props %>% filter(Map %in% c("SC", "CORINE") & Period %in% c(c("1990_1995","1995_2000","2000_2005","2005_2010","2010_2015","2015_2019"))) %>% 
  pivot_wider(names_from = Map, values_from = c(LU_prop, Year))
  
comp_SC_CORINE <- x %>% mutate(
  LU_prop_CORINE = ifelse(is.na(LU_prop_CORINE) & Period %in% c("1990_1995","2000_2005","2005_2010","2010_2015","2015_2019"), 0, LU_prop_CORINE),  # Sustite NAs for 0 in LU_prop in case there is CORINE map in that Period
  Year_CORINE = ifelse(is.na(Year_CORINE) & Period  == "1990_1995", 1990,
                ifelse(is.na(Year_CORINE) & Period  == "2000_2005", 2000,
                ifelse(is.na(Year_CORINE) & Period  == "2005_2010", 2006,
                ifelse(is.na(Year_CORINE) & Period  == "2010_2015", 2012,
                ifelse(is.na(Year_CORINE) & Period  == "2015_2019", 2018, Year_CORINE))))),
  LU_prop_SC = ifelse(is.na(LU_prop_SC), 0, LU_prop_SC),
  Year_SC = ifelse(Period  == "1990_1995", 1992,
            ifelse(Period  == "1995_2000", 1997,
            ifelse(Period  == "2000_2005", 2002,
            ifelse(Period  == "2005_2010", 2007,
            ifelse(Period  == "2010_2015", 2012,
            ifelse(Period  == "2015_2019", 2017, 
            ifelse(Period  == "2019_2023", 2021, Year_SC)))))))) %>%
  mutate(Year_evo_SC = Year_SC - min(Year_SC),
         Year_evo_CORINE = Year_CORINE - min(Year_CORINE, na.rm = T))

#save it as a csv
write_csv(comp_SC_CORINE, file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_CORINE_evo.csv")

###SC_vs_SIOSE----
x <- all_LU_props %>% filter(Map %in% c("SC", "SIOSE") & Period %in% c("2005_2010","2010_2015","2015_2019")) %>% 
  pivot_wider(names_from = Map, values_from = c(LU_prop, Year))

x = x %>% unnest_longer(col = c( LU_prop_SIOSE, Year_SIOSE))
x = x %>%  mutate(across(where(is.list), ~ map(.x, ~ if(length(.x) == 0) NA else .x[[1]]))) %>% 
  mutate(across(where(is.list), unlist))%>%
  mutate(Period_Year_SIOSE = ifelse(is.na(Year_SIOSE),NA, paste0(Period, "_", Year_SIOSE)))

c_siose <- expand.grid(Park = unique(x$Park),
                       land_use = unique(x$land_use),
                       LU_prop_SIOSE = "",
                       LU_prop_SC = "",
                       Scale = unique(x$Scale),
                       Period_Year_SIOSE = unique(x$Period_Year_SIOSE)[!is.na(unique(x$Period_Year_SIOSE))],
                       Year_SC = "")%>%
  mutate(Period = str_sub(Period_Year_SIOSE, 1,9),
         Year_SIOSE = as.numeric(str_sub(Period_Year_SIOSE, 11,14)))%>%
  select(!Period_Year_SIOSE)


x <- x %>% select(!Period_Year_SIOSE)%>%
  right_join(c_siose, x, by = c("Park", "Period","Year_SIOSE", "land_use", "Scale"))%>%
  select(Park,Period,land_use,Scale, LU_prop_SC = LU_prop_SC.x, LU_prop_SIOSE = LU_prop_SIOSE.x, Year_SC = Year_SC.x, Year_SIOSE)
  
  

comp_SC_SIOSE <- x %>% mutate(
  LU_prop_SC = ifelse(is.na(LU_prop_SC), 0, LU_prop_SC),
  LU_prop_SIOSE = ifelse(is.na(LU_prop_SIOSE), 0, LU_prop_SIOSE),
  Year_SC = ifelse(Period  == "1990_1995", 1992,
            ifelse(Period  == "1995_2000", 1997,
            ifelse(Period  == "2000_2005", 2002,
            ifelse(Period  == "2005_2010", 2007,
            ifelse(Period  == "2010_2015", 2012,
            ifelse(Period  == "2015_2019", 2017, 
            ifelse(Period  == "2019_2023", 2021, Year_SC)))))))) %>%
  mutate(Year_evo_SC = Year_SC - min(Year_SC),
         Year_evo_SIOSE = Year_SIOSE - min(Year_SIOSE, na.rm = T))

#save it as a csv
write_csv(comp_SC_SIOSE, file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_SIOSE_evo.csv")

###SC_vs_INV_FOR----
x <- all_LU_props %>% filter(Map %in% c("SC", "INV_FOR") & Period %in% c("2005_2010","2010_2015","2015_2019","2019_2023")) %>% 
  pivot_wider(names_from = Map, values_from = c(LU_prop, Year))

comp_SC_INV_FOR <- x %>% mutate(
  LU_prop_INV_FOR = ifelse(is.na(LU_prop_INV_FOR) & Period %in% c("2005_2010","2010_2015","2015_2019", "2019_2023"), 0, LU_prop_INV_FOR),  # Sustite NAs for 0 in LU_prop in case there is INV_FOR map in that Period
  Year_INV_FOR =ifelse(is.na(Year_INV_FOR) & Period  == "2005_2010", 2006,
                ifelse(is.na(Year_INV_FOR) & Period  == "2010_2015", 2010,
                ifelse(is.na(Year_INV_FOR) & Period  == "2015_2019", 2016,
                ifelse(is.na(Year_INV_FOR) & Period  == "2019_2023", 2022,Year_INV_FOR)))),
  LU_prop_SC = ifelse(is.na(LU_prop_SC), 0, LU_prop_SC),
  Year_SC = ifelse(Period  == "1990_1995", 1992,
            ifelse(Period  == "1995_2000", 1997,
            ifelse(Period  == "2000_2005", 2002,
            ifelse(Period  == "2005_2010", 2007,
            ifelse(Period  == "2010_2015", 2012,
            ifelse(Period  == "2015_2019", 2017, 
            ifelse(Period  == "2019_2023", 2021, Year_SC)))))))) %>%
  mutate(Year_evo_SC = Year_SC - min(Year_SC),
         Year_evo_INV_FOR = Year_INV_FOR - min(Year_INV_FOR, na.rm = T))

#save it as a csv
write_csv(comp_SC_INV_FOR, file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_INV_FOR_evo.csv")

##Model building for comparison----
#load comparison tables
comparison_SC_CORINE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_CORINE_evo.csv")
comparison_SC_SIOSE <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_SIOSE_evo.csv")
comparison_SC_INV_FOR <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/comp_SC_INV_FOR_evo.csv")

###CORINE----
#Create a list we we are going to save the models 
LU_evo_models_CORINE <- list() 
#And create the models
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      #supervised classification: GEE
      comparison_SC_CORINE %>% filter(Scale == unique(comparison_SC_CORINE$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_CORINE$land_use)[j])%>%
        filter(Park == unique(comparison_SC_CORINE$Park)[k])%>%
        lm(LU_prop_SC ~ Year_evo_SC,.) -> LU_evo_models_CORINE[[unique(comparison_SC_CORINE$Scale)[i]]][[unique(comparison_SC_CORINE$land_use)[j]]][["SC"]] [[unique(comparison_SC_CORINE$Park)[k]]]
      #CORINE
      comparison_SC_CORINE %>% filter(Scale== unique(comparison_SC_CORINE$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_CORINE$land_use)[j])%>%
        filter(Park == unique(comparison_SC_CORINE$Park)[k])%>%
        lm(LU_prop_CORINE ~ Year_evo_CORINE,.) -> LU_evo_models_CORINE[[unique(comparison_SC_CORINE$Scale)[i]]][[unique(comparison_SC_CORINE$land_use)[j]]][["CORINE"]][[unique(comparison_SC_CORINE$Park)[k]]]
    }}}

#write the slopes of the models in a table
#Then we are creating a data frame with the slopes of the models
model_slopes_CORINE <- tibble(
  land_use = character(0),
  Scale = character(0),
  Park = character(0),
  SC_slope = numeric(0),
  CORINE_slope = numeric(0))


x <- tibble(
  land_use = character(1),
  Scale = character(1),
  Park= character(1),
  SC_slope = numeric(1),
  CORINE_slope = numeric(1)
)


#paste the values of the slopes into the empty tibble
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      x$Scale[] <- unique(comparison_SC_CORINE$Scale)[i]
      x$land_use[] <- unique(comparison_SC_CORINE$land_use)[j]
      x$Park[] <- unique(comparison_SC_CORINE$Park)[k]
      LU_evo_models_CORINE[[unique(comparison_SC_CORINE$Scale)[i]]][[unique(comparison_SC_CORINE$land_use)[j]]][["SC"]] [[unique(comparison_SC_CORINE$Park)[k]]][["coefficients"]][[2]] -> x$SC_slope[]
      LU_evo_models_CORINE[[unique(comparison_SC_CORINE$Scale)[i]]][[unique(comparison_SC_CORINE$land_use)[j]]][["CORINE"]] [[unique(comparison_SC_CORINE$Park)[k]]][["coefficients"]][[2]] -> x$CORINE_slope[]
      
      model_slopes_CORINE <- rbind(model_slopes_CORINE, x)
    }}}

model_slopes_CORINE[is.na(model_slopes_CORINE)]<- 0

#save it as a csv
saveRDS(LU_evo_models_CORINE , file = "../RIMSEC_general_classification/SC_vs_synt/evolution_models/LU_evo_models_CORINE.RDS")
write_csv(model_slopes_CORINE, file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_CORINE.csv")

###SIOSE----
#Create a list we we are going to save the models 
LU_evo_models_SIOSE <- list() 
#And create the models
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      #supervised classification: GEE
      comparison_SC_SIOSE %>% filter(Scale == unique(comparison_SC_SIOSE$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_SIOSE$land_use)[j])%>%
        filter(Park == unique(comparison_SC_SIOSE$Park)[k])%>%
        lm(LU_prop_SC ~ Year_evo_SC,.) -> LU_evo_models_SIOSE[[unique(comparison_SC_SIOSE$Scale)[i]]][[unique(comparison_SC_SIOSE$land_use)[j]]][["SC"]] [[unique(comparison_SC_SIOSE$Park)[k]]]
      #SIOSE
      comparison_SC_SIOSE %>% filter(Scale== unique(comparison_SC_SIOSE$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_SIOSE$land_use)[j])%>%
        filter(Park == unique(comparison_SC_SIOSE$Park)[k])%>%
        lm(LU_prop_SIOSE ~ Year_evo_SIOSE,.) -> LU_evo_models_SIOSE[[unique(comparison_SC_SIOSE$Scale)[i]]][[unique(comparison_SC_SIOSE$land_use)[j]]][["SIOSE"]][[unique(comparison_SC_SIOSE$Park)[k]]]
    }}}

#write the slopes of the models in a table
#Then we are creating a data frame with the slopes of the models
model_slopes_SIOSE <- tibble(
  land_use = character(0),
  Scale = character(0),
  Park = character(0),
  SC_slope = numeric(0),
  SIOSE_slope = numeric(0))


x <- tibble(
  land_use = character(1),
  Scale = character(1),
  Park= character(1),
  SC_slope = numeric(1),
  SIOSE_slope = numeric(1)
)


#paste the values of the slopes into the empty tibble
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      x$Scale[] <- unique(comparison_SC_SIOSE$Scale)[i]
      x$land_use[] <- unique(comparison_SC_SIOSE$land_use)[j]
      x$Park[] <- unique(comparison_SC_SIOSE$Park)[k]
      LU_evo_models_SIOSE[[unique(comparison_SC_SIOSE$Scale)[i]]][[unique(comparison_SC_SIOSE$land_use)[j]]][["SC"]] [[unique(comparison_SC_SIOSE$Park)[k]]][["coefficients"]][[2]] -> x$SC_slope[]
      LU_evo_models_SIOSE[[unique(comparison_SC_SIOSE$Scale)[i]]][[unique(comparison_SC_SIOSE$land_use)[j]]][["SIOSE"]] [[unique(comparison_SC_SIOSE$Park)[k]]][["coefficients"]][[2]] -> x$SIOSE_slope[]
      
      model_slopes_SIOSE <- rbind(model_slopes_SIOSE, x)
    }}}

model_slopes_SIOSE[is.na(model_slopes_SIOSE)]<- 0

#save it as a csv
saveRDS(LU_evo_models_SIOSE , file = "../RIMSEC_general_classification/SC_vs_synt/evolution_models/LU_evo_models_SIOSE.RDS")
write_csv(model_slopes_SIOSE, file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_SIOSE.csv")


###INV_FOR----
#Create a list we we are going to save the models 
LU_evo_models_INV_FOR <- list() 
#And create the models
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      #supervised classification: GEE
      comparison_SC_INV_FOR %>% filter(Scale == unique(comparison_SC_INV_FOR$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_INV_FOR$land_use)[j])%>%
        filter(Park == unique(comparison_SC_INV_FOR$Park)[k])%>%
        lm(LU_prop_SC ~ Year_evo_SC,.) -> LU_evo_models_INV_FOR[[unique(comparison_SC_INV_FOR$Scale)[i]]][[unique(comparison_SC_INV_FOR$land_use)[j]]][["SC"]] [[unique(comparison_SC_INV_FOR$Park)[k]]]
      #INV_FOR
      comparison_SC_INV_FOR %>% filter(Scale== unique(comparison_SC_INV_FOR$Scale)[i])%>%
        filter(land_use == unique(comparison_SC_INV_FOR$land_use)[j])%>%
        filter(Park == unique(comparison_SC_INV_FOR$Park)[k])%>%
        lm(LU_prop_INV_FOR ~ Year_evo_INV_FOR,.) -> LU_evo_models_INV_FOR[[unique(comparison_SC_INV_FOR$Scale)[i]]][[unique(comparison_SC_INV_FOR$land_use)[j]]][["INV_FOR"]][[unique(comparison_SC_INV_FOR$Park)[k]]]
    }}}

#write the slopes of the models in a table
#Then we are creating a data frame with the slopes of the models
model_slopes_INV_FOR <- tibble(
  land_use = character(0),
  Scale = character(0),
  Park = character(0),
  SC_slope = numeric(0),
  INV_FOR_slope = numeric(0))


x <- tibble(
  land_use = character(1),
  Scale = character(1),
  Park= character(1),
  SC_slope = numeric(1),
  INV_FOR_slope = numeric(1)
)


#paste the values of the slopes into the empty tibble
for (i in 1:3){#for each scale
  for(j in 1:8){#for each land-use
    for(k in 1:5){#for each park
      x$Scale[] <- unique(comparison_SC_INV_FOR$Scale)[i]
      x$land_use[] <- unique(comparison_SC_INV_FOR$land_use)[j]
      x$Park[] <- unique(comparison_SC_INV_FOR$Park)[k]
      LU_evo_models_INV_FOR[[unique(comparison_SC_INV_FOR$Scale)[i]]][[unique(comparison_SC_INV_FOR$land_use)[j]]][["SC"]] [[unique(comparison_SC_INV_FOR$Park)[k]]][["coefficients"]][[2]] -> x$SC_slope[]
      LU_evo_models_INV_FOR[[unique(comparison_SC_INV_FOR$Scale)[i]]][[unique(comparison_SC_INV_FOR$land_use)[j]]][["INV_FOR"]] [[unique(comparison_SC_INV_FOR$Park)[k]]][["coefficients"]][[2]] -> x$INV_FOR_slope[]
      
      model_slopes_INV_FOR <- rbind(model_slopes_INV_FOR, x)
    }}}

model_slopes_INV_FOR[is.na(model_slopes_INV_FOR)]<- 0

#save it as a csv
saveRDS(LU_evo_models_INV_FOR , file = "../RIMSEC_general_classification/SC_vs_synt/evolution_models/LU_evo_models_INV_FOR.RDS")
write_csv(model_slopes_INV_FOR, file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_INV_FOR.csv")


#PLOT comparison----
#load the files containing the slopes
model_slopes_CORINE <- read_csv( file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_CORINE.csv")
model_slopes_SIOSE <- read_csv( file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_SIOSE.csv")
model_slopes_INV_FOR <- read_csv( file = "../RIMSEC_general_classification/SC_vs_synt/model_slopes_INV_FOR.csv")

plot_comp_CORINE_evo <- model_slopes_CORINE %>% 
  group_by(land_use, Scale)%>%
  summarise(LU_PROP_SC_mean = mean(SC_slope), 
            LU_PROP_SC_min = quantile(SC_slope, 0.1),
            LU_PROP_SC_max = quantile(SC_slope, 0.9),
            LU_PROP_CORINE_mean = mean(CORINE_slope),
            LU_PROP_CORINE_min = quantile(CORINE_slope, 0.1),
            LU_PROP_CORINE_max = quantile(CORINE_slope, 0.9))%>%
  ggplot(aes(LU_PROP_SC_mean, LU_PROP_CORINE_mean, color = land_use, shape = Scale))+
  geom_point(size = 4)+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
  geom_errorbar(aes(ymax = LU_PROP_CORINE_max, ymin = LU_PROP_CORINE_min))+
  geom_errorbarh(aes(xmax = LU_PROP_SC_max, xmin = LU_PROP_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean slope in SC (Q10-Q90)", y ="Mean slope in CORINE (Q10-Q90)")+
  theme_classic()+
  theme(legend.position = "none",
                       axis.text.x = element_text(size = 25),
                       axis.text.y = element_text(size = 25))


plot_comp_SIOSE_evo <- model_slopes_SIOSE %>% 
  group_by(land_use, Scale)%>%
  summarise(LU_PROP_SC_mean = mean(SC_slope), 
            LU_PROP_SC_min = quantile(SC_slope, 0.1),
            LU_PROP_SC_max = quantile(SC_slope, 0.9),
            LU_PROP_SIOSE_mean = mean(SIOSE_slope),
            LU_PROP_SIOSE_min = quantile(SIOSE_slope, 0.1),
            LU_PROP_SIOSE_max = quantile(SIOSE_slope, 0.9))%>%
  ggplot(aes(LU_PROP_SC_mean, LU_PROP_SIOSE_mean, color = land_use, shape = Scale))+
  geom_point(size = 4)+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
  geom_errorbar(aes(ymax = LU_PROP_SIOSE_max, ymin = LU_PROP_SIOSE_min))+
  geom_errorbarh(aes(xmax = LU_PROP_SC_max, xmin = LU_PROP_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean slope in SC (Q10-Q90)", y ="Mean slope in SIOSE (Q10-Q90)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))


plot_comp_INV_FOR_evo <- model_slopes_INV_FOR %>% 
  group_by(land_use, Scale)%>%
  summarise(LU_PROP_SC_mean = mean(SC_slope), 
            LU_PROP_SC_min = quantile(SC_slope, 0.1),
            LU_PROP_SC_max = quantile(SC_slope, 0.9),
            LU_PROP_INV_FOR_mean = mean(INV_FOR_slope),
            LU_PROP_INV_FOR_min = quantile(INV_FOR_slope, 0.1),
            LU_PROP_INV_FOR_max = quantile(INV_FOR_slope, 0.9))%>%
  ggplot(aes(LU_PROP_SC_mean, LU_PROP_INV_FOR_mean, color = land_use, shape = Scale))+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax = LU_PROP_INV_FOR_max, ymin = LU_PROP_INV_FOR_min))+
  geom_errorbarh(aes(xmax = LU_PROP_SC_max, xmin = LU_PROP_SC_min))+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  labs(x = "Mean slope in SC (Q10-Q90)", y ="Mean slope in NFI (Q10-Q90)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))

#plot altogether proportion and evolution comparison plots
gridExtra::grid.arrange(plot_comp_CORINE, plot_comp_SIOSE, plot_comp_INV_FOR,
                        plot_comp_CORINE_evo, plot_comp_SIOSE_evo, plot_comp_INV_FOR_evo,
                        ncol = 3, nrow = 2)

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
  
  return(data.frame(Slope = Slope, Intercept = Intercept, F_stat = F_stat, p_valor = p_valor))
}

#apply the function to each map and scale
param_grid <- expand_grid(
  Scale = unique(model_slopes_CORINE$Scale)
)

process_combination <- function(Scale) {
  filtered_data <- model_slopes_CORINE %>%
    filter(land_use != "Unknown",
           Scale == !!Scale)  
  
  null_model <- lm(CORINE_slope ~ 0 + offset(SC_slope), data = filtered_data)
  real_model <- lm(CORINE_slope ~ SC_slope, data = filtered_data)
  
  slope_comp(real_model, null_model, filtered_data) %>% 
    mutate(Scale = Scale, .before = 1)
}

resultados_CORINE <- param_grid %>% 
  pmap_dfr(process_combination)%>%
  mutate(Map = "CORINE")

process_combination <- function(Scale) {
  filtered_data <- model_slopes_SIOSE %>%
    filter(land_use != "Unknown",
           Scale == !!Scale)  
  
  null_model <- lm(SIOSE_slope ~ 0 + offset(SC_slope), data = filtered_data)
  real_model <- lm(SIOSE_slope ~ SC_slope, data = filtered_data)
  
  slope_comp(real_model, null_model, filtered_data) %>% 
    mutate(Scale = Scale, .before = 1)
}

resultados_SIOSE <- param_grid %>% 
  pmap_dfr(process_combination)%>%
  mutate(Map = "SIOSE")

process_combination <- function(Scale) {
  filtered_data <- model_slopes_INV_FOR %>%
    filter(land_use != "Unknown",
           Scale == !!Scale)  
  
  null_model <- lm(INV_FOR_slope ~ 0 + offset(SC_slope), data = filtered_data)
  real_model <- lm(INV_FOR_slope ~ SC_slope, data = filtered_data)
  
  slope_comp(real_model, null_model, filtered_data) %>% 
    mutate(Scale = Scale, .before = 1)
}

resultados_INV_FOR <- param_grid %>% 
  pmap_dfr(process_combination) %>%
  mutate(Map = "INV_FOR")

#save a csv
resultados_evo <- rbind(resultados_CORINE, resultados_SIOSE, resultados_INV_FOR)
write_csv(resultados_evo, file = "../RIMSEC_general_classification/SC_vs_synt/LC_evo_F_stats.csv")


#PLOTS----
##Option 1 plots----
#here we grouped the points by Scale (that will be represented by shape) and Land cover (represented by colors). We calculated the mean values and sd (as error bars at both axis). Linear models were plotted in black and the linetype represent each scale.

####CORINE----
#plot
summary_data <- model_slopes_CORINE %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_CORINE = mean(CORINE_slope, na.rm = TRUE),
    sd_CORINE = sd(CORINE_slope, na.rm = TRUE)
  )

plot1_evo_SC_CORINE_lm <- model_slopes_CORINE %>%
  ggplot(aes(x = SC_slope, y = CORINE_slope)) +
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
       y ="Land Cover Slope in SPM (mean ± SD)") +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "d)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####SIOSE----
#plot
summary_data <- model_slopes_SIOSE %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_SIOSE = mean(SIOSE_slope, na.rm = TRUE),
    sd_SIOSE = sd(SIOSE_slope, na.rm = TRUE)
  )

plot1_evo_SC_SIOSE_lm <- model_slopes_SIOSE %>%
  ggplot(aes(x = SC_slope, y = SIOSE_slope)) +
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
    size = 3)+
  labs(x = "",
         y ="") +
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "e)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####INV_FOR----
#plot
summary_data <- model_slopes_INV_FOR %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_INV_FOR = mean(INV_FOR_slope, na.rm = TRUE),
    sd_INV_FOR = sd(INV_FOR_slope, na.rm = TRUE)
  )

plot1_evo_SC_INV_FOR_lm <- model_slopes_INV_FOR %>%
  ggplot(aes(x = SC_slope, y = INV_FOR_slope)) +
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
  labs(x = "", y = "",) +
  theme_minimal()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  scale_color_manual(values = c("yellow", "grey", "lightgreen", "darkgreen","brown", "purple", "red", "blue"))+
  theme_bw()+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "f)",
           size = 10, hjust = -0.1, vjust = 1.1, )



##
option1_plots_evo <- gridExtra::grid.arrange(plot_evo_SC_CORINE_lm, plot_evo_SC_SIOSE_lm, plot_evo_SC_INV_FOR_lm, ncol = 3)

##Option 2 plots----
#here we grouped the points by Scale (that will be represented by shape and color) and Land cover (no representation). We calculated the mean values and sd (as error bars at both axis). Linear models were coloured by Scale.

####CORINE----
#plot
summary_data <- model_slopes_CORINE %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_CORINE = mean(CORINE_slope, na.rm = TRUE),
    sd_CORINE = sd(CORINE_slope, na.rm = TRUE)
  )

plot2_evo_SC_CORINE_lm <- model_slopes_CORINE %>%
  ggplot(aes(x = SC_slope, y = CORINE_slope)) +
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
  labs(x = "", y = "Land Cover Slope in SPM (mean ± SD)") +
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="black")+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "d)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####SIOSE----
#plot
summary_data <- model_slopes_SIOSE %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_SIOSE = mean(SIOSE_slope, na.rm = TRUE),
    sd_SIOSE = sd(SIOSE_slope, na.rm = TRUE)
  )

plot2_evo_SC_SIOSE_lm <- model_slopes_SIOSE %>%
  ggplot(aes(x = SC_slope, y = SIOSE_slope)) +
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
  labs(x = "", y = "") +
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="black")+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "e)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####INV_FOR----
#plot
summary_data <- model_slopes_INV_FOR %>%
  group_by(land_use, Scale) %>%
  summarise(
    mean_SC = mean(SC_slope, na.rm = TRUE),
    sd_SC = sd(SC_slope, na.rm = TRUE),
    mean_INV_FOR = mean(INV_FOR_slope, na.rm = TRUE),
    sd_INV_FOR = sd(INV_FOR_slope, na.rm = TRUE)
  )

plot2_evo_SC_INV_FOR_lm <- model_slopes_INV_FOR %>%
  ggplot(aes(x = SC_slope, y = INV_FOR_slope)) +
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
  labs(x = "", y = "") +
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="black")+
  xlim(-0.01,0.035)+
  ylim(-0.01,0.035 )+
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "f)",
           size = 10, hjust = -0.1, vjust = 1.1, )
##
option2_plots_evo <- gridExtra::grid.arrange(plot2_evo_SC_CORINE_lm, plot2_evo_SC_SIOSE_lm, plot2_evo_SC_INV_FOR_lm, ncol = 3)



##Option 3 plots----
#here we didn´t group the points, so all points are plotted and coloured by Scale.Linear models were coloured by Scale.

####CORINE----
plot3_evo_SC_CORINE_lm <- model_slopes_CORINE %>%
  ggplot(aes(x = SC_slope, y = CORINE_slope)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  labs(x = "", y = "Land Cover Slope in SPM") +
  theme_bw()+
  coord_cartesian(xlim = c(-0.01, 0.035), ylim = c(-0.01, 0.035)) +
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "d)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####SIOSE----
plot3_evo_SC_SIOSE_lm <-  model_slopes_SIOSE %>%
  ggplot(aes(x = SC_slope, y = SIOSE_slope)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2) +
  labs(x = "", y = "") +
  theme_bw()+
  coord_cartesian(xlim = c(-0.01, 0.035), ylim = c(-0.01, 0.035)) +
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "e)",
           size = 10, hjust = -0.1, vjust = 1.1, )

####INV_FOR----
plot3_evo_SC_INV_FOR_lm <- model_slopes_INV_FOR %>%
  ggplot(aes(x = SC_slope, y = INV_FOR_slope)) +
  geom_abline (slope=1, linetype = "dashed", color="black", linewidth = 2)+
  geom_point(aes(color = Scale), size = 2)+
  geom_smooth(aes(color = Scale, fill = Scale), 
              method = "lm", alpha = 0.2)  +
  labs(x = "", y = "") +
  theme_bw()+
  coord_cartesian(xlim = c(-0.01, 0.035), ylim = c(-0.01, 0.035)) +
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
  annotate(geom = "text", x = -Inf, y = Inf, label = "f)",
           size = 10, hjust = -0.1, vjust = 1.1, )


##Join plots----
library(patchwork)

#option1
plot1_prop_SC_CORINE_lm + plot1_prop_SC_SIOSE_lm + plot1_prop_SC_INV_FOR_lm + 
  plot1_evo_SC_CORINE_lm + plot1_evo_SC_SIOSE_lm + plot1_evo_SC_INV_FOR_lm + plot_layout(guides = "collect") & 
  theme(axis.text.y = element_text(margin = margin(r = 5)))

#option2
plot2_prop_SC_CORINE_lm + plot2_prop_SC_SIOSE_lm + plot2_prop_SC_INV_FOR_lm + 
  plot2_evo_SC_CORINE_lm + plot2_evo_SC_SIOSE_lm + plot2_evo_SC_INV_FOR_lm + plot_layout(guides = "collect") & 
  theme(axis.text.y = element_text(margin = margin(r = 5)))

#option3
plot3_prop_SC_CORINE_lm + plot3_prop_SC_SIOSE_lm + plot3_prop_SC_INV_FOR_lm + 
plot3_evo_SC_CORINE_lm + plot3_evo_SC_SIOSE_lm + plot3_evo_SC_INV_FOR_lm + plot_layout(guides = "collect") & 
  theme(axis.text.y = element_text(margin = margin(r = 5)))

