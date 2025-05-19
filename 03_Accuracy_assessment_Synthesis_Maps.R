#In this script we want to assess the accuracy synthesis maps: CORINE, SIOSE and Spanish National Forest Inventory(NFI)
#February 2025

#Load packages
library(tidyverse)
library(readxl)
library(terra)
library(sf)
library(caret)

#Load data----
synthesis_maps <- list()

#SIOSE layers ######
#2005
synthesis_maps[["SIOSE_2005"]] <- vect("../../../mapas_euskadi/SIOSE/junto/SIOSE_2005.shp")
#2009
synthesis_maps[["SIOSE_2009"]] <- vect("../../../mapas_euskadi/SIOSE/junto/SIOSE_2009.shp")
#2011
synthesis_maps[["SIOSE_2011"]] <- vect("../../../mapas_euskadi/SIOSE/junto/SIOSE_2011.shp")
#2014
synthesis_maps[["SIOSE_2014"]] <- vect("../../../mapas_euskadi/SIOSE/junto/SIOSE_2014.shp")
#2017
synthesis_maps[["SIOSE_2017"]] <- vect("../../../mapas_euskadi/SIOSE/junto/SIOSE_2017.shp")

#INVENTARIO FORESTAL layers #####
#2005
synthesis_maps[["INVFOR_2005"]] <- vect("../../../mapas_euskadi/INV_FOR/INV_FOR05/INV_FORESTAL_2005_10000_ETRS89.shp")
#2010
synthesis_maps[["INVFOR_2010"]] <- vect("../../../mapas_euskadi/INV_FOR/INV_FOR10/INV_FORESTAL_2010_10000_ETRS89.shp")
#2016
synthesis_maps[["INVFOR_2016"]] <- vect("../../../mapas_euskadi/INV_FOR/INV_FOR16/INV_FORESTAL_2016_10000_ETRS89.shp")
#2022
synthesis_maps[["INVFOR_2022"]] <- vect("../../../mapas_euskadi/INV_FOR/INV_FOR22/INV_FORESTAL_2022_10000_ETRS89.shp")

#CORINE layers #####
#1990
synthesis_maps[["CORINE_1990"]] <- vect("../../../mapas_euskadi/CORINE/1990/corine_eusk.shp")
#2000
synthesis_maps[["CORINE_2000"]] <- vect("../../../mapas_euskadi/CORINE/2000/corine2000_eusk.shp")
#2006
synthesis_maps[["CORINE_2006"]] <- vect("../../../mapas_euskadi/CORINE/2006/corine2006_eusk.shp")
#2012
synthesis_maps[["CORINE_2012"]] <- vect("../../../mapas_euskadi/CORINE/2012/corine2012_eusk.shp")
#2018
synthesis_maps[["CORINE_2018"]] <- vect("../../../mapas_euskadi/CORINE/2018/corine2018_eusk.shp")


#Validation points crossed with the SC data
VP_SC <- readRDS(file = "../RIMSEC_general_classification/Validation_points/VP_SC.RDS")

#Define some useful objects
land_uses <- c("Water","Urban","Bare_soil","Grassland","Agriculture","Shrubland","Nat_forest","Plant_forest")
periods <- c("1984_1990","1990_1995","1995_2000","2000_2005","2005_2010","2010_2015","2015_2019","2019_2023")
maps <- c("SIOSE","CORINE","INVFOR")

#Intersect VP with synthesis maps----
## Take information out of synthesis_maps ----
#Year and type of map
mapas_info <- tibble(
  mapa = synthesis_maps,  
  map = str_extract(names(synthesis_maps), "SIOSE|CORINE|INVFOR"),  
  year = as.numeric(str_extract(names(synthesis_maps), "\\d{4}")) 
)

crs_target <- crs(synthesis_maps[[1]])  # Suponiendo que todos los mapas tienen el mismo CRS
VP_SC <- map(VP_SC, ~ st_transform(.x, crs_target))  # Transformar a CRS de los mapas

## Process data for each period in SC ----
VP_SC_synt_maps <- imap(VP_SC, function(vp, period) {
  
  # Obtain the years included in the period
  years <- as.numeric(str_split(period, "_")[[1]])
  
  # Filter available maps for the periods
  mapas_periodo <- mapas_info %>%
    filter(year >= years[1], year <= years[2])
  
  # If not available maps, return NAs
  if (nrow(mapas_periodo) == 0) return(vp)
  
  # Extract land use values from availables maps
  for (i in seq_len(nrow(mapas_periodo))) {
    mapa <- mapas_periodo$mapa[[i]]
    tipo_mapa <- mapas_periodo$map[i]
    
    valores <- terra::extract(mapa, vect(vp))$LAND_USE
    
    # Paste the values on the VP_SC table
    vp[[paste0("land_use_", tipo_mapa)]] <- valores
  }
  
  return(vp)
})
#Maintain names for the periods
names(VP_SC_synt_maps) <- names(VP_SC)

#Save the object (as SpatVector layers)
saveRDS(VP_SC_synt_maps, file = "../RIMSEC_general_classification/validation_points/VP_SC_synt_maps.RDS")

##Save the object (as dataframes)
VP_SC_synt_maps_df <- list()

#function to change values to sentence format
change_to_sentence <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      str_to_sentence(col)  # Convierte texto a formato frase
    } else if (is.factor(col)) {
      factor(str_to_sentence(as.character(col)))  # Convierte factores a formato frase
    } else {
      col  # Mantiene los valores numéricos o de otro tipo sin cambios
    }
  })
  return(df)
}
#convert sf to dataframes and homogenize the names
for (i in 1:length(periods)){
   as.data.frame(VP_SC_synt_maps[[periods[i]]])%>%
    select(!geometry)%>%
    change_to_sentence()-> VP_SC_synt_maps_df[[periods[i]]] 
}
saveRDS(VP_SC_synt_maps_df, file = "../RIMSEC_general_classification/validation_points/VP_SC_synt_maps_df.RDS")

#Build confusion matrix-----
#VP_SC_synt_maps_df <- readRDS( file = "../RIMSEC_general_classification/validation_points/VP_SC_synt_maps_df.RDS")

#create a list we are pasting the resulting confusion matrices
conf_mat_synt_maps <- list()
##split by periods----
#1984_1990: no synthesis map available
conf_mat_synt_maps[["1984_1990"]] <- "No available map"
#1990_1995: CORINE map available

VP_SC_synt_maps_df[["1990_1995"]] %>% 
  filter(!is.na(land_use_CORINE )) -> VP_SC_CORINE_df

  confusionMatrix(data= as.factor(VP_SC_CORINE_df$land_use_CORINE), reference = as.factor(VP_SC_CORINE_df$VP_land_use)) -> conf_mat_synt_maps[["1990_1995"]][["CORINE"]]

#1995_2000: no synthesis map available 
conf_mat_synt_maps[["1995_2000"]] <- "No available map"

#2000_2005: CORINE map available
VP_SC_synt_maps_df[["2000_2005"]] %>% 
    filter(!is.na(land_use_CORINE ))-> VP_SC_CORINE_df
  
confusionMatrix(data= as.factor(VP_SC_CORINE_df$land_use_CORINE), reference = as.factor(VP_SC_CORINE_df$VP_land_use)) -> conf_mat_synt_maps[["2000_2005"]][["CORINE"]]

#2005_2010: CORINE, SIOSE and INVFOR available
#CORINE
VP_SC_synt_maps_df[["2005_2010"]] %>% 
  filter(!is.na(land_use_CORINE ))-> VP_SC_CORINE_df

confusionMatrix(data= as.factor(VP_SC_CORINE_df$land_use_CORINE), reference = as.factor(VP_SC_CORINE_df$VP_land_use)) -> conf_mat_synt_maps[["2005_2010"]][["CORINE"]]
#SIOSE
VP_SC_synt_maps_df[["2005_2010"]] %>% 
  filter(!is.na(land_use_SIOSE ))-> VP_SC_SIOSE_df

confusionMatrix(data= as.factor(VP_SC_SIOSE_df$land_use_SIOSE), reference = as.factor(VP_SC_SIOSE_df$VP_land_use)) -> conf_mat_synt_maps[["2005_2010"]][["SIOSE"]]
#INVFOR
VP_SC_synt_maps_df[["2005_2010"]] %>% 
  filter(!is.na(land_use_INVFOR))-> VP_SC_INVFOR_df

confusionMatrix(data= as.factor(VP_SC_INVFOR_df$land_use_INVFOR), reference = as.factor(VP_SC_INVFOR_df$VP_land_use)) -> conf_mat_synt_maps[["2005_2010"]][["INVFOR"]]

#2010_2015: CORINE, SIOSE and INVFOR available
#CORINE
VP_SC_synt_maps_df[["2010_2015"]] %>% 
  filter(!is.na(land_use_CORINE ))-> VP_SC_CORINE_df

confusionMatrix(data= as.factor(VP_SC_CORINE_df$land_use_CORINE), reference = as.factor(VP_SC_CORINE_df$VP_land_use)) -> conf_mat_synt_maps[["2010_2015"]][["CORINE"]]
#SIOSE
VP_SC_synt_maps_df[["2010_2015"]] %>% 
  filter(!is.na(land_use_SIOSE ))-> VP_SC_SIOSE_df

confusionMatrix(data= as.factor(VP_SC_SIOSE_df$land_use_SIOSE), reference = as.factor(VP_SC_SIOSE_df$VP_land_use)) -> conf_mat_synt_maps[["2010_2015"]][["SIOSE"]]
#INVFOR
VP_SC_synt_maps_df[["2010_2015"]] %>% 
  filter(!is.na(land_use_INVFOR))-> VP_SC_INVFOR_df

confusionMatrix(data= as.factor(VP_SC_INVFOR_df$land_use_INVFOR), reference = as.factor(VP_SC_INVFOR_df$VP_land_use)) -> conf_mat_synt_maps[["2010_2015"]][["INVFOR"]]

#2015_2019: CORINE, SIOSE and INVFOR available
#CORINE
VP_SC_synt_maps_df[["2015_2019"]] %>% 
  filter(!is.na(land_use_CORINE ))-> VP_SC_CORINE_df

confusionMatrix(data= as.factor(VP_SC_CORINE_df$land_use_CORINE), reference = as.factor(VP_SC_CORINE_df$VP_land_use)) -> conf_mat_synt_maps[["2015_2019"]][["CORINE"]]
#SIOSE
VP_SC_synt_maps_df[["2015_2019"]] %>% 
  filter(!is.na(land_use_SIOSE ))-> VP_SC_SIOSE_df

confusionMatrix(data= as.factor(VP_SC_SIOSE_df$land_use_SIOSE), reference = as.factor(VP_SC_SIOSE_df$VP_land_use)) -> conf_mat_synt_maps[["2015_2019"]][["SIOSE"]]
#INVFOR
VP_SC_synt_maps_df[["2015_2019"]] %>% 
  filter(!is.na(land_use_INVFOR))-> VP_SC_INVFOR_df

confusionMatrix(data= as.factor(VP_SC_INVFOR_df$land_use_INVFOR), reference = as.factor(VP_SC_INVFOR_df$VP_land_use)) -> conf_mat_synt_maps[["2015_2019"]][["INVFOR"]]

#2019_2023: INVFOR available
#INVFOR
VP_SC_synt_maps_df[["2019_2023"]] %>% 
  filter(!is.na(land_use_INVFOR))-> VP_SC_INVFOR_df

confusionMatrix(data= as.factor(VP_SC_INVFOR_df$land_use_INVFOR), reference = as.factor(VP_SC_INVFOR_df$VP_land_use)) -> conf_mat_synt_maps[["2019_2023"]][["INVFOR"]]

#Save confusion matrices as R objects
saveRDS(conf_mat_synt_maps, file = "../RIMSEC_general_classification/synthesis_maps/accuracy_assessment/confusion_matrices_synthesis_maps.RDS")

#Accuracy measures----

# 📌 Función para extraer métricas de una confusionMatrix
extract_metrics <- function(cm_obj) {
  if (inherits(cm_obj, "confusionMatrix")) {
    overall_acc <- cm_obj$overall["Accuracy"]
    min_val <- cm_obj$overall["AccuracyLower"]
    max_val <- cm_obj$overall["AccuracyUpper"]
    
    return(data.frame(mean_acc = overall_acc, low_acc = min_val, upp_acc = max_val))
  } else {
    return(NULL)
  }
}

# 📌 Aplicar la función sobre la lista de periods y maps
synt_map_accuracies <- map_dfr(names(conf_mat_synt_maps), function(period) {
  map_dfr(names(conf_mat_synt_maps[[period]]), function(map) {
    cm_data <- extract_metrics(conf_mat_synt_maps[[period]][[map]])
    
    if (!is.null(cm_data)) {
      cm_data <- mutate(cm_data, period = period, map = map)
    }
    
    return(cm_data)
  })
})

#Save as a csv
write_csv(synt_map_accuracies, file = "../RIMSEC_general_classification/synthesis_maps/accuracy_assessment/synt_map_accuracies.csv" )

#Accuracy comparison to SC----

#synt_map_accuracies <- read_csv(file = "../RIMSEC_general_classification/synthesis_maps/accuracy_assessment/synt_map_accuracies.csv")

#load accuracy data of Supervised Classification
overall_acc <- read_csv(file = "../RIMSEC_general_classification/accuracy_assessment/overall_accuracy.csv")
overall_acc = overall_acc %>% mutate(map = "SC")%>% select(!X)

#bind together
accuracy_comparison <- rbind(synt_map_accuracies, overall_acc)
write_csv(accuracy_comparison, file = "../RIMSEC_general_classification/SC_vs_synt/accuracy_comparison.csv" )

#PLOT------
accuracy_comparison <- read_csv(file = "../RIMSEC_general_classification/SC_vs_synt/accuracy_comparison.csv" )

accuracy_comparison %>% mutate(year = ifelse(period == "1984_1990", 1987, 
                                      ifelse(period == "1990_1995", 1992, 
                                      ifelse(period == "1995_2000", 1997,
                                      ifelse(period == "2000_2005", 2002,
                                      ifelse(period == "2005_2010", 2007,
                                      ifelse(period == "2010_2015", 2012,
                                      ifelse(period == "2015_2019", 2017,
                                      ifelse(period == "2019_2023", 2021, "")))))))))%>%
  ggplot(aes(x = year, y = mean_acc*100, ymin = low_acc*100, ymax = upp_acc*100, col = map))+
  geom_pointrange(position = position_dodge(width = 1), size = 3, linewidth = 3)+
  scale_colour_manual(name = "Map:", labels = c("CORINE","INVFOR", "SC","SIOSE" ), values =c("red", "green", "black", "blue"))+
  labs(y ="Overall accuracy %")+
  coord_cartesian( ylim = c(0, 100))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 25),
        axis.text.y = element_text(size = 25),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", size = 25),
        axis.title.y = element_text(face = "bold", size = 35),
        axis.title.x = element_blank())
  
  
