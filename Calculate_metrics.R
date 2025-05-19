#In this script we want to calculate metrics (total area and proportions) of land cover from the classified composites developed in Google Earth Engine 
#February 2025#

# Load packages
library(terra)
library(sf)
library(tidyverse)

# define some useful vectors
periods <- c("1984_1990", "1990_1995", "1995_2000", "2000_2005", "2005_2010", "2010_2015", "2015_2019", "2019_2023" )
scales <- c("catchment", "riparian", "reach")
land_uses <- c("Water", "Urban", "Bare_soil", "Grassland", "Agriculture", "Shrubland", "Nat_forest", "Plant_forest")

# 1) Load layers----
## 1.1) Composite raster layers----
comp_path <- "../RIMSEC_general_classification/classified_composites"  # This is the directory where the composites are located

composites <- list()

for (i in 1:length(periods)) {
  rast(list.files(path = paste0(comp_path, "/Comp_", periods[i]), pattern = "^General.*ied\\.tif$", full.names = TRUE)) -> composites[[periods[i]]]
}

# Now are going to tranform raster layers into polygons
composites_vect <- list()

for (i in 1:length(periods)) {
  composites_vect[[periods[i]]] <- terra::as.polygons(composites[[periods[i]]], values = T, aggregate = T)
 }

#Add a field to the attribute table naming land uses
for (i in 1:length(periods)){
    values(composites_vect[[periods[i]]])$land_use = land_uses[values(composites_vect[[periods[i]]])$classification+1]
}

#As sf object
for (i in 1:length(periods)){
  composites_vect[[periods[i]]] <- st_as_sf((composites_vect[[periods[i]]]))
}


##1.2) Load vectorial layers-----
#this vectorial layers include catchments, riparian buffer (30m) and reach scale buffer (100m)
vector_path <- "../capas_corte"  # location of the shapefiles 

scale_layers <- list()

scale_layers[[scales[1]]] <- st_read(paste0(vector_path, "/subWS_WGS84.shp")) #Catchment scale layer
scale_layers[[scales[2]]] <- st_read(paste0(vector_path, "/buffer30m_subcuencaWGS84.shp"))#Riparian scale layer
scale_layers[[scales[3]]] <- st_read(paste0(vector_path, "/buffer100m_SamplingLocationWGS84.shp"))#Reach scale layer

#2) Estimate land cover area for each of the land uses
##2.1) Make the intersection between layers----
intersected_layers <- list()

for (i in 1:length(scales)){
  for (j in 1:length(periods)){
    intersected_layers[[scales[i]]][[periods[j]]] <- st_intersection(composites_vect[[j]], scale_layers[[i]])
}}

#save the result and reread it (it takes nearly 2 days to make the intersectio)
saveRDS(composites_vect, file = "../metrics/composites_vect.RDS")
saveRDS(intersected_layers, file = "../metrics/intersected_layers.RDS")

#composites_vect <- readRDS( file = "../metrics/composites_vect.RDS")
#intersected_layers <- readRDS( file = "../metrics/intersected_layers.RDS")

##2.2) Estimate the area and proportion -----
#(ha) for each land use at each site in each of the scales

for (j in 1:length(periods)){#this loops create a Site column for the catchment scale data to uniform it to the other two scales
intersected_layers[["catchment"]][[periods[j]]] = intersected_layers[["catchment"]][[periods[j]]] %>%
  mutate(Site = str_c(str_to_upper(str_sub(NAME, 1, 4)), "_", 
                      if_else(str_sub(NAME, -1, -1) == "0", 
                              str_sub(NAME, -2, -1), 
                              str_c("0", str_sub(NAME, -1, -1))))) 
}

#first estimate area of each polygon
for (i in 1:length(scales)){
  for (j in 1:length(periods)){
    intersected_layers[[scales[i]]][[periods[j]]]$area_m2 <- lwgeom::st_geod_area(intersected_layers[[scales[i]]][[periods[j]]])
  }}

#then, filter empty pixels and estimate area of the entire study scale polygon and finally divide the area of each land use to get the proportion
for (i in 1:length(scales)){
  for (j in 1:length(periods)){
    intersected_layers[[scales[i]]][[periods[j]]] = intersected_layers[[scales[i]]][[periods[j]]] %>% 
      filter(classification != 8) %>% 
      group_by(Site) %>%
      mutate(sum_area_m2 = sum(area_m2))%>%
      mutate(LU_prop = area_m2/sum_area_m2,
             Park = str_sub(Site, 1, 4))%>%
      select(Park, Site, land_use, area_m2, LU_prop, sum_area_m2)
  }}

for (i in 1:length(scales)){
  for (j in 1:length(periods)){
    intersected_layers[[scales[i]]][[periods[j]]] %>% 
      as.data.frame()%>%
      select(!geometry)%>%
    write_csv(file = paste0("../RIMSEC_general_classification/metrics/general/",scales[i],"/",periods[j],"/","LU_prop_",scales[i],"_",periods[j],".csv"))
  }}

###* for the outlet-----
#Now we are going to filter only the values of polygons representing the outlet of the watershed
for (i in 1:2){#this loop is only for catchment and riparian scale
  for (j in 1:length(periods)){
    intersected_layers[[scales[i]]][[periods[j]]] %>% 
      as.data.frame()%>%
      select(!geometry)%>%
      filter(Site %in% c("AIZK_10", "ARAL_01", "ARTI_10", "GORB_10", "IZKI_10")) %>%
      write_csv(file = paste0("../RIMSEC_general_classification/metrics/outlet/",scales[i],"/",periods[j],"/","LU_prop_",scales[i],"_",periods[j],".csv"))
  }}

#Estimate LU proportion reach scale wa re going to sum the areas and recalculate the proportion
for (j in 1:length(periods)){
intersected_layers[["reach"]][[periods[j]]] %>% 
  as.data.frame()%>%
  select(!geometry)%>%
  group_by(Park, land_use)%>%
  summarise(area_m2 = sum(area_m2)) -> summed_values
  
  summed_values %>% group_by(Park)%>%
  mutate(sum_area_m2 = sum(area_m2),
         LU_prop = area_m2/sum_area_m2)%>%
  write_csv(file = paste0("../RIMSEC_general_classification/metrics/outlet/","reach","/",periods[j],"/","LU_prop_","reach","_",periods[j],".csv"))
}


#Join them in single df for each scale and for all data(scale and period)
# Ruta base donde están las carpetas con los archivos CSV
ruta_base <- "../RIMSEC_general_classification/metrics/outlet/"  # 

# Obtener lista de archivos CSV con rutas completas
archivos <- list.files(ruta_base, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Función para leer un CSV y extraer la escala y el período desde el nombre del archivo
leer_csv_con_info <- function(archivo) {
  # Extraer el nombre del archivo sin la ruta
  nombre_archivo <- basename(archivo)  # Ejemplo: "LU_prop_regional_2000_2005.csv"
  
  # Extraer la parte relevante (asumiendo formato: "LU_prop_escala_periodo1_periodo2.csv")
  partes <- str_split(nombre_archivo, "_", simplify = TRUE)
  
  # Extraer valores de escala y periodo
  escala <- partes[, 3]  # La tercera parte es la escala
  periodo <- str_c(partes[, 4], "_", str_remove(partes[, 5], "\\.csv$"))  # Período es la 4ta y 5ta parte unidas con "_"
  
  # Leer el archivo y añadir columnas de Escala y Periodo
  read_csv(archivo) %>%
    mutate(Scale = escala, Period = periodo)
}

# Aplicar la función a todos los archivos y combinarlos en una sola tabla
LU_prop_outlet <- map_dfr(archivos, leer_csv_con_info)

#save it in a csv: all data and filtered by scale
write.csv(LU_prop_outlet, file = "../RIMSEC_general_classification/metrics/outlet/LU_prop.csv" )

for (i in 1: length(scales)){
  LU_prop_outlet%>% filter(Scale == scales[i])%>%
    write.csv(paste0("../RIMSEC_general_classification/metrics/outlet/",scales[i],"/","/","LU_prop_",scales[i],"_",".csv"))
}



