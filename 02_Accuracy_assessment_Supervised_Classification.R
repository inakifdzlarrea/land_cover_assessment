#In this script we want to assess the accuracy supervised classification developed in Google Earth Engine
#February 2025#

# Load packages
library(terra)
library(sf)
library(tidyverse)
library(caret)

# define some useful vectors
periods <- c("1984_1990", "1990_1995", "1995_2000", "2000_2005", "2005_2010", "2010_2015", "2015_2019", "2019_2023" )
scales <- c("catchment", "riparian", "reach")
land_uses <- c("Water", "Urban", "Bare_soil", "Grassland", "Agriculture", "Shrubland", "Nat_forest", "Plant_forest")

#Load layers:----
#Of the vectorized classified composites
composites_vect <- readRDS(file = "../metrics/composites_vect.RDS")

#Of the validation points (VPs)
VP_path <- "../RIMSEC_general_classification/validation_points"  # This is the directory where the composites are located

VPs <- list()

for (i in 1:length(periods)) {
  st_read(list.files(path = paste0(VP_path, "/VP_", periods[i]), pattern = "^VP.*\\.shp$", full.names = TRUE)) -> VPs[[periods[i]]]
}


#Intersect VPs with Supervised Classification------
#Intersect layers, then homogenize the land_use values and select only these columns to make cleaner
VP_SC <- list()

for (i in 1:length(periods)){
  st_intersection(VPs[[periods[i]]], composites_vect[[periods[i]]]) -> VP_SC[[periods[i]]]
  
  VP_SC[[periods[i]]] = VP_SC[[periods[i]]] %>% filter(!is.na(land_use))%>%
      mutate(VP_land_use = if_else(USE == "PLANTATION_FOREST", "Plant_forest",
                           if_else(USE == "NATURAL_FOREST", "Nat_forest",
                           if_else(USE == "SHURBLAND", "Shrubland",
                                           str_to_sentence(USE)))),
             SC_land_use = str_to_sentence(land_use))%>%
      select(VP_land_use, SC_land_use)
}

#save it
saveRDS(VP_SC, file = paste0(VP_path, "/VP_SC.RDS"))

#Build the confusion matrix-----
#VP_SC <- readRDS( file = paste0(VP_path, "/VP_SC.RDS"))
conf_mat <- list()

for (i in 1:length(periods)){
  confusionMatrix(data= as.factor(VP_SC[[periods[i]]]$SC_land_use), reference = as.factor(VP_SC[[periods[i]]]$VP_land_use)) -> conf_mat[[periods[i]]]
}

#save the entire result in a csv 
capture.output(conf_mat, file = "../RIMSEC_general_classification/accuracy_assessment/confusion_matrices/confusion_matrices.csv")

#save the table for each period
for (i in 1:length(periods)){
   conf_mat[[periods[i]]]$table %>% write.csv(file = paste0("../RIMSEC_general_classification/accuracy_assessment/confusion_matrices/confusion_matrix_",periods[i],".csv"))
}

#save only the table for the sum of ALL periods
lista_tablas <- list()
for (i in 1:length(periods)){
lista_tablas[[periods[i]]] <-as.data.frame(conf_mat[[periods[i]]]$table)
}

tabla_suma <- bind_rows(lista_tablas) %>%
  group_by(across(where(is.factor))) %>%  # Mantiene las columnas de texto (si existen)
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")%>%
  pivot_wider(names_from = Reference, values_from = Freq)

write.csv(tabla_suma, file = paste0("../RIMSEC_general_classification/accuracy_assessment/confusion_matrices/confusion_matrix_all_periods.csv"))

#Accuracy measure-----
##Overall accuracy-----
overall_acc <- tibble_row()
mean_acc <- tibble_row()
low_acc <- tibble_row()
upp_acc <- tibble_row()

c <- tibble_row()
d <- tibble_row()
e <- tibble_row()
f <- tibble_row()

for (i in 1 :length(periods)){
periods[i] -> c

overall_acc = rbind(overall_acc, c)
#build mean overall accuracy column
conf_mat[[periods[i]]][["overall"]][["Accuracy"]] -> d
mean_acc = rbind(mean_acc, d)

#build lower overall accuracy column
conf_mat[[periods[i]]][["overall"]][["AccuracyLower"]] -> e
low_acc = rbind(low_acc, e)

#build upper overall accuracy column
conf_mat[[periods[i]]][["overall"]][["AccuracyUpper"]] -> f
upp_acc = rbind(upp_acc, f)
}

overall_acc = cbind(overall_acc, mean_acc)
overall_acc = cbind(overall_acc, low_acc)
overall_acc = cbind(overall_acc, upp_acc)
colnames(overall_acc) <- c("period", "mean_acc", "low_acc", "upp_acc")

overall_acc %>% mutate(map = "SC")
#save it as a csv
write.csv(overall_acc, file = "../RIMSEC_general_classification/accuracy_assessment/overall_accuracy.csv")

##User's and Producer's accuracies----
#These are class specific accuracy measures
#obtain Producer's accuracy and User's accuracy for each class in Supervised Classification in the results of confusionMatrix() PA is named as Sensitivity [,1] and UA as Precision [,5]

#Producer's accuracy
Producer_acc<- tibble(LAND_USE = c("Agriculture","Bare_soil","Grassland","Nat_forest","Plant_forest","Shrubland","Urban","Water"))

for (i in 1:length(periods)){
  conf_mat[[periods[i]]][["byClass"]][,1] -> Producer_acc[i+1]
}
colnames(Producer_acc)<-c("LAND_USE", periods) 

write_csv(Producer_acc, file = "../RIMSEC_general_classification/accuracy_assessment/producer_accuracy.csv")

#User's accuracy
User_acc <- tibble(LAND_USE = c("Agriculture","Bare_soil","Grassland","Nat_forest","Plant_forest","Shrubland","Urban","Water"))

for (i in 1:length(periods)){
  conf_mat[[periods[i]]][["byClass"]][,5] -> User_acc[i+1]
}
colnames(User_acc)<-c("LAND_USE", periods) 

write_csv(User_acc, file = "../RIMSEC_general_classification/accuracy_assessment/user_accuracy.csv")

##Join altogether----

Producer_acc %>% pivot_longer(!LAND_USE)%>%
  mutate(period = name, Acc = "Producer's accuracy", Use_period = paste(LAND_USE, period, sep = "_"), map = "SC") %>%
  select(!name)-> P_acc

User_acc %>% pivot_longer(!LAND_USE)%>%
  mutate(period = name, Acc = "User's accuracy", Use_period = paste(LAND_USE, period, sep = "_"), map = "SC") %>%
  select(!name) -> U_acc

#we will merge Producer´s and User´s accuracies
P_U_acc_mean <-rbind(P_acc, U_acc)%>% 
  group_by(LAND_USE, Acc)%>%
  summarise(acc = mean(value),
            acc_max = max(value),
            acc_min = min(value))

P_U_acc_all <- rbind(P_acc, U_acc)

write.csv(P_U_acc_mean,  file = "../RIMSEC_general_classification/accuracy_assessment/P_U_acc_mean.csv")
write.csv(P_U_acc_all,  file = "../RIMSEC_general_classification/accuracy_assessment/P_U_acc.csv")

#Now we will join them to overall accuracy

overall_acc <- read.csv( file = "../RIMSEC_general_classification/accuracy_assessment/overall_accuracy.csv")

overall_acc %>% mutate(LAND_USE = "OVERALL", Acc = "Overall accuracy", value = mean_acc,
                       Use_period = paste(LAND_USE, period, sep = "_"), map = "SC")%>%
  select(LAND_USE, period, value, Acc, Use_period, map)-> Ov_acc

Ov_P_U_acc <- rbind(Ov_acc, P_U_acc_all)
#Save it
write.csv(Ov_P_U_acc,  file = "../RIMSEC_general_classification/accuracy_assessment/Ov_P_U_acc.csv")
Ov_P_U_acc <-read.csv(  file = "../RIMSEC_general_classification/accuracy_assessment/Ov_P_U_acc.csv")

#PLOTS-----

Ov_P_U_acc = Ov_P_U_acc %>%mutate(year = ifelse(period == "1984_1990", 1987, 
                            ifelse(period == "1990_1995", 1992, 
                            ifelse(period == "1995_2000", 1997,
                            ifelse(period == "2000_2005", 2002,
                            ifelse(period == "2005_2010", 2007,
                            ifelse(period == "2010_2015", 2012,
                            ifelse(period == "2015_2019", 2017,
                            ifelse(period == "2019_2023", 2021, "")))))))))

Ov_P_U_acc %>% ggplot()+
  geom_point(aes(x = year, y = value*100, color = LAND_USE, shape = "2"), data = filter(Ov_P_U_acc, Acc == "Producer's accuracy"), position = position_nudge(-0.20), size = 5)+
  geom_point(aes(x = year, y = value*100, color = LAND_USE, shape = "3"), data = filter(Ov_P_U_acc, Acc == "User's accuracy"), position = position_nudge(0.20), size = 5)+
  geom_point(aes(x = year, y = value*100, shape = "1" ), data = filter(Ov_P_U_acc, Acc == "Overall accuracy"), size = 8)+
  scale_colour_manual(name = "Land use:", labels = c("Agriculture", "Bare soil", "Grassland", "Natural forest", "Plantation forest", "Shrubland", "Urban", "Water"), values =c("yellow", "grey", "lightgreen", "darkgreen", "brown", "purple","red", "blue"))+
  scale_shape_discrete(name = "Accuracy type:", labels = c("Overall accuracy", "Producer's accuracy", "User's accuracy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust=1,size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        )+
  labs(x = "", y = "Accuracy %")

