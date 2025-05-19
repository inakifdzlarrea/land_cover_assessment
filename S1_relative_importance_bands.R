

relative_importance_1984_1990 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_1984_1990.csv")
relative_importance_1990_1995 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_1990_1995.csv")
relative_importance_1995_2000 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_1995_2000.csv")
relative_importance_2000_2005 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_2000_2005.csv")
relative_importance_2005_2010 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_2005_2010.csv")
relative_importance_2010_2015 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_2010_2015.csv")
relative_importance_2015_2019 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_2015_2019.csv")
relative_importance_2019_2023 <- read.csv("../RIMSEC_general_classification/relative_importance/relative_importance_2019_2023.csv")


relative_importance_1984_1990 <- relative_importance_1984_1990 %>% mutate( Period = "1984_1990") %>% select(!c(system.index, .geo))
relative_importance_1990_1995 <- relative_importance_1990_1995 %>% mutate( Period = "1990_1995")%>% select(!c(system.index, .geo))
relative_importance_1995_2000 <- relative_importance_1995_2000 %>% mutate( Period = "1995_2000")%>% select(!c(system.index, .geo))
relative_importance_2000_2005 <- relative_importance_2000_2005 %>% mutate( Period = "2000_2005")%>% select(!c(system.index, .geo))
relative_importance_2005_2010 <- relative_importance_2005_2010 %>% mutate( Period = "2005_2010")%>% select(!c(system.index, .geo))
relative_importance_2010_2015 <- relative_importance_2010_2015 %>% mutate( Period = "2010_2015")%>% select(!c(system.index, .geo))
relative_importance_2015_2019 <- relative_importance_2015_2019 %>% mutate( Period = "2015_2019")%>% select(!c(system.index, .geo))
relative_importance_2019_2023 <- relative_importance_2019_2023 %>% mutate( Period = "2019_2023")%>% select(!c(system.index, .geo))





relative_importance <- rbind(relative_importance_1984_1990 ,
                             relative_importance_1990_1995 ,
                             relative_importance_1995_2000 ,
                             relative_importance_2000_2005 ,
                             relative_importance_2005_2010 ,
                             relative_importance_2010_2015 ,
                             relative_importance_2015_2019 ,
                             relative_importance_2019_2023 )

relative_importance <- relative_importance %>% pivot_longer(!Period)%>%
  group_by(name)%>%
  summarise(mean_importance = mean(value),
            median_importance = median(value),
            max_importance = max(value),
            min_importance = min(value),
            sd_importance = sd(value))

write_csv(relative_importance, file = "../RIMSEC_general_classification/relative_importance/relative_importance_stats.csv")
