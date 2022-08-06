#---------------------------------------------------------------------------#
# Nom : senate_pyramid.R                                                    #
# Description : Population pyramid of senators                              #
# Auteur : Pietro Violo                                                     #
#---------------------------------------------------------------------------#

options(scipen=999)

# Library
library(tidyverse)
library(data.table)
library(rvest)
library(httr)
library(ggpubr)

# clear global environment
rm(list=ls(all=TRUE))

#'*Data*

# Import data from the UNW

pop <- fread("./Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv")
pop <- pop[ISO3_code %in% c("CAN",
                            "FRA",
                            "DEU",
                            "ITA",
                            "JPN",
                            "RUS",
                            "GBR",
                            "USA") &
             Time == 2021,]


pop <- pop %>% mutate(age_group =  case_when(AgeGrpStart %in% 18:20 ~ "18 - 20",
                            AgeGrpStart %in% 21:30 ~ "21 - 30",
                            AgeGrpStart %in% 31:40 ~ "31 - 40",
                            AgeGrpStart %in% 41:45 ~ "41 - 45",
                            AgeGrpStart %in% 46:50 ~ "46 - 50",
                            AgeGrpStart %in% 51:60 ~ "51 - 60",
                            AgeGrpStart %in% 61:70 ~ "61 - 70",
                            AgeGrpStart %in% 71:80 ~ "71 - 80",
                            AgeGrpStart %in% 81:90 ~ "81 - 90",
                            AgeGrpStart %in% 91:110 ~ "91 and over")) %>% 
  group_by(age_group, Location) %>% 
  summarise(Male = sum(PopMale),
            Female = sum(PopFemale)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  as.data.table()

pop <- pop %>% pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  as.data.table()

pop <- pop[,.(Age = age_group,
                   Sex = Sex,
                   proportion_population = Count / sum(Count) * 100), by = .(Location)]



# Canada

link = "https://data.ipu.org/node/32/data-on-youth?chamber_id=13360"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

Canada <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

Canada <- (Canada %>% t())[c(-1, -12),]
colnames(Canada) <- c("age_group", "Male", "Female") 

Canada <- Canada %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "Canada")




# France

link = "https://data.ipu.org/content/france?chamber_id=13397"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

France <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

France <- (France %>% t())[c(-1, -12),]
colnames(France) <- c("age_group", "Male", "Female") 

France <- France %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "France")


# Germany, Federal Council

link = "https://data.ipu.org/content/germany?chamber_id=13317"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

Germany <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

Germany <- (Germany %>% t())[c(-1, -12),]
colnames(Germany) <- c("age_group", "Male", "Female") 

Germany <- Germany %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "Germany")


# Italy

link = "https://data.ipu.org/content/italy?chamber_id=13428"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

Italy <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

Italy <- (Italy %>% t())[c(-1, -12),]
colnames(Italy) <- c("age_group", "Male", "Female") 

Italy <- Italy %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "Italy")


# Japan, House of councillors

link = "https://data.ipu.org/content/japan?chamber_id=13433"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

Japan <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

Japan <- (Japan %>% t())[c(-1, -12),]
colnames(Japan) <- c("age_group", "Male", "Female") 

Japan <- Japan %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "Japan")

# Russia, Council of the Federation

link = "https://data.ipu.org/content/russian-federation?chamber_id=13393"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

Russia <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

Russia <- (Russia %>% t())[c(-1, -12),]
colnames(Russia) <- c("age_group", "Male", "Female") 

Russia <- Russia %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "Russia")




# United Kingdom, House of Lords

link = "https://data.ipu.org/content/united-kingdom?chamber_id=13512"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

United_Kingdom <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

United_Kingdom <- (United_Kingdom %>% t())[c(-1, -12),]
colnames(United_Kingdom) <- c("age_group", "Male", "Female") 

United_Kingdom <- United_Kingdom %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "United Kingdom")


# United States

link = "https://data.ipu.org/content/united-states-america?chamber_id=13388"

page = read_html(GET(link))

data <- page %>% 
  html_nodes(".pane-ipu-country-field-age-sex-breakdown-pane tbody td , .pane-ipu-country-field-age-sex-breakdown-pane th") %>% 
  html_text()

United_States <- data %>% matrix(ncol = 12, byrow = TRUE) %>% as.data.table()

United_States <- (United_States %>% t())[c(-1, -12),]
colnames(United_States) <- c("age_group", "Male", "Female") 

United_States <- United_States %>% as.data.frame() %>% 
  pivot_longer(Male:Female, names_to = "Sex", values_to = "Count") %>% 
  mutate(Count = as.numeric(Count),
         Location = "United States of America")

senators <- rbind(Canada,
                  France,
                  Germany,
                  Italy,
                  Japan,
                  Russia,
                  United_Kingdom,
                  United_States) %>% 
  as.data.table()


senators <- senators[,.(Age = age_group,
                        Sex = Sex,
                        proportion_senators = Count / sum(Count) * 100), by = .(Location)]

rm(Canada,
   France,
   Germany,
   Italy,
   Japan,
   Russia,
   United_Kingdom,
   United_States,
   page,
   data,
   link)


#'*Make plot*

age_group <- c("18 - 20",
           "21 - 30",
           "31 - 40",
           "41 - 45",
           "46 - 50",
           "51 - 60",
           "61 - 70",
           "71 - 80",
           "81 - 90",
           "91 and over")



graph_canada_pop <- ggplot(data=pop %>% filter(Location == "Canada"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "Canada"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "Canada"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30)) +
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "Canada"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "Canada"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 

graph_canada_senators <- ggplot(data=senators %>% filter(Location == "Canada"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "Canada"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "Canada"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "Canada"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "Canada"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 




graph_france_pop <- ggplot(data=pop %>% filter(Location == "France"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "France"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "France"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "France"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "France"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 



graph_france_senators <- ggplot(data=senators %>% filter(Location == "France"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "France"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "France"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "France"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "France"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 





graph_germany_pop <- ggplot(data=pop %>% filter(Location == "Germany"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "Germany"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "Germany"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "Germany"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "Germany"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 



graph_germany_senators <- ggplot(data=senators %>% filter(Location == "Germany"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "Germany"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "Germany"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)") +
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "Germany"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "Germany"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 




graph_italy_pop <- ggplot(data=pop %>% filter(Location == "Italy"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "Italy"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "Italy"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "Italy"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "Italy"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 




graph_italy_senators <- ggplot(data=senators %>% filter(Location == "Italy"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "Italy"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "Italy"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10)))  +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "Italy"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "Italy"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 





graph_japan_pop <- ggplot(data=pop %>% filter(Location == "Japan"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "Japan"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "Japan"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "Japan"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "Japan"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 






graph_japan_senators <- ggplot(data=senators %>% filter(Location == "Japan"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "Japan"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "Japan"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "Japan"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "Japan"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 






graph_russia_pop <- ggplot(data=pop %>% filter(Location == "Russian Federation"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "Russian Federation"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "Russian Federation"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "Russian Federation"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "Russian Federation"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 





graph_russia_senators <- ggplot(data=senators %>% filter(Location == "Russia"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "Russia"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "Russia"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "Russia"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "Russia"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 





graph_uk_pop <- ggplot(data=pop %>% filter(Location == "United Kingdom"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "United Kingdom"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "United Kingdom"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "United Kingdom"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "United Kingdom"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 







graph_uk_senators <- ggplot(data=senators %>% filter(Location == "United Kingdom"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "United Kingdom"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "United Kingdom"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30)) +
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "United Kingdom"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "United Kingdom"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 




graph_usa_pop <- ggplot(data=pop %>% filter(Location == "United States of America"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(pop, Sex == "Male" & Location == "United States of America"), aes(y=proportion_population)) +
  geom_col(data = subset(pop, Sex == "Female" & Location == "United States of America"), aes(y = proportion_population*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(pop, Sex == "Male" & Location == "United States of America"), 
            aes(x = Age, y = proportion_population + 5.5, 
                label = round(proportion_population, digits = 1),
                color = "navy")) +
  geom_text(data = subset(pop, Sex == "Female" & Location == "United States of America"), 
            aes(x = Age, y = - proportion_population - 5.5, 
                label = round(proportion_population, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 






graph_usa_senators <- ggplot(data=senators %>% filter(Location == "United States of America"),aes(x=as.factor(Age), fill=Sex)) +
  geom_col(data = subset(senators, Sex == "Male" & Location == "United States of America"), aes(y=proportion_senators)) +
  geom_col(data = subset(senators, Sex == "Female" & Location == "United States of America"), aes(y = proportion_senators*(-1))) +
  coord_flip() +
  labs(x = "Age group", y = "(%)")+
  scale_x_discrete(labels=age_group, name = "Age group")+ 
  scale_y_continuous(breaks=seq(-30,30,10),labels=abs(seq(-30,30,10))) +
  expand_limits(y = c(-30, 30))+
  theme_minimal(base_size=12) +
  geom_text(data = subset(senators, Sex == "Male" & Location == "United States of America"), 
            aes(x = Age, y = proportion_senators + 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "navy")) +
  geom_text(data = subset(senators, Sex == "Female" & Location == "United States of America"), 
            aes(x = Age, y = - proportion_senators - 5.5, 
                label = round(proportion_senators, digits = 1),
                color = "dark red")) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=12)
  ) +
  scale_fill_manual(values = c("#e63946", "#0096c7")) 



CAN <- ggarrange(graph_canada_pop, graph_canada_senators, common.legend = TRUE)

FRA <- ggarrange(graph_france_pop, graph_france_senators, common.legend = TRUE)

DEU <- ggarrange(graph_germany_pop, graph_germany_senators, common.legend = TRUE)

ITA <- ggarrange(graph_italy_pop, graph_italy_senators, common.legend = TRUE)

JPN <- ggarrange(graph_japan_pop, graph_japan_senators, common.legend = TRUE)

RUS <- ggarrange(graph_russia_pop, graph_russia_senators, common.legend = TRUE)

GBR <- ggarrange(graph_uk_pop, graph_uk_senators, common.legend = TRUE)

USA <- ggarrange(graph_usa_pop, graph_usa_senators, common.legend = TRUE)



average <- c(64.26, 65.85, 56.11, 70, 60.44, 56.02, 57.85, 53.14) # USA, CAN, JPN, GBR, FRA, DEU, RUS, ITA
sort(average) #ITA, #DEU, #JPN, #RUS, #FRA, #USA, #CAN, #UK


png("ITA_DEU_JPN_RUS.png", res = 350, width = 3000, height = 5000)

ggarrange(ITA,DEU,JPN,RUS, ncol = 1, common.legend = TRUE)

dev.off()


png("FRA_USA_CAN_GBR.png", res = 350, width = 3000, height = 5000)

ggarrange(FRA,USA,CAN,GBR, ncol = 1, common.legend = TRUE)

dev.off()

