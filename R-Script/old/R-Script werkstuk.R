install.packages("readr")
library(readr)

# Working Directory wijzigen.
getwd()
setwd("N:/Studeren/Novi Hogeschool/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

# Subvraag: Wat is de polulatie van programmeer taal in Nederland voor System Administrator? 
# (Analyse uitvoeren Nederland uit de data te filteren en kijken hoe dat verschilt in de rest van de wereld.. )
# Benodigd > Kolom Country + 
#Data uitlezen
library(readr)
dataset <- read_csv("survey_results_public.csv")
View(dataset)


# Geeft structure weer uit Dataset 
str(dataset)

# Geeft het aantal rijen en kolommen weer uit de Dataset
dim(dataset)

# Geeft alle Variable / kolom namen weer uit de Dataset. 
names(dataset)

# Geeft een samenvatting van de Data
summary(dataset)

#View plot van 2 kolommen / variables
plot(dataset$Country, dataset$DevType) #Werkt nog niet correct.

# Aantal rijen dataset laten zien
nrow(dataset)

#Aantal kolommen dataset laten zien
ncol(dataset)
     
# 1e 10 rijen weergeven van dataset     
head(dataset,  10)

# Bepaalde Variables / Kolommen weergeven vanuit de Dataset.
# Country geeft de plaats weer
# DevType geeft de jobtype weer. (System Administrator)
splitdataset<- read_csv("survey_results_public.csv")
  
# Onderstaande packages zijn nodig voor het filteren van de data.
library(dplyr)
library(tidyr)

# Kolommen splitten.
dataset2 <- dataset %>%
  filter(Country == "Netherlands")%>%
  select(Respondent, DevType)
  

View(dataset2)

# Inhoud Kolommen splitten
dataset3 <- dataset2 %>%
  separate(DevType, c("A","B","C","D","E","F","H","L","M","N","O","P","Q","R","S","T","U","V","W"), sep = ";")

View(dataset3)

 
data <- read_csv("survey_results_public.csv") ("N:/Studeren/Novi Hogeschool/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

View(data)
names(data)


# Omgaan met lege waarden. R document NAS. Pagina 50

