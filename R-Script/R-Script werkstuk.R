
# Variable:
# dataset = bevat de complete dataset 
# functie = bevat de job-functies alleen van de personen uit Nederland! (alles staat in 1 kolom)
# jobfunctie = alle functies uit DevType kolom van Dataset verder uit-gesplit.



# Onderstaande installeert de packages voor R-studio wat je nodig hebt voor de data bewerking
install.packages("readr")
library(readr)

# Onderstaande packages zijn nodig voor het filteren van de data. Die ook geinstalleerd moeten worden.
install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

# Working Directory wijzigen.
getwd()
setwd("N:/Studeren/Novi Hogeschool/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

# Working Directory (prive)laptop
setwd("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

# Working Directory (PwC)laptop
setwd("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/") < AANPASSEN

# Subvraag: Wat is de polulatie van programmeer taal in Nederland voor System Administrator? 
# (Analyse uitvoeren Nederland uit de data te filteren en kijken hoe dat verschilt in de rest van de wereld.. )
# Benodigd > Kolom Country + 
#Data uitlezen
# package > library(readr) is hiervoor nodig.
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
# WERKT NIET splitdataset<- read_csv("survey_results_public.csv")
  


# Kolommen splitsen ( Waarvan alles met Nederland eruit gefilterd wordt en word opgesplit in nummer van deelnemer + Devtype)
functie <- dataset %>%
  filter(Country == "Netherlands")%>%
  select(Respondent, DevType)
  
View(functie)


# Inhoud Kolommen splitten in correcte job / kolom namen.
jobfunctie <- functie %>%
  separate(DevType, c("Academic researcher","Data or business analyst","Data scientist or machine learning specialist","Database administrator",
                      "Designer","Developer, backend","Developer, desktop or enterprise applications","Developer, embedded applications or devices",
                      "Developer, frontend","Developer, fullstack","Developer, game or graphics","Developer, mobile","Developer, QA or test","DevOps specialist",
                      "Educator","Engineer, data","Engineer, site reliability","Engineering manager","Marketing or sales professional","Product manager",
                      "Scientist", "Senior Executive (CSuite, VP, etc.)", "Student", "System administrator", "Other"), sep = ";")


# jobfunctie is nu uitgewerkt met de correcte Kolom namen vanuit varialble functie.
View(jobfunctie) 


# Geeft alle Variable / kolom namen weer uit de jobfunctie dataset. 
names(jobfunctie)

# Mooiste zou zijn om de job's ook ook nog onder elkaar te plaatsen in de kolom naam. AAAAAAAAAAAAAA

# rijen onder elkaar geplaatst. Nu per Job een nieuwe Variable maken.
jobsonderelkaar <- separate_rows(functie,DevType,sep=";")

View(jobsonderelkaar)

# Onderstaande variable's worden aangemaakt vanuit jobsonderelkaar variable. Dit is een variable met alle Jobs vanuit Nederland.
# Nieuwe Variable aanmaken om alle Respondenten van System Administrator eruit te filteren en plaatsen in variable.
SystemAdministrator <- filter(jobsonderelkaar, DevType == "System administrator")
View(SystemAdministrator)

# Nieuwe Variable aanmaken om alle Respondenten van Development ( Developer, back-end )eruit te filteren en plaatsen in variable.
Development <- filter(jobsonderelkaar, DevType == "Developer, back-end")
View(Development)

# Nieuwe Variable aanmaken om alle Respondenten van Development ( Developer, back-end )eruit te filteren en plaatsen in variable.
DatabaseAdministrator <- filter(jobsonderelkaar, DevType == "Database administrator")
View(DatabaseAdministrator)



# Tot bovenstaande gaat goed. Onderstaande is nog in bewerking voor R-Script.
# ================================================================================================ 
  

# Wat wil ik doen? > Nu moeten de rijen uitgewerkt worden naar dezelfde kolom namen. 
jobfunctie1 <- jobfunctie %>%  
  separate_rows(jobfunctie, jobfunctie1, DevType,sep=","),DevType,sep=";")



jobfunctieTEST <- jobfunctie %>%
    gather(Academic researcher,Data or business analyst,C,D, c("Academic researcher","Data or business analyst","Data scientist or machine learning specialist","Database administrator",
                      "Designer","Developer, backend","Developer, desktop or enterprise applications","Developer, embedded applications or devices",
                      "Developer, frontend","Developer, fullstack","Developer, game or graphics","Developer, mobile","Developer, QA or test","DevOps specialist",
                      "Educator","Engineer, data","Engineer, site reliability","Engineering manager","Marketing or sales professional","Product manager",
                      "Scientist", "Senior Executive (CSuite, VP, etc.)", "Student", "System administrator", "Other"), 1:20)

names(functie)
View(jobfunctieTEST)





# Onderstaande is Backup tekst. 
   
functie <- separate(functie, DevType, into = c("Academic researcher","Data or business analyst","Data scientist or machine learning specialist","Database administrator",
                                               "Designer","Developer, backend","Developer, desktop or enterprise applications","Developer, embedded applications or devices",
                                               "Developer, frontend","Developer, fullstack","Developer, game or graphics","Developer, mobile","Developer, QA or test","DevOps specialist",
                                               "Educator","Engineer, data","Engineer, site reliability","Engineering manager","Marketing or sales professional","Product manager",
                                               "Scientist", "Senior Executive (CSuite,VP, etc.", "Student", "System administrator", "Other"), sep = ";")


View(functie)

# Inhoud Kolommen splitten
jobfunctie <- functie %>%
  separate(DevType, c("A","B","C","D","E","F","H","L","M","N","O","P","Q","R","S","T","U","V","W"), sep = ";")
  
  
    separate(DevType, c("A","B","C","D","E","F","H","L","M","N","O","P","Q","R","S","T","U","V","W"), sep = ";")

jobfunctie1 <- jobfunctie %>%
  gather(DevType, c(A,B,"C","D","E","F","H","L","M","N","O","P","Q","R","S","T","U","V","W"))

  gather(A, key = "A", value = "Academic")
  rlang::last_error()
  rlang::last_trace()
  

View(jobfunctie)
View(jobfunctie1)
glimpse(jobfunctie)

 





data <- read_csv("survey_results_public.csv") ("N:/Studeren/Novi Hogeschool/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

View(data)
names(data)


# Omgaan met lege waarden. R document NAS. Pagina 50

