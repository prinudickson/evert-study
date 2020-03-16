
# Variable:
# datasetXXXX = bevat de complete dataset uit het jaar?
# columnsplitXXX = bevat de job-functies alleen van de personen uit Nederland! (alles staat in 1 kolom) uit het jaar.
# separatejobrowsXXXX = alle functies uit DevType kolom van Dataset verder uit-gesplit per jaartal.



# Onderstaande installeert de packages voor R-studio wat je nodig hebt voor de data bewerking
# readr
# dplyr
# tidyr

install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")

# onderstaande start de packages in R-Studio zodat je ermee kan werken.
library(readr)
library(dplyr)
library(tidyr)



# Working Directory wijzigen.
getwd()
setwd("N:/Studeren/Novi Hogeschool/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

# Working Directory (prive)laptop
setwd("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/")

# Working Directory (PwC)laptop
setwd("H:/My Drive/DATA SCIENCE Werkstuk/Dataset/developer_survey_2019/")

# Subvraag: Wat is de polulatie van programmeer taal in Nederland voor System Administrator? 
# (Analyse uitvoeren Nederland uit de data te filteren en kijken hoe dat verschilt in de rest van de wereld.. )
# Benodigd > Kolom Country + 


#Data uitlezen ( Survey Results 2017 + 2018 + 2019 )
# package > library(readr) is hiervoor nodig.

datasetset2017 <- read_csv("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/developer_survey_2017/survey_results_public.csv")
View(datasetset2017)

datasetset2018 <- read_csv("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/developer_survey_2018/survey_results_public.csv")
View(datasetset2018)

datasetset2019 <- read_csv("C:/Users/Steur/SynologyDrive/Leerlijnen/Data Science/Dataset/Bewerkte Data-set/developer_survey_2019/survey_results_public.csv")
View(datasetset2019)



# Geeft structure weer uit Dataset 
str(datasetset2019)

# Geeft het aantal rijen en kolommen weer uit de Dataset
dim(datasetset2019)

# Geeft alle Variable / kolom namen weer uit de Dataset. 
names(datasetset2019)

# Geeft een samenvatting van de Data
summary(datasetset2019)

#View plot van 2 kolommen / variables
plot(datasetset2019$Country, dataset$DevType) #Werkt nog niet correct.

# Aantal rijen dataset laten zien
nrow(datasetset2019)

#Aantal kolommen dataset laten zien
ncol(datasetset2019)
     
# 1e 10 rijen weergeven van dataset     
head(datasetset2019,  10)

# Bepaalde Variables / Kolommen weergeven vanuit de Dataset.
# Country geeft de plaats weer
# DevType geeft de jobtype weer. (System Administrator)
# WERKT NIET splitdataset<- read_csv("survey_results_public.csv")
  


# Kolommen splitsen ( Waarvan alles met Nederland eruit gefilterd wordt en word opgesplit in nummer van deelnemer + Devtype)
# FROM THIS > It's Only filtered in the rest of the SCRIPT FROM NETHERLANDS!

columnsplit2017 <- datasetset2017 %>%
  filter(Country == "Netherlands")%>%
  select(Respondent, DeveloperType)

columnsplit2018 <- datasetset2018 %>%
  filter(Country == "Netherlands")%>%
  select(Respondent, DevType)


columnsplit2019 <- datasetset2019 %>%
  filter(Country == "Netherlands")%>%
  select(Respondent, DevType)


View(columnsplit2017)
View(columnsplit2018)
View(columnsplit2019)


#Rename colum DeveloperType to DevType from the Survey 2017 to the same name as in the other Data Sets. 

columnsplit2017 <- rename(columnsplit2017, DevType = DeveloperType)
colnames(columnsplit2017)  


# Inhoud Kolommen splitten in correcte job / kolom namen.
jobfunctie2019 <- columnsplit2019 %>%
  separate(DevType, c("Academic researcher","Data or business analyst","Data scientist or machine learning specialist","Database administrator",
                      "Designer","Developer, backend","Developer, desktop or enterprise applications","Developer, embedded applications or devices",
                      "Developer, frontend","Developer, fullstack","Developer, game or graphics","Developer, mobile","Developer, QA or test","DevOps specialist",
                      "Educator","Engineer, data","Engineer, site reliability","Engineering manager","Marketing or sales professional","Product manager",
                      "Scientist", "Senior Executive (CSuite, VP, etc.)", "Student", "System administrator", "Other"), sep = ";")


# jobfunctie is nu uitgewerkt met de correcte Kolom namen vanuit varialble functie.
View(jobfunctie2019) 


# Geeft alle Variable / kolom namen weer uit de jobfunctie dataset. 
names(jobfunctie2019)

# Mooiste zou zijn om de job's ook ook nog onder elkaar te plaatsen in de kolom naam. AAAAAAAAAAAAAA

# rijen onder elkaar geplaatst. Nu per Job een nieuwe Variable maken.

separatejobrows2017 <- separate_rows(columnsplit2017,DevType,sep=";")

separatejobrows2018 <- separate_rows(columnsplit2018,DevType,sep=";")

separatejobrows2019 <- separate_rows(columnsplit2019,DevType,sep=";")

View(separatejobrows2017)
View(separatejobrows2018)
View(separatejobrows2019)


# SYSTEM ADMINISTRATOR voor afdeling Operations
# Onderstaande variable's worden aangemaakt vanuit jobsonderelkaar variable. Dit is een variable met alle Jobs vanuit Nederland.
# Nieuwe Variable aanmaken om alle Respondenten van System Administrator eruit te filteren en plaatsen in variable.
SystemAdministrator2017 <- filter(separatejobrows2017, DevType == "Systems administrator")
View(SystemAdministrator2017)

SystemAdministrator2018 <- filter(separatejobrows2018, DevType == "System administrator")
View(SystemAdministrator2018)

SystemAdministrator2019 <- filter(separatejobrows2019, DevType == "System administrator")
View(SystemAdministrator2019)


# DEVELOPER voor afdeling Developent
# Nieuwe Variable aanmaken om alle Respondenten van Development ( Developer, back-end )eruit te filteren en plaatsen in variable.
Development2017 <- filter(separatejobrows2017, DevType == "Desktop applications developer")
View(Development2017)

Development2018 <- filter(separatejobrows2018, DevType == "Back-end developer")
View(Development2018)

Development2019 <- filter(separatejobrows2019, DevType == "Developer, back-end")
View(Development2019)


# DATABASE ADMINISTRATOR voor afdeling DATA
# Nieuwe Variable aanmaken om alle Respondenten van Databas Administrators ( Database administrator )eruit te filteren en plaatsen in variable.
DatabaseAdministrator2017 <- filter(separatejobrows2017, DevType == "Database administrator" | DevType == " Database administrator")
View(DatabaseAdministrator2017)

DatabaseAdministrator2018 <- filter(separatejobrows2018, DevType == "Database administrator")
View(DatabaseAdministrator2018

DatabaseAdministrator2019 <- filter(separatejobrows2019, DevType == "Database administrator")
View(DatabaseAdministrator2019)



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

