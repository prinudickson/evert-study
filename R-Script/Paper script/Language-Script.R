# Installatie benodigde packages
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages('janitor')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("data.table")
install.packages('htmlwidgets')
install.packages("grid")

# Benodigde packages activeren.
library("readr")
library("dplyr")
library("tidyr")
#library("janitor")
library("ggplot2")
#library("reshape2")
library("data.table")
#library("htmlwidgets")
library("grid")

#Working directory----
getwd()

# Set Working Directory
setwd("C:/DataScience/dataset")


#Datasets inladen----------------------------############################################################################################

#data voor 2017
data_ini_2017 <- read_csv(file = "C:/DataScience/dataset/developer_survey_2017/survey_results_public.csv")
View(data_ini_2017)


#data voor 2018
data_ini_2018 <- read_csv(file = "C:/DataScience/dataset/developer_survey_2018/survey_results_public.csv")
View(data_ini_2018)

#data voor 2019
data_ini_2019 <- read_csv(file = "C:/DataScience/dataset/developer_survey_2019/survey_results_public.csv")
View(data_ini_2019)


#Overview krijgen van de datasets----------------------------############################################################################################
# Onderstaande geeft wat meer duidelijkheid / inzicht / feeling in de DataSets.
# Geeft structure weer uit Dataset 
str(data_ini_2019)

# Geeft het aantal rijen en kolommen weer uit de Dataset
dim(data_ini_2019)

# Geeft alle Variable / kolom namen weer uit de Dataset. 
names(data_ini_2019)

# Geeft een samenvatting van de Data
summary(data_ini_2019)


# Aantal rijen dataset laten zien
nrow(data_ini_2019)

#Aantal kolommen dataset laten zien
ncol(data_ini_2019)

# 1e 10 rijen weergeven van dataset     
head(data_ini_2019,  10)

# Laat de Dataset zien per kolom.
View(data_ini_2019)





#Data prepareren voor 2017----------------------------############################################################################################

#Als eerste is gestart met het selecteren van de vereiste kolommen die in kaart worden gebracht om gegevens voor 2017 voor te bereiden.

#Het valt op dat de vragen voor talen, frameworks en platforms gaan over wat een gebruiker tot dan toe heeft gewerkt en wat hij in de toekomst wil gaan gebruiken in de dataset van 2017.
#Dit is anders als de vragen uit de dataset 2018 + 2019. Daar is de vraag meer specifiek waar gevraagd wordt naar de taal, het platform of het framework waar de gebruiker daarvoor mee werkte.
#Voor het gemak gaan we uit van de kolommen die worden gebruikt om te begrijpen met welke taal gebruikers daarvoor mee werkte aan de taal, het framework of het platform waar ze voor 2017 aan werkte en wat ze volgend jaar willen.



data_2017_filter <- data_ini_2017 %>%
                    select(Respondent, Country, EmploymentStatus, DeveloperType, FormalEducation, HaveWorkedLanguage, WantWorkLanguage, 
                           HaveWorkedFramework, WantWorkFramework, HaveWorkedDatabase, WantWorkDatabase, HaveWorkedPlatform, WantWorkPlatform) %>%
                    rename(Employment = EmploymentStatus, DevType = DeveloperType, LanguageWorkedWith = HaveWorkedLanguage, LanguageDesireNextYear = WantWorkLanguage, 
                           FrameworkWorkedWith = HaveWorkedFramework, FrameworkDesireNextYear = WantWorkFramework, DatabaseWorkedWith = HaveWorkedDatabase,
                           DatabaseDesireNextYear = WantWorkDatabase, PlatformWorkedWith = HaveWorkedPlatform, PlatformDesireNextYear = WantWorkPlatform)


View(data_2017_filter)

# "I prefer not to say" verwijderd uit de dataset van 2017. Dit omdat deze optie niet aanwezig is in de dataset van 2018 en 2019
# Dit geeft een schonere dataset van 2017


data_2017_filter <- filter(data_2017_filter, Employment != "I prefer not to say")

View(data_2017_filter)





#Data transformatie 2017 

#DevType Analyse----

data_2017_devtype_ini <- data_2017_filter %>%
                         select(Respondent, DevType)
  
data_2017_devtype <- separate_rows(data_2017_devtype_ini,DevType,sep=";")

data_2017_devtype$DevType <- trimws(data_2017_devtype$DevType)


data_2017_devtype$devtype_clean <-  ifelse(data_2017_devtype$DevType == "Developer with a statistics or mathematics background", "Developer", 
                                                 ifelse(data_2017_devtype$DevType == "Mobile developer", "Developer",
                                                        ifelse(data_2017_devtype$DevType == "Web developer", "Developer",
                                                               ifelse(data_2017_devtype$DevType == "Desktop applications developer", "Developer",
                                                                      ifelse(data_2017_devtype$DevType == "Embedded applications/devices developer", "Developer",
                                                                             ifelse(data_2017_devtype$DevType == "Machine learning specialist", "Developer",
                                                                                    ifelse(data_2017_devtype$DevType == "Data scientist", "Developer",
                                                                                           ifelse(data_2017_devtype$DevType == "Graphics programming", "Developer",
                                                                                                  ifelse(data_2017_devtype$DevType == "Systems administrator", "Sys Admin",
                                                                                                         ifelse(data_2017_devtype$DevType == "Database administrator", "Db Admin",
                                                                                                                ifelse(data_2017_devtype$DevType == "DevOps specialist", "DevOps", "Others" )))))))))))

users_by_year_2017 <- data_2017_devtype %>%
                      summarize(overall_users = n_distinct(Respondent))

devtype_by_year_2017 <- data_2017_devtype %>%
                        group_by(devtype_clean) %>%
                      summarize(overall_users = n_distinct(Respondent))


View(devtype_by_year_2017)


#Language Analyse----

data_2017_language_ini <- data_2017_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_language <- separate_rows(data_2017_language_ini, LanguageWorkedWith, sep=";")

data_2017_language$LanguageWorkedWith <- trimws(data_2017_language$LanguageWorkedWith)

data_2017_language$LanguageWorkedWith <- ifelse(data_2017_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_language$LanguageWorkedWith)

data_2017_language$Year <- "2017"


View(data_2017_language)


#Developer Language Analyse----

data_2017_developer_language_ini <- data_2017_filter %>%
                                    filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Developer")) %>%
                                    select(Respondent, LanguageWorkedWith, Country)

data_2017_developer_language <- separate_rows(data_2017_developer_language_ini, LanguageWorkedWith, sep=";")

data_2017_developer_language$LanguageWorkedWith <- trimws(data_2017_developer_language$LanguageWorkedWith)

data_2017_developer_language$LanguageWorkedWith <- ifelse(data_2017_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_developer_language$LanguageWorkedWith)

data_2017_developer_language$Year <- "2017"

View(data_2017_developer_language)


#System Administrator Language Analyse----

data_2017_sys_admin_language_ini <- data_2017_filter %>%
  filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_sys_admin_language <- separate_rows(data_2017_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2017_sys_admin_language$LanguageWorkedWith <- trimws(data_2017_sys_admin_language$LanguageWorkedWith)

data_2017_sys_admin_language$LanguageWorkedWith <- ifelse(data_2017_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_sys_admin_language$LanguageWorkedWith)

data_2017_sys_admin_language$Year <- "2017"

View(data_2017_sys_admin_language)


#Database Administrator Language Analyse----

data_2017_db_admin_language_ini <- data_2017_filter %>%
  filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_db_admin_language <- separate_rows(data_2017_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2017_db_admin_language$LanguageWorkedWith <- trimws(data_2017_db_admin_language$LanguageWorkedWith)

data_2017_db_admin_language$LanguageWorkedWith <- ifelse(data_2017_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_db_admin_language$LanguageWorkedWith)

data_2017_db_admin_language$Year <- "2017"

View(data_2017_db_admin_language)



#Data prepareren voor 2018---------------------############################################################################################

#Selecteert de benodigde kolommen voor verdere analyse van dataset 2018
# De gegevens van dataset 2018 zijn niet veel getransformeerd of opgeschoond, omdat dit werd gebruikt als basis voor het transformeren van de gegevens
# van de dataset uit 2017 en 2019
# Vanuit deze dataset is gewerkt om 2017 en 2019 in hetzelfde formaat te krijgen als 2018

data_2018_filter <- data_ini_2018 %>%
                    select(Respondent, Country, Employment, DevType, FormalEducation, LanguageWorkedWith, LanguageDesireNextYear, 
                           FrameworkWorkedWith, FrameworkDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear)

View(data_2018_filter)

#DevType Analyse----

data_2018_devtype_ini <- data_2018_filter %>%
  select(Respondent, DevType)

data_2018_devtype <- separate_rows(data_2018_devtype_ini,DevType,sep=";")

data_2018_devtype$DevType <- trimws(data_2018_devtype$DevType)

data_2018_devtype$devtype_clean <-  ifelse(data_2018_devtype$DevType == "Back-end developer", "Developer", 
                                           ifelse(data_2018_devtype$DevType == "Data scientist or machine learning specialist", "Developer",
                                                  ifelse(data_2018_devtype$DevType == "Desktop or enterprise applications developer", "Developer",
                                                         ifelse(data_2018_devtype$DevType == "Embedded applications or devices developer", "Developer",
                                                                ifelse(data_2018_devtype$DevType == "Full-stack developer", "Developer",
                                                                       ifelse(data_2018_devtype$DevType == "Mobile developer", "Developer",
                                                                              ifelse(data_2018_devtype$DevType == "Data scientist", "Developer",
                                                                                     ifelse(data_2018_devtype$DevType == "Game or graphics developer", "Developer",
                                                                                            ifelse(data_2018_devtype$DevType == "Front-end developer", "Developer",
                                                                                                  ifelse(data_2018_devtype$DevType == "System administrator", "Sys Admin",
                                                                                                         ifelse(data_2018_devtype$DevType == "Database administrator", "Db Admin",
                                                                                                                ifelse(data_2018_devtype$DevType == "DevOps specialist", "DevOps", "Others" ))))))))))))


users_by_year_2018 <- data_2018_devtype %>%
  summarize(overall_users = n_distinct(Respondent))

devtype_by_year_2018 <- data_2018_devtype %>%
  group_by(devtype_clean) %>%
  summarize(overall_users = n_distinct(Respondent))

View(devtype_by_year_2018)

#Language Analyse----

data_2018_language_ini <- data_2018_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_language <- separate_rows(data_2018_language_ini, LanguageWorkedWith, sep=";")

data_2018_language$LanguageWorkedWith <- trimws(data_2018_language$LanguageWorkedWith)

data_2018_language$LanguageWorkedWith <- ifelse(data_2018_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_language$LanguageWorkedWith)

data_2018_language$Year <- "2018"

View(data_2018_language)


#Developer Language Analyse----

data_2018_developer_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Developer")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_developer_language <- separate_rows(data_2018_developer_language_ini, LanguageWorkedWith, sep=";")

data_2018_developer_language$LanguageWorkedWith <- trimws(data_2018_developer_language$LanguageWorkedWith)

data_2018_developer_language$LanguageWorkedWith <- ifelse(data_2018_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_developer_language$LanguageWorkedWith)

data_2018_developer_language$Year <- "2018"

View(data_2018_developer_language)


#System Administrator Language Analyse----

data_2018_sys_admin_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_sys_admin_language <- separate_rows(data_2018_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2018_sys_admin_language$LanguageWorkedWith <- trimws(data_2018_sys_admin_language$LanguageWorkedWith)

data_2018_sys_admin_language$LanguageWorkedWith <- ifelse(data_2018_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_sys_admin_language$LanguageWorkedWith)

data_2018_sys_admin_language$Year <- "2018"

View(data_2018_sys_admin_language)


#Database Administrator Language Analyse----

data_2018_db_admin_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_db_admin_language <- separate_rows(data_2018_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2018_db_admin_language$LanguageWorkedWith <- trimws(data_2018_db_admin_language$LanguageWorkedWith)

data_2018_db_admin_language$LanguageWorkedWith <- ifelse(data_2018_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_db_admin_language$LanguageWorkedWith)

data_2018_db_admin_language$Year <- "2018"

View(data_2018_db_admin_language)


#Data prepareren voor 2019-----------------------############################################################################################

#Selecteer de kolommen----

data_2019_filter <- data_ini_2019 %>%
                    select(Respondent, Country, Employment, DevType, EdLevel, LanguageWorkedWith, LanguageDesireNextYear, 
                           WebFrameWorkedWith, WebFrameDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear) %>%
                    rename(FormalEducation = EdLevel, FrameworkWorkedWith = WebFrameWorkedWith, FrameworkDesireNextYear = WebFrameDesireNextYear )

View(data_2019_filter)

#DevType Analyse----

data_2019_devtype_ini <- data_2019_filter %>%
  select(Respondent, DevType)

data_2019_devtype <- separate_rows(data_2019_devtype_ini,DevType,sep=";")

data_2019_devtype$DevType <- trimws(data_2019_devtype$DevType)

data_2019_devtype$devtype_clean <-  ifelse(data_2019_devtype$DevType == "Developer, desktop or enterprise applications", "Developer", 
                                           ifelse(data_2019_devtype$DevType == "Developer, full-stack", "Developer",
                                                  ifelse(data_2019_devtype$DevType == "Developer, QA or test", "Developer",
                                                         ifelse(data_2019_devtype$DevType == "Developer, embedded applications or devices", "Developer",
                                                                ifelse(data_2019_devtype$DevType == "Developer, game or graphics", "Developer",
                                                                       ifelse(data_2019_devtype$DevType == "Data scientist or machine learning specialist", "Developer",
                                                                              ifelse(data_2019_devtype$DevType == "Developer, back-end", "Developer",
                                                                                     ifelse(data_2019_devtype$DevType == "Developer, front-end", "Developer",
                                                                                            ifelse(data_2019_devtype$DevType == "Developer, mobile", "Developer",
                                                                                                   ifelse(data_2019_devtype$DevType == "System administrator", "Sys Admin",
                                                                                                          ifelse(data_2019_devtype$DevType == "Database administrator", "Db Admin",
                                                                                                                 ifelse(data_2019_devtype$DevType == "DevOps specialist", "DevOps", "Others" ))))))))))))


users_by_year_2019 <- data_2019_devtype %>%
  summarize(overall_users = n_distinct(Respondent))

devtype_by_year_2019 <- data_2019_devtype %>%
  group_by(devtype_clean) %>%
  summarize(overall_users = n_distinct(Respondent))

View(devtype_by_year_2019)

#Language Analyse----

data_2019_language_ini <- data_2019_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_language <- separate_rows(data_2019_language_ini, LanguageWorkedWith, sep=";")

data_2019_language$LanguageWorkedWith <- trimws(data_2019_language$LanguageWorkedWith)

data_2019_language$LanguageWorkedWith <- ifelse(data_2019_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_language$LanguageWorkedWith)

data_2019_language$Year <- "2019"

View(data_2019_language)

#Developer Language Analyse----

data_2019_developer_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Developer")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_developer_language <- separate_rows(data_2019_developer_language_ini, LanguageWorkedWith, sep=";")

data_2019_developer_language$LanguageWorkedWith <- trimws(data_2019_developer_language$LanguageWorkedWith)

data_2019_developer_language$LanguageWorkedWith <- ifelse(data_2019_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_developer_language$LanguageWorkedWith)

data_2019_developer_language$Year <- "2019"

View(data_2019_developer_language)

#System Administrator Language Analyse----

data_2019_sys_admin_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_sys_admin_language <- separate_rows(data_2019_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2019_sys_admin_language$LanguageWorkedWith <- trimws(data_2019_sys_admin_language$LanguageWorkedWith)

data_2019_sys_admin_language$LanguageWorkedWith <- ifelse(data_2019_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_sys_admin_language$LanguageWorkedWith)

data_2019_sys_admin_language$Year <- "2019"

View(data_2019_sys_admin_language)

#Database Administrator Language Analyse----

data_2019_db_admin_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_db_admin_language <- separate_rows(data_2019_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2019_db_admin_language$LanguageWorkedWith <- trimws(data_2019_db_admin_language$LanguageWorkedWith)

data_2019_db_admin_language$LanguageWorkedWith <- ifelse(data_2019_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_db_admin_language$LanguageWorkedWith)

data_2019_db_admin_language$Year <- "2019"

View(data_2019_db_admin_language)



#Plot van distributie populaire programmeertalen wereldwijd------############################################################################################
# Combineert alle 3 Language per jaar tot 1 variabele
data_language <- rbind(data_2017_language, data_2018_language, data_2019_language)

View(data_language)

# Verwijderd alle NA antwoorden uit kolom LanguageWorkedWith
data_language_clean<- na.omit(data_language, cols = "LanguageWorkedWith")

View(data_language_clean)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal!
data_language_dt <- data_language_clean %>%
                    group_by(LanguageWorkedWith, Year) %>%
                    summarize(users = n_distinct(Respondent))

View(data_language_dt)

# Deze variabele geeft een overall overzicht van het aantal users PER jaar uit de data_language variabele. 
# Waarin alle respondenten verwerkt zijn.
users_by_year <- data_language %>%
                  group_by(Year) %>%
                  summarize(overall_users = n_distinct(Respondent))

View(users_by_year)


# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
languages_presence <- data_language %>%
                        group_by(LanguageWorkedWith) %>%
                        summarize(presence = n_distinct(Year))

View(languages_presence)

#Voegt beide varialele toe tot 1 compleet variabele.
data_language_dt_percent <- merge(data_language_dt, users_by_year)

View(data_language_dt_percent)

# Voegt de kolom precense toe
data_language_dt_percent <- merge(data_language_dt_percent, languages_presence)

View(data_language_dt_percent)

# Filtert alleen de talen die alle jaren voorkwamen.
data_language_dt_percent <- subset(data_language_dt_percent, data_language_dt_percent$presence == 3 | data_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_language_dt_percent)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_language_dt_percent$distribution <- (data_language_dt_percent$users)*100/data_language_dt_percent$overall_users

View(data_language_dt_percent)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_language_dt_percent$Year <- as.integer(data_language_dt_percent$Year)

View(data_language_dt_percent)

#Lijn grafiek met de verandering van programmeer talen wereldwijd van 2017 - 2019 1
data_language_ggplot <- ggplot(data_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_language_ggplot

#Lijn grafiek met de verandering van programmeer talen wereldwijd van 2017 - 2019 2
data_language_ggplot_grid = ggplot(data_language_dt_percent) + 
                                       geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
                                       geom_text(data = subset(data_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
                                       scale_colour_discrete(guide = 'none')  +    
                                       theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout <- ggplotGrob(data_language_ggplot_grid)
data_language_ggplot_grid_layout$layout$clip[data_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout)


#Plot van distributie populaire programmeertalen wereldwijd voor Developers------############################################################################################

# Combineert alle 3 developer language per jaar tot 1 variabele
data_developer_language <- rbind(data_2017_developer_language, data_2018_developer_language, data_2019_developer_language)

View(data_developer_language)

# Verwijderd alle NA antwoorden uit kolom LanguageWorkedWith
data_developer_language_clean<- na.omit(data_developer_language, cols = "LanguageWorkedWith")

View(data_developer_language_clean)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal!
data_developer_language_dt <- data_developer_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(data_developer_language_dt)

# Deze variabele geeft een overall overzicht van het aantal users PER jaar uit de data_language variabele. 
# Waarin alle respondenten verwerkt zijn. 
developers_by_year <- data_developer_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(developers_by_year)

# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
developer_languages_presence <- data_developer_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(developer_languages_presence)

#Voegt beide varialele toe tot 1 compleet variabele.
data_developer_language_dt_percent <- merge(data_developer_language_dt, users_by_year)

View(data_developer_language_dt_percent)

# Voegt de kolom precense toe
data_developer_language_dt_percent <- merge(data_developer_language_dt_percent, developer_languages_presence)

View(data_developer_language_dt_percent)

# Filtert alleen de talen die alle jaren voorkwamen.
data_developer_language_dt_percent <- subset(data_developer_language_dt_percent, data_developer_language_dt_percent$presence == 3 | data_developer_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_developer_language_dt_percent)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft
data_developer_language_dt_percent$distribution <- (data_developer_language_dt_percent$users)*100/data_developer_language_dt_percent$overall_users

View(data_developer_language_dt_percent)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_developer_language_dt_percent$Year <- as.integer(data_developer_language_dt_percent$Year)

View(data_developer_language_dt_percent)

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor developers van 2017 - 2019 1
data_developer_language_ggplot <- ggplot(data_developer_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_developer_language_ggplot

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor developers van 2017 - 2019 2
data_developer_language_ggplot_grid = ggplot(data_developer_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_developer_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_developer_language_ggplot_grid_layout <- ggplotGrob(data_developer_language_ggplot_grid)
data_developer_language_ggplot_grid_layout$layout$clip[data_developer_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_developer_language_ggplot_grid_layout)


#Plot van distributie populaire programmeertalen wereldwijd voor System Admins------############################################################################################

# Combineert alle 3 Language per jaar tot 1 variabele
data_sys_admin_language <- rbind(data_2017_sys_admin_language, data_2018_sys_admin_language, data_2019_sys_admin_language)

View(data_sys_admin_language)

# Verwijderd alle NA antwoorden uit kolom LanguageWorkedWith
data_sys_admin_language_clean<- na.omit(data_sys_admin_language, cols = "LanguageWorkedWith")

View(data_sys_admin_language_clean)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal! 
data_sys_admin_language_dt <- data_sys_admin_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(data_sys_admin_language_dt)

# Deze variabele geeft een overall overzicht van het aantal users PER jaar uit de data_language variabele. 
# Waarin alle respondenten verwerkt zijn.
sys_admin_by_year <- data_sys_admin_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(sys_admin_by_year)

# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
sys_admin_languages_presence <- data_sys_admin_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(sys_admin_languages_presence)

#Voegt beide varialele toe tot 1 compleet variabele.
data_sys_admin_language_dt_percent <- merge(data_sys_admin_language_dt, sys_admin_by_year)

View(data_sys_admin_language_dt_percent)

# Voegt de kolom precense toe
data_sys_admin_language_dt_percent <- merge(data_sys_admin_language_dt_percent, sys_admin_languages_presence)

View(data_sys_admin_language_dt_percent)

# Filtert alleen de talen die alle jaren voorkwamen. 
data_sys_admin_language_dt_percent <- subset(data_sys_admin_language_dt_percent, data_sys_admin_language_dt_percent$presence == 3 | data_sys_admin_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_sys_admin_language_dt_percent)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_sys_admin_language_dt_percent$distribution <- (data_sys_admin_language_dt_percent$users)*100/data_sys_admin_language_dt_percent$overall_users

View(data_sys_admin_language_dt_percent)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_sys_admin_language_dt_percent$Year <- as.integer(data_sys_admin_language_dt_percent$Year)

View(data_sys_admin_language_dt_percent)

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor System Administrator van 2017 - 2019 1
data_sys_admin_language_ggplot <- ggplot(data_sys_admin_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_sys_admin_language_ggplot


# Legenda wordt bepaald vanuit kolom LanguageWorkedWith. Dit zodat elke programmeer taal in de visualisatie verwerkt wordt.

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor System Administrator van 2017 - 2019 2
data_sys_admin_language_ggplot_grid = ggplot(data_sys_admin_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_sys_admin_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_sys_admin_language_ggplot_grid_layout <- ggplotGrob(data_sys_admin_language_ggplot_grid)
data_sys_admin_language_ggplot_grid_layout$layout$clip[data_sys_admin_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_sys_admin_language_ggplot_grid_layout)


#Plot van distributie populaire programmeertalen wereldwijd voor Database Administrators------############################################################################################

# Combineert alle 3 Language per jaar tot 1 variabele
data_db_admin_language <- rbind(data_2017_db_admin_language, data_2018_db_admin_language, data_2019_db_admin_language)

View(data_db_admin_language)

# Verwijderd alle NA antwoorden uit kolom LanguageWorkedWith
data_db_admin_language_clean<- na.omit(data_db_admin_language, cols = "LanguageWorkedWith")

View(data_db_admin_language_clean)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal! 
data_db_admin_language_dt <- data_db_admin_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(data_db_admin_language_dt)

# Deze variabele geeft een overall overzicht van het aantal users PER jaar uit de data_language variabele. 
# Waarin alle respondenten verwerkt zijn. 
db_admin_by_year <- data_db_admin_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(db_admin_by_year)

# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
db_admin_languages_presence <- data_db_admin_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(db_admin_languages_presence)

#Voegt beide varialele toe tot 1 compleet variabele.
data_db_admin_language_dt_percent <- merge(data_db_admin_language_dt, db_admin_by_year)

View(data_db_admin_language_dt_percent)

# Voegt de kolom precense toe
data_db_admin_language_dt_percent <- merge(data_db_admin_language_dt_percent, db_admin_languages_presence)

View(data_db_admin_language_dt_percent)

# Filtert alleen de talen die alle jaren voorkwamen.
data_db_admin_language_dt_percent <- subset(data_db_admin_language_dt_percent, data_db_admin_language_dt_percent$presence == 3 | data_db_admin_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_db_admin_language_dt_percent)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_db_admin_language_dt_percent$distribution <- (data_db_admin_language_dt_percent$users)*100/data_db_admin_language_dt_percent$overall_users

View(data_db_admin_language_dt_percent)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_db_admin_language_dt_percent$Year <- as.integer(data_db_admin_language_dt_percent$Year)

View(data_db_admin_language_dt_percent)

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor Database Administrator van 2017 - 2019 1
data_db_admin_language_ggplot <- ggplot(data_db_admin_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_db_admin_language_ggplot


# Legenda wordt bepaald vanuit kolom LanguageWorkedWith. Dit zodat elke programmeer taal in de visualisatie verwerkt wordt.

#Lijn grafiek met de verandering van programmeer talen wereldwijd voor Database Administrator van 2017 - 2019 2
data_db_admin_language_ggplot_grid = ggplot(data_db_admin_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_db_admin_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_db_admin_language_ggplot_grid_layout <- ggplotGrob(data_db_admin_language_ggplot_grid)
data_db_admin_language_ggplot_grid_layout$layout$clip[data_db_admin_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_db_admin_language_ggplot_grid_layout)


#Referentie

#Lijn grafiek generator----
#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

# ANALYSE PROGRAMEERTALEN NEDERLAND  ############################################################################################

#Maakt een lijngrafiek van de distributie programmeertalen voor Nederland.

# Geeft alle respondenten weer vanuit Nederland.
data_language_clean_nl <- data_language_clean %>%
                          filter(Country == "Netherlands")

View(data_language_clean_nl)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal!
data_language_dt_nl <- data_language_clean_nl %>%
                        group_by(LanguageWorkedWith, Year) %>%
                        summarize(users = n_distinct(Respondent))

View(data_language_dt_nl)

# Totaal respondenten vanuit Netherlands per jaartal
users_by_year_nl <- data_language %>%
                    filter(Country == "Netherlands")%>%
                    group_by(Year) %>%
                    summarize(overall_users = n_distinct(Respondent))

View(users_by_year_nl)

# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
languages_presence_nl <- data_language  %>%
                          filter(Country == "Netherlands")%>%
                          group_by(LanguageWorkedWith) %>%
                          summarize(presence = n_distinct(Year))

View(languages_presence_nl)


#Voegt beide varialele toe tot 1 compleet variabele.
data_language_dt_percent_nl <- merge(data_language_dt_nl, users_by_year_nl)

View(data_language_dt_percent_nl)

# Voegt de kolom precense toe
data_language_dt_percent_nl <- merge(data_language_dt_percent_nl, languages_presence_nl)

View(data_language_dt_percent_nl)

# Filtert alleen de talen die alle jaren voorkwamen.
data_language_dt_percent_nl <- subset(data_language_dt_percent_nl, data_language_dt_percent_nl$presence == 3)

View(data_language_dt_percent_nl)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_language_dt_percent_nl$distribution <- (data_language_dt_percent_nl$users)*100/data_language_dt_percent_nl$overall_users

View(data_language_dt_percent_nl)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_language_dt_percent_nl$Year <- as.integer(data_language_dt_percent_nl$Year)

View(data_language_dt_percent_nl)

#Lijn grafiek met de verandering van programmeer talen Nederland  van 2017 - 2019 1
data_language_ggplot_nl <- ggplot(data_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_language_ggplot_nl


# Legenda wordt bepaald vanuit kolom LanguageWorkedWith. Dit zodat elke programmeer taal in de visualisatie verwerkt wordt.


#Lijn grafiek met de verandering van programmeer talen Nederland  van 2017 - 2019 2
data_language_ggplot_grid_nl = ggplot(data_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout_nl <- ggplotGrob(data_language_ggplot_grid_nl)
data_language_ggplot_grid_layout_nl$layout$clip[data_language_ggplot_grid_layout_nl$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout_nl)



#Plot van distributie populaire programmeertalen Nederland voor Developers------############################################################################################

# Geeft alle respondenten weer van Developers vanuit Nederland.
data_developer_language_clean_nl <- data_developer_language_clean %>%
                                                 filter(Country == "Netherlands")

View(data_developer_language_clean_nl)


# Geeft het aantal respondenten weer per programmeertaal per jaar voor Developers. 
data_developer_language_dt_nl <- data_developer_language_clean_nl %>%
                                    group_by(LanguageWorkedWith, Year) %>%
                                    summarize(users = n_distinct(Respondent))

View(data_developer_language_dt_nl)

# Totaal respondenten Developers vanuit Nederland per jaartal 
developers_by_year_nl <- data_developer_language %>%
                          filter(Country == "Netherlands") %>%
                          group_by(Year) %>%
                          summarize(overall_users = n_distinct(Respondent))

View(developers_by_year_nl)

# Aantal x gevraagde taal per dataset. Dus 3x betekend dat de programmeer taal per jaar voorkomt. 
developer_languages_presence_nl <- data_developer_language %>%
                                    filter(Country == "Netherlands") %>%
                                    group_by(LanguageWorkedWith) %>%
                                    summarize(presence = n_distinct(Year))

View(developer_languages_presence_nl)


#Voegt beide varialele toe tot 1 compleet variabele.
data_developer_language_dt_percent_nl <- merge(data_developer_language_dt_nl, developers_by_year_nl)

View(data_developer_language_dt_percent_nl)

# Voegt de kolom precense toe
data_developer_language_dt_percent_nl <- merge(data_developer_language_dt_percent_nl, developer_languages_presence_nl)

View(data_developer_language_dt_percent_nl)

# Filtert alleen de talen die alle jaren voorkwamen. 
data_developer_language_dt_percent_nl <- subset(data_developer_language_dt_percent_nl, data_developer_language_dt_percent_nl$presence == 3 | data_developer_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_developer_language_dt_percent_nl)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_developer_language_dt_percent_nl$distribution <- (data_developer_language_dt_percent_nl$users)*100/data_developer_language_dt_percent_nl$overall_users

View(data_developer_language_dt_percent_nl)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_developer_language_dt_percent_nl$Year <- as.integer(data_developer_language_dt_percent_nl$Year)

View(data_developer_language_dt_percent_nl)



#Lijn grafiek met de verandering van programmeer talen voor Developers in Nederland  van 2017 - 2019 1
data_developer_language_dt_percent_nl_ggplot <- ggplot(data_developer_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_developer_language_dt_percent_nl_ggplot

#Lijn grafiek met de verandering van programmeer talen voor Developers in Nederland  van 2017 - 2019 2
data_developer_language_nl_ggplot_grid = ggplot(data_developer_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_developer_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_developer_language_nl_ggplot_grid_layout <- ggplotGrob(data_developer_language_nl_ggplot_grid)
data_developer_language_nl_ggplot_grid_layout$layout$clip[data_developer_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_developer_language_nl_ggplot_grid_layout)





#Plot van distributie populaire programmeertalen Nederland voor System Administrators------############################################################################################

# Geeft alle respondenten weer van Developers vanuit Nederland.
data_sys_admin_language_clean_nl <- data_sys_admin_language_clean %>%
  filter(Country == "Netherlands")

View(data_sys_admin_language_clean_nl)

# Geeft het aantal respondenten weer per programmeertaal per jaar voor Developers. 
data_sys_admin_language_dt_nl <- data_sys_admin_language_clean_nl %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(data_sys_admin_language_dt_nl)

# Totaal respondenten System Administrators vanuit Nederland per jaartal
sys_admin_by_year_nl <- data_sys_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(sys_admin_by_year_nl)

# Aantal x gevraagde taal per dataset. Dus 3x betekend dat de programmeer taal per jaar voorkomt. 
sys_admin_languages_presence_nl <- data_sys_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(sys_admin_languages_presence_nl)

#Voegt beide varialele toe tot 1 compleet variabele.
data_sys_admin_language_dt_percent_nl <- merge(data_sys_admin_language_dt_nl, sys_admin_by_year_nl)

View(data_sys_admin_language_dt_percent_nl)

# Voegt de kolom precense toe
data_sys_admin_language_dt_percent_nl <- merge(data_sys_admin_language_dt_percent_nl, sys_admin_languages_presence_nl)

View(data_sys_admin_language_dt_percent_nl)

# Filtert alleen de talen die alle jaren voorkwamen. 
data_sys_admin_language_dt_percent_nl <- subset(data_sys_admin_language_dt_percent_nl, data_sys_admin_language_dt_percent_nl$presence == 3 | data_sys_admin_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_sys_admin_language_dt_percent_nl)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_sys_admin_language_dt_percent_nl$distribution <- (data_sys_admin_language_dt_percent_nl$users)*100/data_sys_admin_language_dt_percent_nl$overall_users

View(data_sys_admin_language_dt_percent_nl)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_sys_admin_language_dt_percent_nl$Year <- as.integer(data_sys_admin_language_dt_percent_nl$Year)

View(data_sys_admin_language_dt_percent_nl)



#Lijn grafiek met de verandering van programmeer talen voor System Administrators in Nederland  van 2017 - 2019 1
data_sys_admin_language_dt_percent_nl_ggplot <- ggplot(data_sys_admin_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_sys_admin_language_dt_percent_nl_ggplot


#Lijn grafiek met de verandering van programmeer talen voor System Administrators in Nederland  van 2017 - 2019 2
data_sys_admin_language_nl_ggplot_grid = ggplot(data_sys_admin_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_sys_admin_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_sys_admin_language_nl_ggplot_grid_layout <- ggplotGrob(data_sys_admin_language_nl_ggplot_grid)
data_sys_admin_language_nl_ggplot_grid_layout$layout$clip[data_sys_admin_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_sys_admin_language_nl_ggplot_grid_layout)




#Plot van distributie populaire programmeertalen Nederland voor Database Administrors------############################################################################################

# Geeft alle respondenten weer van Database Administrators vanuit Nederland.
data_db_admin_language_clean_nl <- data_db_admin_language_clean %>%
  filter(Country == "Netherlands")

View(data_db_admin_language_clean_nl)

# Geeft het aantal respondenten weer per programmeertaal per jaar voor Database Administrators. 
data_db_admin_language_dt_nl <- data_db_admin_language_clean_nl %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(data_db_admin_language_dt_nl)

# Totaal respondenten Database Administrators vanuit Nederland per jaartal
db_admin_by_year_nl <- data_db_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(db_admin_by_year_nl)

# Aantal x gevraagde taal per dataset. Dus 3x betekend dat de programmeer taal per jaar voorkomt. 
db_admin_languages_presence_nl <- data_db_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(db_admin_languages_presence_nl)

#Voegt beide varialele toe tot 1 compleet variabele.
data_db_admin_language_dt_percent_nl <- merge(data_db_admin_language_dt_nl, db_admin_by_year_nl)

View(data_db_admin_language_dt_percent_nl)

# Voegt de kolom precense toe
data_db_admin_language_dt_percent_nl <- merge(data_db_admin_language_dt_percent_nl, db_admin_languages_presence_nl)

View(data_db_admin_language_dt_percent_nl)

# Filtert alleen de talen die alle jaren voorkwamen.
data_db_admin_language_dt_percent_nl <- subset(data_db_admin_language_dt_percent_nl, data_db_admin_language_dt_percent_nl$presence == 3 | data_db_admin_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

View(data_db_admin_language_dt_percent_nl)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_db_admin_language_dt_percent_nl$distribution <- (data_db_admin_language_dt_percent_nl$users)*100/data_db_admin_language_dt_percent_nl$overall_users

View(data_db_admin_language_dt_percent_nl)

# Maakt deze kolom als waarde getallen (numerich/integer)
data_db_admin_language_dt_percent_nl$Year <- as.integer(data_db_admin_language_dt_percent_nl$Year)

View(data_db_admin_language_dt_percent_nl)




#Lijn grafiek met de verandering van programmeer talen voor Database Administrators in Nederland  van 2017 - 2019 1
data_db_admin_language_dt_percent_nl_ggplot <- ggplot(data_db_admin_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

data_db_admin_language_dt_percent_nl_ggplot


#Lijn grafiek met de verandering van programmeer talen voor Database Administrators in Nederland  van 2017 - 2019 2
data_db_admin_language_nl_ggplot_grid = ggplot(data_db_admin_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_db_admin_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_db_admin_language_nl_ggplot_grid_layout <- ggplotGrob(data_db_admin_language_nl_ggplot_grid)
data_db_admin_language_nl_ggplot_grid_layout$layout$clip[data_db_admin_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_db_admin_language_nl_ggplot_grid_layout)






#Top 5 programmeer talen voor Developers------############################################################################################
# Alleen dataset 2019 is hiervoor gebruikt.
# Staafdiagram met de top 5 programmeer talen voor Developers.

# Filters alle programmeer talen uit het jaar 2019 uit de Developers analyse hierboven
developers_language_top5 <- data_developer_language_dt_percent %>%
                            filter(Year == 2019)

View(developers_language_top5)

#
developers_language_top5 <- arrange(developers_language_top5, desc(users)) %>%
                              mutate(rank = 1:nrow(developers_language_top5)) %>%
                              filter(rank < 6)

View(developers_language_top5)

# Staafdiagram met de Top 5 programemertalen van Developers uit 2019 wereldwijd
developers_language_top5_ggplot <-ggplot(developers_language_top5, aes(LanguageWorkedWith, users))
developers_language_top5_ggplot + geom_bar(stat = "identity", aes(fill = LanguageWorkedWith)) + theme_bw()



#Top 5 programmeer talen voor System Administrators------############################################################################################
# Alleen dataset 2019 is hiervoor gebruikt.
# Filters alle programmeer talen uit het jaar 2019 uit de System Administrators analyse hierboven
sys_admin_language_top5 <- data_sys_admin_language_dt_percent %>%
  filter(Year == 2019)

View(sys_admin_language_top5)

#
sys_admin_language_top5 <- arrange(sys_admin_language_top5, desc(users)) %>%
  mutate(rank = 1:nrow(sys_admin_language_top5)) %>%
  filter(rank < 6)

View(sys_admin_language_top5)

# Staafdiagram met de Top 5 programemertalen van System Administrators uit 2019 wereldwijd
sys_admin_language_top5_ggplot <-ggplot(sys_admin_language_top5, aes(LanguageWorkedWith, users))
sys_admin_language_top5_ggplot + geom_bar(stat = "identity", aes(fill = LanguageWorkedWith)) + theme_bw()


#Top 5 programmeer talen voor Database Administrators------############################################################################################
# Alleen dataset 2019 is hiervoor gebruikt.
# Filters alle programmeer talen uit het jaar 2019 uit de Database Administrators analyse hierboven
db_admin_language_top5 <- data_db_admin_language_dt_percent %>%
  filter(Year == 2019)

View(db_admin_language_top5)


#
db_admin_language_top5 <- arrange(db_admin_language_top5, desc(users)) %>%
  mutate(rank = 1:nrow(db_admin_language_top5)) %>%
  filter(rank < 6)

View(db_admin_language_top5)

# Staafdiagram met de Top 5 programmeertalen van Database Administrators uit 2019 wereldwijd
db_admin_language_top5_ggplot <-ggplot(db_admin_language_top5, aes(LanguageWorkedWith, users))
db_admin_language_top5_ggplot + geom_bar(stat = "identity", aes(fill = LanguageWorkedWith)) + theme_bw()



#Respondents Stackoverflow Elk jaar------############################################################################################
#Maakt een staafdiagram met de aantal respondents van de StackoverFlow survey elk jaar.
users_by_year_ggplot <- ggplot(users_by_year, aes(Year, overall_users))
users_by_year_ggplot + geom_bar(stat = "identity", aes(fill = Year)) + theme_bw()



