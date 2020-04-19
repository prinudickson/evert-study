#Install the required packages----
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages('janitor')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("data.table")
install.packages('htmlwidgets')
install.packages("grid")

#Load the required packages----
library("readr")
library("dplyr")
library("tidyr")
# library("janitor")
library("ggplot2")
# library("reshape2")
# library("data.table")
# library("htmlwidgets")
library("grid")


# Get Working directory----
getwd()

# Set Working Directory
setwd("C:/Users/esteur002/Documents/GitHub/evert-study")
setwd("C:/Users/evert/OneDrive/Documenten/GitHub/evert-study")


#data for 2017
data_ini_2017 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2017/survey_results_public.csv")
View(data_ini_2017)

#data for 2018
data_ini_2018 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2018/survey_results_public.csv")
View(data_ini_2018)

#data for 2019
data_ini_2019 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2019/survey_results_public.csv")
View(data_ini_2019)

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




#Prepare the data for 2017----

#Selecting the required columnms which will be mapped to prepare the 2017 data.----
#Data Mapping 2017 1----
#Note that during 2017 the questions for languages, frameworks and platforms are on what a user has worked till then and what they want to use in the future. 
#This is different from 2018 and 2019 where the question is specific and asks about the language, platform or framework the user worked during 
#that specific and year and what they want to work on the next year. For the ease of use we are assuming the columns used to understand what the users have 
#worked with till then to the language, framework or platform they worked for 2017 and what they indend for the next year. 


data_2017_filter <- data_ini_2017 %>%
  select(Respondent, Country, EmploymentStatus, DeveloperType, FormalEducation, HaveWorkedLanguage, WantWorkLanguage, 
         HaveWorkedFramework, WantWorkFramework, HaveWorkedDatabase, WantWorkDatabase, HaveWorkedPlatform, WantWorkPlatform) %>%
  rename(Employment = EmploymentStatus, DevType = DeveloperType, LanguageWorkedWith = HaveWorkedLanguage, LanguageDesireNextYear = WantWorkLanguage, 
         FrameworkWorkedWith = HaveWorkedFramework, FrameworkDesireNextYear = WantWorkFramework, DatabaseWorkedWith = HaveWorkedDatabase,
         DatabaseDesireNextYear = WantWorkDatabase, PlatformWorkedWith = HaveWorkedPlatform, PlatformDesireNextYear = WantWorkPlatform)


#Data Cleaning 2017 1----
#Filter out people from employment who have chosen "I prefer not to say".
#We are doing this as in the 2018 and 2019 data sets this option is not there. 
#Removing this gives us a cleaner data set comparitively. 

data_2017_filter <- filter(data_2017_filter, Employment != "I prefer not to say")

View(data_2017_filter)


#Language Analysis----

data_2017_language_ini <- data_2017_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_language <- separate_rows(data_2017_language_ini, LanguageWorkedWith, sep=";")

data_2017_language$LanguageWorkedWith <- trimws(data_2017_language$LanguageWorkedWith)

data_2017_language$Year <- "2017"

View(data_2017_language)


#Prepare the data for 2018----

#Selecting the required columnms which will be mapped to prepare the 2018 data.----
#Note that the 2018 data has not been transformed or cleaned a lot as this was used as a base for transforming 2017 and 2019 data. 
# 2018 is de basis. Vanuit deze dataset is gewerkt om 2017 + 2018 hetzelfde te krijgen als 2018.

data_2018_filter <- data_ini_2018 %>%
  select(Respondent, Country, Employment, DevType, FormalEducation, LanguageWorkedWith, LanguageDesireNextYear, 
         FrameworkWorkedWith, FrameworkDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear)


#Language Analysis----

data_2018_language_ini <- data_2018_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_language <- separate_rows(data_2018_language_ini, LanguageWorkedWith, sep=";")

data_2018_language$LanguageWorkedWith <- trimws(data_2018_language$LanguageWorkedWith)

data_2018_language$Year <- "2018"

data_2018_language$LanguageWorkedWith <- ifelse(data_2018_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_language$LanguageWorkedWith)

View(data_2018_language)

#Select the required columns----

data_2019_filter <- data_ini_2019 %>%
  select(Respondent, Country, Employment, DevType, EdLevel, LanguageWorkedWith, LanguageDesireNextYear, 
         WebFrameWorkedWith, WebFrameDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear) %>%
  rename(FormalEducation = EdLevel, FrameworkWorkedWith = WebFrameWorkedWith, FrameworkDesireNextYear = WebFrameDesireNextYear )


#Language Analysis----

data_2019_language_ini <- data_2019_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_language <- separate_rows(data_2019_language_ini, LanguageWorkedWith, sep=";")

data_2019_language$LanguageWorkedWith <- trimws(data_2019_language$LanguageWorkedWith)

data_2019_language$Year <- "2019"

data_2019_language$LanguageWorkedWith <- ifelse(data_2019_language$LanguageWorkedWith=="Bash/Shell/PowerShell", "Bash/Shell/PowerShell", data_2019_language$LanguageWorkedWith)

View(data_2019_language)



#Aggregated data sets----

# Plot van de wereld distributie programmeer talen--------


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

# Filtert alleen de talen die alle jaren voorkwamen. 
data_language_dt_percent <- subset(data_language_dt_percent, data_language_dt_percent$presence == 3)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_language_dt_percent$distribution <- (data_language_dt_percent$users)*100/data_language_dt_percent$overall_users

# Maakt deze kolom als waarde getallen (numerich/integer)
data_language_dt_percent$Year <- as.integer(data_language_dt_percent$Year)

#Lijn grafiek met de verandering van programmeer talen wereldwijd van 2017 - 2019 1 
data_language_ggplot <- ggplot(data_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

# Legenda wordt bepaald vanuit kolom LanguageWorkedWith. Dit zodat elke programmeer taal in de visualisatie verwerkt wordt.

#Lijn grafiek met de verandering van programmeer talen wereldwijd van 2017 - 2019 2
data_language_ggplot_grid = ggplot(data_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout <- ggplotGrob(data_language_ggplot_grid)
data_language_ggplot_grid_layout$layout$clip[data_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout)

 

# ANALYSE PROGRAMEERTALEN NEDERLAND  ############################################################################################

#Reference------

#For the line graph generator----
#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

#Maakt een lijngrafiek van de distributie programmeertalen voor Nederland.
#Create the Netherlands distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

# Geeft alle respondenten weer vanuit Nederland.
data_language_clean_nl <- data_language_clean %>%
  filter(Country == "Netherlands")

View(data_language_clean_nl)

# Geeft het aantal respondenten weer per programmeertaal per jaar!
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

# Aantal x gevraagde taal per dataset. Dus 3x betekend dat de programmeer taal per jaar voorkomt. 
languages_presence_nl <- data_language  %>%
  filter(Country == "Netherlands")%>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(languages_presence_nl)

# Voegt variabele data_language_dt_nl en users_by_year_nl samen bij elkaar in 1 variabele.
data_language_dt_percent_nl <- merge(data_language_dt_nl, users_by_year_nl)

# Voegt variabele languages_presence_nl erbij 
data_language_dt_percent_nl <- merge(data_language_dt_percent_nl, languages_presence_nl)

View(data_language_dt_percent_nl)

#Filtert alleen de talen die alle jaren voorkwamen. 
data_language_dt_percent_nl <- subset(data_language_dt_percent_nl, data_language_dt_percent_nl$presence == 3)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
data_language_dt_percent_nl$distribution <- (data_language_dt_percent_nl$users)*100/data_language_dt_percent_nl$overall_users

# Maakt deze kolom als waarde getallen (numerich/integer)
data_language_dt_percent_nl$Year <- as.integer(data_language_dt_percent_nl$Year)

#Lijn grafiek met de verandering van programmeer talen Nederland van 2017 - 2019 1
data_language_ggplot_nl <- ggplot(data_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()


#Lijn grafiek met de verandering van programmeer talen Nederland van 2017 - 2019 2
data_language_ggplot_grid_nl = ggplot(data_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout_nl <- ggplotGrob(data_language_ggplot_grid_nl)
data_language_ggplot_grid_layout_nl$layout$clip[data_language_ggplot_grid_layout_nl$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout_nl)



