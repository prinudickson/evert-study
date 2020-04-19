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
library("janitor")
library("ggplot2")
library("reshape2")
library("data.table")
library("htmlwidgets")
library("grid")

#working directory----
getwd()


#data for 2017
database_ini_2017 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2017/survey_results_public.csv")


#data for 2018
database_ini_2018 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2018/survey_results_public.csv")


#data for 2019
database_ini_2019 <- read_csv(file = "C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2019/survey_results_public.csv")


# Onderstaande geeft wat meer duidelijkheid / inzicht / feeling in de DataSets.
# Geeft structure weer uit Dataset 
str(database_ini_2019)

# Geeft het aantal rijen en kolommen weer uit de Dataset
dim(database_ini_2019)

# Geeft alle Variable / kolom namen weer uit de Dataset. 
names(database_ini_2019)

# Geeft een samenvatting van de Data
summary(database_ini_2019)


# Aantal rijen dataset laten zien
nrow(database_ini_2019)

#Aantal kolommen dataset laten zien
ncol(database_ini_2019)

# 1e 10 rijen weergeven van dataset     
head(database_ini_2019,  10)

# Laat de Dataset zien per kolom.
View(database_ini_2019)


#Prepare the data for 2017----

#Selecting the required columnms which will be mapped to prepare the 2017 data.----
#Data Mapping 2017 1----
#Note that during 2017 the questions for languages, frameworks and platforms are on what a user has worked till then and what they want to use in the future. 
#This is different from 2018 and 2019 where the question is specific and asks about the language, platform or framework the user worked during 
#that specific and year and what they want to work on the next year. For the ease of use we are assuming the columns used to understand what the users have 
#worked with till then to the language, framework or platform they worked for 2017 and what they indend for the next year. 


database_2017_filter <- database_ini_2017 %>%
  select(Respondent, Country, EmploymentStatus, DeveloperType, FormalEducation, HaveWorkedLanguage, WantWorkLanguage, 
         HaveWorkedFramework, WantWorkFramework, HaveWorkedDatabase, WantWorkDatabase, HaveWorkedPlatform, WantWorkPlatform) %>%
  rename(Employment = EmploymentStatus, DevType = DeveloperType, LanguageWorkedWith = HaveWorkedLanguage, LanguageDesireNextYear = WantWorkLanguage, 
         FrameworkWorkedWith = HaveWorkedFramework, FrameworkDesireNextYear = WantWorkFramework, DatabaseWorkedWith = HaveWorkedDatabase,
         DatabaseDesireNextYear = WantWorkDatabase, PlatformWorkedWith = HaveWorkedPlatform, PlatformDesireNextYear = WantWorkPlatform)



#Data Cleaning 2017 1----
#Filter out people from employment who have chosen "I prefer not to say".
#We are doing this as in the 2018 and 2019 data sets this option is not there. 
#Removing this gives us a cleaner data set comparitively. 

database_2017_filter <- filter(database_2017_filter, Employment != "I prefer not to say")

View(database_2017_filter)


#Database Environment Analysis----

database_2017_environments_ini <- database_2017_filter %>%
  select(Respondent, DatabaseWorkedWith, Country)

database_2017_environments <- separate_rows(database_2017_environments_ini, DatabaseWorkedWith, sep=";")

database_2017_environments$DatabaseWorkedWith <- trimws(database_2017_environments$DatabaseWorkedWith)

database_2017_environments$Year <- "2017"

View(database_2017_environments)


#Prepare the data for 2018----

#Selecting the required columnms which will be mapped to prepare the 2018 data.----
#Note that the 2018 data has not been transformed or cleaned a lot as this was used as a base for transforming 2017 and 2019 data. 
# 2018 is de basis. Vanuit deze dataset is gewerkt om 2017 + 2018 hetzelfde te krijgen als 2018.

database_2018_filter <- database_ini_2018 %>%
  select(Respondent, Country, Employment, DevType, FormalEducation, LanguageWorkedWith, LanguageDesireNextYear, 
         FrameworkWorkedWith, FrameworkDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear)

     
#Database Environment Analysis----

database_2018_environments_ini <- database_2018_filter %>%
  select(Respondent, DatabaseWorkedWith, Country)

database_2018_environments <- separate_rows(database_2018_environments_ini, DatabaseWorkedWith, sep=";")

database_2018_environments$DatabaseWorkedWith <- trimws(database_2018_environments$DatabaseWorkedWith)

database_2018_environments$Year <- "2018"



#Prepare the data for 2019----


#Select the required columns----

database_2019_filter <- database_ini_2019 %>%
  select(Respondent, Country, Employment, DevType, EdLevel, LanguageWorkedWith, LanguageDesireNextYear, 
         WebFrameWorkedWith, WebFrameDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear) %>%
  rename(FormalEducation = EdLevel, FrameworkWorkedWith = WebFrameWorkedWith, FrameworkDesireNextYear = WebFrameDesireNextYear )



#Database Environment Analysis----

database_2019_environments_ini <- database_2019_filter %>%
  select(Respondent, DatabaseWorkedWith, Country)

database_2019_environments <- separate_rows(database_2019_environments_ini, DatabaseWorkedWith, sep=";")

database_2019_environments$DatabaseWorkedWith <- trimws(database_2019_environments$DatabaseWorkedWith)

database_2019_environments$Year <- "2019"




#Aggregated data sets----

#Create the World distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

# Combineert alle 3 Language per jaar tot 1 variabele
database_environments <- rbind(database_2017_environments, database_2018_environments, database_2019_environments)

View(database_2017_environments)
View(database_2018_environments)
View(database_2019_environments)

# Verwijderd alle NA antwoorden uit kolom LanguageWorkedWith
database_environments_clean<- na.omit(database_environments, cols = "DatabaseWorkedWith")

View(data_language_clean)

# Extra kolom toegevoegd. Geeft het totaal users weer wat met de programmeer taal gewerkt heeft PER jaartal en PER programmeertaal!
database_environments_dt <- database_environments_clean %>%
  group_by(DatabaseWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(database_environments_dt)

# Deze variabele geeft een overall overzicht van het aantal users PER jaar uit de data_language variabele. 
# Waarin alle respondenten verwerkt zijn.
dbusers_by_year <- database_environments %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(dbusers_by_year)

# Vertelt hoevaak de programmeer taal voorkomt. Per jaar. dus staat er 1 dan komt die 1x in de survey voor. Staat er 2 dan komt de 
# taal 2 keer bij 3x komt die in alle 3 survey's voor.
database_presence <- database_environments %>%
  group_by(DatabaseWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(database_presence)

#Voegt beide varialele toe tot 1 compleet variabele.
database_environments_dt_percent <- merge(database_environments_dt, dbusers_by_year)

View(database_environments_dt_percent)

# Voegt de kolom precense bij de complete duidelijke dataset van data_language_dt_percent
database_environments_dt_percent <- merge(database_environments_dt_percent, database_presence)

# Filtert alleen de talen die alle jaren voorkwamen. 
database_environments_dt_percent <- subset(database_environments_dt_percent, database_environments_dt_percent$presence == 3)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
database_environments_dt_percent$distribution <- (database_environments_dt_percent$users)*100/database_environments_dt_percent$overall_users

# What means below one?
database_environments_dt_percent$Year <- as.integer(database_environments_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
database_environments_ggplot <- ggplot(database_environments_dt_percent, aes(x = Year, y = distribution, colour = DatabaseWorkedWith, label = DatabaseWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
database_environments_ggplot_grid = ggplot(database_environments_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = DatabaseWorkedWith, colour = DatabaseWorkedWith)) + 
  geom_text(data = subset(database_environments_dt_percent, Year == 2019), aes(label = DatabaseWorkedWith, colour = DatabaseWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

database_environments_ggplot_grid_layout <- ggplotGrob(database_environments_ggplot_grid)
database_environments_ggplot_grid_layout$layout$clip[database_environments_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(database_environments_ggplot_grid_layout)











#Reference------

#For the line graph generator----
#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

#Create the Netherlands distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

# Geeft alle respondenten weer vanuit Nederland.
database_environments_clean_nl <- database_environments_clean %>%
  filter(Country == "Netherlands")

View(database_environments_clean_nl)

# Geeft het aantal respondenten weer per programmeertaal per jaar!
database_environments_dt_nl <- database_environments_clean_nl %>%
  group_by(DatabaseWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(database_environments_dt_nl)

# Totaal respondenten vanuit Netherlands per jaartal
dbusers_by_year_nl <- database_environments %>%
  filter(Country == "Netherlands")%>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(dbusers_by_year_nl)

# Aantal x gevraagde taal per dataset. Dus 3x betekend dat de programmeer taal per jaar voorkomt. 
database_environments_presence_nl <- database_environments  %>%
  filter(Country == "Netherlands")%>%
  group_by(DatabaseWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(database_environments_presence_nl)

# Voegt variabele data_language_dt_nl en users_by_year_nl samen bij elkaar in 1 variabele.
database_environments_dt_percent_nl <- merge(database_environments_dt_nl, dbusers_by_year_nl)

# Voegt variabele languages_presence_nl erbij 
database_environments_dt_percent_nl <- merge(database_environments_dt_percent_nl, environments_presence_nl)

View(database_environments_dt_percent_nl)

#Filtert alleen de talen die alle jaren voorkwamen. 
data_language_dt_percent_nl <- subset(data_language_dt_percent_nl, data_language_dt_percent_nl$presence == 3)

# Maakt een extra kolom aan Distribution die de totale verdeling weergeeft.
database_environments_dt_percent_nl$distribution <- (database_environments_dt_percent_nl$users)*100/database_environments_dt_percent_nl$overall_users

# Maakt deze kolom als waarde getallen (numerich/integer)
database_environments_dt_percent_nl$Year <- as.integer(database_environments_dt_percent_nl$Year)

#Line graphs of the percentage change of programming laguages in the Netherlands from 2017 - 2019 1
database_environments_ggplot_nl <- ggplot(database_environments_dt_percent_nl, aes(x = Year, y = distribution, colour = DatabaseWorkedWith, label = DatabaseWorkedWith)) + geom_line()




#Line graphs of the percentage change of programming laguages in the Netherlands from 2017 - 2019 2
database_environments_ggplot_grid_nl = ggplot(database_environments_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = DatabaseWorkedWith, colour = DatabaseWorkedWith)) + 
  geom_text(data = subset(database_environments_dt_percent_nl, Year == 2019), aes(label = DatabaseWorkedWith, colour = DatabaseWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

database_environments_ggplot_grid_layout_nl <- ggplotGrob(database_environments_ggplot_grid_nl)
database_environments_ggplot_grid_layout_nl$layout$clip[database_environments_ggplot_grid_layout_nl$layout$name == "panel"] <- "off"
grid.draw(database_environments_ggplot_grid_layout_nl)

