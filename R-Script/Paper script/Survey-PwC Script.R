#Install the required packages----
install.packages("readr")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages('janitor')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("data.table")
install.packages('htmlwidgets')
install.packages("grid")
install.packages("barplot")

#Load the required packages----
library("readr")
library("readxl")
library("dplyr")
library("tidyr")
library("janitor")
library("ggplot2")
library("reshape2")
library("data.table")
library("htmlwidgets")
library("grid")
library("barplot")

#working directory----
getwd()

#Set this based on your laptop's working directory
setwd("C:/Users/esteur002/Documents/GitHub/evert-study")
setwd("C:/Users/evert/OneDrive/Documenten/GitHub/evert-study")



#Data from PwC-Survey 2020
data_surveypwc <- read_excel("GitHub/evert-study/data/Survey-PwC/Survey-PwC.xlsx",col_types = c("text", "text", "text","text","text", "text", "text", "text","text"))

View(data_surveypwc)



# Onderstaande geeft wat meer duidelijkheid / inzicht / feeling in de DataSets.
# Geeft structure weer uit Dataset 
str(data_surveypwc)

# Geeft het aantal rijen en kolommen weer uit de Dataset
dim(data_surveypwc)

# Geeft alle Variable / kolom namen weer uit de Dataset. 
names(data_surveypwc)

# Geeft een samenvatting van de Data
summary(data_surveypwc)


# Aantal rijen dataset laten zien
nrow(data_surveypwc)

#Aantal kolommen dataset laten zien
ncol(data_surveypwc)

# 1e 10 rijen weergeven van dataset     
head(data_surveypwc,  10)

# Laat de Dataset zien per kolom.
View(data_surveypwc)



#Prepare the data for 2017----

#Selecting the required columnms which will be mapped to prepare the 2017 data.----
#Data Mapping 2017 1----
#Note that during 2017 the questions for languages, frameworks and platforms are on what a user has worked till then and what they want to use in the future. 
#This is different from 2018 and 2019 where the question is specific and asks about the language, platform or framework the user worked during 
#that specific and year and what they want to work on the next year. For the ease of use we are assuming the columns used to understand what the users have 
#worked with till then to the language, framework or platform they worked for 2017 and what they indend for the next year. 


data_surveypwc_filter <- data_surveypwc %>%
  select(Respondent, Country, FormalEducation, DevType, LanguageWorkedWith, LanguageDesireNextYear, DatabaseWorkedWith, 
         DatabaseDesireNextYear, IDE) %>%
  rename(DevEnvironments = IDE)

View(data_surveypwc_filter)


#Language Analysis survey PwC----

data_surveypwc_language_ini <- data_surveypwc_filter %>%
  select(Respondent, LanguageWorkedWith)

data_surveypwc_language <- separate_rows(data_surveypwc_language_ini, LanguageWorkedWith, sep=";")

data_surveypwc_language$LanguageWorkedWith <- trimws(data_surveypwc_language$LanguageWorkedWith)

View(data_surveypwc_language)

data_surveypwc_language$Year <- "2019"

ggplot(data_surveypwc_language, aes(x=LanguageWorkedWith, Y=Respondent))



plot(data_surveypwc_language, aes(x=LanguageWorkedWith,y=Respondent, type="l",lwd=3))

#Aggregated data sets----



survey_language <- rbind(data_surveypwc_language)

View(survey_language)

survey_language_clean<- na.omit(survey_language, cols = "LanguageWorkedWith")

View(survey_language_clean)

survey_language_dt <- survey_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

View(survey_language_dt)

# SKIPPED
surveyusers_by_year <- survey_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

View(surveyusers_by_year)


survey_languages_presence <- survey_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

View(survey_languages_presence)

survey_language_dt_percent <- merge(survey_language_dt, surveyusers_by_year)

survey_language_dt_percent <- merge(survey_language_dt_percent, survey_languages_presence)

survey_language_dt_percent <- subset(survey_language_dt_percent, survey_language_dt_percent$presence == 3)

survey_language_dt_percent$distribution <- (survey_language_dt_percent$users)*100/survey_language_dt_percent$overall_users

survey_language_dt_percent$Year <- as.integer(survey_language_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
survey_language_ggplot <- ggplot(survey_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()

View(survey_language_ggplot)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
survey_language_ggplot_grid = ggplot(survey_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(survey_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

survey_language_ggplot_grid_layout <- ggplotGrob(survey_language_ggplot_grid)
survey_language_ggplot_grid_layout$layout$clip[survey_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(survey_language_ggplot_grid_layout)