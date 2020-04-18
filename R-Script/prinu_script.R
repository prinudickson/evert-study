#Install the required packages----
#install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages('janitor')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("data.table")
install.packages('htmlwidgets')
install.packages("grid")

#Load the required packages----
#library(readr)
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

#Set this based on your laptop's working directory
setwd("C:/Users/pdickson004/Desktop/Personal_Projects/evert-study")

#data for 2016

data_ini_2016 <- read.csv(file = "C:/Users/pdickson004/Desktop/Personal_Projects/evert-study/data/developer_survey_2016/2016_Stack_Overflow_Survey_Results/2016_Stack_Overflow_Survey_Responses.csv")


#data for 2017
data_ini_2017 <- read.csv(file = "C:/Users/pdickson004/Desktop/Personal_Projects/evert-study/data/developer_survey_2017/survey_results_public.csv")

data_schema_2017 <- read.csv(file = "C:/Users/pdickson004/Desktop/Personal_Projects/evert-study/data/developer_survey_2017/survey_results_schema.csv")


#data for 2018
data_ini_2018 <- read.csv(file = "C:/Users/pdickson004/Desktop/Personal_Projects/evert-study/data/developer_survey_2018/survey_results_public.csv")


#data for 2019
data_ini_2019 <- read.csv(file = "C:/Users/pdickson004/Desktop/Personal_Projects/evert-study/data/developer_survey_2019/survey_results_public.csv")

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


#Data transformation 2017 1----
#Creating a common Education level.

data_2017_filter$FormalEducation <- as.character(data_2017_filter$FormalEducation)
 
data_2017_filter$educationlevel_clean <-  ifelse(data_2017_filter$FormalEducation == "Bachelor's degree", "Bachelors", 
                                                 ifelse(data_2017_filter$FormalEducation == "Master's degree", "Masters",
                                                        ifelse(data_2017_filter$FormalEducation == "Some college/university study without earning a bachelor's degree", "Did not finish College/Uni",
                                                               ifelse(data_2017_filter$FormalEducation == "I never completed any formal education", "Never completed any formal education",
                                                                      ifelse(data_2017_filter$FormalEducation == "Primary/elementary school", "Primary school",
                                                                             ifelse(data_2017_filter$FormalEducation == "Secondary school", "Secondary school", "Others" ))))))



#DevType Analysis----

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

#Language Analysis----

data_2017_language_ini <- data_2017_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_language <- separate_rows(data_2017_language_ini, LanguageWorkedWith, sep=";")

data_2017_language$LanguageWorkedWith <- trimws(data_2017_language$LanguageWorkedWith)

data_2017_language$LanguageWorkedWith <- ifelse(data_2017_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_language$LanguageWorkedWith)

data_2017_language$Year <- "2017"

#Developer Language Analysis----

data_2017_developer_language_ini <- data_2017_filter %>%
                                    filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Developer")) %>%
                                    select(Respondent, LanguageWorkedWith, Country)

data_2017_developer_language <- separate_rows(data_2017_developer_language_ini, LanguageWorkedWith, sep=";")

data_2017_developer_language$LanguageWorkedWith <- trimws(data_2017_developer_language$LanguageWorkedWith)

data_2017_developer_language$LanguageWorkedWith <- ifelse(data_2017_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_developer_language$LanguageWorkedWith)

data_2017_developer_language$Year <- "2017"


#Sys Admin Language Analysis----

data_2017_sys_admin_language_ini <- data_2017_filter %>%
  filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_sys_admin_language <- separate_rows(data_2017_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2017_sys_admin_language$LanguageWorkedWith <- trimws(data_2017_sys_admin_language$LanguageWorkedWith)

data_2017_sys_admin_language$LanguageWorkedWith <- ifelse(data_2017_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_sys_admin_language$LanguageWorkedWith)

data_2017_sys_admin_language$Year <- "2017"

#Db Admin Language Analysis----

data_2017_db_admin_language_ini <- data_2017_filter %>%
  filter(Respondent %in% subset(data_2017_devtype$Respondent, data_2017_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_db_admin_language <- separate_rows(data_2017_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2017_db_admin_language$LanguageWorkedWith <- trimws(data_2017_db_admin_language$LanguageWorkedWith)

data_2017_db_admin_language$LanguageWorkedWith <- ifelse(data_2017_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2017_db_admin_language$LanguageWorkedWith)

data_2017_db_admin_language$Year <- "2017"

#Future Language Analysis----

data_2017_futurelanguage_ini <- data_2017_filter %>%
  select(Respondent, LanguageDesireNextYear)

data_2017_futurelanguage <- separate_rows(data_2017_futurelanguage_ini, LanguageDesireNextYear, sep=";")

data_2017_futurelanguage$LanguageDesireNextYear <- trimws(data_2017_futurelanguage$LanguageDesireNextYear)

#Prepare the data for 2018----

#Selecting the required columnms which will be mapped to prepare the 2018 data.----
#Note that the 2018 data has not been transformed or cleaned a lot as this was used as a base for transforming 2017 and 2019 data. 

data_2018_filter <- data_ini_2018 %>%
                    select(Respondent, Country, Employment, DevType, FormalEducation, LanguageWorkedWith, LanguageDesireNextYear, 
                           FrameworkWorkedWith, FrameworkDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear)

data_2018_filter$educationlevel_clean <-  ifelse(data_2018_filter$FormalEducation == "Bachelorâ€™s degree (BA, BS, B.Eng., etc.)", "Bachelors", 
                                                 ifelse(data_2018_filter$FormalEducation == "Masterâ€™s degree (MA, MS, M.Eng., MBA, etc.)", "Masters",
                                                        ifelse(data_2018_filter$FormalEducation == "Some college/university study without earning a degree", "Did not finish College/Uni",
                                                               ifelse(data_2018_filter$FormalEducation == "I never completed any formal education", "Never completed any formal education",
                                                                      ifelse(data_2018_filter$FormalEducation == "Primary/elementary school", "Primary school",
                                                                             ifelse(data_2018_filter$FormalEducation == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", "Secondary school", "Others" ))))))

#data_2018_filter$LanguageWorkedWith <- ifelse(data_2018_filter$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_filter$LanguageWorkedWith)


#DevType Analysis----

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

#Language Analysis----

data_2018_language_ini <- data_2018_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_language <- separate_rows(data_2018_language_ini, LanguageWorkedWith, sep=";")

data_2018_language$LanguageWorkedWith <- trimws(data_2018_language$LanguageWorkedWith)

data_2018_language$LanguageWorkedWith <- ifelse(data_2018_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_language$LanguageWorkedWith)

data_2018_language$Year <- "2018"

#data_2018_language$LanguageWorkedWith <- ifelse(data_2018_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_language$LanguageWorkedWith)

#Developer Language Analysis----

data_2018_developer_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Developer")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_developer_language <- separate_rows(data_2018_developer_language_ini, LanguageWorkedWith, sep=";")

data_2018_developer_language$LanguageWorkedWith <- trimws(data_2018_developer_language$LanguageWorkedWith)

data_2018_developer_language$LanguageWorkedWith <- ifelse(data_2018_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_developer_language$LanguageWorkedWith)

data_2018_developer_language$Year <- "2018"


#Sys Admin Language Analysis----

data_2018_sys_admin_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_sys_admin_language <- separate_rows(data_2018_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2018_sys_admin_language$LanguageWorkedWith <- trimws(data_2018_sys_admin_language$LanguageWorkedWith)

data_2018_sys_admin_language$LanguageWorkedWith <- ifelse(data_2018_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_sys_admin_language$LanguageWorkedWith)

data_2018_sys_admin_language$Year <- "2018"

#Db Admin Language Analysis----

data_2018_db_admin_language_ini <- data_2018_filter %>%
  filter(Respondent %in% subset(data_2018_devtype$Respondent, data_2018_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_db_admin_language <- separate_rows(data_2018_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2018_db_admin_language$LanguageWorkedWith <- trimws(data_2018_db_admin_language$LanguageWorkedWith)

data_2018_db_admin_language$LanguageWorkedWith <- ifelse(data_2018_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2018_db_admin_language$LanguageWorkedWith)

data_2018_db_admin_language$Year <- "2018"

#Future Language Analysis----

data_2018_futurelanguage_ini <- data_2018_filter %>%
  select(Respondent, LanguageDesireNextYear)

data_2018_futurelanguage <- separate_rows(data_2018_futurelanguage_ini, LanguageDesireNextYear, sep=";")

data_2018_futurelanguage$LanguageDesireNextYear <- trimws(data_2018_futurelanguage$LanguageDesireNextYear)


#Prepare the data for 2019----

#Select the required columns----

data_2019_filter <- data_ini_2019 %>%
                    select(Respondent, Country, Employment, DevType, EdLevel, LanguageWorkedWith, LanguageDesireNextYear, 
                           WebFrameWorkedWith, WebFrameDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear) %>%
                    rename(FormalEducation = EdLevel, FrameworkWorkedWith = WebFrameWorkedWith, FrameworkDesireNextYear = WebFrameDesireNextYear )

data_2019_filter$educationlevel_clean <-  ifelse(data_2019_filter$FormalEducation == "Bachelorâ€™s degree (BA, BS, B.Eng., etc.)", "Bachelors", 
                                                 ifelse(data_2019_filter$FormalEducation == "Masterâ€™s degree (MA, MS, M.Eng., MBA, etc.)", "Masters",
                                                        ifelse(data_2019_filter$FormalEducation == "Some college/university study without earning a degree", "Did not finish College/Uni",
                                                               ifelse(data_2019_filter$FormalEducation == "I never completed any formal education", "Never completed any formal education",
                                                                      ifelse(data_2019_filter$FormalEducation == "Primary/elementary school", "Primary school",
                                                                             ifelse(data_2019_filter$FormalEducation == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", "Secondary school", "Others" ))))))

#data_2019_filter$LanguageWorkedWith <- ifelse(data_2019_filter$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_filter$LanguageWorkedWith)


#DevType Analysis----

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

#Language Analysis----

data_2019_language_ini <- data_2019_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_language <- separate_rows(data_2019_language_ini, LanguageWorkedWith, sep=";")

data_2019_language$LanguageWorkedWith <- trimws(data_2019_language$LanguageWorkedWith)

data_2019_language$LanguageWorkedWith <- ifelse(data_2019_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_language$LanguageWorkedWith)

data_2019_language$Year <- "2019"

#Developer Language Analysis----

data_2019_developer_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Developer")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_developer_language <- separate_rows(data_2019_developer_language_ini, LanguageWorkedWith, sep=";")

data_2019_developer_language$LanguageWorkedWith <- trimws(data_2019_developer_language$LanguageWorkedWith)

data_2019_developer_language$LanguageWorkedWith <- ifelse(data_2019_developer_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_developer_language$LanguageWorkedWith)

data_2019_developer_language$Year <- "2019"


#Sys Admin Language Analysis----

data_2019_sys_admin_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Sys Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_sys_admin_language <- separate_rows(data_2019_sys_admin_language_ini, LanguageWorkedWith, sep=";")

data_2019_sys_admin_language$LanguageWorkedWith <- trimws(data_2019_sys_admin_language$LanguageWorkedWith)

data_2019_sys_admin_language$LanguageWorkedWith <- ifelse(data_2019_sys_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_sys_admin_language$LanguageWorkedWith)

data_2019_sys_admin_language$Year <- "2019"

#Db Admin Language Analysis----

data_2019_db_admin_language_ini <- data_2019_filter %>%
  filter(Respondent %in% subset(data_2019_devtype$Respondent, data_2019_devtype$devtype_clean == "Db Admin")) %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_db_admin_language <- separate_rows(data_2019_db_admin_language_ini, LanguageWorkedWith, sep=";")

data_2019_db_admin_language$LanguageWorkedWith <- trimws(data_2019_db_admin_language$LanguageWorkedWith)

data_2019_db_admin_language$LanguageWorkedWith <- ifelse(data_2019_db_admin_language$LanguageWorkedWith=="Bash/Shell", "Bash/Shell/PowerShell", data_2019_db_admin_language$LanguageWorkedWith)

data_2019_db_admin_language$Year <- "2019"

#Future Language Analysis----

data_2019_futurelanguage_ini <- data_2019_filter %>%
  select(Respondent, LanguageDesireNextYear)

data_2019_futurelanguage <- separate_rows(data_2019_futurelanguage_ini, LanguageDesireNextYear, sep=";")

data_2019_futurelanguage$LanguageDesireNextYear <- trimws(data_2019_futurelanguage$LanguageDesireNextYear)

#Aggregated data sets----

#Create the World distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

data_language <- rbind(data_2017_language, data_2018_language, data_2019_language)

data_language_clean<- na.omit(data_language, cols = "LanguageWorkedWith")

#Create a dataframe to see the overall user per language per year. 
data_language_dt <- data_language_clean %>%
                    group_by(LanguageWorkedWith, Year) %>%
                    summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
users_by_year <- data_language %>%
                  group_by(Year) %>%
                  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
languages_presence <- data_language %>%
                        group_by(LanguageWorkedWith) %>%
                        summarize(presence = n_distinct(Year))

data_language_dt_percent <- merge(data_language_dt, users_by_year)

data_language_dt_percent <- merge(data_language_dt_percent, languages_presence)

data_language_dt_percent <- subset(data_language_dt_percent, data_language_dt_percent$presence == 3 | data_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_language_dt_percent$distribution <- (data_language_dt_percent$users)*100/data_language_dt_percent$overall_users

data_language_dt_percent$Year <- as.integer(data_language_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_language_ggplot <- ggplot(data_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_language_ggplot_grid = ggplot(data_language_dt_percent) + 
                                       geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
                                       geom_text(data = subset(data_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
                                       scale_colour_discrete(guide = 'none')  +    
                                       theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout <- ggplotGrob(data_language_ggplot_grid)
data_language_ggplot_grid_layout$layout$clip[data_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout)


#Create the World distribution plot of popular programming languages for developers-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

data_developer_language <- rbind(data_2017_developer_language, data_2018_developer_language, data_2019_developer_language)

data_developer_language_clean<- na.omit(data_developer_language, cols = "LanguageWorkedWith")

#Create a dataframe to see the overall user per language per year. 
data_developer_language_dt <- data_developer_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
developers_by_year <- data_developer_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
developer_languages_presence <- data_developer_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

data_developer_language_dt_percent <- merge(data_developer_language_dt, users_by_year)

data_developer_language_dt_percent <- merge(data_developer_language_dt_percent, developer_languages_presence)

data_developer_language_dt_percent <- subset(data_developer_language_dt_percent, data_developer_language_dt_percent$presence == 3 | data_developer_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_developer_language_dt_percent$distribution <- (data_developer_language_dt_percent$users)*100/data_developer_language_dt_percent$overall_users

data_developer_language_dt_percent$Year <- as.integer(data_developer_language_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_developer_language_ggplot <- ggplot(data_developer_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_developer_language_ggplot_grid = ggplot(data_developer_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_developer_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_developer_language_ggplot_grid_layout <- ggplotGrob(data_developer_language_ggplot_grid)
data_developer_language_ggplot_grid_layout$layout$clip[data_developer_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_developer_language_ggplot_grid_layout)


#Create the World distribution plot of popular programming languages for System Admins-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

data_sys_admin_language <- rbind(data_2017_sys_admin_language, data_2018_sys_admin_language, data_2019_sys_admin_language)

data_sys_admin_language_clean<- na.omit(data_sys_admin_language, cols = "LanguageWorkedWith")

#Create a dataframe to see the overall user per language per year. 
data_sys_admin_language_dt <- data_sys_admin_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
sys_admin_by_year <- data_sys_admin_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
sys_admin_languages_presence <- data_sys_admin_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

data_sys_admin_language_dt_percent <- merge(data_sys_admin_language_dt, sys_admin_by_year)

data_sys_admin_language_dt_percent <- merge(data_sys_admin_language_dt_percent, sys_admin_languages_presence)

data_sys_admin_language_dt_percent <- subset(data_sys_admin_language_dt_percent, data_sys_admin_language_dt_percent$presence == 3 | data_sys_admin_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_sys_admin_language_dt_percent$distribution <- (data_sys_admin_language_dt_percent$users)*100/data_sys_admin_language_dt_percent$overall_users

data_sys_admin_language_dt_percent$Year <- as.integer(data_sys_admin_language_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_sys_admin_language_ggplot <- ggplot(data_sys_admin_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_sys_admin_language_ggplot_grid = ggplot(data_sys_admin_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_sys_admin_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_sys_admin_language_ggplot_grid_layout <- ggplotGrob(data_sys_admin_language_ggplot_grid)
data_sys_admin_language_ggplot_grid_layout$layout$clip[data_sys_admin_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_sys_admin_language_ggplot_grid_layout)



#Create the World distribution plot of popular programming languages for Db Admins-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

data_db_admin_language <- rbind(data_2017_db_admin_language, data_2018_db_admin_language, data_2019_db_admin_language)

data_db_admin_language_clean<- na.omit(data_db_admin_language, cols = "LanguageWorkedWith")

#Create a dataframe to see the overall user per language per year. 
data_db_admin_language_dt <- data_db_admin_language_clean %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
db_admin_by_year <- data_db_admin_language %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
db_admin_languages_presence <- data_db_admin_language %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

data_db_admin_language_dt_percent <- merge(data_db_admin_language_dt, db_admin_by_year)

data_db_admin_language_dt_percent <- merge(data_db_admin_language_dt_percent, db_admin_languages_presence)

data_db_admin_language_dt_percent <- subset(data_db_admin_language_dt_percent, data_db_admin_language_dt_percent$presence == 3 | data_db_admin_language_dt_percent$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_db_admin_language_dt_percent$distribution <- (data_db_admin_language_dt_percent$users)*100/data_db_admin_language_dt_percent$overall_users

data_db_admin_language_dt_percent$Year <- as.integer(data_db_admin_language_dt_percent$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_db_admin_language_ggplot <- ggplot(data_db_admin_language_dt_percent, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_db_admin_language_ggplot_grid = ggplot(data_db_admin_language_dt_percent) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_db_admin_language_dt_percent, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_db_admin_language_ggplot_grid_layout <- ggplotGrob(data_db_admin_language_ggplot_grid)
data_db_admin_language_ggplot_grid_layout$layout$clip[data_db_admin_language_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_db_admin_language_ggplot_grid_layout)

#Reference------

#For the line graph generator----
#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

#Create the Netherlands distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the World 

data_language_clean_nl <- data_language_clean %>%
                          filter(Country == "Netherlands")

data_language_dt_nl <- data_language_clean_nl %>%
                        group_by(LanguageWorkedWith, Year) %>%
                        summarize(users = n_distinct(Respondent))

users_by_year_nl <- data_language %>%
                    filter(Country == "Netherlands")%>%
                    group_by(Year) %>%
                    summarize(overall_users = n_distinct(Respondent))

languages_presence_nl <- data_language  %>%
                          filter(Country == "Netherlands")%>%
                          group_by(LanguageWorkedWith) %>%
                          summarize(presence = n_distinct(Year))

data_language_dt_percent_nl <- merge(data_language_dt_nl, users_by_year_nl)

data_language_dt_percent_nl <- merge(data_language_dt_percent_nl, languages_presence_nl)

data_language_dt_percent_nl <- subset(data_language_dt_percent_nl, data_language_dt_percent_nl$presence == 3)

data_language_dt_percent_nl$distribution <- (data_language_dt_percent_nl$users)*100/data_language_dt_percent_nl$overall_users

data_language_dt_percent_nl$Year <- as.integer(data_language_dt_percent_nl$Year)

#Line graphs of the percentage change of programming laguages in the Netherlands from 2017 - 2019 1
data_language_ggplot_nl <- ggplot(data_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the Netherlands from 2017 - 2019 2
data_language_ggplot_grid_nl = ggplot(data_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_language_ggplot_grid_layout_nl <- ggplotGrob(data_language_ggplot_grid_nl)
data_language_ggplot_grid_layout_nl$layout$clip[data_language_ggplot_grid_layout_nl$layout$name == "panel"] <- "off"
grid.draw(data_language_ggplot_grid_layout_nl)



#Create the World distribution plot of popular programming languages for developers in Netherlands-----

data_developer_language_clean_nl <- data_developer_language_clean %>%
                                                 filter(Country == "Netherlands")

#Create a dataframe to see the overall user per language per year. 
data_developer_language_dt_nl <- data_developer_language_clean_nl %>%
                                    group_by(LanguageWorkedWith, Year) %>%
                                    summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
developers_by_year_nl <- data_developer_language %>%
                          filter(Country == "Netherlands") %>%
                          group_by(Year) %>%
                          summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
developer_languages_presence_nl <- data_developer_language %>%
                                    filter(Country == "Netherlands") %>%
                                    group_by(LanguageWorkedWith) %>%
                                    summarize(presence = n_distinct(Year))

data_developer_language_dt_percent_nl <- merge(data_developer_language_dt_nl, developers_by_year_nl)

data_developer_language_dt_percent_nl <- merge(data_developer_language_dt_percent_nl, developer_languages_presence_nl)

data_developer_language_dt_percent_nl <- subset(data_developer_language_dt_percent_nl, data_developer_language_dt_percent_nl$presence == 3 | data_developer_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_developer_language_dt_percent_nl$distribution <- (data_developer_language_dt_percent_nl$users)*100/data_developer_language_dt_percent_nl$overall_users

data_developer_language_dt_percent_nl$Year <- as.integer(data_developer_language_dt_percent_nl$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_developer_language_dt_percent_nl_ggplot <- ggplot(data_developer_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_developer_language_nl_ggplot_grid = ggplot(data_developer_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_developer_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_developer_language_nl_ggplot_grid_layout <- ggplotGrob(data_developer_language_nl_ggplot_grid)
data_developer_language_nl_ggplot_grid_layout$layout$clip[data_developer_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_developer_language_nl_ggplot_grid_layout)


#Create the World distribution plot of popular programming languages for Sys Admins in Netherlands-----

data_sys_admin_language_clean_nl <- data_sys_admin_language_clean %>%
  filter(Country == "Netherlands")

#Create a dataframe to see the overall user per language per year. 
data_sys_admin_language_dt_nl <- data_sys_admin_language_clean_nl %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
sys_admin_by_year_nl <- data_sys_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
sys_admin_languages_presence_nl <- data_sys_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

data_sys_admin_language_dt_percent_nl <- merge(data_sys_admin_language_dt_nl, sys_admin_by_year_nl)

data_sys_admin_language_dt_percent_nl <- merge(data_sys_admin_language_dt_percent_nl, sys_admin_languages_presence_nl)

data_sys_admin_language_dt_percent_nl <- subset(data_sys_admin_language_dt_percent_nl, data_sys_admin_language_dt_percent_nl$presence == 3 | data_sys_admin_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_sys_admin_language_dt_percent_nl$distribution <- (data_sys_admin_language_dt_percent_nl$users)*100/data_sys_admin_language_dt_percent_nl$overall_users

data_sys_admin_language_dt_percent_nl$Year <- as.integer(data_sys_admin_language_dt_percent_nl$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_sys_admin_language_dt_percent_nl_ggplot <- ggplot(data_sys_admin_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_sys_admin_language_nl_ggplot_grid = ggplot(data_sys_admin_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_sys_admin_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_sys_admin_language_nl_ggplot_grid_layout <- ggplotGrob(data_sys_admin_language_nl_ggplot_grid)
data_sys_admin_language_nl_ggplot_grid_layout$layout$clip[data_sys_admin_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_sys_admin_language_nl_ggplot_grid_layout)


#Create the World distribution plot of popular programming languages for Db Admins in Netherlands-----

data_db_admin_language_clean_nl <- data_db_admin_language_clean %>%
  filter(Country == "Netherlands")

#Create a dataframe to see the overall user per language per year. 
data_db_admin_language_dt_nl <- data_db_admin_language_clean_nl %>%
  group_by(LanguageWorkedWith, Year) %>%
  summarize(users = n_distinct(Respondent))

#Create a dataframe to see the overall users per year. 
db_admin_by_year_nl <- data_db_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(Year) %>%
  summarize(overall_users = n_distinct(Respondent))

#Check the number of years this language was present in the survey
db_admin_languages_presence_nl <- data_db_admin_language %>%
  filter(Country == "Netherlands") %>%
  group_by(LanguageWorkedWith) %>%
  summarize(presence = n_distinct(Year))

data_db_admin_language_dt_percent_nl <- merge(data_db_admin_language_dt_nl, db_admin_by_year_nl)

data_db_admin_language_dt_percent_nl <- merge(data_db_admin_language_dt_percent_nl, db_admin_languages_presence_nl)

data_db_admin_language_dt_percent_nl <- subset(data_db_admin_language_dt_percent_nl, data_db_admin_language_dt_percent_nl$presence == 3 | data_db_admin_language_dt_percent_nl$LanguageWorkedWith =="Bash/Shell/PowerShell" )

data_db_admin_language_dt_percent_nl$distribution <- (data_db_admin_language_dt_percent_nl$users)*100/data_db_admin_language_dt_percent_nl$overall_users

data_db_admin_language_dt_percent_nl$Year <- as.integer(data_db_admin_language_dt_percent_nl$Year)

#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 1
data_db_admin_language_dt_percent_nl_ggplot <- ggplot(data_db_admin_language_dt_percent_nl, aes(x = Year, y = distribution, colour = LanguageWorkedWith, label = LanguageWorkedWith)) + geom_line()



#Line graphs of the percentage change of programming laguages in the world from 2017 - 2019 2
data_db_admin_language_nl_ggplot_grid = ggplot(data_db_admin_language_dt_percent_nl) + 
  geom_line(aes(x = Year, y = distribution, group = LanguageWorkedWith, colour = LanguageWorkedWith)) + 
  geom_text(data = subset(data_db_admin_language_dt_percent_nl, Year == 2019), aes(label = LanguageWorkedWith, colour = LanguageWorkedWith, x = Inf, y = distribution), hjust = -.1) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,6,1,1), "lines")) 

data_db_admin_language_nl_ggplot_grid_layout <- ggplotGrob(data_db_admin_language_nl_ggplot_grid)
data_db_admin_language_nl_ggplot_grid_layout$layout$clip[data_db_admin_language_nl_ggplot_grid_layout$layout$name == "panel"] <- "off"
grid.draw(data_db_admin_language_nl_ggplot_grid_layout)





