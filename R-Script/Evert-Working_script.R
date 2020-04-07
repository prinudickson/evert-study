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

#Set this based on your laptop's working directory
setwd("C:/Users/esteur002/Documents/GitHub/evert-study")
setwd("C:/Users/evert/OneDrive/Documenten/GitHub/evert-study")

#data for 2016

data_ini_2016 <- read_csv(file = "C:/Users/Steur/OneDrive/Documenten/GitHub/evert-study/data/developer_survey_2016/2016 Stack Overflow Survey Responses.csv")


#data for 2017
data_ini_2017 <- read_csv(file = "C:/Users/Steur/OneDrive/Documenten/GitHub/evert-study/data/developer_survey_2017/survey_results_public.csv")

data_schema_2017 <- read_csv(file = "C:/Users/Steur/OneDrive/Documenten/GitHub/evert-study/data/developer_survey_2017/survey_results_schema.csv")


#data for 2018
data_ini_2018 <- read_csv(file = "C:/Users/Steur/OneDrive/Documenten/GitHub/evert-study/data/developer_survey_2018/survey_results_public.csv")


#data for 2019
data_ini_2019 <- read_csv(file = "C:/Users/Steur/OneDrive/Documenten/GitHub/evert-study/data/developer_survey_2019/survey_results_public.csv")


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


# ADDED. WHEN NEEDED WE CAN USE IT. FOR NOW NOT NEEDED.

#Data transformation 2017 1----
#Creating a common Education level.

data_2017_filter$FormalEducation <- as.character(data_2017_filter$FormalEducation)

# if(as.character(data_2017_filter$FormalEducation) == "Bachelor's degree"){
#   data_2017_filter$EducationLevel_clean <- "Bachelors"
# } else if(data_2017_filter$FormalEducation == "Master's degree"){
#   data_2017_filter$EducationLevel_clean <-  "Masters"
# } else if (data_2017_filter$FormalEducation == "Some college/university study without earning a bachelor's degree") {
#   data_2017_filter$EducationLevel_clean <- "Did not finish college/Uni"  
# }else if(data_2017_filter$FormalEducation == "I never completed any formal education"){
#   data_2017_filter$EducationLevel_clean <- "Never completed any formal education"
# } else if(data_2017_filter$FormalEducation == "Primary/elementary school"){
#   data_2017_filter$EducationLevel_clean <- "Primary School"
# }else if(data_2017_filter$FormalEducation == "Secondary school"){
#   data_2017_filter$EducationLevel_clean <- "Secondary school"
# }else{
#   data_2017_filter$EducationLevel_clean <- "others"
# }
 
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

View(data_2017_devtype)

#trims remove the spaces 

#Language Analysis----

data_2017_language_ini <- data_2017_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2017_language <- separate_rows(data_2017_language_ini, LanguageWorkedWith, sep=";")

data_2017_language$LanguageWorkedWith <- trimws(data_2017_language$LanguageWorkedWith)

data_2017_language$Year <- "2017"

View(data_2017_language)

#Future Language Analysis----

data_2017_futurelanguage_ini <- data_2017_filter %>%
  select(Respondent, LanguageDesireNextYear)

data_2017_futurelanguage <- separate_rows(data_2017_futurelanguage_ini, LanguageDesireNextYear, sep=";")


data_2017_futurelanguage$LanguageDesireNextYear <- trimws(data_2017_futurelanguage$LanguageDesireNextYear)

View(data_2017_futurelanguage)

#Prepare the data for 2018----

#Selecting the required columnms which will be mapped to prepare the 2018 data.----
#Note that the 2018 data has not been transformed or cleaned a lot as this was used as a base for transforming 2017 and 2019 data. 
# 2018 is de basis. Vanuit deze dataset is gewerkt om 2017 + 2018 hetzelfde te krijgen als 2018.

data_2018_filter <- data_ini_2018 %>%
                    select(Respondent, Country, Employment, DevType, FormalEducation, LanguageWorkedWith, LanguageDesireNextYear, 
                           FrameworkWorkedWith, FrameworkDesireNextYear, DatabaseWorkedWith, DatabaseDesireNextYear, PlatformWorkedWith, PlatformDesireNextYear)

data_2018_filter$educationlevel_clean <-  ifelse(data_2018_filter$FormalEducation == "Bachelorâ€™s degree (BA, BS, B.Eng., etc.)", "Bachelors", 
                                                 ifelse(data_2018_filter$FormalEducation == "Masterâ€™s degree (MA, MS, M.Eng., MBA, etc.)", "Masters",
                                                        ifelse(data_2018_filter$FormalEducation == "Some college/university study without earning a degree", "Did not finish College/Uni",
                                                               ifelse(data_2018_filter$FormalEducation == "I never completed any formal education", "Never completed any formal education",
                                                                      ifelse(data_2018_filter$FormalEducation == "Primary/elementary school", "Primary school",
                                                                             ifelse(data_2018_filter$FormalEducation == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", "Secondary school", "Others" ))))))

#DevType Analysis----

data_2018_devtype_ini <- data_2018_filter %>%
  select(Respondent, DevType)

data_2018_devtype <- separate_rows(data_2018_devtype_ini,DevType,sep=";")

data_2018_devtype$DevType <- trimws(data_2018_devtype$DevType)

#Language Analysis----

data_2018_language_ini <- data_2018_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2018_language <- separate_rows(data_2018_language_ini, LanguageWorkedWith, sep=";")

data_2018_language$LanguageWorkedWith <- trimws(data_2018_language$LanguageWorkedWith)

data_2018_language$Year <- "2018"


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

# ADDED. WHEN NEEDED WE CAN USE IT. FOR NOW NOT NEEDED.

data_2019_filter$educationlevel_clean <-  ifelse(data_2019_filter$FormalEducation == "Bachelorâ€™s degree (BA, BS, B.Eng., etc.)", "Bachelors", 
                                                 ifelse(data_2019_filter$FormalEducation == "Masterâ€™s degree (MA, MS, M.Eng., MBA, etc.)", "Masters",
                                                        ifelse(data_2019_filter$FormalEducation == "Some college/university study without earning a degree", "Did not finish College/Uni",
                                                               ifelse(data_2019_filter$FormalEducation == "I never completed any formal education", "Never completed any formal education",
                                                                      ifelse(data_2019_filter$FormalEducation == "Primary/elementary school", "Primary school",
                                                                             ifelse(data_2019_filter$FormalEducation == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", "Secondary school", "Others" ))))))

#DevType Analysis----

data_2019_devtype_ini <- data_2019_filter %>%
  select(Respondent, DevType)

data_2019_devtype <- separate_rows(data_2019_devtype_ini,DevType,sep=";")

data_2019_devtype$DevType <- trimws(data_2019_devtype$DevType)

#Language Analysis----

data_2019_language_ini <- data_2019_filter %>%
  select(Respondent, LanguageWorkedWith, Country)

data_2019_language <- separate_rows(data_2019_language_ini, LanguageWorkedWith, sep=";")

data_2019_language$LanguageWorkedWith <- trimws(data_2019_language$LanguageWorkedWith)

data_2019_language$Year <- "2019"

View(data_2019_language)


#Future Language Analysis----

data_2019_futurelanguage_ini <- data_2019_filter %>%
  select(Respondent, LanguageDesireNextYear)

data_2019_futurelanguage <- separate_rows(data_2019_futurelanguage_ini, LanguageDesireNextYear, sep=";")

data_2019_futurelanguage$LanguageDesireNextYear <- trimws(data_2019_futurelanguage$LanguageDesireNextYear)


#Aggregated data sets----

#Create the World distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

data_language <- rbind(data_2017_language, data_2018_language, data_2019_language)

View(data_language)

data_language_clean<- na.omit(data_language, cols = "LanguageWorkedWith")

View(data_language_clean)

data_language_dt <- data_language_clean %>%
                    group_by(LanguageWorkedWith, Year) %>%
                    summarize(users = n_distinct(Respondent))

View(data_language_dt)

users_by_year <- data_language %>%
                  group_by(Year) %>%
                  summarize(overall_users = n_distinct(Respondent))

View(users_by_year)

languages_presence <- data_language %>%
                        group_by(LanguageWorkedWith) %>%
                        summarize(presence = n_distinct(Year))

View(languages_presence)

data_language_dt_percent <- merge(data_language_dt, users_by_year)

View(data_language_dt_percent)

data_language_dt_percent <- merge(data_language_dt_percent, languages_presence)

data_language_dt_percent <- subset(data_language_dt_percent, data_language_dt_percent$presence == 3)

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

#Reference------

#For the line graph generator----
#https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

#Create the Netherlands distribution plot of popular programming languages-----
#Make sure these are a percentage of the respondents so that we can compare it to the Netherlands. 

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




