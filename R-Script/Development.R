library(readr)
library(dplyr)
library(tidyr)
library(data.table)

dataset <- read_csv ("C:/Users/Steur/Documents/GitHub/evert-study/data/developer_survey_2019/survey_results_public.csv")
dataset <- read_csv ("C:/Users/esteur002/Documents/GitHub/evert-study/data/developer_survey_2019/survey_results_public.csv")
View(dataset)

dataset1 <- dataset %>%
  select(Respondent, DevType)
View(dataset1)

keys <- group_keys(dataset1)
View(keys)

dataset2 <- dataset %>%
  select(Respondent, DevType) %>%
  mutate(DevType = gsub(";.*","",DevType)) %>%
  group_by(DevType) %>%
  count()
View(dataset2)


dataset3 <- dataset %>% 
  select(Respondent, DevType, LanguageDesireNextYear) %>%
  mutate(DevType = gsub(";.*","", DevType))
View(dataset3)

dataset4 <- dataset3 %>%
  separate(LanguageDesireNextYear, into = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"), sep = ";")
View(dataset4)

dataset5 <- dataset4 %>%
  select(Respondent, DevType, A)%>%
  filter(DevType == "System administrator") %>%
  group_by(DevType,A)%>%
  count()

View(dataset5)

NL <- dataset %>%
  select(Respondent, DevType, Country, LanguageDesireNextYear) %>%
  filter(Country == "Netherlands") %>%
  mutate(DevType = gsub(";.*","", DevType)) %>%
  filter(DevType == "Developer, back-end") %>%
  separate(LanguageDesireNextYear, into = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"), sep = ";") %>%
  group_by(DevType, A) %>%
  count() %>%
  arrange(desc(n)) %>%
  top_n( n = 10)
  
library(ggplot2)

ggplot(NL, aes( x =A, y = n)) + 
  geom_col()





