#Load packages
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

#Load data
data <- read_xlsx("Survey Summary.xlsx")

#remove row and column 
data <- data[,-5]
data <- data[-1,]

#Save colnames (full questions) for reference
questions <- as.data.frame(colnames(data)) 

#rename colnames to work with df
colnames(data) <- c("RespondentID", "CollrectorID", "StartDate", "EndDate", "Age", "Gender",
                    "Location", "Education", "Class", "SocialMedia", "v1", "Hours", "Posts",
                    "Information", "Trust", "MMR" , "Rare", "Parents", "Mercury", "Multiple", 
                    "CDC", "Series", "Death", "Decrease", "Global", "VAERS", "Immuno")


#Add scoring system. 0 = Pro, 1 = Unsure, 2 = Anti
data <- data %>% mutate(MMR2 = case_when(MMR == "I believe this is NOT true" ~ 0, 
                                 MMR == "I am unsure" ~ 1, 
                                 MMR == "I believe this is true" ~ 2)) %>% 
  mutate(Rare2 = case_when(Rare == "I believe this is NOT true" ~ 0, 
                          Rare == "I am unsure" ~ 1, 
                          Rare == "I believe this is true" ~ 2)) %>% 
  mutate(Parents2 = case_when(Parents == "I believe this is NOT true" ~ 0, 
                           Parents == "I am unsure" ~ 1, 
                           Parents == "I believe this is true" ~ 2)) %>% 
  mutate(Mercury2 = case_when(Mercury == "I believe this is NOT true" ~ 0, 
                              Mercury == "I am unsure" ~ 1, 
                              Mercury == "I believe this is true" ~ 2)) %>% 
  mutate(Multiple2 = case_when(Multiple == "I believe this is NOT true" ~ 0, 
                               Multiple == "I am unsure" ~ 1, 
                               Multiple == "I believe this is true" ~ 2)) %>% 
  mutate(CDC2 = case_when(CDC == "I believe this is NOT true" ~ 0, 
                          CDC == "I am unsure" ~ 1, 
                          CDC == "I believe this is true" ~ 2)) %>% 
  mutate(Series2 = case_when(Series == "I believe this is NOT true" ~ 2, 
                             Series == "I am unsure" ~ 1, 
                             Series == "I believe this is true" ~ 0)) %>% 
  mutate(Decrease2 = case_when(Decrease == "I believe this is NOT true" ~ 2, 
                               Decrease == "I am unsure" ~ 1, 
                               Decrease == "I believe this is true" ~ 0)) %>% 
  mutate(Global2 = case_when(Global == "I believe this is NOT true" ~ 2, 
                            Global == "I am unsure" ~ 1, 
                            Global == "I believe this is true" ~ 0)) %>% 
  mutate(VAERS2 = case_when(VAERS == "I believe this is NOT true" ~ 2, 
                            VAERS == "I am unsure" ~ 1, 
                            VAERS == "I believe this is true" ~ 0)) %>% 
  mutate(Immuno2 = case_when(Immuno == "I believe this is NOT true" ~ 2, 
                             Immuno == "I am unsure" ~ 1, 
                             Immuno == "I believe this is true" ~ 0)) %>% 
  mutate(Total = MMR2 + Rare2 + Parents2 + Mercury2 + CDC2 + Series2 + Decrease2 + Global2 +
           VAERS2 + Immuno2) %>% 
  mutate(TotalCat = case_when(Total == 0 ~ 0, 
                              MMR2 == 2 | Rare2 == 2 | Parents2 == 2 | Mercury2 == 2 |
                                CDC2 == 2 | Series2 == 2 |  Decrease2 == 2 |
                                Global2 == 2 | VAERS2 == 2 | Immuno2 == 2 ~ 2,
                              Total >= 1 & Total <= 10 ~ 1, 
                              Total > 10 ~ 2))

#Check distribution of summary score
ggplot(data, aes(x = TotalCat)) +
  geom_bar()

 data %>% count(TotalCat)

 #Export file
 write.csv(data, "AntiVaxUpdatedFile.csv")
 