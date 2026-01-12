# Load necessary libraries
# install.packages("dplyr")
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(lubridate) # for date manipulation


################################################################################
# Parliamentary debates in SPA and GER #########################################
################################################################################

## You can download the ParlSpeech V2 data from here:
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L4OAKN 

# Spain
## Load the data
spain_speeches <- readRDS("data/Corp_Congreso_V2.rds")

## Correct date format and filter out chair speeches
spain_speeches <- spain_speeches %>% 
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  dplyr::filter(! chair == TRUE)

## Identify speeches mentioning protests
spain_speeches <- spain_speeches %>%
  mutate(protesta = grepl("movizilaci.n|protestas|marcha|marchas|demostraciones", 
                             tolower(text), 
                             ignore.case = TRUE))

# Germany
## Load the data
german_speeches <- readRDS("data/Corp_Bundestag_V2.rds")

## Correct date format and filter out chair speeches
german_speeches <- german_speeches %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>% 
  dplyr::filter(! chair == TRUE)

## Identify speeches mentioning protests
german_speeches <- german_speeches %>%
  mutate(protesta = grepl("protest|proteste|demonstration|demonstrationen|kundgebung|kundgebungen|demos|demonstrieren", 
                          tolower(text), 
                          ignore.case = TRUE))

# Combine both datasets
protest_parliament_data <- bind_rows(
  spain_speeches %>% 
    mutate(country = "Spain") %>%
    select(date, protesta, country, party),
  german_speeches %>%
    mutate(country = "Germany") %>%
    select(date, protesta, country, party)
) 

# saveRDS(protest_parliament_data, 
    #    file = "data/protest_parliament_data.rds")



## Create year variable
protest_parliament_data <- protest_parliament_data %>%
  mutate(year = lubridate::year(date))


# Plot lines over time
ggplot(protest_parliament_data %>% 
         group_by(year, country) %>%
         summarise(total_speeches = n(),
                   protesta_speeches = sum(protesta == TRUE),
                   protesta_share = sum(protesta == TRUE) / n() * 100),
       aes(x = year, y = protesta_share, color = country)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  ggtitle("Share of parliamentary speeches mentioning protests")

protest_parliament_data <- protest_parliament_data %>% 
  mutate(month_date = as.Date(paste0(year, "-", 
                                     lubridate::month(date),
                                     "-15"), 
                              format="%Y-%m-%d"))


## Clean up
rm(spain_speeches, german_speeches)

################################################################################
# Comparative proteste data in Europe, PolDem ##################################
################################################################################

# Download the PolDem protest-30 data from: https://poldem.eui.eu/download/

# Load the data
protest_data <- read.csv("data/poldem-protest_30.csv")
protest_data <- protest_data %>% 
  dplyr::filter(country_name %in% c("spain", "germany"))

protest_data <- protest_data %>% 
  mutate(date = as.Date(date))

# Plot number of protests over time by country

ggplot(protest_data %>% 
         group_by(year, country_name) %>%
         summarise(total_protests = n()),
       aes(x = year, y = total_protests, color = country_name)) +
  geom_line(size = 1.25) +
  theme_minimal() +
  ggtitle("Number of protests over time by country") +
  xlab("Year") +
  ylab("Number of protests")


protest_data <- protest_data %>% 
  mutate(month_date = as.Date(paste0(year, "-", month, "-15"), 
                             format="%Y-%m-%d")) 

################################################################################
# Data aggregation for TSCS analysis ###########################################
################################################################################

options(scipen = 999)

parliamentary_references <- protest_parliament_data %>%
  group_by(country, month_date) %>%
  summarise(total_speeches = n(),
            protest_speeches = sum(protesta == TRUE)) %>% 
  mutate(protest_speech_share = protest_speeches / total_speeches * 100) %>% 
  ungroup()

protest_events <- protest_data %>%
  group_by(country_name, month_date) %>%
  summarise(total_protests = n(),
            participants = sum(weighted_part_all, na.rm = TRUE),
            not_radical = sum(radical_action == "not radical")) %>% 
  ungroup()

parliamentary_references <- parliamentary_references %>%
  mutate(country_name = recode(country,
                                 "Spain" = "spain",
                                 "Germany" = "germany")) %>% 
  dplyr::select(-country)
  
  
  
data_ts <- left_join(
  parliamentary_references,
  protest_events,
  by = c("country_name" = "country_name", "month_date" = "month_date")
)


rm(protest_parliament_data, protest_data,
   protest_events, parliamentary_references)

data_ts <- data_ts %>%
  arrange(country_name, month_date) %>% 
  group_by(country_name) %>%
  mutate(participants.lag1 = dplyr::lag(participants, n = 1),
         total_protests.lag1 = dplyr::lag(total_protests, n = 1),
         protest_speech_share.lag1 = dplyr::lag(protest_speech_share, n = 1),
         not_radical.lag1 = dplyr::lag(not_radical, n = 1)) %>%
  ungroup()



################################################################################
# Time-series regression analysis  #############################################
################################################################################


## Law and crime ###############################################################

# We want to explain the current level of law-crime parliamentary questions
# by the previous level of the same variable and the previous level of media
# coverage on law and crime
  
basic_model <- lm(protest_speech_share ~ total_protests.lag1 +
                protest_speech_share.lag1 + country_name,
                          data = data_ts)

summary(basic_model) 
 

# Spain
spain_basic <- lm(protest_speech_share ~ total_protests.lag1 +
                     protest_speech_share.lag1,
                   data = data_ts %>% dplyr::filter(country_name == "spain"))
 
summary(spain_basic)  

 
spain_participants <- lm(protest_speech_share ~ log(participants.lag1) +
                    protest_speech_share.lag1,
                  data = data_ts %>% dplyr::filter(country_name == "spain"))
 
summary(spain_participants)  

spain_not.radical <- lm(protest_speech_share ~ not_radical.lag1 +
                           protest_speech_share.lag1,
                         data = data_ts %>% dplyr::filter(country_name == "spain"))

summary(spain_not.radical)  


# Germany
germany_basic <- lm(protest_speech_share ~ total_protests.lag1 +
                    protest_speech_share.lag1 ,
                  data = data_ts %>% dplyr::filter(country_name == "germany"))

summary(germany_basic)  


germany_participants <- lm(protest_speech_share ~ log(participants.lag1) +
                           protest_speech_share.lag1 ,
                         data = data_ts %>% dplyr::filter(country_name == "germany"))

summary(germany_participants)  

germany_not.radical <- lm(protest_speech_share ~ not_radical.lag1 +
                          protest_speech_share.lag1 ,
                        data = data_ts %>% dplyr::filter(country_name == "germany"))

summary(germany_not.radical)

