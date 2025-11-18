# Load necessary libraries
library(dplyr) # for data manipulation
library(ggplot2) # for data visualization
library(stargazer) # for regression tables
library(lubridate) # for date manipulation


################################################################################
# News data from El País #######################################################
################################################################################

# Load news data from CAP (El País, Spain)
news <- read.csv("data/Media_El_Pas_Web_CAP_csv.csv")


# Create a clean date variable
news <- news %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day), 
                        format="%Y-%m-%d"))


# Let's focus on the topics health, law and crime, social welfare, and housing
# Select the topics based on the codes
news <- news %>% 
  mutate(health = majortopic == 3,
         law_crime = majortopic == 12,
         social_welfare = majortopic == 13,
         housing = majortopic == 14)


# Summarise the data based on total numbers and percentages
news %>% 
  summarise(total_articles = n(),
            health_articles = sum(health == TRUE) / n() * 100,
            law_crime_articles = sum(law_crime == TRUE) / n() * 100,
            social_welfare_articles = sum(social_welfare == TRUE) / n() * 100,
            housing_articles = sum(housing = TRUE)*100 / n() * 100) 
  

################################################################################
# Parliamentary oral question data, Congreso ###################################
################################################################################

parl_questions <- read.csv("data/Spain_OralQuestions19772019_19.1.csv")

# Let's correct the data format for the date variable
parl_questions <- parl_questions %>% 
  mutate(date = as.Date(date, format="%m/%d/%Y"))


# Let's focus on the topics health, law and crime, social welfare, and housing
# Select the topics based on the codes
parl_questions <- parl_questions %>% 
  mutate(health = majortopic == 3,
         law_crime = majortopic == 12,
         social_welfare = majortopic == 13,
         housing = majortopic == 14)


# Summarise the data based on total numbers and percentages
parl_questions %>% 
summarise(total_questions = n(),
          health_questions = sum(health == TRUE) / n() * 100,
          law_crime_questions = sum(law_crime == TRUE) / n() * 100,
          social_questions = sum(social_welfare == TRUE) / n() * 100,
          housing_questions = sum(housing = TRUE)*100 / n() * 100) 


################################################################################
# Year aggregation of the data and plotting ####################################
################################################################################

# Aggregate by year
news_yearly <- news %>% 
  group_by(year) %>% 
  summarise(total_articles = n(),
            health_share = sum(health == TRUE) / n() * 100,
            law_crime_share = sum(law_crime == TRUE) / n() * 100,
            social_welfare_share = sum(social_welfare == TRUE) / n() * 100,
            housing_share = sum(housing = TRUE)*100 / n() * 100) %>% 
  mutate(arena = "media")

# Aggregate by year
parl_questions_yearly <- parl_questions %>%
  group_by(year) %>% 
  summarise(total_questions = n(),
            health_share = sum(health == TRUE) / n() * 100,
            law_crime_share = sum(law_crime == TRUE) / n() * 100,
            social_welfare_share = sum(social_welfare == TRUE) / n() * 100,
            housing_share = sum(housing == TRUE)*100 / n() * 100) %>% 
  mutate(arena = "parliament")


# Combine both datasets ("vertically", long format)
yearly_data <- bind_rows(news_yearly, parl_questions_yearly)



# Time-Series plotting for **law & crime** based on each arena (media vs parliament)
# We define arena in the grouping specification through 'linetype'

ggplot(yearly_data, aes(x = year, y = law_crime_share,
                        linetype = arena)) +
  geom_line(size = 1.25) +
  theme_minimal()


# Time-Series plotting for **health** based on each arena (media vs parliament)
# We define arena in the grouping specification through 'linetype'

ggplot(yearly_data, aes(x = year, y = health_share,
                        linetype = arena)) +
  geom_line(size = 1.25) +
  theme_minimal()


################################################################################
# Time-series structure for regression analysis  ###############################
################################################################################

# Let's create a time-series data-set at the month level


# News monthly
news_monthly <- news %>% 
  
  # By year-month (middle of the month)
  mutate(year_month = as.Date(paste0(year, "-", month, "-15"), 
                               format="%Y-%m-%d")) %>%
  
  group_by(year_month) %>% 
  
  summarise(total_articles = n(),
            health_share.m = sum(health == TRUE) / n() * 100,
            law_crime_share.m = sum(law_crime == TRUE) / n() * 100,
            social_welfare_share.m = sum(social_welfare == TRUE) / n() * 100,
            housing_share.m = sum(housing == TRUE)*100 / n() * 100) %>% 
  
  # Assign an identifier for this data-set "part"
  mutate(arena = "media")


# Parliamentary questions monthly
parliamentry_questions_monthly <- parl_questions %>%
  
  # By year-month (middle of the month)
  # Here, we do not have a 'month' variable, so we extract it the date variable
  # using the lubridate package
  # Why do we not have a month variable here? IDK, ask the authors!!!
  # Ideal data-set harmonisation would offer all variables symmetrically
  
  mutate(year_month = as.Date(paste0(year, "-", lubridate::month(date), "-15"), 
                               format="%Y-%m-%d")) %>%
  
  group_by(year_month) %>% 
  
  summarise(total_questions = n(),
            health_share.p = sum(health == TRUE) / n() * 100,
            law_crime_share.p = sum(law_crime == TRUE) / n() * 100,
            social_welfare_share.p = sum(social_welfare == TRUE) / n() * 100,
            housing_share.p = sum(housing == TRUE)*100 / n() * 100) %>%
  # Assign an identifier for this data-set "part"
  mutate(arena = "parliament")


# Check date ranges to check the data structure!!!
min(parl_questions$date, na.rm = TRUE)
max(parl_questions$date, na.rm = TRUE)


# Merge both data-sets by year-month "horizontally", wide-format (!!!)
monthly_ts <- news_monthly %>% 
  dplyr::left_join(parliamentry_questions_monthly,
                   by = c("year_month"))


# Arrange in ascending order by year-month
monthly_ts <- monthly_ts %>% 
  arrange(year_month)


# Create lags by one month (!) 

monthly_ts <- monthly_ts %>% 
  # Create lagged variables for several variables for y year-month
  
  mutate(# Parliament lags
         law_crime_share.p.lag1 = dplyr::lag(law_crime_share.p, n = 1),
         health_share.p.lag1 = dplyr::lag(health_share.p, n = 1),
         social_welfare_share.p.lag1 = dplyr::lag(social_welfare_share.p, n = 1),
         housing_share.p.lag1 = dplyr::lag(housing_share.p, n = 1),
  
         # Media lags
         law_crime_share.m.lag1 = dplyr::lag(law_crime_share.m, n = 1),
         health_share.m.lag1 = dplyr::lag(health_share.m, n = 1),
         social_welfare_share.m.lag1 = dplyr::lag(social_welfare_share.m, n = 1),
         housing_share.m.lag1 = dplyr::lag(housing_share.m, n = 1)
         )


################################################################################
# TS regression analysis #######################################################
################################################################################

## Law and crime ###############################################################

# We want to explain the current level of law-crime parliamentary questions
# by the previous level of the same variable and the previous level of media
# coverage on law and crime
  
mp_law_crime.simple <- lm(law_crime_share.p ~ law_crime_share.m.lag1 +
                            law_crime_share.p.lag1,
                          data = monthly_ts)

# summary(mp_law_crime.simple)  


## Health ######################################################################
mp_health.simple <- lm(health_share.p ~ health_share.m.lag1 +
                         health_share.p.lag1,
                        data = monthly_ts)


## Social welfare ##############################################################
mp_social_welfare.simple <- lm(social_welfare_share.p ~ social_welfare_share.m.lag1 +
                                 social_welfare_share.p.lag1,
                        data = monthly_ts)


## Housing #####################################################################
mp_housing.simple <- lm(housing_share.p ~ housing_share.m.lag1 +
                          housing_share.p.lag1,
                        data = monthly_ts)


# Summarise the regressions!
stargazer::stargazer(mp_law_crime.simple, mp_health.simple, type = "text")
stargazer::stargazer(mp_social_welfare.simple, mp_housing.simple, type = "text")







# The End ######################################################################

  