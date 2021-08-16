# // Analysis script for PSY6009 Research Project - Jarod Wilson //

# Packages
library(tidyverse) # General data science uses
library(knitr) # Better tables
library(easystats) # Statistical modelling and assessments -> correlation, parameters, performance
library(lavaan) # Structural Equation Modelling
library(tidySEM)

# Working directory
setwd("/Volumes/JAROD/C19PRC-UK_Waves1and2OSF/data")

# Import data
# Notes.
# Prior to importing the data I saved the original SPSS file as a CSV file to make importing easier.
# The C19PRC data is openly available throught the Open Science Framework, accessible from: https://osf.io/v2zur/.
# The data regarding the facial trustworthiness task was kindly forwarded to me by Anton Martinez.

C19PRC <- read.csv('/Volumes/JAROD/C19PRC-UK_Waves1and2OSF/data/C19PRC-UK-W1_W2.csv')
trust_faces <- read.csv('/Volumes/JAROD/C19PRC-UK_Waves1and2OSF/data/C19PRC-faces-trust.csv')

# / Data management and manipulation / --------------------------------------------------------------------------------

# Create new 'working' dataframes and select varibles to be used in analysis
C19PRC_w <- C19PRC %>% 
  select('pid', 'W1_Age_year', 'W1_Gender', # Demographics 
         'W1_Income_2019', 'W1_Eng_Crime_Decile', 'W1_Pop_Density', # Predictors and Controls
         'W1_Depression_Total', 'W1_Depression_Cat', 'W1_GAD_Total', 'W1_GAD_Cat', 'W1_Paranoia_Total', 'W1_Conspiracy_Total', # Outcomes
         'W1_Urbanicity', 'W1_Country_geo', 'W1_LA_Code', 'W1_LA_Name', 'W1_LA_Geography') # Geographical variables

trust_faces_w <- trust_faces %>% 
  select('pid', 'Neighbourhood_Trust', 'Bias')

# Join dataframes into single one for analysis
df <- full_join(C19PRC_w, trust_faces_w, by = "pid")

# For ease I am going to rename several variables, some for ease and some to follow naming conventions. 
df <- df %>%
  rename(age = W1_Age_year,
         gender = W1_Gender,
         income = W1_Income_2019,
         crime_decile = W1_Eng_Crime_Decile,
         population_density = W1_Pop_Density,
         depression_total = W1_Depression_Total,
         depression_case = W1_Depression_Cat,
         anxiety_total = W1_GAD_Total,
         anxiety_case = W1_GAD_Cat,
         paranoia = W1_Paranoia_Total,
         conspiracy = W1_Conspiracy_Total,
         trust_bias = Bias,
         neighbourhood_trust = Neighbourhood_Trust)

# Recode NA values for crime_decile as 5, i.e. the median.
df <- df %>%
  mutate(crime_decile = replace(crime_decile,is.na(crime_decile),5))

# Need to convert multiple variables into factors. 
factors <- c('gender', 'depression_case', 'anxiety_case')
df[,factors] <- lapply(df[,factors] , factor)

# and some into ordered factors (?)
# df$crime_decile <- factor(df$crime_decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
# df$income <- factor(df$income, levels = c("1", "2", "3", "4", "5")) 


# and give names for the gender factor for easier interpretation
df$gender <- recode_factor(df$gender, "1" = "Male", 
                                      "2" = "Female",
                                      "3" = "Transgender",
                                      "4" = "PNTS",
                                      "5" = "Other")

# Check
str(df)

# Save cleaned and manipulated dataframe as a .csv file
write.csv(df,"/Volumes/JAROD/C19PRC-UK_Waves1and2OSF/data/data.csv", row.names = TRUE)

# / Analyses / --------------------------------------------------------------------------------------------------------

# Further details regarding the variables can be found in the report of this study or from the codebooks available
# either in the GitHub repo for this study on the Open Science Framework.

# Descriptive statistics for appropriate variables
# Mean averages
kable(df %>%
  summarise(
    depression_mean = mean(depression_total, na.rm = TRUE),
    anxiety_mean = mean(anxiety_total, na.rm = TRUE),
    paranoia_mean = mean(paranoia, na.rm = TRUE),
    trust_bias_mean = mean(trust_bias, na.rm = TRUE),
    neighbourhood_trust_mean = mean(neighbourhood_trust, na.rm = TRUE),
    conspiracy_mean = mean(conspiracy, na.rm = TRUE)
    )
)

# Standard deviations
kable(df %>%
  summarise(
    depression_sd = sd(depression_total, na.rm = TRUE),
    anxiety_sd = sd(anxiety_total, na.rm = TRUE),
    paranoia_sd = sd(paranoia, na.rm = TRUE),
    trust_bias_sd = sd(trust_bias, na.rm = TRUE),
    neighbourhood_trust_sd = sd(neighbourhood_trust, na.rm = TRUE),
    conspiracy_sd = sd(conspiracy, na.rm = TRUE)
    )
)

# Correlations
df_cor <- df %>%
  select(depression_total,
         anxiety_total,
         paranoia,
         trust_bias,
         neighbourhood_trust,
         conspiracy)

correlations <- correlation(df_cor)
summary(correlations, redundant = TRUE)

# === Regression models ===

# H1 - Income inequality, depression and anxiety.

# Models for depression scores
lm_dep_score1 <- lm(depression_total ~ age + gender, 
                    data = df, na.action = na.exclude)

lm_dep_score2 <- lm(depression_total ~ age + gender + income + population_density + crime_decile, 
                    data = df, na.action = na.exclude)

anova(lm_dep_score1, lm_dep_score2)

summary(lm_dep_score2)
model_parameters(lm_dep_score2, standardize = "refit")

# Models for anxiety scores
lm_anx_score1 <- lm(anxiety_total ~ age + gender, 
                    data = df, na.action = na.exclude)

lm_anx_score2 <- lm(anxiety_total ~ age + gender + income + population_density + crime_decile, 
                    data = df, na.action = na.exclude)

anova(lm_anx_score1, lm_anx_score2)

summary(lm_anx_score2)
model_parameters(lm_anx_score2, standardize = "refit")

# Models for depression caseness
glm_dep_case1 <- glm(depression_case ~ age + gender,
                     data = df, family = binomial(link = 'logit'))

glm_dep_case2 <- glm(depression_case ~ age + gender + income + population_density + crime_decile, 
                     data = df, family = binomial(link = 'logit'))

anova(glm_dep_case1, glm_dep_case2, test = "LR")
summary(glm_dep_case2, test = "LR")
exp(glm_dep_case2$coefficients)

# Models for anxiety caseness
glm_anx_case1 <- glm(anxiety_case ~ age + gender,
                    data = df, family = binomial(link = 'logit'))

glm_anx_case2 <- glm(anxiety_case ~ age + gender + income + population_density + crime_decile, 
                     data = df, family = binomial(link = 'logit'))

anova(glm_anx_case1, glm_anx_case2, test = "LR")
summary(glm_anx_case2, test = "LR")
exp(glm_anx_case2$coefficients)

# H2 - Crime rate, paranoia and trust judgements

# Models for paranoia
lm_paranoia1 <- lm(paranoia ~ age + gender,
                   data = df, na.action = na.exclude)

lm_paranoia2 <- lm(paranoia ~ age + gender + crime_decile + population_density + income,
                   data = df, na.action = na.exclude)

anova(lm_paranoia1, lm_paranoia2)

summary(lm_paranoia2)
model_parameters(lm_paranoia2, standardize = "refit")

# Models for trust judgements (bias)
lm_trust1 <- lm(trust_bias ~ age + gender,
                data = df, na.action = na.exclude)

lm_trust2 <- lm(trust_bias ~ age + gender + crime_decile + population_density + income,
                   data = df, na.action = na.exclude)

anova(lm_trust1, lm_trust2)

summary(lm_trust2)
model_parameters(lm_trust2, standardize = "refit")

# === Structural Equation Model === 

# Select variables to be used
 
C19PRC_sem <- C19PRC %>% 
  select('pid',
         'W1_Paranoia1', 'W1_Paranoia2', 'W1_Paranoia3', 'W1_Paranoia4', 'W1_Paranoia5', 'W1_Paranoia_Total',
         'W1_Conspiracy_1', 'W1_Conspiracy_2', 'W1_Conspiracy_3', 'W1_Conspiracy_4', 'W1_Conspiracy_5', 'W1_Conspiracy_Total',
         'W1_Eng_Crime_Decile')

# Recode NA values for crime_decile as 5, i.e. the median.
C19PRC_sem <- C19PRC_sem %>% mutate_all(funs(replace_na(.,5)))

trust_faces_sem <- trust_faces %>%
  select('pid',
         'Neighbourhood_Trust', 'Bias')

# Join dataframes for full SEM data 
df_sem <- full_join(C19PRC_sem, trust_faces_sem, by = "pid")

# Rename
df_sem <- df_sem %>% 
  rename(PaDS1 = W1_Paranoia1, 
         PaDS2 = W1_Paranoia2, 
         PaDS3 = W1_Paranoia3, 
         PaDS4 = W1_Paranoia4,
         PaDS5 = W1_Paranoia5,
         Paranoia_Total = W1_Paranoia_Total,
         CMQ1 = W1_Conspiracy_1,
         CMQ2 = W1_Conspiracy_2,
         CMQ3 = W1_Conspiracy_3,
         CMQ4 = W1_Conspiracy_4,
         CMQ5 = W1_Conspiracy_5,
         Conspiracy_Total = W1_Conspiracy_Total,
         Crime_decile = W1_Eng_Crime_Decile)

df_sem <- df_sem %>%
  mutate(Crime_decile = replace(Crime_decile,is.na(Crime_decile),5))

# Save dataframe 
write.csv(df_sem,"/Volumes/JAROD/C19PRC-UK_Waves1and2OSF/data/SEModel_data.csv", row.names = TRUE)




