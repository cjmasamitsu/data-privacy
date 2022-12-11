
# Casey Masamitsu | Data Analytics & Policy Capstone 2022 -----------------

# Load packages ----------------------------------------------------------

library(ggplot2)
library(AER)
library(plm)
library(tidyverse)
library(stargazer)


# Load data and select observations ---------------------------------------

data <- read_csv("apnorc_poll_apr2018.csv")
glimpse(data)

data <- data %>% select("SU_ID", 
                        "PRIV3A":"PRIV4F", 
                        "PRIV10A":"PRIV10D",
                        "POLITICS",
                        "URBAN":"CENSUS_REGION")


# Quantify responses to questions of interest -----------------------------

data$PRIV3A[data$PRIV3A == "(1) Extremely concerned"] <- 4
data$PRIV3A[data$PRIV3A == "(2) Very concerned"] <- 3
data$PRIV3A[data$PRIV3A == "(3) Moderately concerned"] <- 2
data$PRIV3A[data$PRIV3A == "(4) Not very concerned" ] <- 1
data$PRIV3A[data$PRIV3A == "(5) Not at all concerned"] <- 0
data$PRIV3A[data$PRIV3A == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV3A <- as.numeric(data$PRIV3A)

data$PRIV3B[data$PRIV3B == "(1) Extremely concerned"] <- 4
data$PRIV3B[data$PRIV3B == "(2) Very concerned"] <- 3
data$PRIV3B[data$PRIV3B == "(3) Moderately concerned"] <- 2
data$PRIV3B[data$PRIV3B == "(4) Not very concerned" ] <- 1
data$PRIV3B[data$PRIV3B == "(5) Not at all concerned"] <- 0
data$PRIV3B[data$PRIV3B == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV3B <- as.numeric(data$PRIV3B)

data$PRIV3C[data$PRIV3C == "(1) Extremely concerned"] <- 4
data$PRIV3C[data$PRIV3C == "(2) Very concerned"] <- 3
data$PRIV3C[data$PRIV3C == "(3) Moderately concerned"] <- 2
data$PRIV3C[data$PRIV3C == "(4) Not very concerned" ] <- 1
data$PRIV3C[data$PRIV3C == "(5) Not at all concerned"] <- 0
data$PRIV3C[data$PRIV3C == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV3C <- as.numeric(data$PRIV3C)

data$PRIV3D[data$PRIV3D == "(1) Extremely concerned"] <- 4
data$PRIV3D[data$PRIV3D == "(2) Very concerned"] <- 3
data$PRIV3D[data$PRIV3D == "(3) Moderately concerned"] <- 2
data$PRIV3D[data$PRIV3D == "(4) Not very concerned" ] <- 1
data$PRIV3D[data$PRIV3D == "(5) Not at all concerned"] <- 0
data$PRIV3D[data$PRIV3D == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV3D <- as.numeric(data$PRIV3D)

data$PRIV4A[data$PRIV4A == "(1) Extremely concerned"] <- 4
data$PRIV4A[data$PRIV4A == "(2) Very concerned"] <- 3
data$PRIV4A[data$PRIV4A == "(3) Moderately concerned"] <- 2
data$PRIV4A[data$PRIV4A == "(4) Not very concerned" ] <- 1
data$PRIV4A[data$PRIV4A == "(5) Not at all concerned"] <- 0
data$PRIV4A[data$PRIV4A == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4A <- as.numeric(data$PRIV4A)

data$PRIV4B[data$PRIV4B == "(1) Extremely concerned"] <- 4
data$PRIV4B[data$PRIV4B == "(2) Very concerned"] <- 3
data$PRIV4B[data$PRIV4B == "(3) Moderately concerned"] <- 2
data$PRIV4B[data$PRIV4B == "(4) Not very concerned" ] <- 1
data$PRIV4B[data$PRIV4B == "(5) Not at all concerned"] <- 0
data$PRIV4B[data$PRIV4B == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4B <- as.numeric(data$PRIV4B)

data$PRIV4C[data$PRIV4C == "(1) Extremely concerned"] <- 4
data$PRIV4C[data$PRIV4C == "(2) Very concerned"] <- 3
data$PRIV4C[data$PRIV4C == "(3) Moderately concerned"] <- 2
data$PRIV4C[data$PRIV4C == "(4) Not very concerned" ] <- 1
data$PRIV4C[data$PRIV4C == "(5) Not at all concerned"] <- 0
data$PRIV4C[data$PRIV4C == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4C <- as.numeric(data$PRIV4C)

data$PRIV4D[data$PRIV4D == "(1) Extremely concerned"] <- 4
data$PRIV4D[data$PRIV4D == "(2) Very concerned"] <- 3
data$PRIV4D[data$PRIV4D == "(3) Moderately concerned"] <- 2
data$PRIV4D[data$PRIV4D == "(4) Not very concerned" ] <- 1
data$PRIV4D[data$PRIV4D == "(5) Not at all concerned"] <- 0
data$PRIV4D[data$PRIV4D == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4D <- as.numeric(data$PRIV4D)

data$PRIV4E[data$PRIV4E == "(1) Extremely concerned"] <- 4
data$PRIV4E[data$PRIV4E == "(2) Very concerned"] <- 3
data$PRIV4E[data$PRIV4E == "(3) Moderately concerned"] <- 2
data$PRIV4E[data$PRIV4E == "(4) Not very concerned" ] <- 1
data$PRIV4E[data$PRIV4E == "(5) Not at all concerned"] <- 0
data$PRIV4E[data$PRIV4E == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4E <- as.numeric(data$PRIV4E)

data$PRIV4F[data$PRIV4F == "(1) Extremely concerned"] <- 4
data$PRIV4F[data$PRIV4F == "(2) Very concerned"] <- 3
data$PRIV4F[data$PRIV4F == "(3) Moderately concerned"] <- 2
data$PRIV4F[data$PRIV4F == "(4) Not very concerned" ] <- 1
data$PRIV4F[data$PRIV4F == "(5) Not at all concerned"] <- 0
data$PRIV4F[data$PRIV4F == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV4F <- as.numeric(data$PRIV4F)

data$PRIV10A[data$PRIV10A == "(1) Very large"] <- 4
data$PRIV10A[data$PRIV10A == "(2) Large"] <- 3
data$PRIV10A[data$PRIV10A == "(3) Moderate"] <- 2
data$PRIV10A[data$PRIV10A == "(4) Small"] <- 1
data$PRIV10A[data$PRIV10A == "(5) None at all"] <- 0
data$PRIV10A[data$PRIV10A == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV10A <- as.numeric(data$PRIV10A)

data$PRIV10B[data$PRIV10B == "(1) Very large"] <- 4
data$PRIV10B[data$PRIV10B == "(2) Large"] <- 3
data$PRIV10B[data$PRIV10B == "(3) Moderate"] <- 2
data$PRIV10B[data$PRIV10B == "(4) Small"] <- 1
data$PRIV10B[data$PRIV10B == "(5) None at all"] <- 0
data$PRIV10B[data$PRIV10B == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV10B <- as.numeric(data$PRIV10B)

data$PRIV10C[data$PRIV10C == "(1) Very large"] <- 4
data$PRIV10C[data$PRIV10C == "(2) Large"] <- 3
data$PRIV10C[data$PRIV10C == "(3) Moderate"] <- 2
data$PRIV10C[data$PRIV10C == "(4) Small"] <- 1
data$PRIV10C[data$PRIV10C == "(5) None at all"] <- 0
data$PRIV10C[data$PRIV10C == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV10C <- as.numeric(data$PRIV10C)

data$PRIV10D[data$PRIV10D == "(1) Very large"] <- 4
data$PRIV10D[data$PRIV10D == "(2) Large"] <- 3
data$PRIV10D[data$PRIV10D == "(3) Moderate"] <- 2
data$PRIV10D[data$PRIV10D == "(4) Small"] <- 1
data$PRIV10D[data$PRIV10D == "(5) None at all"] <- 0
data$PRIV10D[data$PRIV10D == "(99) DON\x92T KNOW/SKIPPED/REFUSED (VOL)"] <- NA
data$PRIV10D <- as.numeric(data$PRIV10D)


# Drop NA's and obtain statistics for Californians vs Other States -------------

data <- drop_na(data)
data$CALI <- ifelse(data$STATE == "(CA) California", 1, 0)
cali <- subset(data, data$CALI == "1") 
calino <- subset(data, data$CALI == "0")

cali_one <- (cali$PRIV3A + 
               cali$PRIV3B + 
               cali$PRIV3C + 
               cali$PRIV3D) / 4 
cali_two <- (cali$PRIV4A + 
               cali$PRIV4B + 
               cali$PRIV4C + 
               cali$PRIV4D + 
               cali$PRIV4E + 
               cali$PRIV4F) / 6

calino_one <- (calino$PRIV3A + 
                 calino$PRIV3B + 
                 calino$PRIV3C + 
                 calino$PRIV3D) / 4 
calino_two <- (calino$PRIV4A + 
                 calino$PRIV4B + 
                 calino$PRIV4C + 
                 calino$PRIV4D + 
                 calino$PRIV4E + 
                 calino$PRIV4F) / 6

umb_one <- (data$PRIV3A + 
              data$PRIV3B + 
              data$PRIV3C + 
              data$PRIV3D) / 4 
umb_two <- (data$PRIV4A + 
              data$PRIV4B + 
              data$PRIV4C + 
              data$PRIV4D + 
              data$PRIV4E + 
              data$PRIV4F) / 6

# Create Data Privacy Tolerance Score -------------------------------------

data$DPTS <- (((data$PRIV3A + 
    data$PRIV3B + 
    data$PRIV3C + 
    data$PRIV3D) / 4) + 
  ((data$PRIV4A + 
     data$PRIV4B + 
     data$PRIV4C + 
     data$PRIV4D + 
     data$PRIV4E + 
     data$PRIV4F) / 6)) / 2
data$DPTS <- round(data$DPTS, 2)

cali$DPTS <- (((cali$PRIV3A + 
                  cali$PRIV3B + 
                  cali$PRIV3C + 
                  cali$PRIV3D) / 4) + 
                ((cali$PRIV4A + 
                    cali$PRIV4B + 
                    cali$PRIV4C + 
                    cali$PRIV4D + 
                    cali$PRIV4E + 
                    cali$PRIV4F) / 6)) / 2
cali$DPTS <- round(cali$DPTS, 2)

calino$DPTS <- (((calino$PRIV3A + 
                    calino$PRIV3B + 
                    calino$PRIV3C + 
                    calino$PRIV3D) / 4) + 
                ((calino$PRIV4A + 
                    calino$PRIV4B + 
                    calino$PRIV4C + 
                    calino$PRIV4D + 
                    calino$PRIV4E + 
                    calino$PRIV4F) / 6)) / 2
calino$DPTS <- round(calino$DPTS, 2)


# Select and rename columns -----------------------------------------------

data <- data %>% select("CALI", "POLITICS":"GENDER", "PRIV10A":"PRIV10D", "DPTS")
colnames(data)[colnames(data) == "PRIV10A"] = "users"
colnames(data)[colnames(data) == "PRIV10B"] = "government"
colnames(data)[colnames(data) == "PRIV10C"] = "socialmedia"
colnames(data)[colnames(data) == "PRIV10D"] = "thirdparties"
colnames(data)[colnames(data) == "POLITICS"] = "party"
colnames(data)[colnames(data) == "URBAN"] = "urban"
colnames(data)[colnames(data) == "MARITAL"] = "married"
colnames(data)[colnames(data) == "AGEGRP"] = "over40years"
colnames(data)[colnames(data) == "EMPSTATUS"] = "employed"
colnames(data)[colnames(data) == "EDUCATION"] = "graduate"
colnames(data)[colnames(data) == "RACETH"] = "white"
colnames(data)[colnames(data) == "HHINCOME"] = "over50k"
colnames(data)[colnames(data) == "GENDER"] = "male"
colnames(data)[colnames(data) == "CALI"] = "californian"
colnames(data)[colnames(data) == "DPTS"] = "score"

data$party <- ifelse(data$party == "(1) Democrat", 1, 0)
data$urban <- ifelse(data$urban == "(1) Urban area", 1, 0)
data$married <- ifelse(data$married == "(1) Married", 1, 0)
data$over40years <- ifelse(data$over40years == "(3) 40-59" |
                             data$over40years == "(4) 60-64" |
                             data$over40years == "(5) 65 and older", 1, 0)
data$employed <- ifelse(data$employed == "(1) Employed", 1, 0)
data$graduate <- ifelse(data$graduate == "(4) College graduate", 1, 0)
data$white <- ifelse(data$white == "(1) White, non-Hispanic", 1, 0)
data$over50k <- ifelse(data$over50k == "(6) $50,000 to under $75,000" |
                         data$over50k == "(7) $75,000 to under $100,000" |
                         data$over50k == "(8) $100,000 to under $150,000" |
                         data$over50k == "(9) $150,000 or more", 1, 0)
data$male <- ifelse(data$male == "(1) Male", 1, 0)


# Calculate percentages for summary tables --------------------------------

# California
mean(cali_one) * 25
mean(cali_two)* 25
mean(cali$DPTS)* 25
mean(cali$PRIV10A)* 25
mean(cali$PRIV10B)* 25
mean(cali$PRIV10C)* 25
mean(cali$PRIV10D)* 25

# Other states
mean(calino_one) * 25
mean(calino_two)* 25
mean(calino$DPTS)* 25
mean(calino$PRIV10A)* 25
mean(calino$PRIV10B)* 25
mean(calino$PRIV10C)* 25
mean(calino$PRIV10D)* 25

# Adjust data to discrete 0-100 scale
data$users <- data$users * 25
data$government <- data$government * 25
data$socialmedia <- data$socialmedia * 25
data$thirdparties <- data$thirdparties * 25
data$score <- data$score * 25


# Regression Models -------------------------------------------------------

model_one <- lm(score ~ californian, data = data)

model_two <- lm(score ~ californian + party + urban + married + over40years +
                  employed + graduate + white + over50k + male,
                data = data)

model_three <- lm(users ~ californian + party + urban + married + over40years +
                   employed + graduate + white + over50k + male, 
                 data = data)

model_four <- lm(government ~ californian + party + urban + married + over40years +
                   employed + graduate + white + over50k + male, 
                 data = data)

model_five <- lm(socialmedia ~ californian + party + urban + married + over40years +
                  employed + graduate + white + over50k + male, 
                data = data)

model_six <- lm(thirdparties ~ californian + party + urban + married + over40years +
                  employed + graduate + white + over50k + male, 
                data = data)

# Robust Standard Errors (RSEs) -------------------------------------------

model_one$rse <- sqrt(diag(vcovHC(model_one, type = "HC1")))
model_two$rse <- sqrt(diag(vcovHC(model_two, type = "HC1")))
model_three$rse <- sqrt(diag(vcovHC(model_three, type = "HC1")))
model_four$rse <- sqrt(diag(vcovHC(model_four, type = "HC1")))
model_five$rse <- sqrt(diag(vcovHC(model_five, type = "HC1")))
model_six$rse <- sqrt(diag(vcovHC(model_six, type = "HC1")))


# Create summary tables ---------------------------------------------------

data <- as.data.frame(data)

variables <- colnames(data)
definitions <- c("State of residence (1 = California, 0 = Not California)",
                "Political party (1 = Democrat, 0 = Other)",
                 "Lives in urban setting (1 = Urban, 0 = Other)",
                 "Marital status (1 = Married, 0 = Unmarried)",
                 "Whether the respondent is over 40 years old (1 = Over 40, 0 = Under 40)",
                 "Employment status (1 = Employed, 0 = Unemployed)",
                 "Educational attainment (1 = College graduate, 0 = Not college graduate)",
                 "White or non-white (1 = White, 0 = Non-white)",
                 "Whether respondent's household income is over $50k (1 = Yes, 0 = No)",
                 "Gender (1 = Male, 0 = Female)",
                 "Individual users should be responsible for data privacy",
                 "The federal government should be responsible for data privacy",
                 "Social media companies should be responsible for data privacy",
                 "Third-parties should be responsible for data privacy",
                 "Data tolerance score (higher indicating greater interest in data privacy)")

descriptions <- as.data.frame(definitions, variables)


stargazer(descriptions, 
          summary = FALSE,
          type = "latex",
          title = "Table 1. Variables and Definitions",
          out = "cap1.html")

stargazer(data, 
          type = "latex",
          digits = 2,
          title = "Table 2. Summary Statistics",
          summary.stat = c("n", "mean", "sd", "min", "median", "max"),
          out = "cap2.html")


# Create regression tables ------------------------------------------------

stargazer(model_one, model_two,
          digits = 2,
          header = FALSE,
          type = "latex",
          se = list(model_one$rse, model_two$rse),
          title = "Table 3. Data Tolerance Privacy Score",
          out = "cap3.html",
          column.labels = c("Bivariate", "Multivariate"),
          single.row = TRUE)

stargazer(model_three, model_four, model_five, model_six,
          digits = 2,
          header = TRUE,
          type = "latex",
          se = list(model_three$rse, model_four$rse, model_five$rse, model_six$rse),
          title = "Table 4. Responsibility for Data Privacy Protections",
          out = "cap4.html",
          single.row = TRUE)





