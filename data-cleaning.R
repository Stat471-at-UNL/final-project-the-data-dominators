library(haven)
library(tidyverse)
library(mice)
library(patchwork)

###################################
##### EDA / Preparation ###########
###################################

# Load the data
brfss_2024 <- read_xpt(unz("data/LLCP2024XPT.zip", "LLCP2024.XPT ")) # 457,670 records
brfss_2023 <- read_xpt(unz("data/LLCP2023XPT.zip", "LLCP2023.XPT ")) # 433,323 records

# Looking at the different state codes (Missouri is 29)
unique(brfss_2024$`_STATE`)

# Get just Missouri's data
brfss_2024 <- brfss_2024 %>% filter(`_STATE` == 29.00)
brfss_2023 <-brfss_2023 %>% filter(`_STATE` == 29)

# Shape of the data
dim(brfss_2024)
dim(brfss_2023)

# Basic info on each of the columns
summary(brfss_2024)
summary(brfss_2023)

# Basic info
head(brfss_2024)
head(brfss_2023)

# Locating columns that weren't included in Missouri's modules
brfss_2024 %>%
  summarise(
    across(everything(), ~ sum(!is.na(.)))
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  filter(n_na != 0) %>% view()

# Selecting only the columns that we are going to focus on (this is just a start, might add/drop)
brfss_2024_tob_cog <- brfss_2024 %>%
  select(`SEQNO`, # Key
         `IDAY`, `IMONTH`, `IYEAR`, # Survey Date
         `SMOKE100`, `ECIGNOW3`, # Tobacco Use
         `CIMEMLO1`, # Cognitive Decline
         `MARITAL`, `EDUCA`, `VETERAN3`, `EMPLOY1`, `CHILDREN`, `INCOME3`, `PREGNANT`, `WEIGHT2`, `_CRACE1`, # Demographics
         `_ASTHMS1`, `_SMOKER3`, `CAGEG`, `_AGE80`, `CHCOCNC1`, `ALCDAY4`, `MARJSMOK`, `GENHLTH`, `MENTHLTH` # Extras
         ) %>%
  rename(ECIGNOW = ECIGNOW3, # for uniform names
         CRACE1 = `_CRACE1`, # the rest are changed to not cause problems in mice
         ASTHMS1 = `_ASTHMS1`,
         SMOKER3 = `_SMOKER3`,
         AGE80 = `_AGE80`
         )
summary(brfss_2024_tob_cog)

brfss_2023_tob_cog <- brfss_2023 %>%
  select(`SEQNO`, # Key
         `IDAY`, `IMONTH`, `IYEAR`, # Survey Date
         `SMOKE100`, `ECIGNOW2`, # Tobacco Use
         `CIMEMLO1`, # Cognitive Decline
         `MARITAL`, `EDUCA`, `VETERAN3`, `EMPLOY1`, `CHILDREN`, `INCOME3`, `PREGNANT`, `WEIGHT2`, `_CRACE1`, # Demographics
         `_ASTHMS1`, `_SMOKER3`, `CAGEG`, `_AGE80`, `CHCOCNC1`, `ALCDAY4`, `MARJSMOK`, `GENHLTH`, `MENTHLTH` # Extras
         ) %>%
  rename(ECIGNOW = ECIGNOW2, # for uniform names
         CRACE1 = `_CRACE1`, # the rest are changed to not cause problems in mice
         ASTHMS1 = `_ASTHMS1`,
         SMOKER3 = `_SMOKER3`,
         AGE80 = `_AGE80`
         )

summary(brfss_2023_tob_cog)


###################################
##### IMPUTING MISSING VALUES #####
###################################

# Joining the two datasets for imputation / tidying
brfss_tob_cog <- bind_rows(brfss_2023_tob_cog, brfss_2024_tob_cog)
summary(brfss_tob_cog) # 14,528 combined observations

# Turning the values of 7 (Don't know) and 9 (Refused) to NAs so that they are imputed too
brfss_for_imp <- brfss_tob_cog %>%
  mutate(
    SMOKE100 = ifelse(SMOKE100 %in% c(7, 9), NA, SMOKE100),
    ECIGNOW = ifelse(ECIGNOW %in% c(7, 9), NA, ECIGNOW),
    CIMEMLO1 = ifelse(CIMEMLO1 %in% c(7, 9), NA, CIMEMLO1)
  )

# Number of missing values in the columns to be imputed
sum(is.na(brfss_for_imp$`SMOKE100`)) # 439 --> 532 w/ 79s
sum(is.na(brfss_for_imp$`ECIGNOW`)) # 457 --> 489 w/ 79s
sum(is.na(brfss_for_imp$`CIMEMLO1`)) # 5908 --> 6077 w/ 79s

#
# Imputing
#

# Dry run to get the matrix skeleton, so that we can alter it for each column
ini <- mice(brfss_for_imp, maxit = 0)
pred <- ini$predictorMatrix
# Reset the entire matrix to 0
pred[,] <- 0

# Predictors (everything except the targets)
predictors <- c("MARITAL", "EDUCA", "VETERAN3", "EMPLOY1", "CHILDREN", "INCOME3", "PREGNANT", "WEIGHT2", "CRACE1",
                "ASTHMS1", "CAGEG", "AGE80", "CHCOCNC1", "ALCDAY4", "MARJSMOK", "GENHLTH", "MENTHLTH")

# Set Predictors for each target column
pred["SMOKE100", predictors] <- 1
pred["ECIGNOW", predictors] <- 1
pred["CIMEMLO1", predictors] <- 1

# Viewing the predictor matrix to confirm it looks right
print(pred[c("SMOKE100", "ECIGNOW", "CIMEMLO1"), ])

# Actual imputation
brfss_imputed <- mice(
  brfss_for_imp,
  m = 3,
  predictorMatrix = pred,
  method = "pmm",
  seed = 20251124,
  print = FALSE
)

#
# Loop to add imputed values
#
brfss_complete <- brfss_tob_cog
for(i in 1:3) {
  # Imputed data for the i-th imputation
  completed <- complete(brfss_imputed, action = i)

  # Select only the target columns
  imp_cols <- completed %>%
    select(all_of(c("SMOKE100", "ECIGNOW", "CIMEMLO1"))) %>%
    rename_with(~ paste0(., "_IMP", i))  # renaming them for clarity

  # Append to complete dataset
  brfss_complete <- bind_cols(brfss_complete, imp_cols)
}

# Check the result
summary(brfss_complete)

# Confirm no missing values (all imps zero)
sum(is.na(brfss_complete$`SMOKE100`))
sum(is.na(brfss_complete$`SMOKE100_IMP1`))
sum(is.na(brfss_complete$`SMOKE100_IMP2`))
sum(is.na(brfss_complete$`SMOKE100_IMP3`))
sum(is.na(brfss_complete$`ECIGNOW`))
sum(is.na(brfss_complete$`ECIGNOW_IMP1`))
sum(is.na(brfss_complete$`ECIGNOW_IMP2`))
sum(is.na(brfss_complete$`ECIGNOW_IMP3`))
sum(is.na(brfss_complete$`CIMEMLO1`))
sum(is.na(brfss_complete$`CIMEMLO1_IMP1`))
sum(is.na(brfss_complete$`CIMEMLO1_IMP2`))
sum(is.na(brfss_complete$`CIMEMLO1_IMP3`))

#
# Visually analyzing imputed values
#

# SMOKE100
p1 <- brfss_complete %>% ggplot(aes(x = SMOKE100)) + geom_bar() + ggtitle("Original")
p2 <- brfss_complete %>% ggplot(aes(x = SMOKE100_IMP1)) + geom_bar() + ggtitle("Imputation 1")
p3 <- brfss_complete %>% ggplot(aes(x = SMOKE100_IMP2)) + geom_bar() + ggtitle("Imputation 2")
p4 <- brfss_complete %>% ggplot(aes(x = SMOKE100_IMP3)) + geom_bar() + ggtitle("Imputation 3")
(p1 + p2) / (p3 + p4)

# ECIGNOW
p5 <- brfss_complete %>% ggplot(aes(x = ECIGNOW)) + geom_bar() + ggtitle("Original")
p6 <- brfss_complete %>% ggplot(aes(x = ECIGNOW_IMP1)) + geom_bar() + ggtitle("Imputation 1")
p7 <- brfss_complete %>% ggplot(aes(x = ECIGNOW_IMP2)) + geom_bar() + ggtitle("Imputation 2")
p8 <- brfss_complete %>% ggplot(aes(x = ECIGNOW_IMP3)) + geom_bar() + ggtitle("Imputation 3")
(p5 + p6) / (p7 + p8)

# CIMEMLO1
p9 <- brfss_complete %>% ggplot(aes(x = CIMEMLO1)) + geom_bar() + ggtitle("Original")
p10 <- brfss_complete %>% ggplot(aes(x = CIMEMLO1_IMP1)) + geom_bar() + ggtitle("Imputation 1")
p11 <- brfss_complete %>% ggplot(aes(x = CIMEMLO1_IMP2)) + geom_bar() + ggtitle("Imputation 2")
p12 <- brfss_complete %>% ggplot(aes(x = CIMEMLO1_IMP3)) + geom_bar() + ggtitle("Imputation 3")
(p9 + p10) / (p11 + p12)

# Plotting all together
final_plot <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12
final_plot + plot_layout(ncol = 4, nrow = 3)


###################################
##### TIDYING DATASET #############
###################################

head(brfss_complete)

# Creation of Demographics Dataset
Demographics <- brfss_complete %>% select(SEQNO, MARITAL, EDUCA, VETERAN3, EMPLOY1, CHILDREN, INCOME3, PREGNANT, WEIGHT2, CRACE1,
                                          ASTHMS1, CAGEG, AGE80, CHCOCNC1, ALCDAY4, MARJSMOK, GENHLTH, MENTHLTH)
head(Demographics)

# Creation of Cognitive Decline Dataset
CognitiveDecline <- brfss_complete %>% select(SEQNO, CIMEMLO1, CIMEMLO1_IMP1,CIMEMLO1_IMP2,CIMEMLO1_IMP3)
head(CognitiveDecline)

# Creation of Tobacco Use Dataset
TobaccoUse <- brfss_complete %>% select(SEQNO, SMOKE100, SMOKE100_IMP1, SMOKE100_IMP2, SMOKE100_IMP3, ECIGNOW, ECIGNOW_IMP1, ECIGNOW_IMP2, ECIGNOW_IMP3, SMOKER3)
head(TobaccoUse)

# Identifying Key (`SEQNO` for both)
brfss_2024 %>% group_by(`SEQNO`) %>% summarise(n = n()) %>% filter(n > 1)
brfss_2023 %>% group_by(`SEQNO`) %>% summarise(n = n()) %>% filter(n > 1)

