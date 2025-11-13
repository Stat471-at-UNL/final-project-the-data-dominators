#########################
## EDA for Data Cleaning
#########################


library(haven)
library(tidyverse)

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

# Identifying Key (`SEQNO` for both)
brfss_2024 %>% group_by(`SEQNO`) %>% summarise(n = n()) %>% filter(n > 1)
brfss_2023 %>% group_by(`SEQNO`) %>% summarise(n = n()) %>% filter(n > 1)

# Selecting only the columns that we are going to focus on (this is just a start, might add/drop)
brfss_2024_tob_asthma <- brfss_2024 %>%
  select(`SEQNO`, # Key
         `SMOKE100`, `SMOKDAY2`, `USENOW3`, `ECIGNOW3`, # Tobacco Use
         `CASTHDX2`, `CASTHNO2`, # Childhood Asthma Prevalence
         `MARITAL`, `EDUCA`, `VETERAN3`, `EMPLOY1`, `CHILDREN`, `INCOME3`, `PREGNANT`, `WEIGHT2`, `_CRACE1`, # Demographics
         `_ASTHMS1`, `_SMOKER3`, `CAGEG`, `_AGE80`, `CHCOCNC1`, `ALCDAY4`, `MARJSMOK` # Extras
         )
summary(brfss_2024_tob_asthma)

brfss_2023_tob_asthma <- brfss_2023 %>%
          select(`SEQNO`, # Key
                 `SMOKE100`, `SMOKDAY2`, `USENOW3`, `ECIGNOW2`, # Tobacco Use
                 `CASTHDX2`, `CASTHNO2`, # Childhood Asthma Prevalence
                 `MARITAL`, `EDUCA`, `VETERAN3`, `EMPLOY1`, `CHILDREN`, `INCOME3`, `PREGNANT`, `WEIGHT2`, `_CRACE1`, # Demographics
                 `_ASTHMS1`, `_SMOKER3`, `CAGEG`, `_AGE80`, `CHCOCNC1`, `ALCDAY4`, `MARJSMOK` # Extras
          )
summary(brfss_2023_tob_asthma)
