library(dplyr)
library(naniar)
library(haven)
library(tidyverse)
library(ggplot2)
library(visdat)

# load data
brfss_2024 <- read_xpt(unz("data/LLCP2024XPT.zip", "LLCP2024.XPT ")) |>
  filter(`_STATE` == 29)

brfss_2023 <- read_xpt(unz("data/LLCP2023XPT.zip", "LLCP2023.XPT ")) |>
  filter(`_STATE` == 29)

# define variable name
tobacco_vars <- c("SMOKE100", "SMOKDAY2", "USENOW3", "ECIGNOW2", "ECIGNOW3")

tobacco_vars_2023 <- intersect(tobacco_vars, names(brfss_2023))
tobacco_vars_2024 <- intersect(tobacco_vars, names(brfss_2024))

cognitive_vars <- c("CIMEMLOS", "CIMEMLO1", "CDHOUSE", "CDHOUS1",
                    "CDASSIST", "CDSOCIAL", "CDSOCIA1")

cognitive_vars_2023 <- intersect(cognitive_vars, names(brfss_2023))
cognitive_vars_2024 <- intersect(cognitive_vars, names(brfss_2024))

# recode missing on those vars

brfss_2023 <- brfss_2023 |>
  mutate(across(all_of(tobacco_vars_2023),   recode_brfs_missing),
         across(all_of(cognitive_vars_2023), recode_brfs_missing))

brfss_2024 <- brfss_2024 |>
  mutate(across(all_of(tobacco_vars_2024),   recode_brfs_missing),
         across(all_of(cognitive_vars_2024), recode_brfs_missing))

# missing summary function

missing_summary <- function(df, var) {
  x <- df[[var]]
  data.frame(
    variable    = var,
    n           = length(x),
    n_missing   = sum(is.na(x)),
    pct_missing = mean(is.na(x)) * 100
  )
}

# apply it to name vectors

miss_tob_2024 <- do.call(
  rbind,
  lapply(tobacco_vars_2024, \(v) missing_summary(brfss_2024, v))
)

miss_tob_2023 <- do.call(
  rbind,
  lapply(tobacco_vars_2023, \(v) missing_summary(brfss_2023, v))
)

miss_cognitive_2024 <- do.call(
  rbind,
  lapply(cognitive_vars_2024, \(v) missing_summary(brfss_2024, v))
)

miss_cognitive_2023 <- do.call(
  rbind,
  lapply(cognitive_vars_2023, \(v) missing_summary(brfss_2023, v))
)

# tobacco MCAR test
tobacco_2024 <- brfss_2024 |>
  select(all_of(tobacco_vars_2024))

tobacco_2023 <- brfss_2023 |>
  select(all_of(tobacco_vars_2023))

mcar_test(tobacco_2024)
mcar_test(tobacco_2023)

# cognitive MCAR test
cognitive_2024 <- brfss_2024 |>
  select(all_of(cognitive_vars_2024))

cognitive_2023 <- brfss_2023 |>
  select(all_of(cognitive_vars_2023))

mcar_test(cognitive_2024)
mcar_test(cognitive_2023)

# creating plot

# find variables that exists in both years
key_vars_2023 <- c(tobacco_vars_2023, cognitive_vars_2023)
key_vars_2024 <- c(tobacco_vars_2024, cognitive_vars_2024)

# only keep variables present in both
key_vars <- intersect(key_vars_2023, key_vars_2024)

# combine 2023 and 2024
combined_brfss <- bind_rows(
  brfss_2023 |>
    mutate(year = "2023") |>
    select(year, all_of(key_vars)),
  brfss_2024 |>
    mutate(year = "2024") |>
    select(year, all_of(key_vars))
)

# get % missing per variable and year
miss_summary <- combined_brfss |>
  group_by(year) |>
  miss_var_summary()

# barplot
p_miss_bar_both <-
  ggplot(miss_summary,
         aes(x = variable, y = pct_miss, fill = year)) +
  geom_col(position = "dodge") +
  labs(
    title = "Percentage of missing values by variable (2023 vs 2024)",
    x = "Variable",
    y = "% missing",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p_miss_bar_both
