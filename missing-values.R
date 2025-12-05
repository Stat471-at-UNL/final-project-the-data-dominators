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

# recode 'dont know' and 'refused' to NA
recode_brfs_missing <- function(x) {
  miss_codes <- c(7, 9, 77, 99)
  replace(x, x %in% miss_codes, NA)
}

# define variable name
tobacco_vars  <- c("SMOKE100", "SMOKDAY2", "USENOW3", "ECIGNOW2", "ECIGNOW3")

cognitive_vars <- c("CIMEMLO1", "CDHOUS1", "CDSOCIA1")

demo_vars <- c(
  "MARITAL", "EDUCA", "VETERAN3", "EMPLOY1", "CHILDREN",
  "INCOME3", "PREGNANT", "WEIGHT2", "_CRACE1", "_ASTHMS1",
  "CAGEG", "_AGE80", "CHCOCNC1", "ALCDAY4", "MARJSMOK",
  "GENHLTH", "MENTHLTH"
)

# figure out which of these exist in each year
tobacco_vars_2023   <- intersect(tobacco_vars,   names(brfss_2023))
tobacco_vars_2024   <- intersect(tobacco_vars,   names(brfss_2024))
cognitive_vars_2023 <- intersect(cognitive_vars, names(brfss_2023))
cognitive_vars_2024 <- intersect(cognitive_vars, names(brfss_2024))

# recode missing for selected vars
brfss_2023_missing <- brfss_2023 |>
  mutate(
    across(all_of(tobacco_vars_2023),   recode_brfs_missing),
    across(all_of(cognitive_vars_2023), recode_brfs_missing),
    `_SMOKER3` = replace(`_SMOKER3`, `_SMOKER3` == 9, NA)
  )

brfss_2024_missing <- brfss_2024 |>
  mutate(
    across(all_of(tobacco_vars_2024),   recode_brfs_missing),
    across(all_of(cognitive_vars_2024), recode_brfs_missing),
    `_SMOKER3` = replace(`_SMOKER3`, `_SMOKER3` == 9, NA)
  )

# only keep vars present in both years
vars_all <- c(demo_vars, cognitive_vars, tobacco_vars)

vars_2023 <- intersect(vars_all, names(brfss_2023_missing))
vars_2024 <- intersect(vars_all, names(brfss_2024_missing))

vars_both <- intersect(vars_2023, vars_2024)

# combined 2023 and 2024
combined_brfss <- bind_rows(
  brfss_2023_missing|>
    mutate(year = "2023") |>
    select(year, all_of(vars_both)),
  brfss_2024_missing|>
    mutate(year = "2024") |>
    select(year, all_of(vars_both))
)

# summary of missingness
miss_summary <- combined_brfss |>
  group_by(year) |>
  miss_var_summary() |>
  ungroup()


# map variable to group
var_domains <- tibble(
  variable = vars_both,
  domain   = case_when(
    variable %in% demo_vars      ~ "Demographic",
    variable %in% cognitive_vars ~ "Cognitive",
    variable %in% tobacco_vars   ~ "Tobacco",
    TRUE ~ "Other"
  )
)

miss_summary <- miss_summary |>
  left_join(var_domains, by = "variable") |>
  mutate(
    domain   = if_else(is.na(domain), "Other", domain),
    pct_miss = as.numeric(pct_miss)
  )


# make the grey lighter and reduce the alpha blending

p_miss_bar <- ggplot(
  miss_summary |> filter(domain != "Other"),
  aes(
    x   = as.numeric(pct_miss),
    y   = fct_reorder(variable, as.numeric(pct_miss)),
    fill = year
  )
) +
  geom_col(
    aes(x = 50),
    fill  = "grey80",
    colour = NA,
    alpha = 0.3
  ) +
  geom_col(position = "dodge", alpha = 1) +
  facet_grid(domain ~ ., scales = "free_y", space = "free") +
  labs(
    title = "Percent missing by variable and year (2023 vs 2024)",
    x     = "% missing",
    y     = "Variable",
    fill  = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8)
  )

p_miss_bar
