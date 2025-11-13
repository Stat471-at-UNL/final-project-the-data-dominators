library(dplyr)
library(naniar)
library(haven)
library(tidyverse)

brfss_2024 <- read_xpt(unz("data/LLCP2024XPT.zip", "LLCP2024.XPT ")) %>%
  filter(`_STATE` == 29)

brfss_2023 <- read_xpt(unz("data/LLCP2023XPT.zip", "LLCP2023.XPT ")) %>%
  filter(`_STATE` == 29)

# recode common BRFSS "DK/Refused/Not asked" codes to NA
recode_brfs_missing <- function(x) {
  x |>
    na_if(7)  |> na_if(8)  |> na_if(9)  |>
    na_if(77) |> na_if(88) |> na_if(99) |>
    na_if(777) |> na_if(888) |> na_if(999)
}

# tobacco variables
tobacco_vars <- c(
  "SMOKE100",
  "SMOKDAY2",
  "USENOW3",
  "ECIGNOW2",
  "ECIGNOW3"
)

tobacco_2023 <- intersect(tobacco_vars, names(brfss_2023))
tobacco_2024 <- intersect(tobacco_vars, names(brfss_2024))

brfss_2023 <- brfss_2023 %>%
  mutate(across(all_of(tobacco_2023), recode_brfs_missing))

brfss_2024 <- brfss_2024 %>%
  mutate(across(all_of(tobacco_2024), recode_brfs_missing))

# childhood variables
child_asthma_vars <- c("CASTHDX2", "CASTHNO2")

child_asthma_2023 <- intersect(child_asthma_vars, names(brfss_2023))
child_asthma_2024 <- intersect(child_asthma_vars, names(brfss_2024))

brfss_2023 <- brfss_2023 %>%
  mutate(across(all_of(child_asthma_2023), recode_brfs_missing))

brfss_2024 <- brfss_2024 %>%
  mutate(across(all_of(child_asthma_2024), recode_brfs_missing))

missing_summary <- function(df, var) {
  x <- df[[var]]
  data.frame(
    variable    = var,
    n           = length(x),
    n_missing   = sum(is.na(x)),
    pct_missing = mean(is.na(x)) * 100
  )
}

# tobacco 2024
miss_tob_2024 <- do.call(
  rbind,
  lapply(tobacco_2024, \(v) missing_summary(brfss_2024, v))
)

# tobacco 2023
miss_tob_2023 <- do.call(
  rbind,
  lapply(tobacco_2023, \(v) missing_summary(brfss_2023, v))
)

# childhood asthma
miss_child_2024 <- do.call(
  rbind,
  lapply(child_asthma_2024, \(v) missing_summary(brfss_2024, v))
)

miss_child_2023 <- do.call(
  rbind,
  lapply(child_asthma_2023, \(v) missing_summary(brfss_2023, v))
)

miss_tob_2024
miss_tob_2023
miss_child_2024
miss_child_2023

# mcar test on tobacco
tobacco_2024 <- brfss_2024 %>%
  select(SMOKE100, SMOKDAY2, USENOW3, ECIGNOW3)

tobacco_2023 <- brfss_2023 %>%
  select(SMOKE100, SMOKDAY2, USENOW3, ECIGNOW2)

mcar_test(tobacco_2024)
mcar_test(tobacco_2023)

# mcar test on childhood asthma & filter households with children
child_2024 <- brfss_2024 %>%
  filter(`_CHLDCNT` > 0) %>%
  select(CASTHDX2, CASTHNO2)

child_2023 <- brfss_2023 %>%
  filter(`_CHLDCNT` > 0) %>%
  select(CASTHDX2, CASTHNO2)

mcar_test(child_2024)
mcar_test(child_2023)

tob_miss_2024 <- data.frame(
  variable    = c("SMOKE100", "SMOKDAY2", "USENOW3", "ECIGNOW3"),
  pct_missing = c(3.789848, 59.091531, 3.502531, 3.625667),
  year        = "2024"
)

tob_miss_2023 <- data.frame(
  variable    = c("SMOKE100", "SMOKDAY2", "USENOW3", "ECIGNOW2"),
  pct_missing = c(3.532345, 59.495775, 2.895138, 3.102923),
  year        = "2023"
)

tob_miss <- bind_rows(tob_miss_2023, tob_miss_2024) |>
  mutate(
    label = paste0(round(pct_missing, 1), "%")
  )

ggplot(tob_miss, aes(x = variable, y = pct_missing, fill = year)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Percent Missing for Tobacco Use Variables (Missouri BRFSS 2023–2024)",
    x = "Variable",
    y = "Percent Missing"
  )


child_miss_2024 <- data.frame(
  variable    = c("CASTHDX2", "CASTHNO2"),
  pct_missing = c(79.79204, 98.02983),
  year        = "2024"
)

child_miss_2023 <- data.frame(
  variable    = c("CASTHDX2", "CASTHNO2"),
  pct_missing = c(80.57903, 98.01912),
  year        = "2023"
)

child_miss <- bind_rows(child_miss_2023, child_miss_2024) |>
  mutate(
    label = paste0(round(pct_missing, 1), "%")
  )

ggplot(child_miss, aes(x = variable, y = pct_missing, fill = year)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Percent Missing for Childhood Asthma Variables (Households with Children)",
    x = "Variable",
    y = "Percent Missing"
  )
