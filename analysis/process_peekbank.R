library(tidyverse)
library(here)


sal_data <- read_csv(here("data/salient.csv"))%>%
  mutate(exp = "Salient")

nonsal_data <- read_csv(here("data/nonsalient.csv")) %>%
  mutate(exp = "NonSalient")

balanced_data <- read_csv(here("data/balanced.csv")) %>%
  mutate(exp = "Balanced")

data <- bind_rows(sal_data,nonsal_data,balanced_data) %>%
  #pre-process data values to be more English-readable
  mutate(
    trial.type = factor(trial.type,
                        labels = c("Learning", "Familiar", "Novel", "ME")),
    aoi = factor(aoi, labels = c("Target", "Competitor","Face", "Other","NA")),
    time.step = time.step / 60 - 1, # 0 is the Point of Disambiguation
    gender = factor(gender,labels=c("Male","Female"))) %>%
  filter(age >= 1)

write_csv(data, here("data/peekbank_data.csv"))
