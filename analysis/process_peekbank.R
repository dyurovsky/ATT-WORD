library(tidyverse)
library(here)
library(janitor)

sal_data <- read_csv(here("data/salient.csv"))%>%
  mutate(exp = "Salient")
nonsal_data <- read_csv(here("data/nonsalient.csv")) %>%
  mutate(exp = "NonSalient")
balanced_data <- read_csv(here("data/balanced.csv")) %>%
  mutate(exp = "Balanced")

familiar <- c("book", "dog", "lamp", "clock", "car", 
              "banana", "frog", "carrot")
novel <- c("skwish", "balls")

#pre-process data values to be more English-readable
data <- bind_rows(sal_data,nonsal_data,balanced_data) %>%
  mutate(
    trial.type = factor(trial.type,
                        labels = c("Learning", "Familiar", "Novel", "ME")),
    aoi = factor(aoi, labels = c("Target", "Competitor","Face", "Other","NA")),
    time.step = time.step / 60 - 1, # 0 is the Point of Disambiguation
    gender = factor(gender,labels=c("Male","Female"))) %>%
  filter(age >= 1, trial.type != "Learning") %>%
  clean_names()

design <- read_delim(here("data/design.txt"), delim = "\t") %>%
  clean_names() %>%
  filter(type == "Image", !is.na(target)) %>%
  select(-x3, -type) %>%
  mutate(name = gsub(".jpg", "", name)) %>%
  separate(name, into = c("obj1", "obj2"), sep = "_") %>%
  mutate(trial_type = if_else(trial_type == "new", "Novel", 
                              if_else(trial_type == "me", "ME",
                                      "Familiar"))) %>%
  group_by(trial_type) %>%
  mutate(trial_num = 1:n())

tidy_data <- left_join(data, design, by = c("trial_type", "trial_num")) %>%
  mutate(aoi = as.character(aoi), 
         aoi = if_else(aoi == "NA", as.character(NA), aoi))

write_csv(data, here("data/peekbank_data.csv"))

