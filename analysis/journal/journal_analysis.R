# clear all previous variables
rm(list=ls())

#get lab version of useful R functions
source('~/Projects/Other/Ranalysis/useful_dplyr.R')

#load libraries for data manipulation and graphing
library(directlabels)
library(xtable)
library(magrittr)
library(tidyr)

#splits ages into half-years
split.ages <-function(x) {floor(x*2)/2}

#Constants for window-of-interest analysis
TEST_START <- 1
TEST_END <- 4
TRAIN_START <- 1
TRAIN_END <- 4

#Use color-brewer colors for graphing
man_cols <- c("#e41a1c","#377eb8","#4daf4a",
              "#984ea3","#ff7f00","#a65628")

###############################################################################
################################ LOADING DATA #################################
###############################################################################
#read looking data
sal.data <- read.csv("../../data/salient.csv",header=TRUE)
nonsal.data <- read.csv("../../data/nonsalient.csv",header=TRUE)
balanced.data <- read.csv("../../data/balanced.csv",header=TRUE)

#mark experiment
sal.data$exp <- "Salient"
nonsal.data$exp <- "NonSalient"
balanced.data$exp <- "Balanced"

data <- rbind(sal.data,nonsal.data,balanced.data) %>%
  #pre-process data values to be more English-readable
  mutate(
    trial.type = factor(trial.type,
                        labels=c("Learning", "Familiar", "Novel", "ME")),
    aoi = factor(aoi, labels=c("Target", "Competitor","Face", "Other","NA")),
    time.step = time.step/60 - 1,
    gender = factor(gender,labels=c("Male","Female")),
    age.grp = split.ages(age)) %>%
  filter(age >= 1)

# Reshape data for subsequent analyses 
source('munge_data.R')

# Create window-of-analysis graphs
source('dot_graphs.R')

# Compute statistical models for paper
source('statistical_models.R')

# Reshape timecourse data for subequent analyses
source('timecourse_data.R')

# Create timecourse graphs for paper
source('timecourse_graphs.R')
