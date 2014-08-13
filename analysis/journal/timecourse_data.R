###############################################################################
############################### TIMECOURSE DATA ###############################
###############################################################################
library(zoo)
TIMECOURSE_END <- 4

##### EXPERIMENT 1 DATA
# Subset to test
e1.timecourse.data <- data %>%
  filter(exp == "Balanced",trial.type != "Learning",
         time.step <= TIMECOURSE_END, age.grp <4) %>%
  mutate(age.grp = factor(age.grp),)

# Subset to train
e1.timecourse.train <- data %>%
  filter(exp == "Balanced",trial.type == "Learning",
         time.step <= TRAIN_END, age.grp <4) %>%
  mutate(age.grp = factor(age.grp)) %>%
  group_by(age.grp,trial.type,time.step,trial.num,aoi) %>%
  summarise(n = n()) %>%
  filter(aoi != "NA") %.%
  mutate(n = n / sum(n)) %>%
  group_by(age.grp,trial.type,time.step,aoi,add=FALSE) %>%
  filter(aoi != "Other") %>%
  summarise_each(funs(na.mean,sem),n)%>%
  group_by(age.grp,aoi)
names(e1.timecourse.train)[5] <- "prop"

# Do window averaging
e1.timecourse.train <- mutate(e1.timecourse.train,
                             roll.mean = rollapply(prop,6,FUN=na.mean, 
                                                   partial=TRUE))
e1.timecourse.train <- mutate(e1.timecourse.train,
                             roll.sem = rollapply(sem,6,FUN=na.mean, 
                                                  partial=TRUE))


# Compute timecourse for standard analysis
e1.timecourse.test <- e1.timecourse.data %>%
  group_by(age.grp,trial.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1.timecourse.test)[4] <- "prop"

#Do window averaging
e1.timecourse.test <- mutate(e1.timecourse.test,
                             roll.mean = rollapply(prop,6,FUN=na.mean, 
                                                   partial=TRUE))
e1.timecourse.test <- mutate(e1.timecourse.test,
                             roll.sem = rollapply(sem,6,FUN=na.mean, 
                                                   partial=TRUE))

# Compute side of the screen Ps were on for each test trial
e1.split.type <- e1.timecourse.data %>%
  filter(time.step == 0) %>%
  mutate(split.type = aoi) %>%
  select(subj,trial.type,trial.num,split.type)

# Subset test data to just for split analysis
e1.split.data <- merge(e1.timecourse.data,e1.split.type) %>%
  filter(split.type == "Target" | split.type == "Competitor",
         time.step >= 0)

# Compute timecourses for split analysis
e1.split.timecourse <- e1.split.data %>%
  group_by(age.grp,trial.type,split.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1.split.timecourse)[5] <- "prop"
e1.split.timecourse[e1.split.timecourse$split.type=="Target","prop"] <- 
  1 - e1.split.timecourse[e1.split.timecourse$split.type=="Target","prop"] 

e1.split.timecourse <- e1.split.timecourse %>%
  mutate(roll.mean = rollapply(prop,6,FUN=na.mean,partial=TRUE)) %>%
  group_by(age.grp,trial.type,time.step) %>%
  mutate(max = max(roll.mean),min= min(roll.mean))

e1.split.timecourse[with(e1.split.timecourse,
                             split.type=="Target" & min != roll.mean),
                    c("min","max")]<- 0
e1.split.timecourse[with(e1.split.timecourse,
                         split.type=="Competitor" & max != roll.mean),
                    c("min","max")]<- 0


##### EXPERIMENT 1 and 2 DATA
# Subset to test
e1and2.timecourse.data <- data %>%
  filter(trial.type != "Learning", time.step <= TIMECOURSE_END, 
         age.grp <2.5) %>%
  mutate(age.grp = factor(age.grp),
         exp = factor(exp,levels=c("NonSalient","Balanced","Salient")))

e1and2.timecourse.train <- data %>%
  filter(trial.type == "Learning",
         time.step <= TRAIN_END, age.grp <2.5) %>%
  mutate(age.grp = factor(age.grp)) %>%
  group_by(exp,age.grp,trial.type,time.step,trial.num,aoi) %>%
  summarise(n = n()) %>%
  filter(aoi != "NA") %.%
  mutate(n = n / sum(n)) %>%
  group_by(exp,age.grp,trial.type,time.step,aoi,add=FALSE) %>%
  filter(aoi != "Other") %>%
  summarise_each(funs(na.mean,sem),n) %>%
  group_by(age.grp,aoi,exp)
names(e1and2.timecourse.train)[6] <- "prop"

e1and2.timecourse.train <- mutate(e1and2.timecourse.train,
                              roll.mean = rollapply(prop,6,FUN=na.mean, 
                                                    partial=TRUE))
e1and2.timecourse.train <- mutate(e1and2.timecourse.train,
                              roll.sem = rollapply(sem,6,FUN=na.mean, 
                                                   partial=TRUE))

e1and2.timecourse.train$exp <- factor(e1and2.timecourse.train$exp,
                                      levels=c("NonSalient",
                                              "Balanced",
                                              "Salient"))

# Compute timecourse for standard analysis
e1and2.timecourse.test <- e1and2.timecourse.data %>%
  group_by(exp,age.grp,trial.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1and2.timecourse.test)[5] <- "prop"

e1and2.timecourse.test <- mutate(e1and2.timecourse.test,
                             roll.mean = rollapply(prop,6,FUN=na.mean, 
                                                   partial=TRUE))
e1and2.timecourse.test <- mutate(e1and2.timecourse.test,
                             roll.sem = rollapply(sem,6,FUN=na.mean, 
                                                  partial=TRUE))

# Compute side of the screen Ps were on for each test trial
e1and2.split.type <- e1and2.timecourse.data %>%
  filter(time.step == 0) %>%
  mutate(split.type = aoi) %>%
  select(exp,subj,trial.type,trial.num,split.type)

# Subset test data to just for split analysis
e1and2.split.data <- merge(e1and2.timecourse.data,e1and2.split.type) %>%
  filter(split.type == "Target" | split.type == "Competitor",
         time.step >= 0)



# Compute timecourses for split analysis
e1and2.split.timecourse <- e1and2.split.data %>%
  group_by(exp,age.grp,trial.type,split.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1and2.split.timecourse)[6] <- "prop"
e1and2.split.timecourse[e1and2.split.timecourse$split.type=="Target","prop"] <- 
  1 - e1and2.split.timecourse[e1and2.split.timecourse$split.type=="Target",
                              "prop"] 

e1and2.split.timecourse <- e1and2.split.timecourse %>%
  mutate(roll.mean = rollapply(prop,6,FUN=na.mean,partial=TRUE)) %>%
  group_by(exp,age.grp,trial.type,time.step) %>%
  mutate(max = max(roll.mean),min= min(roll.mean))

e1and2.split.timecourse[with(e1and2.split.timecourse,
                             split.type=="Target" & min != roll.mean),
                        c("min","max")]<- 0
e1and2.split.timecourse[with(e1and2.split.timecourse,
                             split.type=="Competitor" & max != roll.mean),
                        c("min","max")]<- 0



