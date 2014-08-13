###############################################################################
############################# SPLIT TRAIN AND TEST ############################
###############################################################################

#summarize across individual trials
test.data <- data %>% 
  filter(trial.type != "Learning", time.step >=TEST_START, 
         time.step <= TEST_END) %>%
  group_by(exp,trial.type,age,age.grp,gender,subj,trial.num)%>%
  summarise(
    prop = sum(aoi=="Target")/(sum(aoi=="Target")+sum(aoi=="Competitor")))

#summarize by subject
test.data.subj <- summarise(test.data,prop=na.mean(prop)) 

#summarize by trial.type
test.data.trial <- test.data.subj %>%
  group_by(exp,age.grp,trial.type,add=FALSE) %>%
  summarise_each(funs(na.mean,ci.low,ci.high),prop)
names(test.data.trial)[4:6] <- c("prop","cil","cih")

#summarize across individual trials
train.data <- data %>%
  filter(trial.type == "Learning", time.step >=TRAIN_START, 
         time.step <= TRAIN_END) %>%
  group_by(exp,trial.type,age,age.grp,gender,subj,trial.num)%>%
  summarise(
    Target = sum(aoi=="Target")/(sum(aoi!="NA")),
    Face = sum(aoi=="Face")/(sum(aoi!="NA")),
    Competitor = sum(aoi=="Competitor")/(sum(aoi!="NA")),
    TD = sum(aoi=="Target")/(sum(aoi=="Target")+sum(aoi=="Competitor")))

#summarize by subject
train.data.subj <- summarise_each(train.data,funs(na.mean),(-trial.num))
#melt down to a data.frame
train.data.subj <- melt(as.data.frame(train.data.subj), 
                        measure.vars = c("Face","Target","Competitor","TD"),
                        variable.name='aoi',
                        value.name='prop',na.rm=FALSE)

#aggregate by trial.type
train.data.trial <- train.data.subj %>%
  group_by(exp,age.grp,aoi) %>%
  summarise_each(funs(na.mean,ci.low,ci.high),prop)
names(train.data.trial)[4:6] <- c("prop","cil","cih")


###############################################################################
######################## SUBSET DATA FOR ANALYSES BELOW #######################
###############################################################################

#Experiment 1 Alone -- All Ages
train.data.e1 <- train.data.trial%>%
  filter(exp=="Balanced",age.grp <4) %>%
  mutate(trial.type = aoi) %>%
  select(-aoi)
train.data.e1.td <- train.data.e1 %>%
  filter(trial.type=="TD") %>%
  mutate(trial.type = "Learning")
train.data.e1.notd <- filter(train.data.e1,trial.type!="TD")

test.data.e1 <- test.data.trial %>%
  filter(exp=="Balanced",age.grp < 4) %>%
  mutate(aoi = trial.type) %>%
  select(-aoi)

# All Exp 1 Data
preflook.data.e1 <- rbind(train.data.e1.td,test.data.e1) %>%
  mutate(trial.type = factor(trial.type,levels=c("Learning","Familiar",
                                                 "Novel","ME")))

#Experiments 1 and 2 Together -- Younger Kids
test.data.e1and2 <- filter(test.data.trial,age.grp < 2.5)
test.data.e1and2.notf <- filter(test.data.e1and2,trial.type != "Familiar")

train.data.e1and2 <- train.data.trial %>%
  filter(age.grp <2.5) %>%
  mutate(trial.type = aoi) %>%
  select(-aoi)

train.data.e1and2.td <- train.data.e1and2 %.%
  filter(trial.type=="TD") %>%
  mutate(trial.type = "Learning")

train.data.e1and2.notd <- filter(train.data.e1and2,trial.type != "TD")

preflook.data.e1and2 <- rbind(train.data.e1and2.td,test.data.e1and2.notf)
preflook.data.e1and2$trial.type <- factor(preflook.data.e1and2$trial.type,
                                          levels=c("Learning","Novel","ME"))
preflook.data.e1and2$exp <-  factor(preflook.data.e1and2$exp,
                                    levels=c("Salient", "Balanced", 
                                             "NonSalient"))

#By-subject analysis for statistics
test.data.subj.e1 <- filter(test.data.subj,exp=="Balanced",age.grp < 4)
test.data.subj.e1and2 <- filter(test.data.subj,age.grp < 2.5)
test.data.subj.e2 <- filter(test.data.subj.e1and2,exp!="Balanced")

train.data.subj.e1.td <- filter(train.data.subj,exp=="Balanced",
                                age.grp < 4,aoi=="TD")
train.data.subj.e1.td$aoi <- NULL

train.data.subj.e1and2.td <- filter(train.data.subj, age.grp < 2.5,aoi=="TD")
train.data.subj.e1and2.td$aoi <- NULL
train.data.subj.e2.td <- filter(train.data.subj.e1and2.td,exp!="Balanced")

ttest.data <- rbind(train.data.subj.e1.td,train.data.subj.e2.td,
                    test.data.subj.e1,test.data.subj.e2)

lmer.data.e1 <- rbind(train.data.subj.e1.td,test.data.subj.e1)
lmer.data.e1$trial.type <- factor(lmer.data.e1$trial.type,
                                  levels=c("Novel","Learning","Familiar","ME"))

#Both Experiments -- reshape so Familiar is a control variable for lmer
lmer.data.e1and2 <- rbind(train.data.subj.e1and2.td,test.data.subj.e1and2)
lmer.data.e1and2  <- reshape(lmer.data.e1and2,timevar="trial.type",
                             idvar=c("exp","subj","age","age.grp","gender"),
                             direction="wide")
lmer.data.e1and2  <- reshape(lmer.data.e1and2,
                             varying=c("prop.Learning","prop.Novel","prop.ME"),
                             idvar=c("exp","subj","age","age.grp","gender"),
                             ids = "trial.type",
                             direction="long")
rownames(lmer.data.e1and2) <- NULL
names(lmer.data.e1and2)[6] <- "Familiar"
names(lmer.data.e1and2)[7] <- "trial.type"
lmer.data.e1and2$trial.type <- factor(lmer.data.e1and2$trial.type,
                                      levels=c("Novel","Learning","ME"))

#Demographic data reported for Exps 1 and 2
demo.data <- rbind(test.data.subj.e1,test.data.subj.e2) %>%
  group_by(age.grp,exp,trial.type) %>%
  summarise(n = n(),
            num.girls = sum(gender=="Female")) %>%
  filter(trial.type=="Familiar") %>%
  select(-trial.type)
