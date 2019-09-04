# clear all previous variables
rm(list=ls())

#get lab version of useful R functions
source('Ranalysis/useful.R')

#load libraries for data manipulation and graphing
library(directlabels)
library(data.table)
library(dplyr)

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
sal.data <- read.csv("data/salient.csv",header=TRUE)
nonsal.data <- read.csv("data/nonsalient.csv",header=TRUE)
balanced.data <- read.csv("data/balanced.csv",header=TRUE)

#mark experiment
sal.data$exp <- "Salient"
nonsal.data$exp <- "NonSalient"
balanced.data$exp <- "Balanced"

data <- rbind(sal.data,nonsal.data,balanced.data)

#pre-process data values to be more English-readable
data$trial.type <- factor(data$trial.type, 
                          labels=c("Learning", "Familiar", "Novel", "ME"))
data$aoi <- factor(data$aoi, labels=c("Target", "Competitor", 
                                      "Face", "Other","NA"))
data$time.step <- data$time.step/60 - 1
data$gender <- factor(data$gender,labels=c("Male","Female"))

data <- data.table(data[data$age >=1,]) #drop kids under 1 year of age
data$age.grp <- split.ages(data$age) #apply age-splitting function

###############################################################################
############################# SPLIT TRAIN AND TEST ############################
###############################################################################

#summarize across individual trials
#dplyr/data.table functions make this much faster
test.data <- data %.% 
  filter(trial.type != "Learning", time.step >=TEST_START, 
         time.step <= TEST_END) %.%
  group_by(exp,trial.type,age,age.grp,gender,subj,trial.num)%.%
  summarise(
    prop = sum(aoi=="Target")/(sum(aoi=="Target")+sum(aoi=="Competitor")))

#summarize by subject
test.data.subj <- summarise(test.data,prop=mean(prop,na.rm=TRUE)) 

#summarize by trial.type
test.data.trial <- ungroup(test.data.subj)
test.data.trial <- test.data.trial %.%
  group_by(exp,age.grp,trial.type) %.%
  summarise(
    prop=mean(prop,na.rm=TRUE),
    cih=ci.high(prop),
    cil=ci.low(prop))

#summarize across individual trials
train.data <- data %.%
  filter(trial.type == "Learning", time.step >=TRAIN_START, 
         time.step <= TRAIN_END) %.%
  group_by(exp,trial.type,age,age.grp,gender,subj,trial.num)%.%
  summarise(
    Target = sum(aoi=="Target")/(sum(aoi!="NA")),
    Face = sum(aoi=="Face")/(sum(aoi!="NA")),
    Competitor = sum(aoi=="Competitor")/(sum(aoi!="NA")),
    TD = sum(aoi=="Target")/(sum(aoi=="Target")+sum(aoi=="Competitor")))

#summarize by subject
train.data.subj <- summarise(train.data,
                             Target=mean(Target,na.rm=TRUE),
                             Face=mean(Face,na.rm=TRUE),
                             Competitor=mean(Competitor,na.rm=TRUE),
                             TD=mean(TD,na.rm=TRUE))

#for some reason ci.high/ci.low break when used with with database functions
#melt down to a data.frame
train.data.subj <- melt(as.data.frame(train.data.subj), 
     measure.vars = c("Face","Target","Competitor","TD"),
                        variable.name='aoi',
                        value.name='prop',na.rm=FALSE)

#use good ol' aggregate
train.data.trial <- aggregate(prop ~ aoi +age.grp + exp, 
                              data = train.data.subj, FUN = na.mean)
train.data.trial$cih <- aggregate(prop ~ aoi +age.grp + exp, 
                                  data = train.data.subj, FUN = ci.high)$prop
train.data.trial$cil <- aggregate(prop ~ aoi +age.grp + exp, 
                                  data = train.data.subj, FUN = ci.low)$prop

###############################################################################
######################## SUBSET DATA FOR ANALYSES BELOW #######################
###############################################################################

#Experiment 1 Alone -- All Ages
train.data.e1 <- filter(train.data.trial,exp=="Balanced",age.grp <4)
train.data.e1.notd <- filter(train.data.e1,aoi!="TD")
train.data.e1.td <- filter(train.data.e1,aoi=="TD")

test.data.e1 <- filter(test.data.trial,exp=="Balanced",age.grp < 4)

names(train.data.e1.td)[1] <- "trial.type"
train.data.e1.td$trial.type <- "Learning"

preflook.data.e1 <- rbind(train.data.e1.td,test.data.e1)
preflook.data.e1$trial.type <- factor(preflook.data.e1$trial.type,
                                      levels=c("Learning","Familiar",
                                               "Novel","ME"))

#Experiments 1 and 2 Together -- Younger Kids
test.data.e1and2 <- filter(test.data.trial,age.grp < 2.5)
test.data.e1and2.notf <- filter(test.data.e1and2,trial.type != "Familiar")

train.data.e1and2 <- filter(train.data.trial,age.grp <2.5)
train.data.e1and2.td <- filter(train.data.e1and2,aoi=="TD")

names(train.data.e1and2.td)[1] <- "trial.type"
train.data.e1and2.td$trial.type <- "Learning"

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
demo.data<- aggregate(subj ~ age.grp + exp + trial.type, 
                      data = rbind(test.data.subj.e1,
                                   test.data.subj.e2),
                      FUN = function(x) {length(x)})
demo.data$num.girls <- aggregate(gender ~ age.grp + exp + trial.type, 
                                 data = rbind(test.data.subj.e1,
                                              test.data.subj.e2),
                                 FUN = function(x) {sum(x=="Female")})$gender
demo.data <- subset(demo.data,trial.type="Familiar")


###############################################################################
################## FIGURE 2: LEARNING TRIAL LOOKING IN EXP 1 ##################
###############################################################################
quartz(width=4,height=3,title = "Learning")
ggplot(train.data.e1.notd, aes(x=age.grp, y=prop,colour=aoi, group=aoi)) +
  geom_pointrange(aes(ymin = prop-cih,
                      ymax = prop+cih),
                  position = position_dodge(.1),
                  size=.8)+
  geom_line(aes(group=aoi)) +
  scale_x_continuous(limits = c(.9,4.3), breaks=seq(1,3.5,.5),
                     name = "Age(years)",
                     labels = c("1", "1.5", "2","2.5","3","3.5")) + 
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.2),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=12) + 
  geom_dl(aes(label=aoi),method=list("last.qp",cex=.8,hjust=-.15)) + 
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

###############################################################################
################# FIGURE 3: LEARNING AND TEST PROPS. IN EXP 1 #################
###############################################################################
quartz(width=6,height=6,title = "Test Data")
ggplot(preflook.data.e1, 
       aes(x=age.grp, y=prop,colour=trial.type))+
  facet_wrap(~ trial.type) +
  geom_pointrange(aes(ymin = prop-cih,
                      ymax = prop+cih),
                  position = position_dodge(.3),
                  size=.8)+
  geom_hline(aes(yintercept=.5),lty=2)  +
  geom_line(aes(group=trial.type)) +
  scale_x_continuous(breaks=c(1,1.5,2,2.5,3,3.5),
                     name = "Age(years)",
                     labels = c("1","1.5","2","2.5","3","3.5")) + 
  scale_y_continuous(limits = c(.4,1), breaks=seq(.4,1,.1),
                     name = "Prop. Looks to Target vs. Competitor") +
  theme_bw(base_size=18) + 
  theme(legend.position=c(.95,.6),legend.title=element_blank()) +
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

###############################################################################
############## FIGURE 4: COMPARING SALIENCE AT LEARNING AND TEST ##############
###############################################################################
quartz(width=4,height=7,title = "Test Data")
ggplot(preflook.data.e1and2, 
       aes(x=age.grp, y=prop,colour=exp, lty=exp))+
  facet_grid(trial.type ~ .) +
  geom_pointrange(aes(ymin = prop-cih,
                      ymax = prop+cih),
                  position = position_dodge(.1),
                  size=.8)+
  geom_hline(aes(yintercept=.5),lty=2)  +
  geom_line(aes(group=exp)) +
  scale_x_continuous(limits = c(.9,2.4), breaks=c(1,1.5,2),name = "Age(years)",
                     labels = c("1", "1.5", "2")) + 
  scale_y_continuous(limits = c(.25,1), breaks=seq(.3,1,.1),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=12) + 
  theme(legend.position="none")+
  geom_dl(aes(label=exp),method=list("last.qp",cex=.8,hjust=-.15)) + 
  scale_color_manual(values=man_cols,breaks=c("2","1.5","1"))

###############################################################################
################################# T-STATISTICS ################################
###############################################################################
ts <- aggregate(prop ~ trial.type + age.grp + exp, data=ttest.data,
                FUN=na.mean)

#Make a table of t, df, and p-vals for all conditions
ts$t <- aggregate(prop ~ trial.type + age.grp + exp, data=ttest.data,
                FUN=function(x) {t.test(x,mu=.5)}$statistic)$prop
ts$df <- aggregate(prop ~ trial.type + age.grp + exp, data=ttest.data,
                  FUN=function(x) {t.test(x,mu=.5)}$parameter)$prop
ts$p.val <- aggregate(prop ~ trial.type + age.grp + exp, data=ttest.data,
                   FUN=function(x) {t.test(x,mu=.5)}$p.value)$prop


###############################################################################
############################# MIXED-EFFECTS MODELS ############################
###############################################################################

### EXPERIMENT 1 ###
e1.lm1 <- glmer(prop ~ age + (1 | subj), 
                  family="binomial", data = lmer.data.e1)

#Model Reported in Experiment 1
e1.lm2 <- glmer(prop ~ age + trial.type + (1 | subj), 
                family="binomial", data = lmer.data.e1)
anova(e1.lm1,e1.lm2)

e1.lm3 <- glmer(prop ~ age * trial.type + (1 | subj), 
                family="binomial", data = lmer.data.e1)
anova(e1.lm2,e1.lm3)

### EXPERIMENTS 1 AND 2 ###
e1and2.lm1 <- glmer(prop ~ age + exp + (1 | subj), 
                    family="binomial", data = lmer.data.e1and2)

e1and2.lm2 <- glmer(prop ~ age + exp + trial.type + (1 | subj),
             family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm1,e1and2.lm2)

e1and2.lm3 <- glmer(prop ~ age + exp + trial.type + Familiar + (1 | subj),
             family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm2,e1and2.lm3)

#Model reported in Table 1
e1and2.lm4 <- glmer(prop ~ age + exp*trial.type + Familiar + (1 | subj), 
             family="binomial", data = lmer.data.e1and2)

anova(e1and2.lm3,e1and2.lm4)

###############################################################################
################################ AGE HISTORGRAM ###############################
###############################################################################
quartz()
ggplot(demo.data, aes(x=age.grp, y=subj, fill = exp))+
  facet_grid(exp ~ .) +
  geom_bar(width=.4,position=position_dodge(),stat="identity") +
  scale_x_discrete(name = "Age") + 
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,5),
                     name = "Number of Children") +
  theme_bw(base_size=18) + theme(legend.position="none")


