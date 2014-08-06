# clear all previous variables
rm(list=ls())

#get lab version of useful R functions
source('../helpers/useful.R')

#load libraries for data manipulation and graphing
library(directlabels)
library(xtable)

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


## TIMECOURSE GRAPHS
#source('timecourse_graphs.R')

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

###############################################################################
################## FIGURE 2: LEARNING TRIAL LOOKING IN EXP 1 ##################
###############################################################################
quartz(width=4,height=3,title = "Learning")
ggplot(train.data.e1.notd, aes(x=age.grp, y=prop,colour=trial.type, 
                               group=trial.type)) +
  geom_pointrange(aes(ymin = prop-cih,
                      ymax = prop+cih),
                  position = position_dodge(.1),
                  size=.8)+
  geom_line() +
  scale_x_continuous(limits = c(.9,4.3), breaks=seq(1,3.5,.5),
                     name = "Age(years)",
                     labels = c("1", "1.5", "2","2.5","3","3.5")) + 
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,.2),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=12) + 
  geom_dl(aes(label=trial.type),method=list("last.qp",cex=.8,hjust=-.15)) + 
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
quartz(width=10,height=3.5,title = "Test Data")
ggplot(preflook.data.e1and2, 
       aes(x=age.grp, y=prop,colour=exp, lty=exp))+
  facet_grid(. ~ trial.type) +
  geom_pointrange(aes(ymin = prop-cih,
                      ymax = prop+cih),
                  position = position_dodge(.1),
                  size=.8)+
  geom_hline(aes(yintercept=.5),lty=2)  +
  geom_line(aes(group=exp)) +
  scale_x_continuous(limits = c(.9,2.5), breaks=c(1,1.5,2),name = "Age(years)",
                     labels = c("1", "1.5", "2")) + 
  scale_y_continuous(limits = c(.25,1), breaks=seq(.3,1,.1),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=14) + 
  theme(legend.position="none")+
  geom_dl(aes(label=exp),method=list("last.qp",cex=1,hjust=-.15)) + 
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
################################# PAPER TABLES ################################
###############################################################################
#TABLE 1
e1and2.tab <- as.data.frame(summary(e1and2.lm4)$coef)
e1and2.tab$Predictor <- c("Intercept","Age(years)","NonSalient",
                          "Salient","Learning","ME","Familiar",
                          "Non-Salient*Learning","Salient*Learning",
                          "Non-Salient*ME","Salient*ME")
e1and2.tab <- e1and2.tab[c(1,2,7,4,3,5,6,9,8,11,10),]
rownames(e1and2.tab) <- NULL
e1and2.tab <- e1and2.tab[,c(5,1:4)]
e1and2.tab$stars <- sapply(e1and2.tab[,5],getstars)
names(e1and2.tab)[6] <- ""

names(e1and2.tab)[4:5] <- c("$z$ value","$p$ value")

print(xtable(e1and2.tab,
             align = c("l","l","r","r","r","r","l"),
             label = "tab:model_table"),
      include.rownames=FALSE,hline.after=c(0,nrow(e1and2.tab)),
      sanitize.text.function=function(x){x})

###############################################################################
################################ AGE HISTORGRAM ###############################
###############################################################################
quartz()
ggplot(train.data.subj.e1.td, aes(x=age.grp))+
#  facet_grid(exp ~ .) +
  geom_bar(width=.4,binwidth=.5,fill="white",color="black") +
  scale_x_continuous(name = "Age",limits = c(1,4),breaks=seq(1,4,.5)) + 
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,5),
                     name = "Number of Children") +
  theme_bw(base_size=18) + theme(legend.position="none",
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank())


