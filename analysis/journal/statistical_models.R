###############################################################################
################################# T-STATISTICS ################################
###############################################################################
library(compute.es)

#Make a table of t, df, p-vals, and cohen's d for all conditions
ts <- ttest.data %>%
  group_by(exp,age.grp,trial.type) %>%
  summarise( t = t.test(prop,mu=.5)$statistic,
             df = t.test(prop,mu=.5)$parameter,
             p.val = t.test(prop,mu=.5)$p.value,
             sd = na.sd(prop),
             prop = na.mean(prop)) %>%
  mutate(d = abs(prop-.5)/sd)


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