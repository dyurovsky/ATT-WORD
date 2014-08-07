###############################################################################
############################## TIMECOURSE GRAPHS ##############################
###############################################################################
##### EXPERIMENT 1 DATA

##SPLIT RT HIST
quartz(width=12,height=5,title = "Switch RTs")
ggplot(e1.split.rt,aes(x = rt)) + 
  facet_grid(trial.type ~ age.grp,scales = "free") +
  geom_histogram(aes(y=..density..),binwidth=.25,
                 color="black",fill=man_cols[1]) +
  geom_density(adjust=2) + 
  geom_vline(data=e1.mean.rts, aes(xintercept=rt),
             linetype="dashed", size=.5) +
  theme_bw(base_size=14) + 
  theme(legend.position=c(.945, .9)) +
  scale_x_continuous(limits = c(0,TIMECOURSE_END),
                     breaks=seq(0,TIMECOURSE_END),
                     name = "Reaction Time (s)") +
  scale_y_continuous(breaks=NULL,
    name = "Proportion of Children Making their First Switch")

##E1 SPLIT GRAPH
quartz(width=12,height=6,title = "Test Looking")
ggplot(e1.split.timecourse, aes(x=time.step, y=roll.mean, 
                               colour=trial.type, fill = trial.type,
                               linetype=split.type,))+
  facet_grid(trial.type ~ age.grp) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  geom_ribbon(aes(ymin=min,ymax=max),fill="gray",alpha=.2, 
              colour=NA) +
  scale_x_continuous(limits = c(0,TIMECOURSE_END),
                     breaks=seq(-1,TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + theme(legend.position=c(.055,.932)) +
  guides(colour=FALSE,linetype=guide_legend(title=NULL)) +
  scale_color_manual(values=man_cols[1:3]) +
  scale_fill_manual(values=man_cols[1:3]) +
  scale_linetype_discrete(name="Split Type")

##TEST GRAPH
quartz(width=10,height=3,title = "Test Looking")
ggplot(filter(e1.timecourse.test,time.step==round(time.step,2)),
              aes(x=time.step, y=roll.mean, 
                               colour=age.grp, fill = age.grp))+
  facet_grid(~ trial.type) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2)  +
  scale_x_continuous(limits = c(-1,4.5),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(.25,1), breaks=c(.25,.5,.75),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=14) + theme(legend.position="none") +
  geom_dl(aes(label=age.grp),method=list("last.qp",cex=1.2,hjust=-.15)) +
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

## EXP1&2
##SPLIT RT HIST
quartz(width=10,height=5,title = "Switch RTs")
ggplot(e1and2.split.rt,aes(x = rt,color=exp,fill=exp)) + 
  facet_grid(trial.type ~ age.grp,scales = "free") +
  geom_histogram(aes(y=..density..),binwidth=.25,
                 color="black",position="dodge") +
  geom_density(alpha=0,adjust=2) + 
  geom_vline(data=e1and2.mean.rts, aes(xintercept=rt,color=exp),
             linetype="dashed", size=1) +
  theme_bw(base_size=14) + 
  theme(legend.position=c(.93, .87)) +
  scale_color_manual(values=man_cols[c(3,2,1)]) +
  scale_fill_manual(values=man_cols[c(3,2,1)]) +
  scale_x_continuous(limits = c(0,TIMECOURSE_END),breaks=seq(0,TIMECOURSE_END),
                     name = "Reaction Time (s)") +
  scale_y_continuous(breaks=NULL,
                     name = "Proportion of Children Making their First Switch")


##SPLIT GRAPH
quartz(width=6.5,height=6,title = "Test Looking")
ggplot(filter(e1and2.split.timecourse, trial.type=="Familiar"), 
       aes(x=time.step, y=prop, colour=trial.type, fill = trial.type,
                                linetype=split.type))+
  facet_grid(exp ~ age.grp) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  geom_ribbon(aes(ymin=min,ymax=max),fill="gray",alpha=.2, 
              colour=NA) +
  scale_x_continuous(limits = c(0,TIMECOURSE_END),
                     breaks=seq(-1,TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + theme(legend.position=c(.11,.934)) +
  scale_color_manual(values=man_cols[1]) +
  scale_fill_manual(values=man_cols[1]) +
  guides(colour=FALSE,linetype=guide_legend(title=NULL))

e1and2.timecourse.test <- data %>%
  filter(trial.type != "Learning",
         time.step <= TEST_END, age.grp <2.5) %>%
  mutate(age.grp = factor(age.grp)) %>%
  group_by(exp,age.grp,trial.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1and2.timecourse.test)[5] <- "prop"

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
  summarise_each(funs(na.mean,sem),n)
names(e1and2.timecourse.train)[6] <- "prop"


##TRAIN GRAPH
quartz(width=10,height=6.5,title = "Train Looking")
ggplot(e1and2.timecourse.train, aes(x=time.step, y=prop, 
                                    colour=age.grp, fill = age.grp))+
  facet_grid(exp~ aoi) +
  geom_ribbon(aes(ymin = prop-sem,
                  ymax = prop+sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  scale_x_continuous(limits = c(-1,4.5),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=14) + theme(legend.position="none") +
  geom_dl(aes(label=age.grp),method=list("last.qp",cex=1.2,hjust=-.15)) +
  scale_color_manual(values=man_cols[1:3],breaks=c("2","1.5","1")) +
  scale_fill_manual(values=man_cols[1:3],breaks=c("2","1.5","1"))

##TEST GRAPH
quartz(width=10,height=6.5,title = "Test Looking")
ggplot(e1and2.timecourse.test, aes(x=time.step, y=prop, 
                                   colour=age.grp, fill = age.grp))+
  facet_grid(exp ~ trial.type) +
  geom_ribbon(aes(ymin = prop-sem,
                  ymax = prop+sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2)  +
  scale_x_continuous(limits = c(-1,4.5),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=14) + theme(legend.position="none") +
  geom_dl(aes(label=age.grp),method=list("last.qp",cex=1.2,hjust=-.15)) +
  scale_color_manual(values=man_cols[1:3],breaks=c("2","1.5","1")) +
  scale_fill_manual(values=man_cols[1:3],breaks=c("2","1.5","1"))



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
  summarise_each(funs(na.mean,sem),n)
names(e1.timecourse.train)[5] <- "prop"

##E1 TRAIN GRAPH
quartz(width=10,height=3,title = "Train Looking")
ggplot(e1.timecourse.train, aes(x=time.step, y=prop, 
                                colour=age.grp, fill = age.grp))+
  facet_grid(~ aoi) +
  geom_ribbon(aes(ymin = prop-sem,
                  ymax = prop+sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  scale_x_continuous(limits = c(-1,4),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=14) + #theme(legend.position=c(.5, .5),
                          #       legend.direction = "horizontal") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",
                     values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(name = "Age Group",
                    values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

##E1 TEST GRAPH
quartz(width=10,height=3,title = "Test Looking")
ggplot(e1.timecourse.test, aes(x=time.step, y=prop, 
                                colour=age.grp, fill = age.grp))+
  facet_grid(~ trial.type) +
  geom_ribbon(aes(ymin = prop-sem,
                  ymax = prop+sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2) +
  scale_x_continuous(limits = c(-1,4),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=14) + #theme(legend.position=c(.5, .5),
  #       legend.direction = "horizontal") +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  scale_color_manual(name="Age Group",
                     values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(name = "Age Group",
                    values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

