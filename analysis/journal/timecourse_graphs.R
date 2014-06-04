###############################################################################
############################## TIMECOURSE GRAPHS ##############################
###############################################################################

## EXP1
e1.timecourse.test <- data %>%
  filter(exp == "Balanced",trial.type != "Learning",
         time.step <= TEST_END, age.grp <4) %>%
  mutate(age.grp = factor(age.grp)) 

split.type <- e1.timecourse.test %>%
  filter(time.step == 0) %>%
  mutate(split.type = aoi) %>%
  select(subj,trial.type,trial.num,split.type)

e1.timecourse.split <- merge(e1.timecourse.test,split.type) %>%
  filter(split.type == "Target" | split.type == "Competitor",
         time.step >= 0) %>%

e1.rt.split <- e1.timecourse.split %>%
  group_by(trial.type,trial.num,split.type,age.grp,subj,aoi) %>%
  filter(aoi == "Target" | aoi == "Competitor", aoi != split.type) %>%
  summarise_each(funs(min),time.step) %>%
  mutate(rt = 1000*time.step) %>%
  select(trial.type,split.type,age.grp,rt)

e1.timecourse.split <- e1.timecourse.split %>%
  group_by(age.grp,trial.type,split.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1.timecourse.split)[5] <- "prop"
e1.timecourse.split[e1.timecourse.split$split.type=="Target","prop"] <- 
  1 - e1.timecourse.split[e1.timecourse.split$split.type=="Target","prop"]

e1.timecourse.test <- e1.timecourse.test %>%
  group_by(age.grp,trial.type,time.step,trial.num) %>%
  summarise(prop = sum(aoi=="Target")/
              (sum(aoi=="Target")+sum(aoi=="Competitor"))) %>%
  summarise_each(funs(na.mean,sem),prop)
names(e1.timecourse.test)[4] <- "prop"

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
 
##SPLIT RT HIST
quartz(width=12,height=5,title = "Switch RTs")

ggplot(e1.rt.split,aes(x = rt, fill=trial.type)) + 
  facet_grid(split.type ~ age.grp) +
  geom_density(alpha=.5) +
  scale_fill_manual(values=man_cols[1:3])
  

##SPLIT RT HIST
quartz(width=12,height=5,title = "Switch RTs")

ggplot(e1.rt.split,aes(x = rt, fill=trial.type)) + 
  facet_grid(split.type ~ age.grp) +
  geom_histogram(binwidth=100) 


##SPLIT GRAPH
quartz(width=12,height=5,title = "Test Looking")
ggplot(e1.timecourse.split, aes(x=time.step, y=prop, 
                               colour=trial.type, fill = trial.type,
                               linetype=split.type,))+
  facet_wrap( ~ age.grp) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)  +
  scale_x_continuous(limits = c(0,4),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + #+ theme(legend.position="none") +
  scale_color_manual(values=man_cols[1:3]) +
  scale_fill_manual(values=man_cols[1:3])


##TRAIN GRAPH
quartz(width=10,height=3,title = "Train Looking")
ggplot(e1.timecourse.train, aes(x=time.step, y=prop, 
                                colour=age.grp, fill = age.grp))+
  facet_grid(~ aoi) +
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
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

##TEST GRAPH
quartz(width=10,height=3,title = "Test Looking")
ggplot(e1.timecourse.test, aes(x=time.step, y=prop, 
                               colour=age.grp, fill = age.grp))+
  facet_grid(~ trial.type) +
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
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))

## EXP1&2
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