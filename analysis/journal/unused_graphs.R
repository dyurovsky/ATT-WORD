# Compute rts to switch for each trial
e1.split.rt <- e1.split.data %>%
  filter(aoi == "Target", split.type == "Competitor") %>%
  group_by(trial.type,trial.num,age.grp,subj,aoi) %>%
  summarise_each(funs(min),time.step) %>%
  mutate(rt = time.step) %>%
  select(trial.type,trial.num,subj,age.grp,rt)

# Compute rts to switch for each trial
e1and2.split.rt <- e1and2.split.data %>%
  filter(aoi == "Target", split.type == "Competitor") %>%
  group_by(exp,trial.type,trial.num,age.grp,subj,aoi) %>%
  summarise_each(funs(min),time.step) %>%
  mutate(rt = time.step) %>%
  select(exp,trial.type,age.grp,rt)

# mean RTs for split histogram plots
e1.mean.rts <- e1.split.rt %>%
  group_by(age.grp,trial.type,add=FALSE) %>%
  summarise(rt = na.mean(rt))


# mean RTs for split histogram plots
e1and2.mean.rts <- e1and2.split.rt %>%
  group_by(exp,age.grp,trial.type,add=FALSE) %>%
  summarise(rt = na.mean(rt))

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
