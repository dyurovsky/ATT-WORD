###############################################################################
################ FIGURE 2: LEARNING TRIAL TIMECOURSE IN EXP 1 #################
###############################################################################
quartz(width=10,height=3,title = "Train Looking")
p <- ggplot(e1.timecourse.train, aes(x=time.step, y=prop, 
                                colour=age.grp, fill = age.grp))+
  facet_grid(~ aoi) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name="Age Group",
                     values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(name = "Age Group",
                    values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))
print(p)

###############################################################################
################### FIGURE 3: TEST TRIAL TIMECOURSE IN EXP 1 ##################
###############################################################################
quartz(width=10,height=3,title = "Test Looking")
p <- ggplot(filter(e1.timecourse.test,time.step==round(time.step,2)),
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
  theme_bw(base_size=14) +
  guides(color = guide_legend(reverse = TRUE),
         fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name="Age Group",
                     values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1")) +
  scale_fill_manual(name = "Age Group",
                    values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))
print(p)

###############################################################################
################## FIGURE 4: ONSET-CONTINGENT PLOT FOR EXP 1 ##################
###############################################################################
quartz(width=12,height=4.5,title = "Test Looking")
p <- ggplot(filter(e1.split.timecourse, trial.type %in% c("Novel", "ME")),
            aes(x=time.step, y=roll.mean, 
                               colour=trial.type, fill = trial.type,
                               linetype=split.type))+
  facet_grid(trial.type ~ age.grp) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  scale_x_continuous(limits = c(0,TIMECOURSE_END),
                     breaks=seq(-1,TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + theme(legend.position=c(.15,.932), legend.direction = "horizontal") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(colour=FALSE,linetype=guide_legend(title=NULL)) +
  scale_color_manual(values=man_cols[1:3]) +
  scale_fill_manual(values=man_cols[1:3]) +
  scale_linetype_discrete(name="Split Type")
print(p)


quartz(width=12,height=4.5,title = "Test Looking")
p <- ggplot(filter(e1.split.timecourse,trial.type %in% c("Novel", "ME")),
            aes(x=time.step, y=roll.mean, 
                colour=trial.type, fill = trial.type))+
  facet_grid(split.type ~ age.grp) +
  geom_line(size=.8) +
  geom_hline(aes(yintercept=.5),lty=2)+
  scale_x_continuous(limits = c(0,TIMECOURSE_END),
                     breaks=seq(-1,TIMECOURSE_END),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Switch") +
  theme_bw(base_size=14) + theme(legend.position=c(.15,.932), 
                                 legend.direction="horizontal") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  guides(linetype=FALSE,colour=guide_legend(title=NULL)) +
  scale_color_manual(values=man_cols[1:3]) +
  scale_fill_manual(values=man_cols[1:3]) +
  scale_linetype_discrete(name="Trial Type")
print(p)


###############################################################################
################ FIGURE 7: LEARNING TRIAL TIMECOURSE IN EXP 2 #################
###############################################################################
quartz(width=9,height=6.5,title = "Train Looking")
p <- ggplot(e1and2.timecourse.train, aes(x=time.step, y=prop, 
                                    colour=age.grp, fill = age.grp))+
  facet_grid(exp~ aoi) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  scale_x_continuous(limits = c(-1,4),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to ROI") +
  theme_bw(base_size=14) + guides(color = guide_legend(reverse = TRUE),
                                  fill = guide_legend(reverse = TRUE)) +
  theme(legend.position=c(.26,.905),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Age Group",values=man_cols[1:3],
                     breaks=c("2","1.5","1")) +
  scale_fill_manual(name="Age Group",values=man_cols[1:3],
                    breaks=c("2","1.5","1"))
print(p)

###############################################################################
################### FIGURE 8: TEST TRIAL TIMECOURSE IN EXP 1 ##################
###############################################################################
quartz(width=9,height=7,title = "Test Looking")
p <- ggplot(e1and2.timecourse.test, aes(x=time.step, y=prop, 
                                   colour=age.grp, fill = age.grp))+
  facet_grid(exp ~ trial.type) +
  geom_ribbon(aes(ymin = roll.mean-roll.sem,
                  ymax = roll.mean+roll.sem),
              alpha = .3, linetype = 0) +
  geom_line(size=.8) +
  geom_vline(aes(xintercept=0),lty=2) +
  geom_hline(aes(yintercept=.5),lty=2)  +
  scale_x_continuous(limits = c(-1,4),breaks=c(-1,0,1,2,3,4),
                     name = "Time(s)") + 
  scale_y_continuous(limits = c(0,1), breaks=c(0,.25,.5,.75,1),
                     name = "Prop. Looks to Target") +
  theme_bw(base_size=14) + guides(color = guide_legend(reverse = TRUE),
                                  fill = guide_legend(reverse = TRUE)) +
  theme(legend.position=c(.935,.758),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(name="Age Group",values=man_cols[1:3],
                     breaks=c("2","1.5","1")) +
  scale_fill_manual(name="Age Group",values=man_cols[1:3],
                    breaks=c("2","1.5","1"))
print(p)

###############################################################################
################## FIGURE 9: ONSET-CONTINGENT PLOT FOR EXP 2 ##################
###############################################################################
quartz(width=6.5,height=6.25,title = "Test Looking")
p <- ggplot(filter(e1and2.split.timecourse, trial.type=="Novel"), 
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
  theme_bw(base_size=18) + theme(legend.position=c(.135,.9315)) +
  scale_color_manual(values=man_cols[2]) +
  scale_fill_manual(values=man_cols[2]) +
  guides(colour=FALSE,linetype=guide_legend(title=NULL)) + 
  ggtitle("ME Trials") + 
  theme(title = element_text(vjust=1),
        axis.title.x = element_text(vjust=-.25),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(p)
