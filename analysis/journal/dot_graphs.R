###############################################################################
################## FIGURE 5: LEARNING TRIAL LOOKING IN EXP 1 ##################
###############################################################################
quartz(width=4,height=3,title = "Learning")
p <- ggplot(train.data.e1.notd, aes(x=age.grp, y=prop,colour=trial.type, 
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
  geom_dl(aes(label=trial.type),
          method=list(dl.trans(x=x +.2),"last.qp",cex=.8)) + 
  scale_color_manual(values=man_cols,breaks=c("3.5","3","2.5","2","1.5","1"))
print(p)

###############################################################################
################# FIGURE 6: LEARNING AND TEST PROPS. IN EXP 1 #################
###############################################################################
quartz(width=10,height=4.5,title = "Test Data")
p <- ggplot(preflook.data.e1, 
       aes(x=age.grp, y=prop,colour=trial.type))+
  facet_grid(. ~ trial.type) +
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
print(p)

###############################################################################
############## FIGURE 10: COMPARING SALIENCE AT LEARNING AND TEST #############
###############################################################################
quartz(width=10,height=3.5,title = "Test Data")
p <- ggplot(preflook.data.e1and2, 
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
  geom_dl(aes(label=exp),method=list(dl.trans(x=x +.3),"last.qp",cex=1)) + 
  scale_color_manual(values=man_cols,breaks=c("2","1.5","1"))
print(p)