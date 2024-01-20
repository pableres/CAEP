basreg<- data.frame(basreg[complete.cases(basreg), ])
basreg$Group <- as.factor(basreg$Group)
levels(basreg$Group) <- c("Condom", "No condom")

#Figura 1
#density plot
a1<-ggplot(basreg, aes(x=subj_arou, fill=Group))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("Subjective arousal")+
  theme(legend.position="bottom")
a1

#boxplot
a2<-ggplot(basreg, aes(x = Group, y = subj_arou)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position = position_jitter(0.5), aes(colour = Group))+
  theme_bw()+
  ylab("Subjective arousal")+
  scale_y_continuous(breaks = round(seq(min(basreg$subj_arou), max(basreg$subj_arou), by = 5),0))+
  theme(legend.position="bottom")
a2


Fig3<-grid.arrange(a2, a1, nrow = 1)
Fig3