library(gridExtra)
library(ggplot2)
library(ggpubr)
#Transform agrupation ariable as a factor variable
myplot$Sexo <- as.factor(myplot$Sexo)
myplot$edad <- as.factor(myplot$edad)
myplot$Estudios <- as.factor(myplot$Estudios)
#it gives levels to Grouping variable
levels(myplot$Sexo) <- c("Male", "Female")
levels(myplot$edad) <- c("Young", "Midium","Old")
levels(myplot$Estudios) <- c("No studies", "Primarry","Secondary","University")
myplot<- data.frame(myplot[complete.cases(myplot), ])


#Figura 1
#density plot
b4<-ggplot(myplot, aes(x=ASF,fill=Sexo))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASF")+
  theme(legend.position='none')
b4

b5<-ggplot(myplot, aes(x=ASS, fill=Sexo))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASS")+
  theme(legend.position="bottom")
b5

b6<-ggplot(myplot, aes(x=ASF, fill=edad))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASF")+
  theme(legend.position='none')
b6

b7<-ggplot(myplot, aes(x=ASS, fill=edad))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASS")+
  theme(legend.position="bottom")
b7

b8<-ggplot(myplot, aes(x=ASF, fill=Estudios))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASF")+
  theme(legend.position='none')
b8

b9<-ggplot(myplot, aes(x=ASS, fill=Estudios))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("ASS")+
  theme(legend.position="bottom")
b9 + stat_compare_means(method = "t.test")      
b9

F1<-grid.arrange(b4,b6,b8,b5,b7,b9, nrow= 2)
F1
ggpubr::ggarrange(F1, F2, F3, F4, labels = c("A", "B", "C" ,"D"))


#boxplot
b5<-ggplot(myplot, aes(x = Sexo, y = ASF)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position = position_jitter(0.5), aes(colour = Sexo))+
  theme_bw()+
  ylab("ASF")+
  scale_y_continuous(breaks = round(seq(min(myplot$ASF), max(myplot$ASF), by = 5),1))+
  theme(legend.position="bottom")
b5


F2<-grid.arrange(b5, b4, nrow = 1)
F2

#figura 1
c4<-ggplot(bfisio, aes(x=inc_med, fill=Group))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("av % circumference ??")+
  theme(legend.position="bottom")
c4

c5<-ggplot(bfisio, aes(x = Group, y = inc_med)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position = position_jitter(0.5), aes(colour = Group))+
  theme_bw()+
  ylab("av % circumference ??")+
  scale_y_continuous(breaks = round(seq(min(bfisio$inc_med), max(bfisio$inc_med), by = 5),0))+
  theme(legend.position="bottom")
c5

F1<-grid.arrange(c5, c4, nrow = 1)

#Figura3
d4<-ggplot(bfisio, aes(x=latency, fill=Group))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("erection latency (sec)")+
  theme(legend.position='none')
d4

d5<-ggplot(bfisio, aes(x = Group, y = latency)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position = position_jitter(0.5), aes(colour = Group))+
  theme_bw()+
  ylab("erection latency (sec)")+
  scale_y_continuous(breaks = round(seq(min(bfisio$latency), max(bfisio$latency), by = 10),0))+
  theme(legend.position='none')
d5

F3<-grid.arrange(d5, d4, nrow = 1)

#Figura 4
e4<-ggplot(bfisio, aes(x=duration, fill=Group))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("erection duration (sec)")+
  theme(legend.position='none')
e4

e5<-ggplot(bfisio, aes(x = Group, y = duration)) + 
  geom_boxplot(notch = TRUE) +
  geom_jitter(position = position_jitter(0.5), aes(colour = Group))+
  theme_bw()+
  ylab("erection duration (sec)")+
  scale_y_continuous(breaks = round(seq(min(bfisio$duration), max(bfisio$duration), by = 10),0))+
  theme(legend.position='none')
e5

F4<-grid.arrange(e5, e4, nrow = 1)


#all figures togheter
F5<-grid.arrange(F1,F2,F3,F4, nrow= 2)
ggpubr::ggarrange(F1, F2, F3, F4, labels = c("A", "B", "C" ,"D"))

