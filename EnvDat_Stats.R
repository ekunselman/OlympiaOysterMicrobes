library(readr)
EnvDat<-read.csv("EnvData_2018_24hr.csv")
head(EnvDat)
str(EnvDat)
library(ggplot2)
library(ggpubr)
#plot
SD<-ggplot(EnvDat, aes(x=Site, y=DO)) +
  geom_boxplot()
ST<-ggplot(EnvDat, aes(x=Site, y=Temp)) +
  geom_boxplot()
SS<-ggplot(EnvDat, aes(x=Site, y=Sal)) +
  geom_boxplot()
HD<-ggplot(EnvDat, aes(x=Habitat, y=DO)) +
  geom_boxplot()
HT<-ggplot(EnvDat, aes(x=Habitat, y=Temp)) +
  geom_boxplot()
HS<-ggplot(EnvDat, aes(x=Habitat, y=Sal)) +
  geom_boxplot()
SHD<-ggplot(EnvDat, aes(x=Site_Habitat, y=DO)) +
  geom_boxplot()
SHT<-ggplot(EnvDat, aes(x=Site_Habitat, y=Temp)) +
  geom_boxplot()
SHS<-ggplot(EnvDat, aes(x=Site_Habitat, y=Sal)) +
  geom_boxplot()
ggarrange(SD, ST, SS, HD, HT, HS, SHD, SHT, SHS,
          ncol = 3, nrow = 3)

#Run two way anovas and post hoc tests
mixdAOV_DO<-aov(DO~Site*Habitat, data=EnvDat)
summary(mixdAOV_DO)
TukeyHSD(mixdAOV_DO)

mixdAOV_T<-aov(Temp~Site*Habitat, data=EnvDat)
summary(mixdAOV_T)
TukeyHSD(mixdAOV_T)

mixdAOV_Sal<-aov(Sal~Site*Habitat, data=EnvDat)
summary(mixdAOV_Sal)
TukeyHSD(mixdAOV_Sal)

#Calculate means
aggregate(EnvDat$Sal, list(EnvDat$Site_Habitat), FUN=mean)
Group.1        x
1     CI.Bare       NA
2 CI.Eelgrass       NA
3     FB.Bare 31.94068
4 FB.Eelgrass       NA
5     PG.Bare 32.57594
6 PG.Eelgrass 32.95038
7     SK.Bare 31.69843
8 SK.Eelgrass 30.83543
aggregate(EnvDat$DO, list(EnvDat$Site_Habitat), FUN=mean)
Group.1        x
1     CI.Bare 9.534646
2 CI.Eelgrass 8.993701
3     FB.Bare 7.206780
4 FB.Eelgrass 7.205932
5     PG.Bare 8.541353
6 PG.Eelgrass 8.737594
7     SK.Bare 2.739370
8 SK.Eelgrass 2.356693
aggregate(EnvDat$Temp, list(EnvDat$Site_Habitat), FUN=mean)
Group.1        x
1     CI.Bare 16.75354
2 CI.Eelgrass 16.68189
3     FB.Bare 14.44068
4 FB.Eelgrass 14.37712
5     PG.Bare 15.94511
6 PG.Eelgrass 15.92105
7     SK.Bare 11.45039
8 SK.Eelgrass 11.33937
