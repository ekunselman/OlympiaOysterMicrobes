library(readr)
EnvDat<-read.csv("EnvData_2018_24hr.csv")
head(EnvDat)
str(EnvDat)
library(ggplot2)
library(ggpubr)
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

mixdAOV_DO<-aov(DO~Site*Habitat, data=EnvDat)
summary(mixdAOV_DO)
TukeyHSD(mixdAOV_DO)

mixdAOV_T<-aov(Temp~Site*Habitat, data=EnvDat)
summary(mixdAOV_T)
TukeyHSD(mixdAOV_T)

mixdAOV_Sal<-aov(Sal~Site*Habitat, data=EnvDat)
summary(mixdAOV_Sal)
TukeyHSD(mixdAOV_Sal)
