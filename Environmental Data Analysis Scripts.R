# Environmental Data Analysis

#read in file
EnvDat<-read.csv("EnvData_2018_24hr.csv")

#load packages
library(tidyverse)
library(ggpubr)
library(rstatix)

#Group Data by site and habitat and calc summary statistics
EnvDat %>%
  group_by(Site, Habitat) %>%
  get_summary_stats(Sal, type = "mean_sd")
EnvDat %>%
  group_by(Site, Habitat) %>%
  get_summary_stats(Temp, type = "mean_sd")
EnvDat %>%
  group_by(Site, Habitat) %>%
  get_summary_stats(DO, type = "mean_sd")
  ##these means are used in microbiome analysis: linear regression model for qurro log ratios vs. env data

#Check Assumptions

##Outliers?
EnvDat %>%
  group_by(Site, Habitat) %>%
  identify_outliers(Sal)
###outliers found
EnvDat %>%
  group_by(Site, Habitat) %>%
  identify_outliers(Temp)
###outliers found
EnvDat %>%
  group_by(Site, Habitat) %>%
  identify_outliers(DO)
###outliers found

##Test for Normality

hist(EnvDat$Temp)
hist(EnvDat$DO)

EnvDat %>%
  group_by(Site, Habitat) %>%
  shapiro_test(Sal)
EnvDat %>%
  group_by(Site, Habitat) %>%
  shapiro_test(DO)
EnvDat %>%
  group_by(Site, Habitat) %>%
  shapiro_test(Temp) 
###data is not normal

#Visualize
ptemp<-ggline(EnvDat, x="Time", y="Temp", group = "Site_Habitat", color = "Site_Habitat")+
  theme(axis.text.x=element_blank())+
  theme(legend.position="none")+
  scale_colour_manual(values=c(PG.Bare="mediumpurple4",PG.Eelgrass="mediumpurple3",CI.Bare="sienna3",CI.Eelgrass="sienna1",SK.Bare="deeppink3",SK.Eelgrass="deeppink", FB.Bare="seagreen4", FB.Eelgrass="seagreen3"))
pDO<-ggline(EnvDat, x="Time", y="DO", group = "Site_Habitat", color = "Site_Habitat")+
  theme(axis.text.x=element_blank())+
  theme(legend.position="none")+
  scale_colour_manual(values=c(PG.Bare="mediumpurple4",PG.Eelgrass="mediumpurple3",CI.Bare="sienna3",CI.Eelgrass="sienna1",SK.Bare="deeppink3",SK.Eelgrass="deeppink", FB.Bare="seagreen4", FB.Eelgrass="seagreen3"))
ggarrange(ptemp, pDO)
ggsave("EnvDat_overtime.pdf", scale = 1)
#get legend for figure to add in Inkscape
ptempleg<-ggline(EnvDat, x="Time", y="Temp", group = "Site_Habitat", color = "Site_Habitat")+
  theme(axis.text.x=element_blank())+
  scale_colour_manual(values=c(PG.Bare="mediumpurple4",PG.Eelgrass="mediumpurple3",CI.Bare="sienna3",CI.Eelgrass="sienna1",SK.Bare="deeppink3",SK.Eelgrass="deeppink", FB.Bare="seagreen4", FB.Eelgrass="seagreen3"))
ptempleg
ggsave("EnvDat_legend.pdf", scale = 1)

# Run a Permutational ANOVA for repeated measures since the data are not normally distributed
library(permuco)

aovperm(DO ~ Site*Habitat + Error(Time/c(Site_Habitat)), data=EnvDat)

#data are unbalanced because of correction for tide height
#permutation             P(>F)
#Site                    0.0002
#Habitat                 0.3368
#Site:Habitat            0.4778

aovperm(Temp ~ Site*Habitat + Error(Time/c(Site_Habitat)), data=EnvDat)

#permutation             P(>F)
#Site                    0.0002
#Habitat                 0.5720
#Site:Habitat            0.9954

# due to permutational nature of these tests, p values vary slightly each time the test is run
