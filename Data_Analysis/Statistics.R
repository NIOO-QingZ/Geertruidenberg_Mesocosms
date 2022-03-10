require(dplyr)
require(nlme)
require(car)
require(lubridate)
library(nlme)
require(vegan)
library(multcompView)

#### Data preparation: ####
GEERTANOVA <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\StatisticAnalysis\\Data_Geertruidenberg_Qing.csv")

Datasheet_Geert_LdSD_QAQC <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\StatisticAnalysis\\Datasheet_Geert_LdSD_QAQC_Qing.csv", skip = 1, header = T, nrows = 157)
Datasheet_Geert_LdSD_QAQC$Datum <- Datasheet_Geert_LdSD_QAQC$Datum%>%dmy
Datasheet_Geert_LdSD_QAQC$Exp_day <- factor(Datasheet_Geert_LdSD_QAQC$Exp_day, levels = c("0","2","7","16","30","45","60","end_harvest"))
Datasheet_Geert_LdSD_QAQC$Treatment <- factor(Datasheet_Geert_LdSD_QAQC$Treatment, levels = c("outside cosm", "Control", "Ijzerkalkslib", "Phoslock", "Dredged", "Oxygenated"))

Control <- Datasheet_Geert_LdSD_QAQC[which(Datasheet_Geert_LdSD_QAQC$Cosm=="control"),]

Porewater <- Datasheet_Geert_LdSD_QAQC[which(Datasheet_Geert_LdSD_QAQC$Exp_day=="end_harvest"),]
Porewater <- Porewater[,!apply(is.na(Porewater),MARGIN = 2, any)]

Basic_Var <- c("Temp","DO.","DO","pH")
Light_Var <- c("Secchi.diepte","Turbiditeit","PAR","refPAR","light_k","light_perc_bottom","light_4perc")
Phyto_Var <- c("Cos.chla.bl.PAM","Cos.chla.gr.PAM","Cos.chla.br.PAM","Cos.yield.bl.PAM","Cos.yield.gr.PAM","Cos.yield.br.PAM","Cos.chla.bl.50CM","Cos.chla.gr.50CM","Cos.chla.br.50CM","Cos.yield.bl.50CM","Cos.yield.gr.50CM","Cos.yield.br.50CM")
Nut_Var <- c("disTON","disNH4","disPO4","disNO2","disNO3.mg.l","partC","partN","partP.mg.L","TP")

Ion_Var <- c("Al.ppm","Ca.ppm","Fe.ppm","La.ppm","Mg.ppm","Mn.ppm","P.ppm","S.ppm","As.ppm","Cd.ppm","Cu.ppm","Pb.ppm","Zn.ppm","As.microgram.L","As.ug.L","SpCond","SpCond.1","Sal","TDS")

Water_data <- Datasheet_Geert_LdSD_QAQC[-which(Datasheet_Geert_LdSD_QAQC$Datum=="2019-09-19"),]
Water_data <- Water_data[,-which(names(Water_data)%in%c("As.ppm","As.microgram.L"))]

Water_data <- Water_data[,apply(!is.na(Water_data),MARGIN = 2, any)]

Water_data$Tot.total <- Water_data$Cos.chla.bl.PAM+Water_data$Cos.chla.gr.PAM+Water_data$Cos.chla.br.PAM


#### Temperature ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp")]%>%na.omit
names(Data)[5] <- "Y_Value"
Response <- Data[,"Y_Value"]
Treatment <- factor(Data$Treatment)
Datum <- Data$Datum#%>%as.character()%>%as.numeric()
Subject <- Data$Cosm

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value))
)

aovmodel=aov(Response~Treatment*Datum+Error(Subject/Datum))
pairwise.t.test(Response, Treatment, p.adj = "bonf")%>%print
#capture.output(summary(aovmodel),file= "tOTcHL.doc")

pairwise.t.test(Response, Treatment, p.adj = "holm")%>%print

#### DO ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","DO")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(DO),
  std.er=sd(DO)/sqrt(length(DO)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(DO),
  std.er=sd(DO)/sqrt(length(DO)),
)

pairwise.t.test(Response, Treatment, p.adj = "bonf")

# Global lme:
lme_md <- lme(sqrt(DO)~sqrt(Exp_day), random = ~1|Cosm,data=Data)
shapiro.test(lme_md$residuals[,2])
anova(lme_md)
summary(lme_md)
lme_md$coefficients

### To evaluate heatwave effect
Heatwave_Data <- Data[which(Data$Exp_day>16&Data$Exp_day<=45),]
lme_HW <- lme(DO~Treatment+Temp,data = Heatwave_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients

1.186414/9.606751 # increased DO in Dredging 
sqrt((0.6157039/1.186414)^2+(0.4353684/9.606751)^2)*(1.186414/9.606751) # sd in increase of Dredging

0.973669/9.606751 # increased DO in LMB
sqrt((0.6157039/0.973669)^2+(0.4353684/9.606751)^2)*(0.973669/9.606751) # sd in increase of LMB

0.815930/9.606751 # increased DO in iron-lime sludge
sqrt((0.6157039/0.815930)^2+(0.4353684/9.606751)^2)*(0.815930/9.606751) # sd in increase of iron-lime sludge

0.247936/9.606751 # increased DO in aeration
sqrt((0.6157039/0.247936)^2+(0.4353684/9.606751)^2)*(0.247936/9.606751) # sd in increase of iron-lime sludge

1.852966/9.606751 # increased DO in outside cosm
sqrt((0.7025298/1.852966)^2+(0.4353684/9.606751)^2)*(1.852966/9.606751) # sd in increase of outside cosm

#### Secchi Depth ####

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","Secchi.diepte")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Secchi.diepte),
  std.er=sd(Secchi.diepte)/sqrt(length(Secchi.diepte))
)

# decrease in the last two samplings excluding canal water
Data.sum <- na.omit(Data[which(Data$Treatment!="outside cosm"),])%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Secchi.diepte),
  std.er=sd(Secchi.diepte)/sqrt(length(Secchi.diepte))
)

(130-102)/130 # mean
((sqrt(7.24^2+7.73^2)/(130-102))^2+(7.24/130)^2)*((130-102)/130)# se

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(Secchi.diepte),
  std.er=sd(Secchi.diepte)/sqrt(length(Secchi.diepte)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Secchi.diepte),
  std.er=sd(Secchi.diepte)/sqrt(length(Secchi.diepte)),
)%>%as.data.frame

# Global lme:
lme_md <- lme((Secchi.diepte)~Treatment+Exp_day,data = Data, random = ~1|Cosm)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(Secchi.diepte~Treatment+Temp,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients

#increase in dredging
33.25000/85.87500 # mean
sqrt((12.873820/33.25000)^2+(9.103165/85.87500)^2)*(33.25000/85.87500) # sd

#increase in Phoslock
16.41667/85.87500 # mean
sqrt((12.873820/16.41667)^2+(9.103165/85.87500)^2)*(16.41667/85.87500) # sd

#increase in Iron-lime sludge
9.54167/85.87500 # mean
sqrt((12.873820/9.54167)^2+(9.103165/85.87500)^2)*(9.54167/85.87500) # sd

#decrease in aeration
-11.12500/85.87500 # mean
sqrt((12.873820/-11.12500)^2+(9.103165/85.87500)^2)*(-11.12500/85.87500) # sd

#decrease in canal water
-28.95192/85.87500 # mean
sqrt((18.322892/-28.95192)^2+(9.103165/85.87500)^2)*(-28.95192/85.87500) # sd



#### pH ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","pH")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(pH),
  std.er=sd(pH)/sqrt(length(pH)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(pH),
  std.er=sd(pH)/sqrt(length(pH)),
)

mean(Data$pH)
sd(Data$pH)/sqrt(length(Data$pH))

# Global lme:
lme_md <- lme(pH~Treatment+Exp_day,data = Data, random = ~1|Cosm)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(pH~Treatment+Temp,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients

#increase in dredging
0.053194/6.904734 # mean
sqrt((0.0667764/0.053194)^2+(0.4821646/6.904734)^2)*(0.053194/6.904734) # sd

#decrease in Phoslock
-0.079894/6.904734 # mean
sqrt((0.0667643/-0.079894)^2+(0.4821646/6.904734)^2)*(-0.079894/6.904734) # sd

#decrease in Iron-lime sludge
-0.032863/6.904734 # mean
sqrt((0.0667655/-0.032863)^2+(0.4821646/6.904734)^2)*(-0.032863/6.904734) # sd

#increase in aeration
0.130227/6.904734 # mean
sqrt((0.0667655/0.130227)^2+(0.4821646/6.904734)^2)*(0.130227/6.904734) # sd

#increase in canal water
0.105901/6.904734 # mean
sqrt((0.0802269/0.105901)^2+(0.4821646/6.904734)^2)*(0.105901/6.904734) # sd

# effect of temperature:
0.105054/6.904734

# normality test on the residuals
par(mfrow=c(1,2))
plot(lme_md$residuals[,2])
hist(lme_md$residuals[,2])

#### dissolved PO4 ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","disPO4")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(disPO4),
  std.er=sd(disPO4)/sqrt(length(disPO4)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(disPO4),
  std.er=sd(disPO4)/sqrt(length(disPO4)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(disPO4),
  std.er=sd(disPO4)/sqrt(length(disPO4)),
)

# Estimate of P fluxes: 
# Control treatments:
(0.08875-0.01350)/(30-16)*1e3*2.3#g/m3/d
# LMB treatments:
(0.11575-0.01850)/(30-16)*1e3*2.3#g/m3/d

# lme:
lme_md <- lme(log(disPO4+1e-5)~Exp_day,data=Data, random = ~1|Cosm)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

Data$Canal <- NA
Data$Canal[which(Data$Treatment=="outside cosm")]<-"Canal"
Data$Canal[which(Data$Treatment!="outside cosm")]<-"Enclosures"
lme_canal <- lme(sqrt(disPO4)~Canal,data=Data, random = ~1|Cosm)
shapiro.test(lme_canal$residuals[,2])%>%print
anova(lme_canal)
summary(lm.canal)
lm.canal$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
HW.sum <- na.omit(HW_Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(disPO4),
  std.er=sd(disPO4)/sqrt(length(disPO4)),
)

lme_HW <- lme(log(disPO4*1e3)~Temp+Treatment,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients
mean(c(0.0222,0.0822,0.0484))-mean(c(0.00495,0.0222))

#### TP-Water ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","disPO4","partP.mg.L")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data$TP <- Data$disPO4*1e3+Data$partP.mg.L


Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(TP),
  std.er=sd(TP)/sqrt(length(TP)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(TP),
  std.er=sd(TP)/sqrt(length(TP)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(TP),
  std.er=sd(TP)/sqrt(length(TP)),
)

# lme:
lme_md <- lme(log(TP)~Exp_day+Treatment,data=Data, random = ~1|Cosm)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

Data$Canal <- NA
Data$Canal[which(Data$Treatment=="outside cosm")]<-"Canal"
Data$Canal[which(Data$Treatment!="outside cosm")]<-"Enclosures"
lme_canal <- lme(log(TP)~Canal,data=Data, random = ~1|Cosm)
shapiro.test(lme_canal$residuals[,2])%>%print
anova(lme_canal)
summary(lm.canal)
lm.canal$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(log(TP)~Temp+Treatment,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients


#### PO4-Porewater####
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","disPO4")]%>%na.omit

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(disPO4),
  std.er=sd(disPO4)/sqrt(length(disPO4)),
)

Data = Data[which(Data$Treatment!="outside cosm"),]
Response <- Data[,"disPO4"]
Treatment <- factor(Data$Treatment)
Subject <- Data$Cosm

# lme:
lme_md <- lme(Response~Treatment, random = ~1|Subject)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

#### La ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","La.ppm")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(La.ppm),
  std.er=sd(La.ppm)/sqrt(length(La.ppm)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(La.ppm),
  std.er=sd(La.ppm)/sqrt(length(La.ppm)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(La.ppm),
  std.er=sd(La.ppm)/sqrt(length(La.ppm)),
)

# lme:
lme_md <- lme(La.ppm~Exp_day+Treatment,data=Data, random = ~1|Cosm)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

Data$Canal <- NA
Data$Canal[which(Data$Treatment=="outside cosm")]<-"Canal"
Data$Canal[which(Data$Treatment!="outside cosm")]<-"Enclosures"
lme_canal <- lme(sqrt(La.ppm)~Canal,data=Data, random = ~1|Cosm)
shapiro.test(lme_canal$residuals[,2])%>%print
anova(lme_canal)
summary(lm.canal)
lm.canal$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(La.ppm~Temp+Treatment,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients


#### Total Chlorophylla ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","Tot.total","disPO4","disNH4")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

lme_md <- lme(Tot.total~Exp_day:Treatment+Treatment+disPO4+disNH4, random = ~1|Cosm,data = Data)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(log(Tot.total)~Treatment+Temp+disNH4,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients
# +disPO4 not significant

# temperature effect
2.97101/10.07733 # mean
sqrt((1.81158/2.97101)^2+(44.04541/10.07733)^2)*(2.97101/10.07733) # sd

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Tot.total),
  std.er=sd(Tot.total)/sqrt(length(Tot.total)),
)

# increase in the last two sampling excluding canal water
Data.sum <- na.omit(Data[which(Data$Treatment!="outside cosm"),])%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Tot.total),
  std.er=sd(Tot.total)/sqrt(length(Tot.total)),
)
(34.4-11.1)/34.4 # mean
((sqrt(5.04^2+3.96^2)/(34.4-11.1))^2+(5.04/34.4)^2)*((34.4-11.1)/34.4)# se

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(Tot.total),
  std.er=sd(Tot.total)/sqrt(length(Tot.total)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(Tot.total),
  std.er=sd(Tot.total)/sqrt(length(Tot.total)),
)%>%as.data.frame

# decrease in dredging treatments
(50.5250-18.9500)/50.5250 # mean
sqrt((sqrt(15.996277^2+7.111900^2)/(50.5250-18.9500))^2+(15.996277/50.5250)^2)*((50.5250-18.9500)/50.5250) # se

# decrease in Phoslock treatments
(50.5250-26.1525)/50.5250 # mean
sqrt((sqrt(15.996277^2+9.074858^2)/(50.5250-26.1525))^2+(15.996277/50.5250)^2)*((50.5250-26.1525)/50.5250) # se

#decrease in Iron-lime sludge treatments
(50.5250-27.1900)/50.5250 # mean
sqrt((sqrt(15.996277^2+11.079617^2)/(50.5250-27.1900))^2+(15.996277/50.5250)^2)*((50.5250-27.1900)/50.5250) # se

#decrease in aeration treatments
(50.5250-49.1150)/50.5250 # mean
sqrt((sqrt(15.996277^2+3.817871^2)/(50.5250-49.1150))^2+(15.996277/50.5250)^2)*((50.5250-49.1150)/50.5250) # se

# increase in outside cosm
(92.0750-50.5250)/50.5250 # mean
sqrt((sqrt(15.996277^2+1.838648^2)/(92.0750-50.5250))^2+(15.996277/50.5250)^2)*((92.0750-50.5250)/50.5250) # se


#### Bluealgae Chlorophylla ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","Cos.chla.bl.PAM","disPO4","disNH4")]%>%na.omit
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Data$Treatment <- factor(Data$Treatment, levels = c("Control","Dredged","Phoslock","Ijzerkalkslib","Oxygenated","outside cosm"))

lme_md <- lme((Cos.chla.bl.PAM)~Treatment+disPO4+disNH4+Exp_day, random = ~1|Cosm,data = Data)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)
summary(lme_md)
lme_md$coefficients

#### Heatwave effect 
HW_Data <- Data[which(Data$Exp_day>=16&Data$Exp_day<=45),]
lme_HW <- lme(sqrt(Cos.chla.bl.PAM)~Treatment+Temp+disNH4,data = HW_Data, random = ~1|Cosm)
shapiro.test(lme_HW$residuals[,2])%>%print
anova(lme_HW)
summary(lme_HW)
lme_HW$coefficients
# +disPO4 not significant

# temperature effect
2.97101/10.07733 # mean
sqrt((1.81158/2.97101)^2+(44.04541/10.07733)^2)*(2.97101/10.07733) # sd


# increase in the last two samplings excluding canal water
Data.sum <- na.omit(Data[which(Data$Treatment!="outside cosm"),])%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(Cos.chla.bl.PAM),
  std.er=sd(Cos.chla.bl.PAM)/sqrt(length(Cos.chla.bl.PAM)),
)%>%as.data.frame()

(7.8860000-2.0315000)/7.8860000 # mean
((sqrt(1.0113676^2+0.8353676^2)/(7.8860000-2.0315000))^2+(1.0113676/7.8860000)^2)*((7.8860000-2.0315000)/7.8860000)# se

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(Cos.chla.bl.PAM),
  std.er=sd(Cos.chla.bl.PAM)/sqrt(length(Cos.chla.bl.PAM)),
)%>%as.data.frame()

# decrease in dredging treatments
(11.442500-4.752500)/11.442500 # mean
sqrt((sqrt(3.0271119^2+1.6299303^2)/(11.442500-4.752500))^2+(3.0271119/11.442500)^2)*((11.442500-4.752500)/11.442500) # se

# decrease in Phoslock treatments
(11.442500-5.540000)/11.442500 # mean
sqrt((sqrt(3.0271119^2+1.4038934^2)/(11.442500-5.540000))^2+(3.0271119/11.442500)^2)*((11.442500-5.540000)/11.442500) # se

#decrease in Iron-lime sludge treatments
(11.442500-6.770000)/11.442500 # mean
sqrt((sqrt(3.0271119^2+2.0497032^2)/(11.442500-6.770000))^2+(3.0271119/11.442500)^2)*((11.442500-6.770000)/11.442500) # se

#decrease in aeration treatments
(11.442500-10.925000)/11.442500 # mean
sqrt((sqrt(3.0271119^2+1.2573882^2)/(11.442500-10.925000))^2+(3.0271119/11.442500)^2)*((11.442500-10.925000)/11.442500) # se

# increase in outside cosm
(20.917500-11.442500)/11.442500 # mean
sqrt((sqrt(3.0271119^2+0.4276169^2)/(20.917500-11.442500))^2+(3.0271119/11.442500)^2)*((20.917500-11.442500)/11.442500) # se


#### GHG data ####
GHG_boxplot <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\GHG\\Analyser_data\\GHG_CalculationResults.csv")
GHG_boxplot <- GHG_boxplot[,c("Cosm","Treatment","CO2_cal","CH4_cal","N2O_cal")]
#GHG_boxplot <- GHG_boxplot[which(GHG_boxplot$Treatment%in%c("Control","Aeration","Dredging","Phoslock","Iron-lime")),]
GHG_boxplot <- GHG_boxplot[-which(GHG_boxplot$Treatment=="air"),]
GHG_boxplot$Treatment <- factor(GHG_boxplot$Treatment,levels = c("outside","Control","Iron-lime","Phoslock","Dredging","Aeration"))
GHG_boxplot$CO2_cal[which(GHG_boxplot$CO2_cal<0)]=NA
GHG_boxplot$CH4_cal[which(GHG_boxplot$CH4_cal<0)]=NA
GHG_boxplot$N2O_cal[which(GHG_boxplot$N2O_cal<0)]=NA

CO2.sum <- na.omit(GHG_boxplot)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(CO2_cal),
  std.er=sd(CO2_cal)/sqrt(length(CO2_cal)),
)

GHG_Enclosure <- GHG_boxplot[which(GHG_boxplot$Treatment!="outside"),]

aov.CO2 <- aov(CO2_cal~Treatment,GHG_Enclosure)
lm.CO2 <- lm(CO2_cal~Treatment,na.omit(GHG_Enclosure))
summary(lm.CO2)

TukeyHSD(aov.CO2)
(0.000101-0.0000372)/0.0000372 # increase of CO2 in LMB than Control
(0.000101-0.0000110)/0.0000110 # than aeration


aov.CH4 <- aov(CH4_cal~Treatment, GHG_Enclosure)
TukeyHSD(aov.CH4)
CH4.sum <- na.omit(GHG_boxplot)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(CH4_cal),
  std.er=sd(CH4_cal)/sqrt(length(CH4_cal)),
)

aov.N2O <- aov(N2O_cal~Treatment, GHG_Enclosure)
TukeyHSD(aov.N2O)

N2O.sum <- na.omit(GHG_boxplot)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(N2O_cal),
  std.er=sd(N2O_cal)/sqrt(length(N2O_cal)),
)


## SI ##################################################
#### NH4 ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","disNH4")]%>%na.omit

Data.sum <- na.omit(Data)%>%group_by(Exp_day)%>%summarise(
  Data.mean = mean(disNH4),
  std.er=sd(disNH4)/sqrt(length(disNH4)),
)

Data.sum <- na.omit(Data)%>%group_by(Treatment)%>%summarise(
  Data.mean = mean(disNH4),
  std.er=sd(disNH4)/sqrt(length(disNH4)),
)

Data.sum <- na.omit(Data)%>%group_by(Exp_day, Treatment)%>%summarise(
  Data.mean = mean(disNH4),
  std.er=sd(disNH4)/sqrt(length(disNH4)),
)

Data = Data[which(Data$Treatment!="outside cosm"),]
Data$Exp_day<-Data$Exp_day%>%as.character()%>%as.numeric()
Response <- Data[,"disNH4"]
Treatment <- factor(Data$Treatment)
Temp <- Data$Temp
Datum <- Data$Exp_day%>%as.character()%>%as.numeric()
Subject <- Data$Cosm

# lme:
lme_md <- lme(Response~Treatment, random = ~1|Subject)
summary(lme_md)
shapiro.test(lme_md$residuals[,2])%>%print
anova(lme_md)

lme_md$coefficients

pairwise.t.test(Response, Treatment, p.adj = "bonf")
#capture.output(summary(aovmodel),file= "tOTcHL.doc")

pairwise.t.test(Response, Treatment, p.adj = "holm")%>%print
#capture.output(summary(aovmodel),file= "tOTcHLhom.doc")



# normality test on the residuals
par(mfrow=c(1,2))
plot(lme_md$residuals[,2])
hist(lme_md$residuals[,2])

aov.md <- aov(Cos.chla.bl.PAM~Treatment,data = Data)
TukeyHSD(aov.md,which = "Treatment")
