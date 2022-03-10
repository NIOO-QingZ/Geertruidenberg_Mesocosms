require(dplyr)
require(nlme)
require(car)
require(lubridate)
library(nlme)
require(vegan)
library(readr)
library(plotrix)

#### Data preparation: ####
GEERTANOVA <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\StatisticAnalysis\\Data_Geertruidenberg_Qing.csv")

Datasheet_Geert_LdSD_QAQC <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\StatisticAnalysis\\Datasheet_Geert_LdSD_QAQC_Qing.csv", skip = 1, header = T, nrows = 157)
Datasheet_Geert_LdSD_QAQC$Datum <- Datasheet_Geert_LdSD_QAQC$Datum%>%dmy
Datasheet_Geert_LdSD_QAQC$Exp_day <- factor(Datasheet_Geert_LdSD_QAQC$Exp_day, levels = c("0","2","7","16","30","45","60","end_harvest"))
Datasheet_Geert_LdSD_QAQC$Treatment <- factor(Datasheet_Geert_LdSD_QAQC$Treatment, levels = c("outside cosm", "Control", "Ijzerkalkslib", "Phoslock", "Oxygenated", "Dredged"))

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

Water_data$Exp_day <- Water_data$Exp_day%>%as.character()%>%as.numeric

#### Plotting relevant variables: ####
#Control: **green**; Bentonite: **blue**; LMB: **red**.  
#Correlation coefficients for control groups:**black**; for LMB: **red**.

Water_data$Exp_day <- as.numeric(as.character(Water_data$Exp_day))

nums <- unlist(lapply(Water_data, is.numeric))
pairs.dat <- Water_data[,nums]
pairs.dat <- pairs.dat[,c("Exp_day","Temp","DO","pH","light_k","disNH4","disPO4","disNO2","disNO3.mg.l","partP.mg.L","Cos.chla.bl.PAM","Cos.chla.gr.PAM","Cos.chla.br.PAM","Tot.total","Sal","PAR","Secchi.diepte","Turbiditeit")]
# "Mg.ppm","Cd.ppm","Cu.ppm","Pb.ppm","Zn.ppm","disTON","TP","SpCond.1","refPAR","As.ug.L","Al.ppm","Ca.ppm","Fe.ppm","La.ppm","Mn.ppm","P.ppm","S.ppm",
names(pairs.dat) <- c("days", "T", "DO", "pH","light k", "NH4-N", "SRP", "NO2-N","NO3-N","PP","Cyano.","Green.","Diat.","Tot.Chla","Salinity","PAR","Secc.dep","Turdibity")

Treatment <- Water_data$Treatment
my_cols <- 1:length(unique(Treatment))
my_pchs <- 1:length(unique(Treatment))

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  xy.na.rm = na.omit(data.frame(x=x,y=y))
  x = xy.na.rm[,"x"]
  y = xy.na.rm[,"y"]
  r.C = ifelse(is.numeric(x)&is.numeric(y),round(cor(x, y), digits=2),NA)
  
  text(0.5, 0.8, r.C, cex = ifelse(is.na(r.C),0,log(abs(r.C)+exp(1)))*1.5,col=ifelse(abs(r.C)>0.3,"red","black"))
}

# Customize lower panel
lower.panel<-function(x, y){
  points(x,y,pch=my_pchs[Treatment],col=my_cols[Treatment])
}

# Customize diagonal panel:
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Create the plots
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Matrix.jpg",width = 32,height = 32, units = "cm", res = 400)
pairs(pairs.dat, 
      lower.panel = lower.panel,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      cex.labels = 1)
dev.off()

#### Temperature dynamcis ####
Temp_Gilze <- read_csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data/GHG/Temp_Gilze.csv",col_types = cols(YYYYMMDD = col_date(format = "%Y%m%d")))
WT_Flake <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\WaterTemperature_FLake\\flake\\flake\\Geert19_Output.csv")
Temp_Gilze$Exp_day <- as.numeric(Temp_Gilze$YYYYMMDD)-as.numeric(ymd("2019-06-25"))

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\Figure1\\Temperature.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
#1:5
pch <- 1:5
lty <- 1:5
lwd <- 2

ylim <- range(c(Temp_Gilze$TX[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)]*.1,rev(Temp_Gilze$TG[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)]*.1)))



plot(Data.sum$Exp_day,Data.sum$Data.mean,type="n",xlab="",ylab = "",xaxt="n",las=1,xlim=c(0,60),ylim=ylim)
mtext("Temperature [?C]",side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

abline(v=c(16,45), lty=2, col="black")
#text(30, 26, "Heatwave",col = "orange",cex=1.5)



# Add rectangle during heatwave
x = c(0:60, 60:0)
y = c(Temp_Gilze$TX[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)]*.1,rev(Temp_Gilze$TG[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)]*.1))

col_AT <- rgb(204/255,121/255,167/255)
polygon(x,y, border=NA, col=rgb(204/255,121/255,167/255,alpha = 0.5))
#lines(Temp_Gilze$Exp_day[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)], Temp_Gilze$TG[which(Temp_Gilze$Exp_day>=0&Temp_Gilze$Exp_day<=60)]*.1, col=rgb(204/255,121/255,167/255,alpha = 1),lwd=2)

WT_Flake$Exp_day <- as.numeric(WT_Flake$Time%>%dmy)-as.numeric(ymd("2019-06-25"))
col_WT<-rgb(0/255,114/255,178/255)
x_WT = c(0:60, 60:0)
y_WT <- c(WT_Flake$maxT[1:61], rev(WT_Flake$meanT[1:61]))
polygon(x_WT,y_WT, border=NA, col=rgb(0/255,114/255,178/255,alpha = 0.5))

#lines(WT_Flake$Exp_day[which(WT_Flake$Exp_day>=0&WT_Flake$Exp_day<=60)], WT_Flake$Tm[which(WT_Flake$Exp_day>=0&WT_Flake$Exp_day<=60)], col=col_WT,lwd=2)
#lines(WT_Flake$Exp_day, WT_Flake$Tb, col="purple")

legend("topright", legend = c("Simulated water temp.","Measured air temp.","Canal water temp.","Control mesocosm","Iron-lime mesocosm","LMB mesocosm","Aeration mesocosm","Dredging mesocosm"), col = c(col_WT,col_AT,"grey",col),lty=c(1,1,1,lty),pch = c(NA,NA,NA,pch),text.col =c(col_WT,col_AT,"grey",col),cex = .8,lwd = c(10,10,10,2,2,2,2,2),bty="n")

dev.off()



#### DO concentration ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\DO_Conc.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","DO")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("DO [mg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = c(15,2,2,2,2,2),bty="n")

#text(30, 14, "Heatwave",col = "orange",cex=1.5)

dev.off()



#### DO Saturation####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\DO_Saturation.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","DO.")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("DO [%]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = c(15,2,2,2,2,2),bty="n")


#text(30, 14, "Heatwave",col = "orange",cex=1.5)
dev.off()
####
#### pH concentration ####
#jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\pH.jpg",width = 16,height = 12, units = "cm", res = 300)
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\SubmissionToSTOTEN\\Resubmission\\FiguresCheck\\pH.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","pH")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext("pH",side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 9.7, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Secchi Depth ####
#jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\SecchiDepth.jpg",width = 16,height = 12, units = "cm", res = 300)
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\SubmissionToSTOTEN\\Resubmission\\FiguresCheck\\SecchiDepth.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Secchi.diepte")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,200),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext("Secchi Depth [cm]",side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 190, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Turbidity ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Turbidity.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Turbiditeit")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext("Turbidity [NTU]",side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 21, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Light extinction ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\light_k.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","light_k")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("light extinction [ ",m^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 21, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### SRP ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\SRP.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","disPO4")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1e3
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,250),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("SRP [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = c(15,2,2,2,2,2),bty="n")

dev.off()

#### Particulate P ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\PP.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","partP.mg.L")]%>%na.omit
names(Data)[5] <- "Y_Value"
#Data$Y_Value <- Data$Y_Value*1e3
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("PP [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 200, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Tot P (SRP + particular P) in water ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\TP_Water.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","disPO4","partP.mg.L")]%>%na.omit
Data$Y_Value <- Data$disPO4*1e3 + Data$partP.mg.L

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,350),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("TP [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

#text(30, 330, "Heatwave",col = "orange",cex=1.5)


dev.off()

#### NH4-N ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NH4_N.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","disNH4")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,1.8),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste(NH[4],"-N [mg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 1.7, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### NO2-N ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NO2_N.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","disNO2")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,0.7),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste(NO[2],"-N [mg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 0.65, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### NO3-N ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NO3_N.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","disNO3.mg.l")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)#*1.96

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste(NO[3],"-N [mg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 4, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Bluealgae - Draft ####

#jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Bluea.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Cos.chla.bl.PAM")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Exp_day <- Data$Exp_day%>%as.character()%>%as.numeric

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Cyano- Chlorophyll-a [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 26, "Heatwave",col = "orange",cex=1.5)

#dev.off()



#### Bluealgae- Final ####
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Cos.chla.bl.PAM")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Exp_day <- Data$Exp_day%>%as.character()%>%as.numeric
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Bluea.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))

ylim=c(0,22.2)

gap.plot(Data$Exp_day[which(Data$Cosm==1)],Data$Y_Value[which(Data$Cosm==1)],type="p",
         xtics=Data.sum$Exp_day%>%unique(),gap = c(25,40),
         col=col[5],pch=18,cex=1.5,
         xticlab=rep("",7),ytics = c(seq(0,25,5),42),yticlab = c(seq(0,25,5),42),ylab="",xlab="",ylim=c(0,43))

abline(h=seq(24.9,25.7,.001), col="white")  # hiding vertical lines
axis.break(2,25.2,style="slash")               # plotting slashes for breakpoints
axis.break(4,25.3,style="slash") 

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

mtext(expression(paste("Cyano- Chlorophyll-a [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data[which(Data$Cosm!=1),])%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
points(Data$Exp_day[which(Data$Cosm==1)],Data$Y_Value[which(Data$Cosm==1)],col=col[5],pch=18,cex=1.5)
# Visualization 
abline(v=c(16,45), lty=2, col="black")
for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 26, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Greenalgae ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Greena.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Cos.chla.gr.PAM")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)#*1.96

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,60),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Greenalgae Chlorophyll-a [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 55, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Diatom ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Diatom.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Cos.chla.br.PAM")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Diatom Chlorophyll-a [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 50, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Total Chloraphyll ####

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\TotChla.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Tot.total")]%>%na.omit
names(Data)[5] <- "Y_Value"

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,Data.sum$Data.mean-Data.sum$std.er),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(0,105),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Total Chlorophyll-a [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topleft", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = c(15,2,2,2,2,2),bty="n")


#text(30, 95, "Heatwave",col = "orange",cex=1.5)

dev.off()
#### Al ####

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Al.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1000
Data_noLMB = Data[which(Data$Treatment!="Phoslock"),]
Data_LMB = Data[which(Data$Treatment=="Phoslock"),]
plot(Data_LMB$Y_Value~Data_LMB$Cosm)
Data_Omit19 <- Data[which(Data$Cosm!=19),]
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Al.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))

gap.plot(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],gap = c(20,46),
         ylim=c(0,57),type="p",col=col[3],pch=3,cex=1.5,
         xtics=Data.sum$Exp_day%>%unique(),xticlab=rep("",6),ytics = c(seq(0,20,5),seq(45,60,5)),yticlab = c(seq(0,20,5),seq(40,60,5)),ylab="",xlab="")
abline(h=seq(19.95,20.75,.001), col="white")  # hiding vertical lines
axis.break(2,20.3,style="slash")               # plotting slashes for breakpoints
axis.break(4,20.3,style="slash") 

mtext(expression(paste("Al [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data_Omit19)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")
gap.plot(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],gap = c(20,46),type="p",col=col[3],pch=3,cex=1.5,add = T)

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 15, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### La ####

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","La.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1000
Data_noLMB = Data[which(Data$Treatment!="Phoslock"),]
Data_LMB = Data[which(Data$Treatment=="Phoslock"),]
plot(Data_LMB$Y_Value~Data_LMB$Cosm)
Data_Omit19 <- Data[which(Data$Cosm!=19),]
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\La.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data_Omit19)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,ifelse((Data.sum$Data.mean-Data.sum$std.er)>=0,(Data.sum$Data.mean-Data.sum$std.er),0)),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

gap.plot(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],gap = c(5,20),
         ylim=c(0,22.2),type="p",col=col[3],pch=3,cex=1.5,
         xtics=Data.sum$Exp_day%>%unique(),xticlab=rep("",6),ytics = c(0:5,21:22),yticlab = c(0:5,20:21),ylab="",xlab="")
abline(h=seq(4.95,5.15,.001), col="white")  # hiding vertical lines
axis.break(2,5.05,style="slash")               # plotting slashes for breakpoints
axis.break(4,5,style="slash") 

mtext(expression(paste("La [",mu,"g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

abline(v=c(16,45), lty=2, col="black")
gap.plot(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],gap = c(5,20),
         ylim=c(0,22.2),type="p",col=col[3],pch=3,cex=1.5,lwd=2,
         xtics=Data.sum$Exp_day%>%unique(),xticlab=rep("",6),ytics = c(0:5,21:22),yticlab = c(0:5,20:21),ylab="",xlab="",add = T)

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

dev.off()
#### Fe ####

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Fe.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1000
Data_noLMB = Data[which(Data$Treatment!="Phoslock"),]
Data_LMB = Data[which(Data$Treatment=="Phoslock"),]
plot(Data_LMB$Y_Value~Data_LMB$Cosm)
Data_Omit19 <- Data[which(Data$Cosm!=19),]
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Fe.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data_Omit19)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,ifelse((Data.sum$Data.mean-Data.sum$std.er)>=0,(Data.sum$Data.mean-Data.sum$std.er),0)),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Fe [", mu, "g  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}
points(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],col=col[3],pch=3,cex=1.5)

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 15, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### P ####

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","P.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1000
Data_noLMB = Data[which(Data$Treatment!="Phoslock"),]
Data_LMB = Data[which(Data$Treatment=="Phoslock"),]
plot(Data_LMB$Y_Value~Data_LMB$Cosm)
Data_Omit19 <- Data[which(Data$Cosm!=19),]
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\P.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data_Omit19)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,ifelse((Data.sum$Data.mean-Data.sum$std.er)>=0,(Data.sum$Data.mean-Data.sum$std.er),0)),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,400),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("P [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}
points(Data$Exp_day[which(Data$Cosm==19)],Data$Y_Value[which(Data$Cosm==19)],col="green",pch=15,cex=1)

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, 15, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### Ca ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Ca.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(4,4,1,1))
Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Ca.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value

M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- min(c(Data.sum$Data.mean,ifelse((Data.sum$Data.mean-Data.sum$std.er)>=0,(Data.sum$Data.mean-Data.sum$std.er),0)),na.rm = T)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,ylim.max),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Ca [mg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")
abline(v=c(16,45), lty=2, col="black")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,ifelse((box.dat$Data.mean-box.dat$std.er)>=0,(box.dat$Data.mean-box.dat$std.er),0),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")


#text(30, 250, "Heatwave",col = "orange",cex=1.5)

dev.off()

#### (Fe-S)/P ####
#NearZero = 1e-28

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Fe_S__P.jpg",width = 16,height = 12, units = "cm", res = 300)

Data = Water_data[,c("Exp_day", "Datum", "Cosm", "Treatment","Fe.ppm","S.ppm","P.ppm")]%>%na.omit
Data$Y_Value <- (Data$Fe.ppm-Data$S.ppm)/(Data$P.ppm)

par(mar=c(4,4,1,1))
M_name <- c("Jan","Feb","March","April","May","June","July","Aug.")
xlab=format(as.Date(unique(Data$Datum)), "%m-%d")
xlab=paste(M_name[month(unique(Data$Datum))],"-",day(unique(Data$Datum)),sep="")

# Visualization 
# Averaging values over new groups (named column "treat") 
Data.sum <- na.omit(Data)%>%group_by(Exp_day,Treatment)%>%summarise(
  Data.mean = mean(Y_Value),
  std.er=sd(Y_Value)/sqrt(length(Y_Value)),
)

treat<-unique(Data.sum$Treatment[-which(Data.sum$Treatment=="outside cosm")]%>%as.character())
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
pch <- 1:length(unique(Data.sum$Treatment))
lty <- 1:length(unique(Data.sum$Treatment))
lwd <- 2

ylim.min <- quantile(Data.sum$Data.mean,0.2)
ylim.max <- max(c(Data.sum$Data.mean,Data.sum$Data.mean+Data.sum$std.er),na.rm = T)

plot(Data.sum$Exp_day,Data.sum$Data.mean,ylim = c(ylim.min,-100),type="n",xlab="",ylab = "",xaxt="n",las=1)
mtext(expression(paste("Fe-P/S [µg  ",L^{-1},"]")),side = 2,line = 2.5,cex=1)
axis(side = 1,at=Data.sum$Exp_day%>%unique(),labels = xlab,las=2)

# Add rectangle during heatwave
x = c(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")], rev(Data.sum$Exp_day[which(Data.sum$Treatment=="outside cosm")]))
y = c(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")],rep(ylim.min,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))

polygon(x,y, border=NA, col="grey")

for (i in 1:length(treat)) {
  box.dat=Data.sum[which(Data.sum$Treatment==treat[i]),]
  lines(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],lty=lty[i],lwd=lwd)
  points(box.dat$Exp_day+(-length(treat)/2+i)*0.5,box.dat$Data.mean,col=col[i],pch=pch[i],cex=1)
  arrows(box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean-box.dat$std.er),box.dat$Exp_day+(-length(treat)/2+i)*0.5,(box.dat$Data.mean+box.dat$std.er),code=3,angle = 90,length=.05,col=col[i])
}

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

abline(v=c(16,45), lty=2, col="black")
#text(30, -100, "Heatwave",col = "orange",cex=1.5)

dev.off()


#### Pore water SRP ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\SRP_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","disPO4")]%>%na.omit
names(Data)[5] <- "Y_Value"
Data$Y_Value <- Data$Y_Value*1e3

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)
summary(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste("Porewater SRP [ ",mu,"g  ",L^{-1},"]")), col=c("grey",col), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = .8,bty="n")

dev.off()

#### Pore water NH4-N####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NH4_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","disNH4")]%>%na.omit
names(Data)[5] <- "Y_Value"

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste(NH[4],"-N [mg  ",L^{-1},"]")), col=c("grey",col), xlab = "",las=2,xaxt="n")

legend("topleft", legend = c("Canal","Control","Iron-lime","LMB","aeration","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = .8,bty="n")

dev.off()
#### Pore water NO2-N####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NO2_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","disNO2")]%>%na.omit
names(Data)[5] <- "Y_Value"
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste(NO[2],"-N [mg  ",L^{-1},"]")), col=c("grey",col), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = 1,bty="n")

dev.off()


#### Pore water NO3-N####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\NO3_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","disNO3.mg.l")]%>%na.omit
names(Data)[5] <- "Y_Value"

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste(NO[3],"-N [mg  ",L^{-1},"]")), col=c("grey",col), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = 1,bty="n")

dev.off()


#### Pore water Ca ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Ca_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Ca.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste("Ca [mg  ",L^{-1},"]")), col=c("grey",1:6), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("Canal","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",col),lty=c(1,lty),text.col =c("grey",col),cex = 1,lwd = lwd,bty="n")

dev.off()


#### Pore water Fe ####

col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))

Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Fe.ppm")]%>%na.omit
names(Data)[5] <- "Y_Value"
plot(Data$Y_Value~Data$Cosm)
aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)


jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Fe_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))

boxplot(Y_Value~Treatment,data = Data,ylab = expression(paste("Fe [mg  ",L^{-1},"]")), col=c("grey",col), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("Canal","Control","Iron-lime Sludge","LMB","Aeration","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = 1,bty="n")

dev.off()

#### Pore water water (Fe-S)/P ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\Fe_S__P_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Fe.ppm","S.ppm","P.ppm")]%>%na.omit
Data$Y_Value <- (Porewater$Fe.ppm-Porewater$S.ppm)/Porewater$P.ppm
Data$Y_Value[which(Data$Y_Value<(-40))]<-NA

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = "(Fe-S) / P", col=c("grey",1:6), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("outside enclosure","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",col),lty=c(1,lty),text.col =c("grey",col),cex = 1,lwd = lwd,bty="n")

dev.off()


#### Pore water (Fe+Mn-S)/P ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\FeMn_S__P_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Fe.ppm","S.ppm","P.ppm","Mn.ppm")]%>%na.omit
Data$Y_Value <- (Porewater$Fe.ppm+Porewater$Mn.ppm-Porewater$S.ppm)/Porewater$P.ppm
Data$Y_Value[which(Data$Y_Value<(-40))]<-NA

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = "(Fe+Mn-S) / P", col=c("grey",1:6), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("outside enclosure","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",col),lty=c(1,lty),text.col =c("grey",col),cex = 1,lwd = lwd,bty="n")

dev.off()


#### Pore water (Ca+Al)/P ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\CaPlusAl__P_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Ca.ppm","Al.ppm","P.ppm")]%>%na.omit
Data$Y_Value <- (Porewater$Ca.ppm+Porewater$Al.ppm)/Porewater$P.ppm
Data$Y_Value[which(Data$Y_Value>(200))]<-NA

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = "(Ca+Al) / P", col=c("grey",1:6), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("outside enclosure","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",col),lty=c(1,lty),text.col =c("grey",col),cex = 1,lwd = lwd,bty="n")

dev.off()



#### Pore water (Ca+Al-S)/P ####
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\CaPlusAl_S__P_Sed.jpg",width = 16,height = 12, units = "cm", res = 300)
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(86/255,180/255,233/255),rgb(0/255,158/255,115/255))
par(mar=c(2,5,1,1))
Data = Porewater[,c("Exp_day", "Datum", "Cosm", "Treatment","Ca.ppm","Al.ppm","P.ppm","S.ppm")]%>%na.omit
Data$Y_Value <- (Porewater$Ca.ppm+Porewater$Al.ppm-Porewater$S.ppm)/Porewater$P.ppm
Data$Y_Value[which(Data$Y_Value>(200))]<-NA

aov.model <- aov(Y_Value~Treatment, data = Data)
aov(aov.model)

boxplot(Y_Value~Treatment,data = Data,ylab = "(Ca+Al-S) / P", col=c("grey",1:6), xlab = "",las=2,xaxt="n")

legend("topright", legend = c("outside enclosure","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",col),lty=c(1,lty),text.col =c("grey",col),cex = 1,lwd = lwd,bty="n")

dev.off()

#### sediment P fractions ####
P_Fraction <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\PFraction.csv")
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\SedimentPFraction.jpg",width = 16,height = 12, units = "cm", res = 300)
names(P_Fraction) = c("Fraction","Sed_P_Conc")
P_Fraction$Fraction <- factor(P_Fraction$Fraction,levels = c("Porewater-P","Fe/Mn-P","Organic-P","Al/Ca-P","Refractory-P"))
par(mar=c(6,5,1,1))
aov.model <- aov(Sed_P_Conc~Fraction, data = P_Fraction)
aov(aov.model)
summary(aov.model)

boxplot(Sed_P_Conc~Fraction, data = P_Fraction,ylab = expression(paste("Sediment P fractions [",mu,"g  P/g Sed]")), col="grey", xlab = "",las=2)

dev.off()




#### PRC *1.2. Scenario 2: Tot- Chla; Cyano- Chla*####
## my.prc.function
my.prc<-  function (response, treatment, time, ...)
{
  extras <- match.call(expand.dots = FALSE)$...
  if (is.null(extras$data))
    data <- parent.frame()
  else
    data <- eval(extras$data)
  y <- deparse(substitute(response))
  x <- deparse(substitute(treatment))
  z <- deparse(substitute(time))
  oldcon <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(oldcon))
  fla <- as.formula(paste("~", x, "+", z))
  mf <- model.frame(fla, data, na.action = na.pass)
  if (!all(sapply(mf, is.factor)))
    stop(gettextf("%s and %s must be factors", x, z))
  if (any(sapply(mf, is.ordered)))
    stop(gettextf("%s or %s cannot be ordered factors", x, z))
  fla.zx <- as.formula(paste("~", z, ":", x))
  fla.z <- as.formula(paste("~", z))
  # delete first (control) level from the design matrix
  X = model.matrix(fla.zx, mf)[,-c(seq_len(nlevels(time)+1))]
  Z = model.matrix(fla.z, mf)[,-1]
  mod <- rda(response ~ X + Condition(Z), ...)
  mod$terminfo$xlev = list(levels(time), levels(treatment))
  names(mod$terminfo$xlev) = c(paste(z), paste(x))
  mod$call <- match.call()
  class(mod) <- c("prc", class(mod))
  mod
}
# ploting prc

.checkSelect <- function(select, scores) {
  if(is.logical(select) &&
     !isTRUE(all.equal(length(select), NROW(scores)))) {
    warning("length of 'select' does not match the number of scores: ignoring 'select'")
  } else {
    scores <- if(is.matrix(scores)) {
      scores[select, , drop = FALSE]
    } else {
      scores[select]
    }
  }
  scores
}
my.plot.prc <-
  function (x, species = TRUE, select, scaling = "symmetric", axis = 1, correlation = FALSE, const, type, xlab, ylab, ylim,lwd, lty = 1:5, col, pch, legpos, cex = 1.2,font=2, ...){
    ## save level names before getting the summary
    levs <- x$terminfo$xlev[[2]]
    x <- summary(x, axis = axis, scaling = scaling, const,
                 correlation = correlation)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    b <- t(coef(x))
    xax <- rownames(b)
    if (missing(xlab))
      xlab <- x$names[1]
    if (missing(ylab))
      ylab <- expression(paste(bold(italic(C[dt])),"  ",italic(bold(b[k]))))
    if (missing(type))
      type <- "l"
    if (!missing(select))
      x$sp <- .checkSelect(select, x$sp)
    if (missing(ylim))
      ylim <- if (species)
        range(b, na.rm = TRUE)
    else range(b, na.rm = TRUE)
    if (missing(col))
      col=1:6
    if (species) {
      op <- par("mai")
      mrg <- max(strwidth(names(x$sp), cex = cex, units = "in")) + strwidth("mmm", cex = cex, units = "in")
      par(mai = c(op[1:3], max(op[4], mrg)))
    }
    if (missing(pch))
      pch <- as.character(1:nrow(b))
    par(omd=c(0,0.9,0,1))
    
    matplot(xax, b, type = type, xlab = xlab, ylab = ylab, ylim = ylim, cex = cex, lty = lty, col = col, pch = pch, lwd = lwd,font=font,xaxt="n",...)
    #########################################################
    # Add rectangle during heatwave
    x.canal = c(2,7,16,30,45,60,60,45,30,16,7,2)
    y.canal = c(y_canal,rep(-1,6))
    #,rep(0,length(Data.sum$Data.mean[which(Data.sum$Treatment=="outside cosm")])))
    
    polygon(x.canal,y.canal, border=NA, col=rgb(128/253,128/253,128/253,alpha = 0.5))
    #########################################################
    abline(v=c(16,45), lty=2, col="black")
    abline(h = 0, col = "black",lwd=4)
    matplot(xax, b, type = "o", xlab = xlab, ylab = ylab, ylim = ylim, cex = cex, lty = lty, col = col, pch = pch, lwd = lwd,font=font,add = T,xaxt="n", ...)
    axis(side = 1,at=c(0,2,7,16,30,45,60),labels = c("06-25", "06-27", "07-03", "07-12", "07-24", "08-07", "08-29"),las=2)
    
    if (species) {
      par(new=T,omd=c(0,1,0,1))
      plot.new()
      linestack(x$sp, at = par("usr")[2],hoff=0.5,axis = T, cex=1.2)
    }
    if (missing(legpos)) {
      holes <- abs(0 - ylim)
      if (holes[1] > holes[2]){legpos <- "bottomleft"}else{legpos <- "topleft"}
    }
    if (!is.na(legpos)) {
      nl <- length(levs)
      pp <- type %in% c("b", "p")
      pl <- type %in% c("b", "l")
      if (length(lty) == 1)
        lty <- rep(lty, nl-1)
      legend("topleft", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey", "#000000", "#D55E00", "#E69F00", "#56B4E9", "#009E73"),
             lty = if (pl) lty[c(1,1:(nl-1))],
             pch = c(NA,NA,2:5), cex = cex, 
             title =NULL,lwd = c(15,4,4,4,4,4),
             text.font=font,text.col =c("grey", "#000000", "#D55E00", "#E69F00", "#56B4E9", "#009E73"),bty="n")
    }
    #invisible()
  }


PRC.data <- Datasheet_Geert_LdSD_QAQC[,c("Exp_day", "Datum", "Cosm", "Treatment","Temp","Cos.chla.bl.PAM","Cos.chla.gr.PAM","Cos.chla.br.PAM","DO","Secchi.diepte","pH","disPO4","partP.mg.L","disNH4","disNO2","disNO3.mg.l")]%>%na.omit
xlabels=format(as.Date(unique(PRC.data$Datum)), "%m-%d")
axis.at <- unique(PRC.data$Exp_day)
PRC.data$`Total Chla`=PRC.data$Cos.chla.bl.PAM+PRC.data$Cos.chla.br.PAM+PRC.data$Cos.chla.gr.PAM
PRC.data$TP <- PRC.data$disPO4*1e3 + PRC.data$partP.mg.L

Time <- factor(PRC.data$Exp_day,levels = c("0","2","7","16","30","45","60"))
Treatment <- as.character(PRC.data$Treatment)
Treatment[which(Treatment=="Ijzerkalkslib")]<-"Iron-lime Sludge"
Treatment[which(Treatment=="Oxygenated")]<-"Aeration"
Treatment[which(Treatment=="outside cosm")]<-"Outside cosm"

Treatment <- factor(Treatment,levels = c("Control","Outside cosm","Iron-lime Sludge","Phoslock","Aeration","Dredged"))

#Response <- PRC.data[,c("Total Chla","Cos.chla.bl.PAM","DO","Secchi.diepte","pH","disPO4","disNH4","disNO2","disNO3.mg.l")]%>%scale
#colnames(Response)<-c("Total Chla","Bluealgae","DO","SecchiDepth","pH","SRP","NH4-N","NO2-N","NO3-N")

Response <- PRC.data[,c("DO","Secchi.diepte","pH","disPO4","TP","disNH4","disNO2","disNO3.mg.l","Total Chla","Cos.chla.bl.PAM")]%>%scale
colnames(Response)<-c("DO","SecchiDepth","pH","SRP","TP","NH4-N","NO2-N","NO3-N","Total-Chla", "Cyano-Chla")

PRC.mod <- my.prc(response = Response, treatment = Treatment, time = Time)

summary(PRC.mod)
y_canal <- na.omit(summary(PRC.mod)$coefficients[1,])%>%as.numeric()
#jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\Data_Analysis_Graphs\\PRC.jpg",width = 32,height = 18, units = "cm", res = 300)
jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\PRC.jpg",width = 32,height = 18, units = "cm", res = 300)

par(mar=c(4,5,1,1))

my.plot.prc(PRC.mod, col = c("grey", "#D55E00", "#E69F00", "#56B4E9", "#009E73"), lty = 1:6, cex=1.2, lwd=c(4,4,4,4,4,4),xlab="",pch=c(NA,2:6),ylim=c(-0.3,0.22))

#mtext("Heatwave", side = 3, line=0,col = "orange",cex=1.5)
dev.off()


#### GHG data ####
GHG_boxplot <- read.csv("C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Data\\GHG\\Analyser_data\\GHG_CalculationResults.csv")
GHG_boxplot <- GHG_boxplot[,c("Treatment","CO2_cal","CH4_cal","N2O_cal")]
#GHG_boxplot <- GHG_boxplot[which(GHG_boxplot$Treatment%in%c("Control","Aeration","Dredging","Phoslock","Iron-lime")),]
GHG_boxplot <- GHG_boxplot[-which(GHG_boxplot$Treatment=="air"),]
GHG_boxplot$Treatment <- factor(GHG_boxplot$Treatment,levels = c("outside","Control","Iron-lime","Phoslock","Aeration","Dredging"))
GHG_boxplot$CO2_cal[which(GHG_boxplot$CO2_cal<0)]=NA
GHG_boxplot$CH4_cal[which(GHG_boxplot$CH4_cal<0)]=NA
GHG_boxplot$N2O_cal[which(GHG_boxplot$N2O_cal<0)]=NA
GHG_boxplot <- GHG_boxplot[which(GHG_boxplot$Treatment!="Aeration"),]
GHG_boxplot$Treatment <- factor(GHG_boxplot$Treatment,levels = c("outside","Control","Iron-lime","Phoslock","Dredging"))

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\SubmissionToSTOTEN\\Resubmission\\FiguresCheck\\CO2_cal.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
col <- c(rgb(0,0,0),rgb(213/255,94/255,0/255),rgb(230/255,159/255,0/255),rgb(0/255,158/255,115/255))


boxplot(CO2_cal*1e6~Treatment,data = GHG_boxplot,ylab = "", col=c("grey",col), xlab = "",las=2,xaxt="n")
mtext(expression(paste("C",O[2]," [",mu,"mol/L]")),side = 2,line = 2.5,cex=1)

legend("topleft", legend = c("Canal","Control","Iron-lime","LMB","Dredging"), col = c("grey",col),text.col =c("grey",col),cex = 1,bty="n")

dev.off()


jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\SubmissionToSTOTEN\\Resubmission\\FiguresCheck\\CH4_cal.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
boxplot(CH4_cal*1e9~Treatment,data = GHG_boxplot,ylab = "", col=c("grey",col), xlab = "",las=2,xaxt="n")
mtext(expression(paste("C",H[4]," [nmol/L]")),side = 2,line = 2.5,cex=1)

#legend("topright", legend = c("outside","Control","Iron-lime Sludge","Phoslock","Dredging","Artificial mixing"), col = c("grey",1:6),lty=2,text.col =c("grey",col),cex = 1,lwd = 1,bty="n")

dev.off()

jpeg(filename = "C:\\Users\\QingZ\\PhD-Qing\\GitProject\\Enclosure_Geertruidenberg\\Docs\\FirstDraft\\Pre-finalVersion\\FinalVersion\\SubmissionToSTOTEN\\Resubmission\\FiguresCheck\\N2O_cal.jpg",width = 16,height = 12, units = "cm", res = 300)

par(mar=c(2,5,1,1))
boxplot(N2O_cal*1e9~Treatment,data = GHG_boxplot,ylab = "", col=c("grey",col), xlab = "",las=2,xaxt="n")
mtext(expression(paste(N[2],"O"," [nmol/L]")),side = 2,line = 2.5,cex=1)

#legend("topright", legend = c("Canal","Control","Iron-lime","LMB","Aeration","Dredging"), col = c("grey",col),lty=c(1,lty),pch = c(NA,pch),text.col =c("grey",col),cex = .8,lwd = lwd,bty="n")

dev.off()