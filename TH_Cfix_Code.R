

### MISSION: PROCESS C FIXATION DATA ###





setwd("C:/Users/Mikaela/OneDrive/Documents/Masters Stuff/R Stuff")

AvgRep <- read.csv("JRN_metadata_Study549_CfixBiomass.csv", header = TRUE)
AvgRep$Resp<-AvgRep$Resp*-1
AvgRep = AvgRep [,-1]

colnames(AvgRep) = c("Site","Type","Time","Rep","PAR0","PAR25","PAR50","PAR100","PAR150","PAR300","PAR500","PAR750","PAR1000","PAR1250","PAR1600","PAR2000","Resp","Net","QY","LightSat","Shape","Amax","Thickness(mm)","Total Biomass","Diversity","Bacterial Biomass","Actinomycetes Biomass","Gram Neg Biomass","Rhizobia Biomass","Fungal Biomass","Arbuscular Mycorrhizal Biomass","Saprophytes Biomass","Protozoa Biomass","Gram Pos Biomass","Undifferentiated Biomass","Soil.pH","S.Salts(mmhom/cm)","Organic.Matter.LOI","Nitrate(ppm N)","Olsen(ppm P)","Potassium(ppm K)","Sulfate(ppm S)","Calcium(ppm Ca)","Magnesium(ppm Mg)","Sodium(ppm Na)","CEC","%K.Sat","%Ca.Sat","%Mg.Sat")


library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggeasy)
library(dplyr)
library(rlist)
library(lmPerm)
library(reactable)
library(RVAideMemoire)
library(PERMANOVA)
library(huxtable)

### CUSTOM FUNCTIONS ###
PT1 = theme(plot.title = element_text(hjust = 0.5))
TiOrd <- function(x) {factor(x$Time, levels=c("half","two","six","twelve","twenty-four"))}
TyOrd<- function(x){factor(x$Type,levels=c("LA","DA","PT","CL","MO"))}
pullA <- function(x){ x[x$Site == "AMT",]}
pullJ <- function(x){x[x$Site == "JER",]}
pullV <- function(x){x[x$Site == "VOL",]}
pullW <- function(x){x[x$Site == "WHS",]}

pullLA <- function(x) {x[x$Type == "LA",]}
pullDA <- function(x) {x[x$Type == "DA",]}
pullPT <- function(x) {x[x$Type == "PT",]}
pullCL <- function(x) {x[x$Type == "CL",]}
pullMO <- function(x) {x[x$Type == "MO",]}

###


### THICKNESS GRAPH ###

OldTh<-AvgRep %>% dplyr::select(Site, Type, Rep,`Thickness(mm)`)
colnames(OldTh) = c("Site","Type","Rep","Thickness")
NMeta <- read.csv("NFixRready.csv", header = TRUE)
NMeta<-NMeta %>% dplyr::select(Site,Type,Rep,Thickness)
Thickk<-rbind(NMeta,OldTh)
Thickk$Type<-TyOrd(Thickk)

ggplot(Thickk, aes(x=Type, y=Thickness,col=Type)) + 
  geom_boxplot() + geom_point(size=5,alpha=.8,position=position_dodge(width=.75)) +
  scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) +
  ylab("Thickness(mm)")+  facet_grid(~Site) +  theme_bw()

Thaov<-aov(Thickness~Type,data=Thickk)
TKA<-Thickk %>% filter(Site=="WHS")
TKT<-Thickk %>% filter(Type=="MO")
Thaov<-aov(Thickness~Type,data=TKA)
Thaov<-aov(Thickness~Site,data=TKT)
Thaov<-aov(Thickness~Site*Type,data=Thickk)
summary(Thaov)
THIaov<-(TukeyHSD(Thaov,"Type",ordered=TRUE))
THIaov<-(TukeyHSD(Thaov,"Site",ordered=TRUE))
THIaov<-list.cbind(THIaov)
THIs<-as.data.frame(THIaov) %>% filter(`p adj` < .05)


Thaov<-aov(Thickness~Site*Type,data=Thickk)
summary(Thaov)
THIaov<-(TukeyHSD(Thaov,"Site:Type",ordered=TRUE))
THIaov<-list.cbind(THIaov)
THIs<-as.data.frame(THIaov) %>% filter(`p adj` < .05)
THIn<-as.data.frame(THIaov) %>% filter(`p adj` > .05)

# THICKNESS WHEN TAKING THE MEAN OF ALL SITES #
THImall <- aggregate(Thickk$Thickness, by=list(Thickk$Type), FUN=mean)
colnames(THImall) = c("Type","Thickness")
sd <- aggregate(Thickk$Thickness, by=list(Thickk$Type), FUN=sd)
THImall$SE<-sd$x/sqrt(140)
THImall$Pos<-THImall$Thickness+THImall$SE
THImall$Neg<-THImall$Thickness-THImall$SE

COLTY1<-(levels=c("#FFCC99","#FF9966","#FF0066","#660066","#000044"))

ggplot(THImall, aes(x=Type, y=Thickness,col=Type)) + 
  geom_bar(color=c(COLTY1),fill=c(COLTY1), stat="identity")+geom_errorbar(aes(ymin=(Neg),ymax=(Pos)),colour="black", width=.2,alpha=.8)+
  ylab("Thickness(mm)")+  scale_y_continuous(limits = c(0,10)) +  theme_bw()
dev.copy(jpeg,"ThickAll.jpeg",width=10,height=15,units="cm",res=300)
dev.off()

# THICKNESS TAKING THE MEAN AT EACH SITE #
THIm <- aggregate(Thickk$Thickness, by=list(Thickk$Site,Thickk$Type), FUN=mean)
colnames(THIm) = c("Site","Type","Thickness")

THIm$Thickness<-round(THIm$Thickness,digits=2)
sd <- aggregate(Thickk$Thickness, by=list(Thickk$Site,Thickk$Type), FUN=sd)
THIm$SE<-sd$x/sqrt(35)
THIm$SE<-round(THIm$SE,digits=2)
THIm$Pos<-THIm$Thickness+THIm$SE
THIm$Neg<-THIm$Thickness-THIm$SE

ggplot(THIm, aes(x=Type, y=Thickness,col=Type)) + 
  geom_point(size=1.5,alpha=.8,position=position_dodge(width=.75)) +
  geom_errorbar(aes(color=Type,ymin=(Neg),ymax=(Pos)), width=.2,alpha=.8)+
  scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) +
  ylab("Thickness(mm)")+  facet_grid(~Site) +  theme_bw()

COLTY<-(levels=c("#FFCC99","#FF9966","#FF0066","#660066","#000044","#FFCC99","#FF9966","#FF0066","#660066","#000044","#FFCC99","#FF9966","#FF0066","#660066","#000044","#FFCC99","#FF9966","#FF0066","#660066","#000044"))
ggplot(THIm, aes(x=Type, y=Thickness,col=Type)) + 
  geom_bar(color=c(COLTY),fill=c(COLTY), stat="identity")+geom_errorbar(aes(ymin=(Neg),ymax=(Pos)),colour="black", width=.2,alpha=.8)+
  ylab("Thickness(mm)")+  scale_y_continuous(limits = c(0,10.5))+facet_grid(~Site) +  theme_bw()
dev.copy(jpeg,"ThickType.jpeg",width=25,height=7.5,units="cm",res=300)
dev.off()

ggplot(THIm, aes(x=Site, y=Thickness,col=Site)) + 
  geom_point(size=2,alpha=.8,position=position_dodge(width=.75)) +
  geom_errorbar(aes(color=Site,ymin=(Neg),ymax=(Pos)), width=.2,alpha=.8)+
  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) +
  ylab("Thickness(mm)")+  facet_grid(~Type) +  theme_bw()
COLSI<-(levels=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3","#FD612C", "#62D26F", "#FFDD2B","#208EA3","#FD612C", "#62D26F", "#FFDD2B","#208EA3","#FD612C", "#62D26F", "#FFDD2B","#208EA3","#FD612C", "#62D26F", "#FFDD2B","#208EA3"))
ggplot(THIm, aes(x=Site, y=Thickness,col=Site)) + 
  geom_bar(color=c(COLSI),fill=c(COLSI), stat="identity")+geom_errorbar(aes(ymin=(Neg),ymax=(Pos)),colour="black", width=.2,alpha=.8)+
  ylab("Thickness(mm)")+  scale_y_continuous(limits = c(0,10.5)) +facet_grid(~Type) +  theme_bw()
dev.copy(jpeg,"ThickSite.jpeg",width=25,height=6.5,units="cm",res=300)
dev.off()

THIm_hux <- THIm %>% 
  dplyr::select(Site, Type,Thickness,SE) %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

huxtable::quick_docx(THIm_hux, file = "Thickness.docx")

# LOOKING AT QUANTUM YIELD -> DATA NOT USED #
AvR<-AvgRep %>% filter(QY !="NA")
QYm <- aggregate(AvR$QY, by=list(AvR$Site,AvR$Type,AvR$Time), FUN=mean)
colnames(QYm) = c("Site","Type","Time","QY")

QYm$QY<-round(QYm$QY,digits=2)
sd <- aggregate(AvR$QY, by=list(AvR$Site,AvR$Type,AvR$Time), FUN=sd)
QYmm<-cbind(QYm,sd)
QYmm<-QYmm %>% filter(x !="NA")

THIm$SE<-sd$x/sqrt(140)
THIm$SE<-round(THIm$SE,digits=2)
THIm$Pos<-THIm$Thickness+THIm$SE
THIm$Neg<-THIm$Thickness-THIm$SE
QYmm$Pos<-QYmm$QY+QYmm$x
QYmm$Neg<-QYmm$QY-QYmm$x
QYmm$Type<-TyOrd(QYmm)
QYmm$Time<-TiOrd(QYmm)


ggplot(QYmm, aes(x=Time, y=QY,col=Type)) + 
  geom_point(size=1.5,alpha=.8,position=position_dodge(width=.75)) +
  geom_errorbar(aes(color=Type,ymin=(Neg),ymax=(Pos)), width=.2,alpha=.8, position=position_dodge(width=.75))+
  scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) +
  ylab("Quantum Yield")+  facet_grid(~Site) +  theme_bw()
dev.copy(jpeg,"QY.jpeg",width=25,height=6.5,units="cm",res=300)
dev.off()

### ### ###

# PERMANOVA FOR AMAX RESP AND NET REPLICATES #
AAvg<-AvgRep[AvgRep$Site =="AMT",]
JAvg<-AvgRep[AvgRep$Site =="JER",]
VAvg<-AvgRep[AvgRep$Site =="VOL",]
WAvg<-AvgRep[AvgRep$Site =="WHS",]

AAvg$tname<-paste(AAvg$Type,AAvg$Time,sep="")
JAvg$tname<-paste(JAvg$Type,JAvg$Time,sep="")
VAvg$tname<-paste(VAvg$Type,VAvg$Time,sep="")
WAvg$tname<-paste(WAvg$Type,WAvg$Time,sep="")

tname0<-JAvg %>% dplyr::select(tname)
Resp0<-JAvg %>% dplyr::select(Resp)
Amax0<-JAvg %>% dplyr::select(Amax)
Net0<-JAvg %>% dplyr::select(Net)

set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=49)
pairwise.perm.t.test(Net0,tname0,nperm=99)

Raaov<-aovp(Resp~Site*Type*Time,data=AvgRep)
summary(Raaov)
Aaaov<-aovp(Amax~Site*Type*Time,data=AvgRep)
summary(Aaaov)
Naaov<-aovp(Net~Site*Type*Time,data=AvgRep)
summary(Naaov)

### TALLIES FOR LIGHT SATURATION POINT AND SHAPE ###
## NUMBERS WERE EXTRACTED AND TABLED FORMATTED IN EXCEL ##

CountLS<-AvgRep %>% group_by(Site,Type,Time,LightSat) %>% tally()
CountLA<-CountLS[CountLS$Site=="AMT",]
CountLJ<-CountLS[CountLS$Site=="JER",]
CountLV<-CountLS[CountLS$Site=="VOL",]
CountLW<-CountLS[CountLS$Site=="WHS",]

ggplot(CountLS,aes(x=LightSat,y=n,color=Site,shape=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Time (hr)") + ylab("Amax (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CLA<-ggplot(CountLA,aes(x=n,y=LightSat,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Time (hr)") + ylab("Amax (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CLJ<-ggplot(CountLJ,aes(x=n,y=LightSat,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Time (hr)") + ylab("Amax (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CLV<-ggplot(CountLV,aes(x=n,y=LightSat,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Time (hr)") + ylab("Amax (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CLW<-ggplot(CountLW,aes(x=n,y=LightSat,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Time (hr)") + ylab("Amax (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 

ggarrange(CLA,CLJ,CLV,CLW, ncol=2, nrow=2)

CountShape<-AvgRep %>% group_by(Site,Type,Time,Shape) %>% tally()
CountSA<-CountShape[CountShape$Site=="AMT",]
CountSJ<-CountShape[CountShape$Site=="JER",]
CountSV<-CountShape[CountShape$Site=="VOL",]
CountSW<-CountShape[CountShape$Site=="WHS",]

CSA<-ggplot(CountSA,aes(x=n,y=Shape,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Count") + ylab("Curve Shape") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CSJ<-ggplot(CountSJ,aes(x=n,y=Shape,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Count") + ylab("Curve Shape)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CSV<-ggplot(CountSV,aes(x=n,y=Shape,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Count") + ylab("Curve Shape") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 
CSW<-ggplot(CountSW,aes(x=n,y=Shape,color=Time)) + geom_point(size=6,alpha=.5,position=position_dodge(width=.75)) + xlab("Count") + ylab("Curve Shape)") +
  theme_bw() + theme(legend.position="bottom") + theme_bw() + facet_grid(~Type) 

ggarrange(CSA,CSJ,CSV,CSW, ncol=2, nrow=2)

### ### ###

# CALCULATING C FLUX MEANS FOR VISUALS #
SDallrNS <- aggregate(AvgRep$Resp, by=list(AvgRep$Type,AvgRep$Time), FUN=sd)
SDallaNS <- aggregate(AvgRep$Amax, by=list(AvgRep$Type,AvgRep$Time), FUN=sd)
SDallnNS <- aggregate(AvgRep$Net, by=list(AvgRep$Type,AvgRep$Time), FUN=sd)
SDallThiNS <- aggregate(AvgRep$`Thickness(mm)`, by=list(AvgRep$Type,AvgRep$Time), FUN=sd)

SDallr <- aggregate(AvgRep$Resp, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=sd)
SDalla <- aggregate(AvgRep$Amax, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=sd)
SDalln <- aggregate(AvgRep$Net, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=sd)
SDallThi <- aggregate(AvgRep$`Thickness(mm)`, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=sd)

AvgAll<-AvgRep%>% dplyr::select(Site,Type,Time,Rep,`Total Biomass`,Diversity,`Bacterial Biomass`,`Actinomycetes Biomass`,`Gram Neg Biomass`,`Rhizobia Biomass`,`Fungal Biomass`,`Arbuscular Mycorrhizal Biomass`,`Saprophytes Biomass`,`Protozoa Biomass`,`Gram Pos Biomass`,`Undifferentiated Biomass`,`Soil.pH`,`S.Salts(mmhom/cm)`,`Organic.Matter.LOI`,`Nitrate(ppm N)`,`Olsen(ppm P)`,`Potassium(ppm K)`,`Sulfate(ppm S)`,`Calcium(ppm Ca)`,`Magnesium(ppm Mg)`,`Sodium(ppm Na)`,`CEC`,`%K.Sat`,`%Ca.Sat`,`%Mg.Sat`)

SDarNS <- SDallrNS %>% arrange(Group.1,Group.2)
SDaaNS <- SDallaNS %>% arrange(Group.1,Group.2)
SDanNS <- SDallnNS %>% arrange(Group.1,Group.2)
SDThiNS <- SDallThiNS %>% arrange(Group.1,Group.2)

SDar <- SDallr %>% arrange(Group.1,Group.2)
SDaa <- SDalla %>% arrange(Group.1,Group.2)
SDan <- SDalln %>% arrange(Group.1,Group.2)
SDThi <- SDallThi %>% arrange(Group.1,Group.2)

MallrNS <- aggregate(AvgRep$Resp, by=list(AvgRep$Type,AvgRep$Time), FUN=mean)
MallaNS <- aggregate(AvgRep$Amax, by=list(AvgRep$Type,AvgRep$Time), FUN=mean)
MallnNS <- aggregate(AvgRep$Net, by=list(AvgRep$Type,AvgRep$Time), FUN=mean)
MallThiNS <- aggregate(AvgRep$`Thickness(mm)`, by=list(AvgRep$Type,AvgRep$Time), FUN=mean)

Mallr <- aggregate(AvgRep$Resp, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=mean)
Malla <- aggregate(AvgRep$Amax, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=mean)
Malln <- aggregate(AvgRep$Net, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=mean)
MallThi <- aggregate(AvgRep$`Thickness(mm)`, by=list(AvgRep$Site,AvgRep$Type,AvgRep$Time), FUN=mean)

MallaNS <- MallaNS %>% arrange(Group.1,Group.2)
MallrNS <- MallrNS %>% arrange(Group.1,Group.2)
MallnNS <- MallnNS %>% arrange(Group.1,Group.2)
MallThiNS <- MallThiNS %>% arrange(Group.1,Group.2)

Malla <- Malla %>% arrange(Group.1,Group.2)
Mallr <- Mallr %>% arrange(Group.1,Group.2)
Malln <- Malln %>% arrange(Group.1,Group.2)
MallThi <- MallThi %>% arrange(Group.1,Group.2)

BioD<-rbind(AvgAll[1,],AvgAll[27,],AvgAll[52,],AvgAll[77,],AvgAll[102,],AvgAll[127,],AvgAll[152,],AvgAll[177,],AvgAll[202,],AvgAll[227,],AvgAll[252,],AvgAll[277,],AvgAll[302,],AvgAll[327,],AvgAll[352,],AvgAll[377,],AvgAll[402,],AvgAll[427,],AvgAll[452,],AvgAll[477,])
BioD = BioD [,-3]
BioD = BioD [,-3]

NSite <- MallaNS
colnames(NSite) = c("Type","Time","Amax")
NSite$Resp <- MallrNS$x
NSite$Net <-MallnNS$x
NSite$Thickness <-MallThiNS$x

AvgA<-Mallr
colnames(AvgA) = c("Site","Type","Time","Resp")
AvgA$Amax<-Malla$x
AvgA$Net <-Malln$x
AvgA$Thickness <-MallThi$x


P1r <- (SDThiNS[1:9,3]/sqrt(20))
P2r<- (SDThiNS[10,3]/sqrt(19))
P3r<- (SDThiNS[11:25,3]/sqrt(20))
CC<-rbind(c(P1r,P2r,P3r))
NSite$SE.Thickness <-t(CC)

P1r <- (SDThi[1:9,4]/sqrt(5))
P2r<- (SDThi[10,4]/sqrt(4))
P3r<- (SDThi[11:100,4]/sqrt(5))
CC<-rbind(c(P1r,P2r,P3r))
AvgA$SE.Thickness <-t(CC)

P1r <- (SDarNS[1:9,3]/sqrt(20))
P2r<- (SDarNS[10,3]/sqrt(19))
P3r<- (SDarNS[11:25,3]/sqrt(20))
CC<-rbind(c(P1r,P2r,P3r))
NSite$SE.Resp <- t(CC)

P1r <- (SDar[1:9,4]/sqrt(5))
P2r<- (SDar[10,4]/sqrt(4))
P3r<- (SDar[11:100,4]/sqrt(5))
CC<-rbind(c(P1r,P2r,P3r))
AvgA$SE.Resp <- t(CC)

P1a <- (SDaaNS[1:9,3]/sqrt(20))
P2a<- (SDaaNS[10,3]/sqrt(19))
P3a<- (SDaaNS[11:25,3]/sqrt(20))
CC<-rbind(c(P1a,P2a,P3a))
NSite$SE.Amax <- t(CC)

P1a <- (SDaa[1:9,4]/sqrt(5))
P2a<- (SDaa[10,4]/sqrt(4))
P3a<- (SDaa[11:100,4]/sqrt(5))
CC<-rbind(c(P1a,P2a,P3a))
AvgA$SE.Amax <- t(CC)

P1n<- (SDan[1:9,4]/sqrt(5))
P2n<- (SDan[10,4]/sqrt(4))
P3n<- (SDan[11:100,4]/sqrt(5))
CC<-rbind(c(P1n,P2n,P3n))
AvgA$SE.Net <- t(CC)

P1n<- (SDanNS[1:9,3]/sqrt(20))
P2n<- (SDanNS[10,3]/sqrt(19))
P3n<- (SDanNS[11:25,3]/sqrt(20))
CC<-rbind(c(P1n,P2n,P3n))
NSite$SE.Net <- t(CC)

NSite$yminR <- NSite$Resp-NSite$SE.Resp
NSite$ymaxR <- NSite$Resp+NSite$SE.Resp
NSite$yminA <- NSite$Amax-NSite$SE.Amax
NSite$ymaxA <- NSite$Amax+NSite$SE.Amax
NSite$yminN <- NSite$Net-NSite$SE.Net
NSite$ymaxN <- NSite$Net+NSite$SE.Net

AvgA$yminR <- AvgA$Resp-AvgA$SE.Resp
AvgA$ymaxR <- AvgA$Resp+AvgA$SE.Resp
AvgA$yminA <- AvgA$Amax-AvgA$SE.Amax
AvgA$ymaxA <- AvgA$Amax+AvgA$SE.Amax
AvgA$yminN <- AvgA$Net-AvgA$SE.Net
AvgA$ymaxN <- AvgA$Net+AvgA$SE.Net

NSite$Time <- TiOrd(NSite)
NSite$Type<- TyOrd(NSite) 

# THICKNESS COMBIND ALL SITE #
ggplot(AvgRep, aes(x=Time, y=Net,col=Type)) + 
  geom_boxplot() + geom_point(size=5,alpha=.8,position=position_dodge(width=.75)) +
  scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) +
  ylab("Thickness(mm)")+  theme_bw()

AvgA$Time <- TiOrd(AvgA)
AvgA$Type<- TyOrd(AvgA) 

# PERMANOVA FOR THICKNESS #
Raaov<-aov(`Thickness(mm)`~Site,data=AAL)
Raaov<-aovp(Net~Type*Time,data=AvgRep)
summary(Raaov)
AvgRep$tname<-paste(AvgA$Type,AvgRep$Time,sep="")
AAL<-AvgRep %>% filter(Type=="LA")
AAD<-AvgRep %>% filter(Type=="DA")
AAC<-AvgRep %>% filter(Type=="CL")
AAP<-AvgRep %>% filter(Type=="PT")
AAM<-AvgRep %>% filter(Type=="MO")

AA.5<-AvgRep %>% filter(Time=="half")
AA2<-AvgRep %>% filter(Time=="two")
AA6<-AvgRep %>% filter(Time=="six")
AA12<-AvgRep %>% filter(Time=="twelve")
AA24<-AvgRep %>% filter(Time=="twentyfour")

tname0<-AA24 %>% dplyr::select(Type)
Resp0<-AA24 %>% dplyr::select(Resp)
Amax0<-AA24 %>% dplyr::select(Amax)
Net0<-AA24 %>% dplyr::select(Net)
set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)


Raaov<-aovp(Resp~Type*Time,data=AvgA)
summary(Raaov)
Aaaov<-aovp(Amax~Type*Time,data=AvgA)
summary(Aaaov)
Naaov<-aovp(Net~Type*Time,data=AvgA)
summary(Naaov)

AvgA$tname<-paste(AvgA$Type,AvgA$Time,sep="")

AAL<-AvgA %>% filter(Type=="LA")
AAD<-AvgA %>% filter(Type=="DA")
AAC<-AvgA %>% filter(Type=="CL")
AAP<-AvgA %>% filter(Type=="PT")
AAM<-AvgA %>% filter(Type=="MO")

tname0<-AAL %>% dplyr::select(tname)
Resp0<-AAL %>% dplyr::select(Resp)
Amax0<-AAL %>% dplyr::select(Amax)
Net0<-AAL %>% dplyr::select(Net)

set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,nperm=99)
pairwise.perm.t.test(Resp0,tname0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)

# TABLE OF THICKNESS AVERAGES #
AvgA$Time<-as.character(AvgA$Time)
AvgA$Time[is.na(AvgA$Time)] <- "twentyfour"
AvgAtab<-AvgA %>% dplyr::select(Site,Type,Time,Thickness,SE.Thickness,Amax,SE.Amax,Resp,SE.Resp,Net,SE.Net)
colnames(AvgAtab) = c("Site","Type","Time","Thickness","Thickness SE","Amax","Amax SE","Respiration","Respiration SE","Net","Net SE")

AvgAtab$Thickness<-round(AvgAtab$Thickness,digits=2)
AvgAtab$Amax<-round(AvgAtab$Amax,digits=2)
AvgAtab$Respiration<-round(AvgAtab$Respiration,digits=2)
AvgAtab$Net<-round(AvgAtab$Net,digits=2)
AvgAtab$`Thickness SE`<-round(AvgAtab$`Thickness SE`,digits=2)
AvgAtab$`Amax SE`<-round(AvgAtab$`Amax SE`,digits=2)
AvgAtab$`Respiration SE`<-round(AvgAtab$`Respiration SE`,digits=2)
AvgAtab$`Net SE`<-round(AvgAtab$`Net SE`,digits=2)

MaxTabA<-AvgAtab %>% group_by(Site,Type) %>% slice(which.max(Amax))
MaxTabA<-MaxTabA %>% dplyr::select(Site,Type,Time,Amax,`Amax SE`)
MAmax <- MaxTabA %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

huxtable::quick_docx(MAmax, file = "CfixMax.docx")


MaxTabN<-AvgAtab %>% group_by(Site,Type) %>% dplyr::slice(which.max(Net))
MaxTabN<-MaxTabN %>% dplyr::select(Site,Type,Time,Amax,`Amax SE`,Respiration,`Respiration SE`,Net,`Net SE`)
MNet <- MaxTabN %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

huxtable::quick_docx(MNet, file = "NetMax.docx")

AATNet<-AvgAtab %>% dplyr::select(Site,Type,Time,Net,`Net SE`)
AAT <- AvgAtab %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

AATNett <- AATNet %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

huxtable::quick_docx(AATNett, file = "CfixAvg.docx")

### SCALE TO SITE ###

Land<-MaxTabN

Land$ScaleUpCfix<-(Land$Amax*(60*60*12*12*48)/(10^-6))
Land$ScaleUpResp<-(Land$Respiration*(60*60*24*12*48)/(10^-6))

Land$Percent<-as.vector(c(0.2167,0.1767,0.02,0.0167,0,0.2867,0.1967,0.0033,0,0,0.1067,0.02,0.0233,0.0233,0,0.18,0.1233,0.0567,.25,0))
Land$ScalePerC<-Land$ScaleUpCfix*Land$Percent
Land$ScalePerR<-Land$ScaleUpResp*Land$Percent

YearlyC <- aggregate(Land$ScalePerC, by=list(Land$Site), FUN=sum)
YearlyR <- aggregate(Land$ScalePerR, by=list(Land$Site), FUN=sum)
k
k<-YearlyC$x-YearlyR$x
k/3

Yearly$Mega<-(Yearly$x*(10^-6))
Yearly$Giga<-(Yearly$x*(10^-9))
Yearly$Mega
colnames(Yearly) = c("Site","Rate")

Yearly %>%
  mutate(Group.1 = replace(Group.1, Group.1 == "AMT", AMT[Mega=="Other"]))

TKA<-Thickk %>% filter(Site=="WHS")
View(Land)




# TIME TABLE COMPAIRSON BY AVG #
AvgA.5<- AvgA %>% filter(Time=="half")
AvgA2<- AvgA %>% filter(Time=="two")
AvgA6<- AvgA %>% filter(Time=="six")
AvgA12<- AvgA %>% filter(Time=="twelve")
AvgA24<- AvgA %>% filter(Time=="twentyfour")

AvgA.5$Type<-TyOrd(AvgA.5)
AvgA2$Type<-TyOrd(AvgA2)
AvgA6$Type<-TyOrd(AvgA6)
AvgA12$Type<-TyOrd(AvgA12)
AvgA24$Type<-TyOrd(AvgA24)

Amax.5 <- ggplot(AvgA.5,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8) + scale_y_continuous(limits = c(2,16))+ ggtitle("Fixation .5hr") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp.5 <- ggplot(AvgA.5,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Respiration .5hr") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net.5 <- ggplot(AvgA.5,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(-10,7.5)) +geom_abline(intercept=0,slope=0)+ ggtitle("Net .5hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

Amax2 <- ggplot(AvgA2,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Fixation 2hr") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp2 <- ggplot(AvgA2,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Respiration 2hr") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net2 <- ggplot(AvgA2,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-10,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net 2hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

Amax6 <- ggplot(AvgA6,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Fixation 6hr") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp6 <- ggplot(AvgA6,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Respiration 6hr") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net6 <- ggplot(AvgA6,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-10,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net 6hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

Amax12 <- ggplot(AvgA12,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Fixation 12hr") + xlab("Biocrust Type") + ylab("Fixaiton (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp12 <- ggplot(AvgA12,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Respiration 12hr") + xlab("Biocrust Type") + ylab("Respiraiton (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net12 <- ggplot(AvgA12,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-10,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net 12hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

Amax24 <- ggplot(AvgA24,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Fixation 24hr") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp24 <- ggplot(AvgA24,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,16))+ ggtitle("Respiration 24hr") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net24 <- ggplot(AvgA24,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-10,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net 24hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

Amax24 <- ggplot(AvgA24,aes(x=Type,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,11))+ ggtitle("Fixation 24hr") + xlab("Biocrust Type") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Resp24 <- ggplot(AvgA24,aes(x=Type,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)+ scale_y_continuous(limits = c(2,11))+ ggtitle("Respiration 24hr") + xlab("Biocrust Type") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
Net24 <- ggplot(AvgA24,aes(x=Type,y=Net,color=Site)) + geom_point(size=5,alpha=.8) +geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + scale_y_continuous(limits = c(-3,7.5))+geom_abline(intercept=0,slope=0)+ ggtitle("Net 24hr") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

ggarrange(Amax.5,Resp.5,Net.5, ncol=3, nrow=1)
dev.copy(png,"Avg.5.png",width=25,height=10,units="cm",res=300)
dev.off()

ggarrange(Amax24,Resp24,Net24, ncol=3, nrow=1)
dev.copy(png,"Avg24.png",width=25,height=10,units="cm",res=300)
dev.off()

ggarrange(Amax.5,Resp.5,Net.5,Amax2,Resp2,Net2,Amax6,Resp6,Net6,Amax12,Resp12,Net12,Amax24,Resp24,Net24, ncol=3, nrow=5)
dev.copy(png,"AvgTime.png",width=25,height=40,units="cm",res=300)
dev.off()

ThiAmax24 <- ggplot(AvgA24,aes(x=Thickness,y=Amax,color=Site,shape=Type)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2,9))+ ggtitle("Thickness to Fixation")+ xlab("Thickness(mm)") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+ PT1
ThiResp24 <- ggplot(AvgA24,aes(x=Thickness,y=Resp,color=Site,shape=Type)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2,9)) + ggtitle("Thickness to Respiration") + xlab("Thickness(mm)") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18))) +PT1
ThiNet24 <- ggplot(AvgA24,aes(x=Thickness,y=Net,color=Site,shape=Type)) + geom_point(size=6,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Thickness to Net")  + xlab("Thickness(mm)") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+PT1

ThiAmax24 <- ggplot(AAA24,aes(x=`Thickness(mm)`,y=Amax,color=Site,shape=Type)) + geom_point(size=3,alpha=.8)+scale_y_continuous(limits = c(0,16.5))+ ggtitle("Thickness to Fixation")+ xlab("Thickness(mm)") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+ PT1
ThiResp24 <- ggplot(AAA24,aes(x=`Thickness(mm)`,y=Resp,color=Site,shape=Type)) + geom_point(size=3,alpha=.8)+scale_y_continuous(limits = c(0,16.5)) + ggtitle("Thickness to Respiration") + xlab("Thickness(mm)") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18))) +PT1
ThiNet24 <- ggplot(AAA24,aes(x=`Thickness(mm)`,y=Net,color=Site,shape=Type)) + geom_point(size=3,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Thickness to Net")  + xlab("Thickness(mm)") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+PT1

ggarrange(ThiAmax24,ThiResp24,ThiNet24, ncol=3, nrow=1)
dev.copy(jpeg,"Avg24Scatter.jpeg",width=27,height=9,units="cm",res=300)
dev.off()

# PERMANOVA COMPARISONS FOR FLUX DATA #
AOV24<-aovp(Resp~Type*Site,data=AAA24)
summary(AOV24)

AvgA$tname<-paste(AvgA$Type,AvgA$Time,sep="")

ATy<-AAA24 %>% filter(Site=="AMT")
JTy<-AAA24 %>% filter(Site=="JER")
VTy<-AAA24 %>% filter(Site=="VOL")
WTy<-AAA24 %>% filter(Site=="WHS")

AAL<-AAA24 %>% filter(Type=="LA")
AAD<-AAA24 %>% filter(Type=="DA")
AAC<-AAA24 %>% filter(Type=="CL")
AAP<-AAA24 %>% filter(Type=="PT")
AAM<-AAA24 %>% filter(Type=="MO")

tname0<-WTy %>% dplyr::select(Type)
Resp0<-WTy %>% dplyr::select(Resp)
Amax0<-WTy %>% dplyr::select(Amax)
Net0<-WTy %>% dplyr::select(Net)

set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)


with(AvgA, AvgA[order(Site,Type),])
AmaxS <- ggplot(AvgA,aes(x=Time,y=Amax,color=Site)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + geom_errorbar(aes(color=Site,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8) + scale_y_continuous(limits = c(0,16)) + xlab("Time (hr)") + ylab("Fixation (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1 + facet_grid(~Type) 
dev.copy(jpeg,"A0Time.jpeg",width=37,height=10,units="cm",res=300)
dev.off()

RespS <-ggplot(AvgA,aes(x=Time,y=Resp,color=Site)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + geom_errorbar(aes(color=Site,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8) + scale_y_continuous(limits = c(0,16)) + xlab("Time (hr)") + ylab("Respiration (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1 + facet_grid(~Type) 
dev.copy(jpeg,"R0Time.jpeg",width=37,height=10,units="cm",res=300)
dev.off()

NetS <- ggplot(AvgA,aes(x=Time,y=Net,color=Site)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + geom_errorbar(aes(color=Site,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8) + xlab("Time (hr)") + ylab("Net (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1 + facet_grid(~Type) 
dev.copy(jpeg,"N0Time.jpeg",width=37,height=10,units="cm",res=300)
dev.off()

NSite$Time<-as.character(NSite$Time)
NSite$Time[is.na(NSite$Time)] <- "twentyfour"
NSite$Time<-TiOrd(NSite)

NSAmax<-ggplot(NSite,aes(x=Time,y=Amax,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0)  + scale_y_continuous(limits = c(3,13)) +
  geom_errorbar(aes(color=Type,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Fixation (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1  

AmaxT <- ggplot(AvgA,aes(x=Time,y=Amax,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + scale_y_continuous(limits = c(0,16)) + geom_errorbar(aes(color=Type,ymin=(yminA),ymax=(ymaxA)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Fixation (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1 + facet_grid(~Site) 
dev.copy(jpeg,"A0TimeS.jpeg",width=30,height=10,units="cm",res=300)
dev.off()

NSResp <- ggplot(NSite,aes(x=Time,y=Resp,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + scale_y_continuous(limits = c(3,13))+ geom_errorbar(aes(color=Type,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Respiration (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1  

RespT <- ggplot(AvgA,aes(x=Time,y=Resp,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + scale_y_continuous(limits = c(0,16))+ geom_errorbar(aes(color=Type,ymin=(yminR),ymax=(ymaxR)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Respiration (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1 + facet_grid(~Site) 
dev.copy(jpeg,"R0TimeS.jpeg",width=30,height=10,units="cm",res=300)
dev.off()

NSNet <- ggplot(NSite,aes(x=Time,y=Net,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + geom_errorbar(aes(color=Type,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Net (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1 

NetT <- ggplot(AvgA,aes(x=Time,y=Net,color=Type)) + geom_point(size=5,alpha=.8) + geom_abline(intercept=0,slope=0) + geom_errorbar(aes(color=Type,ymin=(yminN),ymax=(ymaxN)), width=.2,alpha=.8)  + xlab("Time (hr)") + ylab("Net (µmol CO2 m-2 s-1)") +
  theme_bw() + theme(legend.position="bottom") +scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044")) + PT1 + facet_grid(~Site) 
dev.copy(jpeg,"N0TimeS.jpeg",width=30,height=10,units="cm",res=300)
dev.off()

ggarrange(NSAmax, NSResp, NSNet, ncol=3, nrow=1)
dev.copy(png,"NoSiteARN.png",width=28,height=10,units="cm",res=300)
dev.off()

###

# TIME RATE CORRELATION #

TimeLM<-AvgRep

TimeLM$Time[TimeLM$Time == "half"] <- 0.5
TimeLM$Time[TimeLM$Time == "two"] <- 2
TimeLM$Time[TimeLM$Time == "six"] <- 6
TimeLM$Time[TimeLM$Time == "twelve"] <- 12
TimeLM$Time[TimeLM$Time == "twentyfour"] <- 24
TimeLM$Time<-as.numeric(TimeLM$Time)

TAL<-TimeLM %>% filter(Type=="LA")
TAD<-TimeLM%>% filter(Type=="DA")
TAC<-TimeLM %>% filter(Type=="CL")
TAP<- TimeLM %>% filter(Type=="PT")
TAM<-TimeLM %>% filter(Type=="MO")

cor.test(TAP$Resp,TAC$Time,  method = "pearson", use = "complete.obs")

ATime<-lmp(Amax~Type+Time,data=TimeLM)
RTime<-lmp(Resp~Type*Time,data=TimeLM)
NTime<-lmp(Net~Type*Time,data=TimeLM)
summary(ATime)

NNSite<-NSite
NNSite$Time<-as.character(NNSite$Time)
NNSite$Time[NNSite$Time == "half"] <- 0.5
NNSite$Time[NNSite$Time == "two"] <- 2
NNSite$Time[NNSite$Time == "six"] <- 6
NNSite$Time[NNSite$Time == "twelve"] <- 12
NNSite$Time[is.na(NNSite$Time)] <- 24
NNSite$Time<-as.numeric(NNSite$Time)

TAL<-NNSite %>% filter(Type=="LA")
TAD<-TimeLM%>% filter(Type=="DA")
TAC<-NNSite %>% filter(Type=="CL")
TAP<- TimeLM %>% filter(Type=="PT")
TAM<-TimeLM %>% filter(Type=="MO")

### WEIGHTED AVERAGES ###

AvgA$Time<-TiOrd(AvgA)
with(AvgA, AvgA[order(Site,Type),])

TimeWT <- (rep(c(.5/24,6/24,12/24,24/24,2/24),times=20))
AvgA$TimeWT<-TimeWT

SiA <- split.data.frame(AvgA, AvgA$Site)
AMT <- SiA$AMT
JER <- SiA$JER
VOL<-SiA$VOL
WHS<-SiA$WHS
TyA <- split.data.frame(AMT, AMT$Type)
ALA <- TyA$LA
ADA <- TyA$DA
APT<- TyA$PT
ACL<- TyA$CL
AMO<- TyA$MO
TyJ <- split.data.frame(JER, JER$Type)
JLA <- TyJ$LA
JDA <- TyJ$DA
JPT<- TyJ$PT
JCL<- TyJ$CL
JMO<- TyJ$MO
TyV <- split.data.frame(VOL, VOL$Type)
VLA <- TyV$LA
VDA <- TyV$DA
VPT<- TyV$PT
VCL<- TyV$CL
VMO<- TyV$MO
TyW <- split.data.frame(WHS, WHS$Type)
WLA <- TyW$LA
WDA <- TyW$DA
WPT<- TyW$PT
WCL<- TyW$CL
WMO<- TyW$MO

AmaxWt<- rbind((weighted.mean(ALA$Amax, ALA$TimeWT)),(weighted.mean(ADA$Amax, ADA$TimeWT)),(weighted.mean(APT$Amax, APT$TimeWT)),(weighted.mean(ACL$Amax, ACL$TimeWT)),(weighted.mean(AMO$Amax, AMO$TimeWT)),
               (weighted.mean(JLA$Amax, JLA$TimeWT)),(weighted.mean(JDA$Amax, JDA$TimeWT)),(weighted.mean(JPT$Amax, JPT$TimeWT)),(weighted.mean(JCL$Amax, JCL$TimeWT)),(weighted.mean(JMO$Amax, JMO$TimeWT)),
               (weighted.mean(VLA$Amax, VLA$TimeWT)),(weighted.mean(VDA$Amax, VDA$TimeWT)),(weighted.mean(VPT$Amax, VPT$TimeWT)),(weighted.mean(VCL$Amax, VCL$TimeWT)),(weighted.mean(VMO$Amax, VMO$TimeWT)),
               (weighted.mean(WLA$Amax, WLA$TimeWT)),(weighted.mean(WDA$Amax, WDA$TimeWT)),(weighted.mean(WPT$Amax, WPT$TimeWT)),(weighted.mean(WCL$Amax, WCL$TimeWT)),(weighted.mean(WMO$Amax, WMO$TimeWT)))

RespWt<- rbind((weighted.mean(ALA$Resp, ALA$TimeWT)),(weighted.mean(ADA$Resp, ADA$TimeWT)),(weighted.mean(APT$Resp, APT$TimeWT)),(weighted.mean(ACL$Resp, ACL$TimeWT)),(weighted.mean(AMO$Resp, AMO$TimeWT)),
               (weighted.mean(JLA$Resp, JLA$TimeWT)),(weighted.mean(JDA$Resp, JDA$TimeWT)),(weighted.mean(JPT$Resp, JPT$TimeWT)),(weighted.mean(JCL$Resp, JCL$TimeWT)),(weighted.mean(JMO$Resp, JMO$TimeWT)),
               (weighted.mean(VLA$Resp, VLA$TimeWT)),(weighted.mean(VDA$Resp, VDA$TimeWT)),(weighted.mean(VPT$Resp, VPT$TimeWT)),(weighted.mean(VCL$Resp, VCL$TimeWT)),(weighted.mean(VMO$Resp, VMO$TimeWT)),
               (weighted.mean(WLA$Resp, WLA$TimeWT)),(weighted.mean(WDA$Resp, WDA$TimeWT)),(weighted.mean(WPT$Resp, WPT$TimeWT)),(weighted.mean(WCL$Resp, WCL$TimeWT)),(weighted.mean(WMO$Resp, WMO$TimeWT)))

NetWt<- rbind((weighted.mean(ALA$Net, ALA$TimeWT)),(weighted.mean(ADA$Net, ADA$TimeWT)),(weighted.mean(APT$Net, APT$TimeWT)),(weighted.mean(ACL$Net, ACL$TimeWT)),(weighted.mean(AMO$Net, AMO$TimeWT)),
              (weighted.mean(JLA$Net, JLA$TimeWT)),(weighted.mean(JDA$Net, JDA$TimeWT)),(weighted.mean(JPT$Net, JPT$TimeWT)),(weighted.mean(JCL$Net, JCL$TimeWT)),(weighted.mean(JMO$Net, JMO$TimeWT)),
              (weighted.mean(VLA$Net, VLA$TimeWT)),(weighted.mean(VDA$Net, VDA$TimeWT)),(weighted.mean(VPT$Net, VPT$TimeWT)),(weighted.mean(VCL$Net, VCL$TimeWT)),(weighted.mean(VMO$Net, VMO$TimeWT)),
              (weighted.mean(WLA$Net, WLA$TimeWT)),(weighted.mean(WDA$Net, WDA$TimeWT)),(weighted.mean(WPT$Net, WPT$TimeWT)),(weighted.mean(WCL$Net, WCL$TimeWT)),(weighted.mean(WMO$Net, WMO$TimeWT)))

Site <- rep(c("AMT","JER","VOL","WHS"), each = 5)
Type <- rep(c("LA","DA","PT","CL","MO"), times = 4)

MthiAty <- aggregate(AvgA$Thickness, by=list(AvgA$Site,AvgA$Type), FUN=mean)
colnames(MthiAty) = c("Site","Type","Thickness")

ThiMM <-rbind(pullA(MthiAty),pullJ(MthiAty),pullV(MthiAty),pullW(MthiAty))
Thickness<-ThiMM$Thickness

NN<-BioD %>%
  mutate(Type =  factor(Type, levels = c("LA","DA","PT","CL","MO"))) %>%
  arrange(Type)  
BioD<-NN %>%
  mutate(Site =  factor(Site, levels = c("AMT","JER","VOL","WHS"))) %>%
  arrange(Site)  

BioD = BioD [,-1]
BioD = BioD [,-1]

wtAvg <-cbind(Site,Type,Thickness,RespWt,AmaxWt,NetWt,BioD)
wtAvg$Type <- TyOrd(wtAvg)

# TABLE OF WTAVG #
WTtab<-wtAvg %>% dplyr::select(Site,Type,Thickness,AmaxWt,RespWt,NetWt)
colnames(WTtab) = c("Site","Type","Thickness","Amax","Respiration","Net")
WTtab$Thickness<-round(WTtab$Thickness,digits=2)
WTtab$Amax<-round(WTtab$Amax,digits=2)
WTtab$Respiration<-round(WTtab$Respiration,digits=2)
WTtab$Net<-round(WTtab$Net,digits=2)
write.table(WTtab, file = "WTavg.txt", sep = ",", quote = FALSE, row.names = F)

# PERMANOVA FOR WTAVG #
Aaaov<-aovp(AmaxWt~Type,data=wtAvg)
summary(Aaaov)
Raaov<-aovp(RespWt~Type,data=wtAvg)
summary(Raaov)
Naaov<-aovp(NetWt~Type,data=wtAvg)
summary(Naaov)

tname0<-wtAvg %>% dplyr::select(Type)
Resp0<-wtAvg %>% dplyr::select(RespWt)
Amax0<-wtAvg %>% dplyr::select(AmaxWt)
Net0<-wtAvg %>% dplyr::select(NetWt)

set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=999)
pairwise.perm.t.test(Amax0,tname0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)


#
WAmax0 <- ggplot(wtAvg,aes(x=Type,y=AmaxWt,color=Site)) + geom_point(size=5,alpha=.8)  +scale_y_continuous(limits = c(2,9))+ ggtitle("Fixation Weighted Average") + xlab("Biocrust Type") + ylab("Fixaiton (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
WResp0 <- ggplot(wtAvg,aes(x=Type,y=RespWt,color=Site)) + geom_point(size=5,alpha=.8) +scale_y_continuous(limits = c(2,9)) + ggtitle("Respiration Weighted Average") + xlab("Biocrust Type") + ylab("Amax (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1
WNet0 <- ggplot(wtAvg,aes(x=Type,y=NetWt,color=Site)) + geom_point(size=5,alpha=.8)  +geom_abline(intercept=0,slope=0)+ scale_y_continuous(limits = c(-4,4)) + ggtitle("Net Weighted Average") + xlab("Biocrust Type") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1

ggarrange(WAmax0,WResp0,WNet0, ncol=3, nrow=1)
dev.copy(png,"WTavgType.png",width=25,height=10,units="cm",res=300)
dev.off()

# THICKNESS TO OUTPUT CORRELATION #
NoMO<-wtAvg %>% filter(Type!="MO")
cor.test(wtAvg$Thickness,wtAvg$AmaxWt,  method = "pearson", use = "complete.obs")
cor.test(wtAvg$Thickness,wtAvg$RespWt,  method = "pearson", use = "complete.obs")
cor.test(wtAvg$Thickness,wtAvg$NetWt,  method = "pearson", use = "complete.obs")
cor.test(NoMO$Thickness,NoMO$NetWt,  method = "pearson", use = "complete.obs")
cor.test(AAL$`Thickness(mm)`,AAL$Amax,  method = "pearson", use = "complete.obs")
cor.test(AAL$`Thickness(mm)`,AAL$Resp,  method = "pearson", use = "complete.obs")
cor.test(AAL$`Thickness(mm)`,AAL$Net,  method = "pearson", use = "complete.obs")

Ao<-AAA24 %>% filter(Site=="WHS")
AAL<-Ao %>% filter(Type=="LA")
AAD<-AAA24 %>% filter(Type=="DA")
AAC<-AAA24 %>% filter(Type=="CL")
AAP<-AAA24 %>% filter(Type=="PT")
AAM<-AAA24 %>% filter(Type=="MO")

# LINEAR MODELS #
LMAwt<-lmp(AmaxWt~Thickness*Type,data=wtAvg)
LMAws<-lmp(AmaxWt~Thickness*Site,data=wtAvg)
AAAA<-AAA24 %>% filter(Site =="VOL")
LMAws<-lm(Net~`Thickness(mm)`*Site,data=AAA24)
LMAws<-lmp(Amax~`Thickness(mm)`*Type,data=AAA24)
LMAws<-lmp(Resp~`Thickness(mm)`*Site,data=AAA24)
View(AAA24)

### LOOK TO AAAA FOR SPECIFIC SITE INFO ###

summary(LMAws)
AIC(LMAwt)
LMRwt<-lmp(RespWt~Thickness*Type,data=wtAvg)
LMRws<-lmp(RespWt~Thickness*Site,data=wtAvg)
summary(LMRws)
LMNwt<-lmp(NetWt~Thickness*Type,data=wtAvg)
LMNws<-lmp(NetWt~Thickness*Site,data=wtAvg)
summary(LMNwsoo)
AIC(LMAw)
LMNwtoo<-lm(NetWt~Thickness*Type,data=NoMO)
LMNwsoo<-lmp(NetWt~Thickness*Site,data=NoMO)

ThiAmaxT <- ggplot(wtAvg,aes(x=Thickness,y=AmaxWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2.5,9)) + ggtitle("Thickness to Fixation")+ xlab("Thickness(mm)") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) + PT1
ThiRespT <- ggplot(wtAvg,aes(x=Thickness,y=RespWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2.5,9)) + ggtitle("Thickness to Respiration") + xlab("Thickness(mm)") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) +PT1
ThiNetT <- ggplot(wtAvg,aes(x=Thickness,y=NetWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Thickness to Net")  + xlab("Thickness(mm)") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) +PT1

ThiAmaxS <- ggplot(wtAvg,aes(x=Thickness,y=AmaxWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2.5,9)) + ggtitle("Thickness to Fixation")+ xlab("Thickness(mm)") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+ PT1
ThiRespS <- ggplot(wtAvg,aes(x=Thickness,y=RespWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8)+scale_y_continuous(limits = c(2.5,9)) + ggtitle("Thickness to Respiration") + xlab("Thickness(mm)") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18))) +PT1
ThiNetS <- ggplot(wtAvg,aes(x=Thickness,y=NetWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Thickness to Net")  + xlab("Thickness(mm)") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="none") +  scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+PT1

ggarrange(ThiAmaxS,ThiRespS,ThiNetS, ncol=3, nrow=1)
dev.copy(jpeg,"WTavgScatter.jpeg",width=27,height=9,units="cm",res=300)
dev.off()

ggarrange(ThiAmaxT,ThiRespT,ThiNetT, ncol=3, nrow=1)
dev.copy(jpeg,"WTavgScatter2.jpeg",width=27,height=9,units="cm",res=300)
dev.off()

### BIOMASS ####

cor.test(wtAvg$Thickness,wtAvg$`Total Biomass`,  method = "pearson", use = "complete.obs")
ThTBM<-lmp(Thickness~`Total Biomass`*Site,data=wtAvg)
ThTBMt<-lmp(Thickness~`Total Biomass`*Type,data=wtAvg)
summary(ThTBMt)

ThiBio <- ggplot(wtAvg,aes(x=Thickness,y=`Total Biomass`,color=Type,shape=Site)) + geom_point(size=4,alpha=.8) + geom_abline(intercept=3000,slope=0) + ggtitle("Total Biomass to Thickness") +  xlab("Thickness (mm)") + ylab("Total Biomass") + theme_bw() + theme(legend.position="right") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) + PT1 + scale_y_log10()
dev.copy(jpeg,"ThiBio.jpeg",width=12,height=9,units="cm",res=300)
dev.off()


BioNetT <- ggplot(wtAvg,aes(x=`Total Biomass`,y=NetWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Total Biomass to Net") + scale_y_continuous(limits = c(-4,4)) + xlab("Total Biomass") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) + PT1 +scale_x_log10()
BioAmaxT <- ggplot(wtAvg,aes(x=`Total Biomass`,y=AmaxWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8) + ggtitle("Total Biomass to Fixation") + scale_y_continuous(limits = c(2,9)) + xlab("Total Biomass") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) + PT1 + scale_x_log10()
BioRespT <- ggplot(wtAvg,aes(x=`Total Biomass`,y=RespWt,color=Type,shape=Site)) + geom_point(size=6,alpha=.8) + ggtitle("Total Biomass to Respiration") + scale_y_continuous(limits = c(2,9)) + xlab("Total Biomass") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8))) + PT1 + scale_x_log10()

BioNetS <- ggplot(wtAvg,aes(x=`Total Biomass`,y=NetWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8) + geom_abline(intercept=0,slope=0) + ggtitle("Total Biomass to Net") + scale_y_continuous(limits = c(-4,4)) + xlab("Total Biomass") + ylab("Net (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18)))+ PT1 +scale_x_log10()
BioAmaxS <- ggplot(wtAvg,aes(x=`Total Biomass`,y=AmaxWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8) + ggtitle("Total Biomass to Fixation") + scale_y_continuous(limits = c(2,9)) + xlab("Total Biomass") + ylab("Fixation (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18))) + PT1 + scale_x_log10()
BioRespS <- ggplot(wtAvg,aes(x=`Total Biomass`,y=RespWt,color=Site,shape=Type)) + geom_point(size=6,alpha=.8) + ggtitle("Total Biomass to Respiration") + scale_y_continuous(limits = c(2,9)) + xlab("Total Biomass") + ylab("Respiration (µmol CO2 m-2 s-1)") + theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + scale_shape_manual(values=(c(17,15,1,19,18))) + PT1 + scale_x_log10()

ggarrange(BioAmaxS,BioRespS,BioNetS, ncol=3, nrow=1)

### HEATMAP ###

Lables <- wtAvg[,1:2]
SoilBio <- wtAvg[,3:32]
GforHeat<-rbind(c(Lables,SoilBio))

w2 <- round(cor(SoilBio),2)
w2m <- melt(w2)

ggplot(w2m, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()+scale_fill_gradient2(low="#FF3366", mid="#330099", high="#99FFFF", limit=c(-1,1), midpoint=0,name="Pearson\nCorrelation") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

### PCA PLOT ###

wtAvg$Type <- TyOrd(wtAvg)

SoilGoods <-wtAvg[,19:32]
BioGoods <- wtAvg[,9:18]
BioGoods$OtherGramPos<-BioGoods$`Gram Pos Biomass`-BioGoods$`Actinomycetes Biomass` 
BioGoods0<-BioGoods %>% dplyr::select(`Actinomycetes Biomass`,`Gram Neg Biomass`,`Rhizobia Biomass`,`Arbuscular Mycorrhizal Biomass`,`Saprophytes Biomass`,`Protozoa Biomass`,`Protozoa Biomass`,`Undifferentiated Biomass`,OtherGramPos)
Lables <- wtAvg[,1:2]
SoilBio <- wtAvg[,9:32]
SoilBio$OtherGramPos<-BioGoods$OtherGramPos
BioGoods0<-BioGoods %>% dplyr::select(`Actinomycetes Biomass`,`Gram Neg Biomass`,`Rhizobia Biomass`,`Arbuscular Mycorrhizal Biomass`,`Saprophytes Biomass`,`Protozoa Biomass`,`Protozoa Biomass`,`Undifferentiated Biomass`,OtherGramPos)
SoilBio0<-SoilBio %>% dplyr::select(`Actinomycetes Biomass`,`Gram Neg Biomass`,`Rhizobia Biomass`,`Arbuscular Mycorrhizal Biomass`,`Saprophytes Biomass`,`Protozoa Biomass`,`Protozoa Biomass`,`Undifferentiated Biomass`,OtherGramPos,`%Mg.Sat`,`%Ca.Sat`,`%K.Sat`,CEC,`Sodium(ppm Na)`,`Magnesium(ppm Mg)`,`Calcium(ppm Ca)`,`Sulfate(ppm S)`,`Potassium(ppm K)`,`Olsen(ppm P)`,`Nitrate(ppm N)`,Organic.Matter.LOI,`S.Salts(mmhom/cm)`,Soil.pH)
SGL<-cbind(SoilGoods,Lables)

S.pca1 <- princomp(SoilGoods,cor=TRUE,score=TRUE)
summary(S.pca1)

S.pcsCH<-cbind(SoilGoods,S.pca1$scores[,1:2])
cor(SoilGoods,S.pca1$scores[,1:2])
S.pca0 <- princomp(SoilBio0,cor=TRUE,score=TRUE)
summary(S.pca0)


par(mfrow=c(1,1))
plot(S.pca1,main="Eigenvalues for PCA on ENV",col="bisque",las=2)
abline(h=mean(S.pca1),col="blue")
legend("topright","Average Eigenvale",lwd=1,col=2,bty="n")

### THIS IS THE GOOD PLOT ###
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot::ggbiplot(S.pca1,obs.scale=1,var.scale=1,elipse=TRUE,groups=Lables$Site,center=TRUE,ellipse = TRUE, labels = Lables$Type,circle = TRUE,alpha=.8) + ggtitle("PCA Plot")+ theme_bw() + theme(legend.position="bottom") + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + PT1 +  expand_limits(y = c(-5, 4))+  expand_limits(x = c(-6, 8))
dev.copy(jpeg,"PLFApca.jpeg",width=20,height=20,units="cm",res=300)
dev.off()

