

 ### MISSION: REFORMATTING RAW LICOR EXCEL FILES ###
      ### ALL LICOR FILES MUST HAVE SAME NUMBER PTS IN EACH COLUNM ###
        ### BY WHICH I MEAN THE SAME NUMBER OF PHOTO OUT FOR EACH FILE ###

library(tidyverse)
library(readxl)
library(readr)
library(rlist)

### SITE 1 ###
    ### I GAVE EACH SITE ITS OWN WORKBOOK ###
      ### WITH A SET PATTERN FOR TAB LAYOUT ###


### PULLING TAB NAMES TO REFOMAT AS THE COLUMN NAMES ###

setwd("C:/Users/Mikaela/OneDrive/Documents/Masters Stuff")

mm <-"VOLRawLicorFix.xlsx"
tab_names<-excel_sheets(path=mm)

### PULL PHOTO DATA FROM EACH FILE WHICH IS IN "E" COLUMN ###

list_all<-lapply(tab_names,function(x){
  as.data.frame(read_excel("VOLRawLicorFix.xlsx", range = cell_cols("E"),sheet=x)) } )
   
### RENAME THE EXTRACED FILE LISTS BY SAMPLE ###

names(list_all)<-tab_names

### REMOVE EMPTY CELLS AND "OUT" FROM "PHOTO OUT" ###

list_all0<-lapply(list_all, function(x){ as.data.frame(x[!is.na(x)])})
list_all00<-lapply(list_all0, function(x){ as.data.frame(x[x != "out"])})

### BIND THE LISTS TOGETHER INTO ONE DATAFRAME AND RENAME COLUMNS ###

prettypls<-list.cbind(list_all00)
colnames(prettypls) = tab_names

### FOR METADATA I FLIPPED THE COLMNS TO ROWS AND SPLIT UP THE SAMPLE... ###
  ### ...NAMES WHICH WERE FORMATTED IN A SET WAY IN THE EXCEL FILE ###

flip0<-t(prettypls)
Site<-rep(c("VOL"),times=125)
Type<-rep(c("LA","DA","CL","PT","MO"), times = 25)
Time<-rep(c("half","two","six","twelve","twentyfour"), each = 5,times=5)
Rep<-rep(c("1","2","3","4","5"), each = 25)
VOL<-cbind(Site,Type,Time,Rep,flip0)
colnames(VOL)=c("Site","Type","Time","Rep","PAR0","PAR25","PAR50","PAR100","PAR150","PAR300","PAR500","PAR750","PAR1000","PAR1250","PAR1600","PAR2000")

### SITE 2 ###

mm <-"JERRawLicorFix.xlsx"
tab_names<-excel_sheets(path=mm)

list_all<-lapply(tab_names,function(x){
  as.data.frame(read_excel("JERRawLicorFix.xlsx", range = cell_cols("E"),sheet=x)) } )

names(list_all)<-tab_names

list_all0<-lapply(list_all, function(x){ as.data.frame(x[!is.na(x)])})
list_all00<-lapply(list_all0, function(x){ as.data.frame(x[x != "out"])})

prettypls<-list.cbind(list_all00)
colnames(prettypls) = tab_names

flip0<-t(prettypls)
Site<-rep(c("JER"),times=125)
Type<-rep(c("LA","DA","CL","PT","MO"), times = 25)
Time<-rep(c("half","two","six","twelve","twentyfour"), each = 5,times=5)
Rep<-rep(c("1","2","3","4","5"), each = 25)
JER<-cbind(Site,Type,Time,Rep,flip0)
colnames(JER)=c("Site","Type","Time","Rep","PAR0","PAR25","PAR50","PAR100","PAR150","PAR300","PAR500","PAR750","PAR1000","PAR1250","PAR1600","PAR2000")

### SITE 3 ###

mm <-"AMTRawLicorFix.xlsx"
tab_names<-excel_sheets(path=mm)

list_all<-lapply(tab_names,function(x){
  as.data.frame(read_excel("AMTRawLicorFix.xlsx", range = cell_cols("E"),sheet=x)) } )

names(list_all)<-tab_names

list_all0<-lapply(list_all, function(x){ as.data.frame(x[!is.na(x)])})
list_all00<-lapply(list_all0, function(x){ as.data.frame(x[x != "out"])})

prettypls<-list.cbind(list_all00)
colnames(prettypls) = tab_names

flip0<-t(prettypls)
Site<-rep(c("AMT"),times=125)
Type<-rep(c("LA","DA","CL","PT","MO"), times = 25)
Time<-rep(c("half","two","six","twelve","twentyfour"), each = 5,times=5)
Rep<-rep(c("1","2","3","4","5"), each = 25)
AMT<-cbind(Site,Type,Time,Rep,flip0)
colnames(AMT)=c("Site","Type","Time","Rep","PAR0","PAR25","PAR50","PAR100","PAR150","PAR300","PAR500","PAR750","PAR1000","PAR1250","PAR1600","PAR2000")

### SITE 4 ###

mm <-"WHSRawLicorFix.xlsx"
tab_names<-excel_sheets(path=mm)

list_all<-lapply(tab_names,function(x){
  as.data.frame(read_excel("WHSRawLicorFix.xlsx", range = cell_cols("E"),sheet=x)) } )

names(list_all)<-tab_names

list_all0<-lapply(list_all, function(x){ as.data.frame(x[!is.na(x)])})
list_all00<-lapply(list_all0, function(x){ as.data.frame(x[x != "out"])})

prettypls<-list.cbind(list_all00)
colnames(prettypls) = tab_names

flip0<-t(prettypls)
Site<-rep(c("WHS"),times=125)
Type<-rep(c("LA","DA","CL","PT","MO"), times = 25)
Time<-rep(c("half","two","six","twelve","twentyfour"), each = 5,times=5)
Rep<-rep(c("1","2","3","4","5"), each = 25)
WHS<-cbind(Site,Type,Time,Rep,flip0)
colnames(WHS)=c("Site","Type","Time","Rep","PAR0","PAR25","PAR50","PAR100","PAR150","PAR300","PAR500","PAR750","PAR1000","PAR1250","PAR1600","PAR2000")

AllSite<-rbind(AMT,JER,VOL,WHS)
AllSite<-as.data.frame(AllSite)
# MISSING VALUE #
AllSite0<- AllSite[AllSite[1,] != "JER_s_da6_2_2jul2020_",]

### PULL OUT THE DATA INTO CSV PILE ###

write_csv(AllSite,"ResponseCurvesStudy549.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### PULLING PHOTOSYNTHISIS INFO ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### PULL MAX AND MIN FOR NET AND RESP ### ### ###

Resp <- apply(AllSite[,5:16],1,min)
Resp<-cbind(AllSite[,1:4],Resp)
Net<-apply(AllSite[,5:16],1,max)
Net<-cbind(AllSite[,1:4],Net)

### ### ### PULL LIGHT SATURATION ### ### ###
   ### DEFINED AS PAR AT HIGHEST AMAX POINT ###

### PULL THE ROW NUMBER OF MAX FOR EACH COLUNM ###

witchROW<-apply(AllSite[,5:16],1,which.max)
witchROW<-as.data.frame(witchROW)

### LIGHT SATURATION FUNCTION ### 
  ### ROW NUMBER CORRISPONDS TO LIGHT LEVEL (PAR) ###

LightSat <- function(x){  if(x == 1) result <- 0
else if(x  == 2) result <- 25
else if(x  == 3) result <- 50
else if(x  == 4) result <- 100
else if(x  == 5) result <- 150
else if(x  == 6) result <- 300
else if(x  == 7) result <- 500
else if(x  == 8) result <- 750
else if(x  == 9) result <- 1000
else if(x  == 10) result <- 1250
else if(x == 11) result <- 1600
else result <- 2000
return(result)
}

LightSat<-apply(witchROW, 1, LightSat)
LightSat<-cbind(AllSite[,1:4],LightSat)

### ### ### QUANTUM YIELD ### ### ###
  ### AS THE INITIAL SLOPE OF LINE ###

AllSite$PAR0<-as.numeric(AllSite$PAR0)
AllSite$PAR25<-as.numeric(AllSite$PAR25)
AllSite$PAR50<-as.numeric(AllSite$PAR50)
AllSite$PAR100<-as.numeric(AllSite$PAR100)
AllSite$PAR150<-as.numeric(AllSite$PAR150)
AllSite$PAR300<-as.numeric(AllSite$PAR300)
AllSite$PAR500<-as.numeric(AllSite$PAR500)
AllSite$PAR750<-as.numeric(AllSite$PAR750)
AllSite$PAR1000<-as.numeric(AllSite$PAR1000)
AllSite$PAR1250<-as.numeric(AllSite$PAR1250)
AllSite$PAR1600<-as.numeric(AllSite$PAR1600)
AllSite$PAR2000<-as.numeric(AllSite$PAR2000)

### FINDING THE SLOPES BETWEEN THE 1ST 3 PTS ###
  ### WHILE AVOIDING DIPS IN THE DATA ###

NAs <-rep(NA,times=500)
Slope1<- (AllSite$PAR25-AllSite$PAR0)/(25-0)
Slope2<-(AllSite$PAR50-AllSite$PAR25)/(50-25)
Slope3<-(AllSite$PAR100-AllSite$PAR50)/(100-50)
# MEAN IS 0 TO 25 AND 0 TO 50 #
SlopeM<-(((AllSite$PAR25-AllSite$PAR0)/(25-0))+((AllSite$PAR50-AllSite$PAR0)/(50-0)))/2
SlopeM<-as.numeric(SlopeM)

QYcheck<-cbind(AllSite[1:4],Slope1,Slope2,Slope3,SlopeM,NAs)

QYcheck$Slope1<-as.numeric(QYcheck$Slope1)
QYcheck$Slope2<-as.numeric(QYcheck$Slope2)
QYcheck$Slope3<-as.numeric(QYcheck$Slope3)

# DEFINING THE SHAPE BETWEEN INITAL 4 POINTS #
PosPos<-filter(QYcheck, Slope1 > 0 & Slope2>0)
NegPos<-filter(QYcheck, Slope1 < 0 & Slope2>0)
PosNegNeg<-filter(QYcheck, Slope1>0 & Slope2<0 & Slope3>0)
PosNegPos<-filter(QYcheck, Slope1>0 & Slope2<0 & Slope3<0)
NegNeg<-filter(QYcheck, Slope1 < 0 & Slope2<0)
# NegNeg AND PosNegNeg ARE EXCLUDED BC DIPS #

QYpp<-PosPos %>% dplyr::select(Site,Type,Time,Rep,SlopeM)
colnames(QYpp) = c("Site","Type","Time","Rep","QY")
QYnp<-NegPos %>% dplyr::select(Site,Type,Time,Rep,Slope2)
colnames(QYnp) = c("Site","Type","Time","Rep","QY")
QYpnp<-PosNegPos %>% dplyr::select(Site,Type,Time,Rep,Slope3)
colnames(QYpnp) = c("Site","Type","Time","Rep","QY")
QYnn<-NegNeg %>% dplyr::select(Site,Type,Time,Rep,NAs)
colnames(QYnn) = c("Site","Type","Time","Rep","QY")
QYpnn<-PosNegNeg %>% dplyr::select(Site,Type,Time,Rep,NAs)
colnames(QYpnn) = c("Site","Type","Time","Rep","QY")

QY<-rbind(QYpp,QYnp,QYpnp,QYnn,QYpnn)

### ### ### CURVE SHAPE ### ### ###

# PARAMETERS FOR NORMAL CURVE #
AllSite$Minus<- AllSite$PAR500-abs(.15*(AllSite$PAR500))
AllSite$Plus<- AllSite$PAR500+abs(.15*(AllSite$PAR500))

# PAR0 IS HIGHEST MARK: DOWN LIKE A LINE, DROP LIKE CLIFF AND DOWN #
DownDown <- filter(AllSite, PAR0 > PAR150 & PAR0>PAR500 & PAR0>PAR2000 & PAR500>PAR2000)
Pool1 <- filter(AllSite, PAR0 < PAR150 | PAR0<PAR500 | PAR0<PAR2000 | PAR500<PAR2000)

# PAR0 IS HIGHEST MARK: DROPS DOWN AND SCOOPS UP BUT NOT ABOVE PAR0 #
DownDownUp <- filter(Pool1, PAR0 > PAR150 & PAR0>PAR500 & PAR0>PAR2000 & PAR500<PAR2000)
Pool2 <- filter(Pool1, PAR0 < PAR150 | PAR0<PAR500 | PAR0<PAR2000 | PAR500>PAR2000)

# LIKE A CHECK MARK WITH 2000 OVER PAR0 #
Scoop<-filter(Pool2, PAR0>PAR500 & PAR1000<PAR1600 & PAR1000<PAR2000 & PAR0<PAR2000)
Pool3<-filter(Pool2, PAR0<PAR500 | PAR1000>PAR1600 | PAR1000>PAR2000 | PAR0>PAR2000)

# PLATEAUS LIKE A TABLE AFTER ~500 #
Normal<-filter(Pool3, (PAR1000>=Minus & PAR1000<=Plus & PAR2000>=Minus & PAR2000<=Plus))
Pool4<-filter(Pool3, (PAR1000<Minus | PAR1000>Plus|PAR2000<Minus | PAR2000>Plus))

# CURVES UP THEN DIVES DOWN PRESUMABLY DUE TO PHOTODAMAGE #
UpDown <- filter(Pool4, PAR0 < PAR150 & PAR1000>PAR1250 & PAR1250>PAR1600 & PAR1600>PAR2000)
Pool5 <- filter(Pool4, PAR0 > PAR150 | PAR1000<PAR1250 | PAR1250<PAR1600 | PAR1600<PAR2000)

# CURVES UP AND DOESNT COME DOWN UNTIL LAST 2, 3, 4 POINT #
TipDown1<-filter(Pool5, PAR0 < PAR300 & PAR1250<PAR1600 & PAR1600>PAR2000)
Pool6<-filter(Pool5, PAR0 > PAR300 | PAR1250>PAR1600 | PAR1600<PAR2000)
TipDown2<-filter(Pool6, PAR0 < PAR300 & PAR1000<PAR1250 & PAR1250>PAR1600 & PAR1250>PAR2000)
Pool7<-filter(Pool6, PAR0 > PAR300 | PAR1000>PAR1250 | PAR1250<PAR1600 | PAR1250<PAR2000)
TipDown3<-filter(Pool7, PAR0 < PAR300 & PAR750<PAR1000 & PAR1000>PAR1250 & PAR1000>PAR1600 & PAR100>PAR2000)
Pool8<-filter(Pool7, PAR0 > PAR300 | PAR750>PAR1000 | PAR1000<PAR1250 | PAR1000<PAR1600 | PAR100<PAR2000)

# TIPS UP WITHOUT COMING DOWN #
UpUpUp <- filter(Pool8, PAR500<PAR1000 & PAR1000<PAR1600 & PAR1600<PAR2000)
Pool9 <- filter(Pool8, PAR500>PAR1000 | PAR1000>PAR1600 | PAR1600>PAR2000)

# SHAPED LIKE AN 'N' #
UpDownUp <- filter(Pool9, PAR500>PAR1000 & PAR1000<PAR1600 & PAR1600<PAR2000)
Pool10 <- filter(Pool9, PAR500<PAR1000 | PAR1000>PAR1600 | PAR1600>PAR2000)

# CLEAN UP MORE UPDOWN #
UpDownUp2 <- filter(Pool10, PAR1000>PAR1250 & PAR1250<PAR1600)
Pool11 <- filter(Pool10, PAR1000<PAR1250 | PAR1250>PAR1600)
UpDown2 <- filter(Pool11, PAR500>PAR1250 & PAR1000>PAR2000)
Pool12 <- filter(Pool11, PAR500<PAR1250 | PAR1000<PAR2000)
UpUpUp2 <- filter(Pool12, PAR300<PAR1000 & PAR1000<PAR2000)
Pool13 <- filter(Pool12, PAR300>PAR1000 | PAR1000>PAR2000)
TipDown4 <-filter(Pool13, PAR1250>PAR1600 & PAR1600>PAR2000)
# THIS IS WHATEVER'S LEFT BUT FOR ME IT WAS UPDOWNUP #
UpDownUp3 <-filter(Pool13, PAR1250<PAR1600 | PAR1600<PAR2000)

# ASSIGN DISCRIPTOR TO SHAPE #
DownDown$Shape<-rep("DownDown",length(DownDown$Site))
DownDownUp$Shape<-rep("DownDownUp",length(DownDownUp$Site))
Scoop$Shape<-rep("Scoop",length(Scoop$Site))
Normal$Shape<-rep("Normal",length(Normal$Site))
UpDown0<-rbind(UpDown,UpDown2)
UpDown0$Shape<-rep("UpDown",length(UpDown0$Site))
TipDown0<-rbind(TipDown1,TipDown2,TipDown3,TipDown4)
TipDown0$Shape<-rep("TipDown",length(TipDown0$Site))
UpUpUp0<-rbind(UpUpUp, UpUpUp2)
UpUpUp0$Shape<-rep("UpUpUp",length(UpUpUp0$Site))
UpDownUp0<-rbind(UpDownUp,UpDownUp2,UpDownUp3)
UpDownUp0$Shape<-rep("UpDownUp",length(UpDownUp0$Site))

AllShape<-rbind(DownDown,DownDownUp,Scoop,Normal,UpDown0,TipDown0,UpUpUp0,UpDownUp0)

### ### ### JOINING IT ALL TOGETHER ### ### ###

# MAKE SURE EVERYTHING LINES UP #
Resp<-as.data.frame(Resp)
Net<-as.data.frame(Net)
LightSatPt<-as.data.frame(LightSat)
QY<-as.data.frame(QY)
AllShape<-as.data.frame(AllShape)

Resp<-Resp %>% arrange(Site,Type,Time,Rep)
Net<-Net %>% arrange(Site,Type,Time,Rep)
QY<-QY %>% arrange(Site,Type,Time,Rep)
Shape<-AllShape %>% arrange(Site,Type,Time,Rep)
LightSatPt<-LightSatPt%>% arrange(Site,Type,Time,Rep)

# KNIT EVERYTHING TOGETHER #
TheValues<-cbind(Resp,Net,QY,LightSatPt,Shape)
TheValues0<- TheValues %>% dplyr::select(PAR0,PAR25,PAR50,PAR100,PAR150,PAR300,PAR500,PAR750,PAR1000,PAR1250,PAR1600,PAR2000,Resp,Net,QY,LightSat,Shape)
TheValues<-cbind(TheValues[,1:4],TheValues0)

# CALCULATE DIFFERENCE BTW NET AND RESP #
TheValues$Net<-as.numeric(TheValues$Net)
TheValues$Resp<-as.numeric(TheValues$Resp)
TheValues$Amax<-TheValues$Net-TheValues$Resp

### ### ### MISSION ACCOMPLISHED ### ### ###


write_csv(TheValues,"LicorPhotoValues.csv")

### ### ### YAY ### ### ###