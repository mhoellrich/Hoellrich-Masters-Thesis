


                         # # # SITE LPI STUFF # # #

### Tasks: 
 ### Chart to compare % total crust, % total plant, % total physical componenT ###
  ### Chart to compare % individual crust typeS ###


### Steps:

                      ### Organize and upload datA ###

### Uploaded Files: 

setwd("C:/Users/Mikaela/OneDrive/Documents/Masters Stuff/R Stuff")

library(tidyverse)

Transects <- read.csv("AllTheTransects0.csv", header = TRUE) 
  names(Transects)
  head(Transects)
  dim(Transects)
  Transects$Point = NULL
 
Amt1 <- Transects[1:150, ]
Amt2 <- Transects[151:300, ]
Vol1 <- Transects[301:450, ]
Vol2 <- Transects[451:600, ]
Jer1 <- Transects[601:750, ]
Jer2 <- Transects[751:900, ]
Whs1 <- Transects[901:1050, ]
Whs2 <- Transects[1051:1200, ]

Amt1$Site = NULL 
Amt2$Site <- NULL

Vol1$Site <- NULL
Vol2$Site <- NULL
 
Jer1$Site <- NULL
Jer2$Site <- NULL
  
Whs1$Site <- NULL
Whs2$Site <- NULL
  
  
### Classify catigories:

 ### % total plant: KR, AF, HL, UF1, LATR, MUPO, EP, UF1 2, UF2, 
         ### UF3, UF4, US1, US2, CC, TAR, UNK1, UNK2, UNK3, UNK4, 
            ### MESQ, US3, Nostoc,Cruc1, UKG1, UKG2, UNF1, Not Tar
  ### % total crust: Collema, Pelt, Clav, LAC, DAC, IC, M, LAC/PC, 
         ### DAC/PC, IC/PC, DAC/Clav, Pelt/PC, Clav/PC
   ### % total physical component: BG, R, G, PC, Gypsum
    ### % individual litter: HL, WL, EL
  
### Calculate % cover:
  
   ### Indivdidual plant coveR ###
    ### AMT1 ###
CountA1EP <- length(which(Amt1 == "EP"))
  CountA1EP
CountA1MUPO <- length(which(Amt1 == "MUPO"))
  CountA1MUPO
CountA1LATR <- length(which(Amt1 == "LATR"))
  CountA1LATR
CountA1CC <- length(which(Amt1 == "CC"))
  CountA1CC
CountA1US2 <- length(which(Amt1 == "US2"))
  CountA1US2
CountA1US1 <- length(which(Amt1 == "US1"))
  CountA1US1
CountA1UF4 <- length(which(Amt1 == "UF4"))
  CountA1UF4
CountA1UF3 <- length(which(Amt1 == "UF3"))
  CountA1UF3
CountA1UF2 <- length(which(Amt1 == "UF2"))
  CountA1UF2
CountA1UF12 <- length(which(Amt1 == "UF1 2"))
  CountA1UF12
CountA1UF1 <- length(which(Amt1 == "UF1"))
  CountA1UF1
CountA1KR <- length(which(Amt1 == "KR"))
  CountA1KR 
CountA1AF <- length(which(Amt1 == "AF"))
  CountA1AF
CountA1TAR <- length(which(Amt1 == "Tar"))
  CountA1TAR
CountA1Che <- length(which(Amt1 == "Cheese"))
  CountA1Che
CountA1UNK1 <- length(which(Amt1 == "UNk1"))
  CountA1UNK1
CountA1UNK2 <- length(which(Amt1 == "UNk2"))
  CountA1UNK2
CountA1UNK3 <- length(which(Amt1 == "UNk3"))
  CountA1UNK3
CountA1UNK4 <- length(which(Amt1 == "UNk4"))
  CountA1UNK4
CountA1MESQ <- length(which(Amt1 == "MESQ"))
  CountA1MESQ
CountA1US3 <- length(which(Amt1 == "US3"))
  CountA1US3
CountA1Nos <- length(which(Amt1 == "Nostoc"))
  CountA1Nos
    ### AMT2 ###
  ### Indivdidual plant coveR ###
CountA2EP <- length(which(Amt2 == "EP"))
  CountA2EP
CountA2MUPO <- length(which(Amt2 == "MUPO"))
  CountA2MUPO
CountA2LATR <- length(which(Amt2 == "LATR"))
  CountA2LATR
CountA2CC <- length(which(Amt2 == "CC"))
  CountA2CC
CountA2US2 <- length(which(Amt2 == "US2"))
  CountA2US2
CountA2US1 <- length(which(Amt2 == "US1"))
  CountA2US1
CountA2UF4 <- length(which(Amt2 == "UF4"))
  CountA2UF4
CountA2UF3 <- length(which(Amt2 == "UF3"))
  CountA2UF3
CountA2UF2 <- length(which(Amt2 == "UF2"))
  CountA2UF2
CountA2UF12 <- length(which(Amt2 == "UF1 2"))
  CountA2UF12
CountA2UF1 <- length(which(Amt2 == "UF1"))
  CountA2UF1
CountA2KR <- length(which(Amt2 == "KR"))
  CountA2KR 
CountA2AF <- length(which(Amt2 == "AF"))
  CountA2AF
CountA2TAR <- length(which(Amt2 == "Tar"))
  CountA2TAR
CountA2Che <- length(which(Amt2 == "Cheese"))
  CountA2Che
CountA2UNK1 <- length(which(Amt2 == "UNk1"))
  CountA2UNK1
CountA2UNK2 <- length(which(Amt2 == "UNk2"))
  CountA2UNK2
CountA2UNK3 <- length(which(Amt2 == "UNk3"))
  CountA2UNK3
CountA2UNK4 <- length(which(Amt2 == "UNk4"))
  CountA2UNK4
CountA2MESQ <- length(which(Amt2 == "MESQ"))
  CountA2MESQ
CountA2US3 <- length(which(Amt2 == "US3"))
  CountA2US3
CountA2Nos <- length(which(Amt2 == "Nostoc"))
  CountA2Nos
    ### VOL1 ###
  ### Indivdidual plant coveR ###
CountV1EP <- length(which(Vol1 == "EP"))
  CountV1EP
CountV1MUPO <- length(which(Vol1 == "MUPO"))
  CountV1MUPO
CountV1LATR <- length(which(Vol1 == "LATR"))
  CountV1LATR
CountV1CC <- length(which(Vol1 == "CC"))
  CountV1CC
CountV1US2 <- length(which(Vol1 == "US2"))
  CountV1US2
CountV1US1 <- length(which(Vol1 == "US1"))
  CountV1US1
CountV1UF4 <- length(which(Vol1 == "UF4"))
  CountV1UF4
CountV1UF3 <- length(which(Vol1 == "UF3"))
  CountV1UF3
CountV1UF2 <- length(which(Vol1 == "UF2"))
  CountV1UF2
CountV1UF12 <- length(which(Vol1 == "UF1 2"))
  CountV1UF12
CountV1UF1 <- length(which(Vol1 == "UF1"))
  CountV1UF1
CountV1KR <- length(which(Vol1 == "KR"))
  CountV1KR 
CountV1AF <- length(which(Vol1 == "AF"))
  CountV1AF
CountV1TAR <- length(which(Vol1 == "Tar"))
  CountV1TAR
CountV1Che <- length(which(Vol1 == "Cheese"))
  CountV1Che
CountV1UNK1 <- length(which(Vol1 == "UNk1"))
  CountV1UNK1
CountV1UNK2 <- length(which(Vol1 == "UNk2"))
  CountV1UNK2
CountV1UNK3 <- length(which(Vol1 == "UNk3"))
  CountV1UNK3
CountV1UNK4 <- length(which(Vol1 == "UNk4"))
  CountV1UNK4
CountV1MESQ <- length(which(Vol1 == "MESQ"))
  CountV1MESQ
CountV1US3 <- length(which(Vol1 == "US3"))
  CountV1US3
CountV1Nos <- length(which(Vol1 == "Nostoc"))
  CountV1Nos
    ### Vol2 ###
  ### Indivdidual plant coveR ###
CountV2EP <- length(which(Vol2 == "EP"))
  CountV2EP
CountV2MUPO <- length(which(Vol2 == "MUPO"))
  CountV2MUPO
CountV2LATR <- length(which(Vol2 == "LATR"))
  CountV2LATR
CountV2CC <- length(which(Vol2 == "CC"))
  CountV2CC
CountV2US2 <- length(which(Vol2 == "US2"))
  CountV2US2
CountV2US1 <- length(which(Vol2 == "US1"))
  CountV2US1
CountV2UF4 <- length(which(Vol2 == "UF4"))
  CountV2UF4
CountV2UF3 <- length(which(Vol2 == "UF3"))
  CountV2UF3
CountV2UF2 <- length(which(Vol2 == "UF2"))
  CountV2UF2
CountV2UF12 <- length(which(Vol2 == "UF1 2"))
  CountV2UF12
CountV2UF1 <- length(which(Vol2 == "UF1"))
  CountV2UF1
CountV2KR <- length(which(Vol2 == "KR"))
  CountV2KR 
CountV2AF <- length(which(Vol2 == "AF"))
  CountV2AF
CountV2TAR <- length(which(Vol2 == "Tar"))
  CountV2TAR
CountV2Che <- length(which(Vol2 == "Cheese"))
  CountV2Che
CountV2UNK1 <- length(which(Vol2 == "UNk1"))
  CountV2UNK1
CountV2UNK2 <- length(which(Vol2 == "UNk2"))
  CountV2UNK2
CountV2UNK3 <- length(which(Vol2 == "UNk3"))
  CountV2UNK3
CountV2UNK4 <- length(which(Vol2 == "UNk4"))
  CountV2UNK4
CountV2MESQ <- length(which(Vol2 == "MESQ"))
  CountV2MESQ
CountV2US3 <- length(which(Vol2 == "US3"))
  CountV2US3
CountV2Nos <- length(which(Vol2 == "Nostoc"))
  CountV2Nos
  ### Jer1 ###
  ### Individaul plant coveR ###
CountJ1EP <- length(which(Jer1 == "EP"))
  CountJ1EP
CountJ1MUPO <- length(which(Jer1 == "MUPO"))
  CountJ1MUPO
CountJ1LATR <- length(which(Jer1 == "LATR"))
  CountJ1LATR
CountJ1CC <- length(which(Jer1 == "CC"))
  CountJ1CC
CountJ1US2 <- length(which(Jer1 == "US2"))
  CountJ1US2
CountJ1US1 <- length(which(Jer1 == "US1"))
  CountJ1US1
CountJ1UF4 <- length(which(Jer1 == "UF4"))
  CountJ1UF4
CountJ1UF3 <- length(which(Jer1 == "UF3"))
  CountJ1UF3
CountJ1UF2 <- length(which(Jer1 == "UF2"))
  CountJ1UF2
CountJ1UF12 <- length(which(Jer1 == "UF1 2"))
  CountJ1UF12
CountJ1UF1 <- length(which(Jer1 == "UF1"))
  CountJ1UF1
CountJ1KR <- length(which(Jer1 == "KR"))
  CountJ1KR 
CountJ1AF <- length(which(Jer1 == "AF"))
  CountJ1AF
CountJ1TAR <- length(which(Jer1 == "Tar"))
  CountJ1TAR
CountJ1Che <- length(which(Jer1 == "Cheese"))
  CountJ1Che
CountJ1UNK1 <- length(which(Jer1 == "UNk1"))
  CountJ1UNK1
CountJ1UNK2 <- length(which(Jer1 == "UNk2"))
  CountJ1UNK2
CountJ1UNK3 <- length(which(Jer1 == "UNk3"))
  CountJ1UNK3
CountJ1UNK4 <- length(which(Jer1 == "UNk4"))
  CountJ1UNK4
CountJ1MESQ <- length(which(Jer1 == "MESQ"))
  CountJ1MESQ
CountJ1US3 <- length(which(Jer1 == "US3"))
  CountJ1US3
CountJ1Nos <- length(which(Jer1 == "Nostoc"))
  CountJ1Nos
  ### Jer2 ###
  ### Individaul plant coveR ###
CountJ2EP <- length(which(Jer2 == "EP"))
  CountJ2EP
CountJ2MUPO <- length(which(Jer2 == "MUPO"))
  CountJ2MUPO
CountJ2LATR <- length(which(Jer2 == "LATR"))
  CountJ2LATR
CountJ2CC <- length(which(Jer2 == "CC"))
  CountJ2CC
CountJ2US2 <- length(which(Jer2 == "US2"))
  CountJ2US2
CountJ2US1 <- length(which(Jer2 == "US1"))
  CountJ2US1
CountJ2UF4 <- length(which(Jer2 == "UF4"))
  CountJ2UF4
CountJ2UF3 <- length(which(Jer2 == "UF3"))
  CountJ2UF3
CountJ2UF2 <- length(which(Jer2 == "UF2"))
  CountJ2UF2
CountJ2UF12 <- length(which(Jer2 == "UF1 2"))
  CountJ2UF12
CountJ2UF1 <- length(which(Jer2 == "UF1"))
  CountJ2UF1
CountJ2KR <- length(which(Jer2 == "KR"))
  CountJ2KR 
CountJ2AF <- length(which(Jer2 == "AF"))
  CountJ2AF
CountJ2TAR <- length(which(Jer2 == "Tar"))
  CountJ2TAR
CountJ2Che <- length(which(Jer2 == "Cheese"))
  CountJ2Che
CountJ2UNK1 <- length(which(Jer2 == "UNk1"))
  CountJ2UNK1
CountJ2UNK2 <- length(which(Jer2 == "UNk2"))
  CountJ2UNK2
CountJ2UNK3 <- length(which(Jer2 == "UNk3"))
  CountJ2UNK3
CountJ2UNK4 <- length(which(Jer2 == "UNk4"))
  CountJ2UNK4
CountJ2MESQ <- length(which(Jer2 == "MESQ"))
  CountJ2MESQ
CountJ2US3 <- length(which(Jer2 == "US3"))
  CountJ2US3
CountJ2Nos <- length(which(Jer2 == "Nostoc"))
  CountJ2Nos
  ### Whs1 ###
  ### Individaul plant coveR ###
CountW1EP <- length(which(Whs1 == "EP"))
  CountW1EP
CountW1MUPO <- length(which(Whs1 == "MUPO"))
  CountW1MUPO
CountW1LATR <- length(which(Whs1 == "LATR"))
  CountW1LATR
CountW1CC <- length(which(Whs1 == "CC"))
  CountW1CC
CountW1US2 <- length(which(Whs1 == "US2"))
  CountW1US2
CountW1US1 <- length(which(Whs1 == "US1"))
  CountW1US1
CountW1UF4 <- length(which(Whs1 == "UF4"))
  CountW1UF4
CountW1UF3 <- length(which(Whs1 == "UF3"))
  CountW1UF3
CountW1UF2 <- length(which(Whs1 == "UF2"))
  CountW1UF2
CountW1UF12 <- length(which(Whs1 == "UF1 2"))
  CountW1UF12
CountW1UF1 <- length(which(Whs1 == "UF1"))
  CountW1UF1
CountW1KR <- length(which(Whs1 == "KR"))
  CountW1KR 
CountW1AF <- length(which(Whs1 == "AF"))
  CountW1AF
CountW1TAR <- length(which(Whs1 == "Tar"))
  CountW1TAR
CountW1Che <- length(which(Whs1 == "Cheese"))
  CountW1Che
CountW1UNK1 <- length(which(Whs1 == "UNk1"))
  CountW1UNK1
CountW1UNK2 <- length(which(Whs1 == "UNk2"))
  CountW1UNK2
CountW1UNK3 <- length(which(Whs1 == "UNk3"))
  CountW1UNK3
CountW1UNK4 <- length(which(Whs1 == "UNk4"))
  CountW1UNK4
CountW1MESQ <- length(which(Whs1 == "MESQ"))
  CountW1MESQ
CountW1US3 <- length(which(Whs1 == "US3"))
  CountW1US3
CountW1Nos <- length(which(Whs1 == "Nostoc"))
  CountW1Nos
CountW1UKG1 <- length(which(Whs1 == "UKG1"))
  CountW1UKG1
CountW1UKG2 <- length(which(Whs1 == "UKG2"))
  CountW1UKG2
CountW1NotTar <- length(which(Whs1 == "Not Tar"))
  CountW1NotTar
CountW1UNF1 <- length(which(Whs1 == "UNF1"))
  CountW1UNF1
CountW1Cruc1<- length(which(Whs1 == "Cruc1"))
  CountW1Cruc1
  ### Whs2 ###
  ### Individaul plant coveR ###
CountW2EP <- length(which(Whs2 == "EP"))
  CountW2EP
CountW2MUPO <- length(which(Whs2 == "MUPO"))
  CountW2MUPO
CountW2LATR <- length(which(Whs2 == "LATR"))
  CountW2LATR
CountW2CC <- length(which(Whs2 == "CC"))
  CountW2CC
CountW2US2 <- length(which(Whs2 == "US2"))
  CountW2US2
CountW2US1 <- length(which(Whs2 == "US1"))
  CountW2US1
CountW2UF4 <- length(which(Whs2 == "UF4"))
  CountW2UF4
CountW2UF3 <- length(which(Whs2 == "UF3"))
  CountW2UF3
CountW2UF2 <- length(which(Whs2 == "UF2"))
  CountW2UF2
CountW2UF12 <- length(which(Whs2 == "UF1 2"))
  CountW2UF12
CountW2UF1 <- length(which(Whs2 == "UF1"))
  CountW2UF1
CountW2KR <- length(which(Whs2 == "KR"))
  CountW2KR 
CountW2AF <- length(which(Whs2 == "AF"))
  CountW2AF
CountW2TAR <- length(which(Whs2 == "Tar"))
  CountW2TAR
CountW2Che <- length(which(Whs2 == "Cheese"))
  CountW2Che
CountW2UNK1 <- length(which(Whs2 == "UNk1"))
  CountW2UNK1
CountW2UNK2 <- length(which(Whs2 == "UNk2"))
  CountW2UNK2
CountW2UNK3 <- length(which(Whs2 == "UNk3"))
  CountW2UNK3
CountW2UNK4 <- length(which(Whs2 == "UNk4"))
  CountW2UNK4
CountW2MESQ <- length(which(Whs2 == "MESQ"))
  CountW2MESQ
CountW2US3 <- length(which(Whs2 == "US3"))
  CountW2US3
CountW2Nos <- length(which(Whs2 == "Nostoc"))
  CountW2Nos
CountW2UKG1 <- length(which(Whs2 == "UKG1"))
  CountW2UKG1
CountW2UKG2 <- length(which(Whs2 == "UKG2"))
  CountW2UKG2
CountW2NotTar <- length(which(Whs2 == "Not Tar"))
  CountW2NotTar
CountW2UNF1 <- length(which(Whs2 == "UNF1"))
  CountW2UNF1
CountW2Cruc1<- length(which(Whs2 == "Cruc1"))
  CountW2Cruc1
  
  ### Amt1 ###
  ### Individaul crust coveR ###
CountA1Col <- length(which(Amt1 == "Collema"))
  CountA1Col
CountA1Pt <- length(which(Amt1 == "Pelt"))
  CountA1Pt
CountA1Cl <- length(which(Amt1 == "Clav"))
  CountA1Cl
CountA1LA <- length(which(Amt1 == "LAC"))
  CountA1LA
CountA1DA <- length(which(Amt1 == "DAC"))
  CountA1DA
CountA1IC <- length(which(Amt1 == "IC"))
  CountA1IC
CountA1M <- length(which(Amt1 == "M"))
  CountA1M
CountA1LPC <- length(which(Amt1 == "LAC/PC"))
  CountA1LPC
CountA1DPC <- length(which(Amt1 == "DAC/PC"))
  CountA1DPC
CountA1IPC <- length(which(Amt1 == "IC/PC"))
  CountA1IPC
CountA1DCl <- length(which(Amt1 == "DAC/Clav"))
  CountA1DCl
  CountA1CPC <- length(which(Amt1 == "Clav/PC"))
  CountA1CPC
CountA1PPC <- length(which(Amt1 == "Pelt/PC"))
  CountA1PPC
  
  ### Amt2 ###
   ### Individaul crust coveR ###
CountA2Col <- length(which(Amt2 == "Collema"))
  CountA2Col
CountA2Pt <- length(which(Amt2 == "Pelt"))
  CountA2Pt
CountA2Cl <- length(which(Amt2 == "Clav"))
  CountA2Cl
CountA2LA <- length(which(Amt2 == "LAC"))
  CountA2LA
CountA2DA <- length(which(Amt2 == "DAC"))
  CountA2DA
CountA2IC <- length(which(Amt2 == "IC"))
  CountA2IC
CountA2M <- length(which(Amt2 == "M"))
  CountA2M
CountA2LPC <- length(which(Amt2 == "LAC/PC"))
  CountA2LPC
CountA2DPC <- length(which(Amt2 == "DAC/PC"))
  CountA2DPC
CountA2IPC <- length(which(Amt2 == "IC/PC"))
  CountA2IPC
CountA2DCl <- length(which(Amt2 == "DAC/Clav"))
  CountA2DCl
  CountA2CPC <- length(which(Amt2 == "Clav/PC"))
  CountA2CPC
CountA2PPC <- length(which(Amt2 == "Pelt/PC"))
  CountA2PPC
  
  ### Vol1 ###
   ### Individaul crust coveR ###
CountV1Col <- length(which(Vol1 == "Collema"))
  CountV1Col
CountV1Pt <- length(which(Vol1 == "Pelt"))
  CountV1Pt
CountV1Cl <- length(which(Vol1 == "Clav"))
  CountV1Cl
CountV1LA <- length(which(Vol1 == "LAC"))
  CountV1LA
CountV1DA <- length(which(Vol1 == "DAC"))
  CountV1DA
CountV1IC <- length(which(Vol1 == "IC"))
  CountV1IC
CountV1M <- length(which(Vol1 == "M"))
  CountV1M
CountV1LPC <- length(which(Vol1 == "LAC/PC"))
  CountV1LPC
CountV1DPC <- length(which(Vol1 == "DAC/PC"))
  CountV1DPC
CountV1IPC <- length(which(Vol1 == "IC/PC"))
  CountV1IPC
CountV1DCl <- length(which(Vol1 == "DAC/Clav"))
  CountV1DCl
  CountV1CPC <- length(which(Vol1 == "Clav/PC"))
  CountV1CPC
CountV1PPC <- length(which(Vol1 == "Pelt/PC"))
  CountV1PPC
  
  ### Vol2 ###
   ### Individaul crust coveR ###
CountV2Col <- length(which(Vol2 == "Collema"))
  CountV2Col
CountV2Pt <- length(which(Vol2 == "Pelt"))
  CountV2Pt
CountV2Cl <- length(which(Vol2 == "Clav"))
  CountV2Cl
CountV2LA <- length(which(Vol2 == "LAC"))
  CountV2LA
CountV2DA <- length(which(Vol2 == "DAC"))
  CountV2DA
CountV2IC <- length(which(Vol2 == "IC"))
  CountV2IC
CountV2M <- length(which(Vol2 == "M"))
  CountV2M
CountV2LPC <- length(which(Vol2 == "LAC/PC"))
  CountV2LPC
CountV2DPC <- length(which(Vol2 == "DAC/PC"))
  CountV2DPC
CountV2IPC <- length(which(Vol2 == "IC/PC"))
  CountV2IPC
CountV2DCl <- length(which(Vol2 == "DAC/Clav"))
  CountV2DCl
  CountV2CPC <- length(which(Vol2 == "Clav/PC"))
  CountV2CPC
CountV2PPC <- length(which(Vol2 == "Pelt/PC"))
  CountV2PPC
  
  ### Jer1 ###
   ### Individual crust coveR ###
CountJ1Col <- length(which(Jer1 == "Collema"))
  CountJ1Col
CountJ1Pt1 <- length(which(Jer1 == "Pelt"))
  CountJ1Pt1
CountJ1Cl1 <- length(which(Jer1 == "Clav"))
  CountJ1Cl1
CountJ1LA1 <- length(which(Jer1 == "LAC"))
  CountJ1LA1
CountJ1DA1 <- length(which(Jer1 == "DAC"))
  CountJ1DA1
CountJ1IC1 <- length(which(Jer1 == "IC"))
  CountJ1IC1
CountJ1M <- length(which(Jer1 == "M"))
  CountJ1M
CountJ1LPC <- length(which(Jer1 == "LAC/PC"))
  CountJ1LPC
CountJ1DPC <- length(which(Jer1 == "DAC/PC"))
  CountJ1DPC
CountJ1IPC <- length(which(Jer1 == "IC/PC"))
  CountJ1IPC
CountJ1DCl <- length(which(Jer1 == "DAC/Clav"))
  CountJ1DCl
CountJ1PPC <- length(which(Jer1 == "Pelt/PC"))
  CountJ1PPC
  CountJ1CPC <- length(which(Jer1 == "Clav/PC"))
  CountJ1CPC
  
       ### Seperate physical and crusT ###

CountJ1Cl <- (CountJ1Cl1 + CountJ1DCl)
CountJ1Cl

  ### Jer2 ###
   ### Individual crust coveR ###
CountJ2Col <- length(which(Jer2 == "Collema"))
  CountJ2Col
CountJ2Pt1 <- length(which(Jer2 == "Pelt"))
  CountJ2Pt1
CountJ2Cl1 <- length(which(Jer2 == "Clav"))
  CountJ2Cl1
CountJ2LA1 <- length(which(Jer2 == "LAC"))
  CountJ2LA1
CountJ2DA1 <- length(which(Jer2 == "DAC"))
  CountJ2DA1
CountJ2IC1 <- length(which(Jer2 == "IC"))
  CountJ2IC1
CountJ2M <- length(which(Jer2 == "M"))
  CountJ2M
CountJ2LPC <- length(which(Jer2 == "LAC/PC"))
  CountJ2LPC
CountJ2DPC <- length(which(Jer2 == "DAC/PC"))
  CountJ2DPC
CountJ2IPC <- length(which(Jer2 == "IC/PC"))
  CountJ2IPC
CountJ2DCl <- length(which(Jer2 == "DAC/Clav"))
  CountJ2DCl
CountJ2CPC <- length(which(Jer2 == "Clav/PC"))
  CountJ2CPC
CountJ2PPC <- length(which(Jer2 == "Pelt/PC"))
  CountJ2PPC
  
  ### Seperate physical and crusT ###

CountJ2Cl <- (CountJ2Cl1 + CountJ2DCl)
  CountJ2Cl

  ### Whs1 ###
   ### Individual crust coveR ###
CountW1Col <- length(which(Whs1 == "Collema"))
  CountW1Col
CountW1Pt1 <- length(which(Whs1 == "Pelt"))
  CountW1Pt1
CountW1Cl1 <- length(which(Whs1 == "Clav"))
  CountW1Cl1
CountW1LA1 <- length(which(Whs1 == "LAC"))
  CountW1LA1
CountW1DA1 <- length(which(Whs1 == "DAC"))
  CountW1DA1
CountW1IC1 <- length(which(Whs1 == "IC"))
  CountW1IC1
CountW1M <- length(which(Whs1 == "M"))
  CountW1M
CountW1LPC <- length(which(Whs1 == "LAC/PC"))
  CountW1LPC
CountW1DPC <- length(which(Whs1 == "DAC/PC"))
  CountW1DPC
CountW1IPC <- length(which(Whs1 == "IC/PC"))
  CountW1IPC
CountW1DCl <- length(which(Whs1 == "DAC/Clav"))
  CountW1DCl
CountW1PPC <- length(which(Whs1 == "Pelt/PC"))
  CountW1PPC
CountW1CPC <- length(which(Whs1 == "Clav/PC"))
  CountW1CPC
  ### Seperate physical and crusT ###

CountW1Cl <- (CountW1Cl1 + CountW1DCl)
  CountW1Cl

  ### Whs2 ###
   ### Individual crust coveR ###
CountW2Col <- length(which(Whs2 == "Collema"))
  CountW2Col
CountW2Pt1 <- length(which(Whs2 == "Pelt"))
  CountW2Pt1
CountW2Cl1 <- length(which(Whs2 == "Clav"))
  CountW2Cl1
CountW2LA1 <- length(which(Whs2 == "LAC"))
  CountW2LA1
CountW2DA1 <- length(which(Whs2 == "DAC"))
  CountW2DA1
CountW2IC1 <- length(which(Whs2 == "IC"))
  CountW2IC1
CountW2M <- length(which(Whs2 == "M"))
  CountW2M
CountW2LPC <- length(which(Whs2 == "LAC/PC"))
  CountW2LPC
CountW2DPC <- length(which(Whs2 == "DAC/PC"))
  CountW2DPC
CountW2IPC <- length(which(Whs2 == "IC/PC"))
  CountW2IPC
CountW2DCl <- length(which(Whs2 == "DAC/Clav"))
  CountW2DCl
CountW2PPC <- length(which(Whs2 == "Pelt/PC"))
  CountW2PPC
CountW2CPC <- length(which(Whs2 == "Clav/PC"))
  CountW2CPC
  ### Seperate physical and crusT ###

CountW2Cl <- (CountW2Cl1 + CountW2DCl)
  CountW2Cl

  
  ## Amt1 ###
   ### Individaul physical componenT ###
CountA1BG <- length(which(Amt1 == "BG"))
  CountA1BG         
CountA1R <- length(which(Amt1 == "R"))
  CountA1R
CountA1G <- length(which(Amt1 == "G"))
  CountA1G
CountA1PC <- length(which(Amt1 == "PC"))
  CountA1PC
  ## Amt2 ###
   ### Individaul physical componenT ###
CountA2BG <- length(which(Amt2 == "BG"))
  CountA2BG         
CountA2R <- length(which(Amt2 == "R"))
  CountA2R
CountA2G <- length(which(Amt2 == "G"))
  CountA2G
CountA2PC <- length(which(Amt2 == "PC"))
  CountA2PC
  ## Vol1 ###
   ### Individaul physical componenT ###
CountV1BG <- length(which(Vol1 == "BG"))
  CountV1BG         
CountV1R <- length(which(Vol1 == "R"))
  CountV1R
CountV1G <- length(which(Vol1 == "G"))
  CountV1G
CountV1PC <- length(which(Vol1 == "PC"))
  CountV1PC
  ## Vol2 ###
   ### Individaul physical componenT ###
CountV2BG <- length(which(Vol2 == "BG"))
  CountV2BG         
CountV2R <- length(which(Vol2 == "R"))
  CountV2R
CountV2G <- length(which(Vol2 == "G"))
  CountV2G
CountV2PC <- length(which(Vol2 == "PC"))
  CountV2PC
  ## Jer1 ###
   ### Individaul physical componenT ###
CountJ1BG <- length(which(Jer1 == "BG"))
  CountJ1BG         
CountJ1R <- length(which(Jer1 == "R"))
  CountJ1R
CountJ1G <- length(which(Jer1 == "G"))
  CountJ1G
CountJ1PC1 <- length(which(Jer1 == "PC"))
  CountJ1PC1
  
  
  ## Jer2 ###
   ### Individaul physical componenT ###
CountJ2BG <- length(which(Jer2 == "BG"))
  CountJ2BG         
CountJ2R <- length(which(Jer2 == "R"))
  CountJ2R
CountJ2G <- length(which(Jer2 == "G"))
  CountJ2G
CountJ2PC1 <- length(which(Jer2 == "PC"))
  CountJ2PC1
  
  
  ## Whs1 ###
   ### Individaul physical componenT ###
CountW1BG <- length(which(Whs1 == "BG"))
  CountW1BG         
CountW1R <- length(which(Whs1 == "R"))
  CountW1R
CountW1G <- length(which(Whs1 == "G"))
  CountW1G
CountW1PC1 <- length(which(Whs1 == "PC"))
  CountW1PC1
  CountW1Gy <- length(which(Whs1 == "Gypsum"))
  CountW1Gy
  

  
  ## Whs2 ###
   ### Individaul physical componenT ###
CountW2BG <- length(which(Whs2 == "BG"))
  CountW2BG         
CountW2R <- length(which(Whs2 == "R"))
  CountW2R
CountW2G <- length(which(Whs2 == "G"))
  CountW2G
CountW2PC1 <- length(which(Whs2 == "PC"))
  CountW2PC1
CountW2Gy <- length(which(Whs2 == "Gypsum"))
  CountW2Gy
  
  
   ### Iindivaiual litteR ###
CountV1HL <- length(which(Vol1 == "HL"))
  CountV1HL
CountV1WL <- length(which(Vol1 == "WL"))
  CountV1WL
CountV1EL <- length(which(Vol1 == "EL"))
  CountV1EL
  
    ### Total plant CoveR ###
PlantCovRawA1 <- sum(CountA1AF, CountA1CC, CountA1EP, CountA1MUPO,
       CountA1LATR, CountA1US2, CountA1US1, CountA1UF4, CountA1UF3,
        CountA1UF2, CountA1UF12, CountA1UF1, CountA1KR, CountA1AF, 
         CountA1TAR, CountA1Che, CountA1UNK1, CountA1UNK2, 
          CountA1UNK3, CountA1UNK4, CountA1MESQ, CountA1US3, CountA1Nos)
  PlantCovRawA1
  
PlantCovRawA2 <- sum(CountA2AF, CountA2CC, CountA2EP, CountA2MUPO,
        CountA2LATR, CountA2US2, CountA2US1, CountA2UF4, CountA2UF3,
              CountA2UF2, CountA2UF12, CountA2UF1, CountA2KR, CountA2AF, 
                CountA2TAR, CountA2Che, CountA2UNK1, CountA2UNK2, CountA2UNK3, 
                  CountA2UNK4, CountA2MESQ, CountA2US3, CountA2Nos)
  PlantCovRawA2
  
PlantCovRawV1 <- sum(CountV1AF, CountV1CC, CountV1EP, CountV1MUPO,
       CountV1LATR, CountV1US2, CountV1US1, CountV1UF4, CountV1UF3,
              CountV1UF2, CountV1UF12, CountV1UF1, CountV1KR, CountV1AF, 
                CountV1TAR, CountV1Che, CountV1UNK1, CountV1UNK2, 
                  CountV1UNK3, CountV1UNK4, CountV1MESQ, CountV1US3, CountV1Nos)
  PlantCovRawV1
  
PlantCovRawV2 <- sum(CountV2AF, CountV2CC, CountV2EP, CountV2MUPO,
        CountV2LATR, CountV2US2, CountV2US1, CountV2UF4, CountV2UF3,
               CountV2UF2, CountV2UF12, CountV2UF1, CountV2KR, CountV2AF, 
                CountV2TAR, CountV2Che, CountV2UNK1, CountV2UNK2, 
                  CountV2UNK3, CountV2UNK4, CountV2MESQ, CountV2US3, CountV2Nos)
  PlantCovRawV2
  
PlantCovRawJ1 <- sum(CountJ1AF, CountJ1CC, CountJ1EP, CountJ1MUPO,
                CountJ1LATR, CountJ1US2, CountJ1US1, CountJ1UF4, CountJ1UF3,
                  CountJ1UF2, CountJ1UF12, CountJ1UF1, CountJ1KR, CountJ1AF, 
                    CountJ1TAR, CountJ1Che, CountJ1UNK1, CountJ1UNK2, 
                      CountJ1UNK3, CountJ1UNK4, CountJ1MESQ, CountJ1US3, CountJ1Nos)
  PlantCovRawJ1
  
PlantCovRawJ2 <- sum(CountJ2AF, CountJ2CC, CountJ2EP, CountJ2MUPO,
                CountJ2LATR, CountJ2US2, CountJ2US1, CountJ2UF4, CountJ2UF3,
                  CountJ2UF2, CountJ2UF12, CountJ2UF1, CountJ2KR, CountJ2AF, 
                    CountJ2TAR, CountJ2Che, CountJ2UNK1, CountJ2UNK2, 
                      CountJ2UNK3, CountJ2UNK4, CountJ2MESQ, CountJ2US3, CountJ2Nos)
  PlantCovRawJ2
  
PlantCovRawW1 <- sum(CountW1AF, CountW1CC, CountW1EP, CountW1MUPO,
                 CountW1LATR, CountW1US2, CountW1US1, CountW1UF4, CountW1UF3,
                   CountW1UF2, CountW1UF12, CountW1UF1, CountW1KR, CountW1AF, 
                     CountW1TAR, CountW1Che, CountW1UNK1, CountW1UNK2, 
                       CountW1UNK3, CountW1UNK4, CountW1MESQ, CountW1US3, CountW1Nos,
                       CountW1UKG1, CountW1UKG2, CountW1UNF1, CountW1Cruc1, CountW1NotTar)
  PlantCovRawW1
  
PlantCovRawW2 <- sum(CountW2AF, CountW2CC, CountW2EP, CountW2MUPO,
                CountW2LATR, CountW2US2, CountW2US1, CountW2UF4, CountW2UF3,
                  CountW2UF2, CountW2UF12, CountW2UF1, CountW2KR, CountW2AF, 
                    CountW2TAR, CountW2Che, CountW2UNK1, CountW2UNK2, 
                       CountW2UNK3, CountW2UNK4, CountW2MESQ, CountW2US3, CountW2Nos,
                          CountW2UKG1, CountW2UKG2, CountW2UNF1, CountW2Cruc1, CountW2NotTar)
  PlantCovRawW2
  
  ### Total averageS ###
AvgPlantRawA <- mean(c(PlantCovRawA1, PlantCovRawA2))
  AvgPlantRawA
  
AvgPlantRawV <- mean(c(PlantCovRawV1, PlantCovRawV2))
  AvgPlantRawV
  
AvgPlantRawJ <- mean(c(PlantCovRawJ1, PlantCovRawJ2))
  AvgPlantRawJ
  
AvgPlantRawW <- mean(c(PlantCovRawW1, PlantCovRawW2))
  AvgPlantRawW
  
    ### Plant percent coveR ###
PlantCovA <- (AvgPlantRawA/150)*100
  PlantCovA
  
PlantCovV <- (AvgPlantRawV/150)*100
  PlantCovV
  
PlantCovJ <- (AvgPlantRawJ/150)*100
  PlantCovJ
  
PlantCovW <- (AvgPlantRawW/150)*100
  PlantCovW
  
PlantCovWg <- ((5/150)*100)

   ### Crust percent coveR ###
CrustCovRawA1 <- sum(CountA1Col, CountA1Pt, CountA1Cl, CountA1LA, 
        CountA1DA, CountA1IC, CountA1M)
  CrustCovRawA1
  
CrustCovRawA2 <- sum(CountA2Col, CountA2Pt, CountA2Cl, CountA2LA, 
        CountA2DA, CountA2IC, CountA2M)
  CrustCovRawA2
  
CrustCovRawV1 <- sum(CountV1Col, CountV1Pt, CountV1Cl, CountV1LA, 
       CountV1DA, CountV1IC, CountV1M)
  CrustCovRawV1
  
CrustCovRawV2 <- sum(CountV2Col, CountV2Pt, CountV2Cl, CountV2LA, 
       CountV2DA, CountV2IC, CountV2M)
  CrustCovRawV2
  
CrustCovRawJ1 <- sum(CountJ1Col, CountJ1Pt1, CountJ1Cl, CountJ1LA1, 
      CountJ1DA1, CountJ1IC1, CountJ1M)
  CrustCovRawJ1
  
CrustCovRawJ2 <- sum(CountJ2Col, CountJ2Pt1, CountJ2Cl, CountJ2LA1, 
      CountJ2DA1, CountJ2IC1, CountJ2M)
  CrustCovRawJ2
  
CrustCovRawW1 <- sum(CountW1Col, CountW1Pt1, CountW1Cl, CountW1LA1, 
                       CountW1DA1, CountW1IC1, CountW1M)
  CrustCovRawW1
  
CrustCovRawW2 <- sum(CountW2Col, CountW2Pt1, CountW2Cl, CountW2LA1, 
                       CountW2DA1, CountW2IC1, CountW2M)
  CrustCovRawW2
  
  
  ### Mixed percent coveR ###
  
MixedCovRawA1 <- sum(CountA1LPC, CountA1DPC, CountA1IPC, CountA1PPC)
MixedCovRawA2 <- sum(CountA2LPC, CountA2DPC, CountA2IPC, CountA2PPC)

MixedCovRawJ1 <- sum(CountJ1LPC, CountJ1DPC, CountJ1IPC, CountJ1PPC)
MixedCovRawJ2 <- sum(CountJ2LPC, CountJ2DPC, CountJ2IPC, CountJ2PPC)

MixedCovRawV1 <- sum(CountV1LPC, CountV1DPC, CountV1IPC, CountV1PPC)
MixedCovRawV2 <- sum(CountV2LPC, CountV2DPC, CountV2IPC, CountV2PPC)

MixedCovRawW1 <- sum(CountW1LPC, CountW1DPC, CountW1IPC, CountW1PPC)
MixedCovRawW2 <- sum(CountW2LPC, CountW2DPC, CountW2IPC, CountW2PPC)


   ### Raw crust averageS ###
AvgCrustRawA <- mean(c(CrustCovRawA1, CrustCovRawA2))
  AvgCrustRawA
  
AvgCrustRawV <- mean(c(CrustCovRawV1, CrustCovRawV2))
  AvgCrustRawV
  
AvgCrustRawJ <- mean(c(CrustCovRawJ1, CrustCovRawJ2))
  AvgCrustRawJ
  
AvgCrustRawW <- mean(c(CrustCovRawW1, CrustCovRawW2))
  AvgCrustRawW
  
  ### Raw mixed avG ###
  
AvgMixRawA <- mean(c(MixedCovRawA1, MixedCovRawA2))
AvgMixRawJ <- mean(c(MixedCovRawJ1, MixedCovRawJ2))
AvgMixRawV <- mean(c(MixedCovRawV1, MixedCovRawV2))
AvgMixRawW <- mean(c(MixedCovRawW1, MixedCovRawW2))


   ### Crust percent coveR ###
CrustCovA <- (AvgCrustRawA/150)*100
  CrustCovA
  
CrustCovV <- (AvgCrustRawV/150)*100
  CrustCovV
  
CrustCovJ <- (AvgCrustRawJ/150)*100
  CrustCovJ
  
CrustCovW <- (AvgCrustRawW/150)*100
  CrustCovW
  
  ### Mixed percent coveR ###
  
MixCovA <- (AvgMixRawA/150)*100
MixCovJ <- (AvgMixRawJ/150)*100
MixCovV <- (AvgMixRawV/150)*100
MixCovW <- (AvgMixRawW/150)*100

   ### Physical percent coveR ###
PhysCovRawA1 <- sum(CountA1BG, CountA1R, CountA1G, CountA1PC)
  PhysCovRawA1
  
PhysCovRawA2 <- sum(CountA2BG, CountA2R, CountA2G, CountA2PC)
  PhysCovRawA2
  
PhysCovRawV1 <- sum(CountV1BG, CountV1R, CountV1G, CountV1PC)
  PhysCovRawV1
  
PhysCovRawV2 <- sum(CountV2BG, CountV2R, CountV2G, CountV2PC)
  PhysCovRawV2
  
PhysCovRawJ1 <- sum(CountJ1BG, CountJ1R, CountJ1G, CountJ1PC1)
  PhysCovRawJ1
 
PhysCovRawJ2 <- sum(CountJ2BG, CountJ2R, CountJ2G, CountJ2PC1)
  PhysCovRawJ2
  
PhysCovRawW1 <- sum(CountW1BG, CountW1R, CountW1G, CountW1PC1, CountW1Gy)
  PhysCovRawW1
  
PhysCovRawW2 <- sum(CountW2BG, CountW2R, CountW2G, CountW2PC1, CountW2Gy)
  PhysCovRawW2
  
  ### Physical averageS ###
AvgPhysRawA <- mean(c(PhysCovRawA1, PhysCovRawA2))
  AvgPhysRawA
  
AvgPhysRawV <- mean(c(PhysCovRawV1, PhysCovRawV2))
  AvgPhysRawV
  
AvgPhysRawJ <- mean(c(PhysCovRawJ1, PhysCovRawJ2))
  AvgPhysRawJ
  
AvgPhysRawW <- mean(c(PhysCovRawW1, PhysCovRawW2))
  AvgPhysRawW
  
  ### Physical percent coveR ###
PhysCovA <- (AvgPhysRawA/150)*100
  PhysCovA
  
PhysCovV <- (AvgPhysRawV/150)*100
  PhysCovV
  
PhysCovJ <- (AvgPhysRawJ/150)*100
  PhysCovJ
  
PhysCovW <- (AvgPhysRawW/150)*100
  PhysCovW
  
### Create Graphs: 

PlantCovAg <- 0
PlantCovJg <- 0
PlantCovVg <- 0

  ### Type cover data framE ###
TypeCoverA <- data.frame(
  Cover_Type = c("Plant", "Crust", "Mixed", "Physical"),
  Percent = c(PlantCovAg, CrustCovA, MixCovA, PhysCovA))
TypeCoverA
  
TypeCoverV <- data.frame(
  Cover_Type = c("Plant", "Crust", "Mixed", "Physical"),
  Percent = c(PlantCovVg, CrustCovV, MixCovV, PhysCovV))
TypeCoverV

TypeCoverJ <- data.frame(
  Cover_Type = c("Plant", "Crust", "Mixed", "Physical"),
  Percent = c(PlantCovJg, CrustCovJ, MixCovJ, PhysCovJ))
TypeCoverJ

TypeCoverW <- data.frame(
  Cover_Type = c("Plant", "Crust", "Mixed", "Physical"),
  Percent = c(PlantCovWg, CrustCovW, MixCovW, PhysCovW))
TypeCoverW

### Plant cover data frameS ###

Apl <- 40
Aop <- 60
Jpl <- 14.3
Jop <- 85.7
Vpl <- 10
Vop <- 90
Wpl <- 17.7
Wop <- 82.3

PlOpA <- data.frame(
  Cover_Type = c("Plant", "Open"),
  Percent = c(Apl,Aop))
PlOpA

PlOpJ <- data.frame(
  Cover_Type = c("Plant", "Open"),
  Percent = c(Jpl,Jop))
PlOpJ

PlOpV <- data.frame(
  Cover_Type = c("Plant", "Open"),
  Percent = c(Vpl,Vop))
PlOpV

PlOpW <- data.frame(
  Cover_Type = c("Plant", "Open"),
  Percent = c(Wpl,Wop))
PlOpW

  ### Type cover grapH ###

library(ggplot2)

library(ggpubr)
PT1 = theme(plot.title = element_text(hjust = 0.5))

TypeCover<-rbind(TypeCoverA, TypeCoverJ, TypeCoverV,TypeCoverW)
TypeCover$Site<-rep(c("AMT","JER","VOL","WHS"), each = 4)
CoTy<- function(x){factor(x$Cover_Type,levels=c("Crust","Mixed","Physical","Plant"))}
TypeCover$Cover_Type<-CoTy(TypeCover)

# COLOR ORDER NONSENSE AS IS IN LIST: DARKEST, 3RD LIGHTEST,2ND LIGHTEST, LIGHTEST #
COLSCALE<-(levels=c("#4DB686","#80CF7F","#B9E576","#F9F871",  "#4DB686","#80CF7F","#B9E576","#F9F871","#4DB686","#80CF7F","#B9E576","#F9F871","#4DB686","#80CF7F","#B9E576","#F9F871"))

with(TypeCover, TypeCover[order(Site,Cover_Type,Percent),])
BaseCov<-ggplot(data=TypeCover,aes(x=Cover_Type,y=Percent)) + geom_bar(color=c(COLSCALE),fill=c(COLSCALE),stat="identity") +
  scale_x_discrete(limits = c(  "Physical","Mixed","Crust", "Plant"))+
  scale_y_continuous(limits = c(0,100)) + xlab("Cover Type") + ylab("Percent Base Cover") +
  facet_grid(~Site) +theme_bw()
dev.copy(jpeg,"BaseCov.jpeg",width=25,height=7.5,units="cm",res=300)
dev.off()

TTypeCover<-TypeCover %>% dplyr::select(Site,Cover_Type,Percent)
colnames(TTypeCover) = c("Site","Type","Percent Cover")
TTypeCover$`Percent Cover`<-round(TTypeCover$`Percent Cover`,digits=2)
write.table(TTypeCover, file = "TypeCover.txt", sep = ",", quote = FALSE, row.names = F)


TypeA <- ggplot(data=TypeCoverA, aes(x=Cover_Type,y=Percent)) + ggtitle("A Mountain") + 
  scale_y_continuous(limits=c(0,100)) + xlab("Cover Type") + ylab("Percent Cover")+
    geom_bar(fill= c("#D2B48C", "#B0A774", "#899A63", "#237E5B"), 
             stat="identity", position = "dodge")+PT1+theme_bw()

TypeV <- ggplot(data=TypeCoverV, aes(x=Cover_Type,y=Percent)) + ggtitle("Volcanic") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33", "#FFFFCC", "#FFFF00", "#FFCC33"), 
           stat="identity", position = "dodge")
TypeJ <- ggplot(data=TypeCoverJ, aes(x=Cover_Type,y=Percent)) + ggtitle("Jornada") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33", "#FFFFCC", "#FFFF00","#FFCC33"), 
           stat="identity", position = "dodge")
TypeW <- ggplot(data=TypeCoverW, aes(x=Cover_Type,y=Percent)) + ggtitle("       White Sands") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33", "#FFFFCC", "#FFFF00", "#FFCC33"), 
           stat="identity", position = "dodge")

ggarrange(TypeA, TypeV, TypeJ, TypeW, ncol=2, nrow=2)

### Plot plant coV ###

PlantCover<-rbind(PlOpA, PlOpJ, PlOpV,PlOpW)
PlantCover$Site<-rep(c("AMT","JER","VOL","WHS"), each = 2)

# COLOR ORDER NONSENSE AS IS IN LIST: DARKEST, 3RD LIGHTEST,2ND LIGHTEST, LIGHTEST #
COLSCALE2<-(levels=c( "#4DB686","#ccece6","#4DB686","#ccece6","#4DB686","#ccece6","#4DB686","#ccece6"))
                     
with(TypeCover, TypeCover[order(Site,Cover_Type,Percent),])
PlantCov<-ggplot(data=PlantCover,aes(x=Cover_Type,y=Percent)) + geom_bar(color=c(COLSCALE2),fill=c(COLSCALE2),stat="identity") +
  scale_x_discrete(limits = c(  "Open","Plant"))+
  scale_y_continuous(limits = c(0,100)) + xlab("Cover Type") + ylab("Percent Canopy Cover") +
  facet_grid(~Site) +theme_bw()
dev.copy(png,"PlantCov.png",width=25,height=7.5,units="cm",res=300)
dev.off()

PPlantCover<-PlantCover %>% dplyr::select(Site,Cover_Type,Percent)
colnames(PPlantCover) = c("Site","Type","Percent Cover")
PPlantCover$`Percent Cover`<-round(PPlantCover$`Percent Cover`,digits=2)
write.table(PPlantCover, file = "PlantCover.txt", sep = ",", quote = FALSE, row.names = F)

PA <- ggplot(data=PlOpA, aes(x=Cover_Type,y=Percent)) + ggtitle("         A Mountain") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33","#FFFF99"), 
           stat="identity", position = "dodge")

PJ <- ggplot(data=PlOpJ, aes(x=Cover_Type,y=Percent)) + ggtitle("            Jornada") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33","#FFFF99"), 
           stat="identity", position = "dodge")

PV <- ggplot(data=PlOpV, aes(x=Cover_Type,y=Percent)) + ggtitle("            Volcanic") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33","#FFFF99"), 
           stat="identity", position = "dodge")

PW <- ggplot(data=PlOpW, aes(x=Cover_Type,y=Percent)) + ggtitle("       White Sands") +
  scale_y_continuous(limits=c(0,100)) +
  geom_bar(fill= c("#66FF33","#FFFF99"), 
           stat="identity", position = "dodge")

ggarrange(PA, PV, PJ, PW, ncol=2, nrow=2)

### Individual crust averageS ###


CountA1IC <- sum(c(CountA1IPC,CountA1IC))
CountA1LA <- sum(c(CountA1LPC,CountA1LA))
CountA1DA <- sum(c(CountA1DPC,CountA1DA))
CountA1Cl <- sum(c(CountA1CPC,CountA1Cl))
CountA1Pt <- sum(c(CountA1PPC,CountA1Pt))

CountA2IC <- sum(c(CountA2IPC,CountA2IC))
CountA2LA <- sum(c(CountA2LPC,CountA2LA))
CountA2DA <- sum(c(CountA2DPC,CountA2DA))
CountA2Cl <- sum(c(CountA2CPC,CountA2Cl))
CountA2Pt <- sum(c(CountA2PPC,CountA2Pt))

CountJ1IC <- sum(c(CountJ1IC1,CountJ1IPC))
CountJ1LA <- sum(c(CountJ1LPC,CountJ1LA1))
CountJ1DA <- sum(c(CountJ1DPC,CountJ1DA1))
CountJ1Cl <- sum(c(CountJ1CPC,CountJ1Cl1))
CountJ1Pt <- sum(c(CountJ1PPC,CountJ1Pt1))

CountJ2IC <- sum(c(CountJ2IC1,CountJ2IPC))
CountJ2LA <- sum(c(CountJ2LPC,CountJ2LA1))
CountJ2DA <- sum(c(CountJ2DPC,CountJ2DA1))
CountJ2Cl <- sum(c(CountJ2CPC,CountJ2Cl1))
CountJ2Pt <- sum(c(CountJ2PPC,CountJ2Pt1))

CountV1IC <- sum(c(CountV1IC,CountV1IPC))
CountV1LA <- sum(c(CountV1LPC,CountV1LA))
CountV1DA <- sum(c(CountV1DPC,CountV1DA))
CountV1Cl <- sum(c(CountV1CPC,CountV1Cl))
CountV1Pt <- sum(c(CountV1PPC,CountV1Pt))

CountV2IC <- sum(c(CountV2IC,CountV2IPC))
CountV2LA <- sum(c(CountV2LPC,CountV2LA))
CountV2DA <- sum(c(CountV2DPC,CountV2DA))
CountV2Cl <- sum(c(CountV2CPC,CountV2Cl))
CountV2Pt <- sum(c(CountV2PPC,CountV2Pt))

CountW1IC <- sum(c(CountW1IC1,CountW1IPC))
CountW1LA <- sum(c(CountW1LPC,CountW1LA1))
CountW1DA <- sum(c(CountW1DPC,CountW1DA1))
CountW1Cl <- sum(c(CountW1CPC,CountW1Cl1))
CountW1Pt <- sum(c(CountW1PPC,CountW1Pt1))

CountW2IC <- sum(c(CountW2IC1,CountW2IPC))
CountW2LA <- sum(c(CountW2LPC,CountW2LA1))
CountW2DA <- sum(c(CountW2DPC,CountW2DA1))
CountW2Cl <- sum(c(CountW2CPC,CountW2Cl1))
CountW2Pt <- sum(c(CountW2PPC,CountW2Pt1))

AvgICA <- mean(c(CountA1IC, CountA2IC))
AvgICV <- mean(c(CountV1IC, CountV2IC))
AvgICJ <- mean(c(CountJ1IC, CountJ2IC))
AvgICW <- mean(c(CountW1IC, CountW2IC))

AvgLAA <- mean(c(CountA1IC, CountA2LA))
AvgLAV <- mean(c(CountV1IC, CountV2LA))
AvgLAJ <- mean(c(CountJ1IC, CountJ2LA))
AvgLAW <- mean(c(CountW1IC, CountW2LA))

AvgDAA <- mean(c(CountA1DA, CountA2DA))
AvgDAV <- mean(c(CountV1DA, CountV2DA))
AvgDAJ <- mean(c(CountJ1DA, CountJ2DA))
AvgDAW <- mean(c(CountW1DA, CountW2DA))
sum(c(AvgICA,AvgICJ,AvgICV,AvgICW))

AvgClA <- mean(c(CountA1Cl, CountA2Cl))
AvgClV <- mean(c(CountV1Cl, CountV2Cl))
AvgClJ <- mean(c(CountJ1Cl, CountJ2Cl))
AvgClW <- mean(c(CountW1Cl, CountW2Cl))

AvgPtA <- mean(c(CountA1Pt, CountA2Pt))
AvgPtV <- mean(c(CountV1Pt, CountV2Pt))
AvgPtJ <- mean(c(CountJ1Pt, CountJ2Pt))
AvgPtW <- mean(c(CountW1Pt, CountW2Pt))

AvgColA <- mean(c(CountA1Col, CountA2Col))
AvgColV <- mean(c(CountV1Col, CountV2Col))
AvgColJ <- mean(c(CountJ1Col, CountJ2Col))
AvgColW <- mean(c(CountW1Col, CountW2Col))

AvgMA <- mean(c(CountA1M, CountA2M))
AvgMV <- mean(c(CountV1M, CountV2M))
AvgMJ <- mean(c(CountJ1M, CountJ2M))
AvgMW <- mean(c(CountW1M, CountW2M))

  ### Individual  crust percent coveR ###

ICCovA <- (AvgICA/150)*100
ICCovV <- (AvgICV/150)*100
ICCovJ <- (AvgICJ/150)*100
ICCovW <- (AvgICW/150)*100

LACovA <- (AvgLAA/150)*100
LACovV <- (AvgLAV/150)*100
LACovJ <- (AvgLAJ/150)*100
LACovW <- (AvgLAW/150)*100

DACovA <- (AvgDAA/150)*100
DACovV <- (AvgDAV/150)*100
DACovJ <- (AvgDAJ/150)*100
DACovW <- (AvgDAW/150)*100

ClCovA <- (AvgClA/150)*100
ClCovV <- (AvgClV/150)*100
ClCovJ <- (AvgClJ/150)*100
ClCovW <- (AvgClW/150)*100

PtCovA <- (AvgPtA/150)*100
PtCovV <- (AvgPtV/150)*100
PtCovJ <- (AvgPtJ/150)*100
PtCovW <- (AvgPtW/150)*100

ColCovA <- (AvgColA/150)*100
ColCovV <- (AvgColV/150)*100
ColCovJ <- (AvgColJ/150)*100
ColCovW <- (AvgColW/150)*100

MCovA <- (AvgMA/150)*100
MCovV <- (AvgMV/150)*100
MCovJ <- (AvgMJ/150)*100
MCovW <- (AvgMW/150)*100



ICCovA
ICCovJ
ICCovV
ICCovW

LACovA
LACovJ
LACovV
LACovW

DACovA
DACovJ
DACovV
DACovW

ClCovA
ClCovJ
ClCovV
ClCovW

PtCovA
PtCovJ
PtCovV
PtCovW

ColCovA
ColCovJ
ColCovV
ColCovW

  ### Crust cover data framE ###
library(ggplot2)
library(ggpubr)

CrustCoverA <- data.frame(
  Crust_Type = c("IC", "LAC", "DAC", "Clav",
                 "Pelt", "Col", "M"),
  Percent = c(ICCovA, LACovA, DACovA, ClCovA, 
              PtCovA, ColCovA, MCovA))
CrustCoverA

CrustCoverV <- data.frame(
  Crust_Type = c("IC", "LAC", "DAC", "Clav",
                 "Pelt", "Col", "M"), 
  Percent = c(ICCovV, LACovV, DACovV, ClCovV, 
              PtCovV, ColCovV, MCovV))
CrustCoverV 

CrustCoverJ <- data.frame(
  Crust_Type = c("IC", "LAC", "DAC", "Clav",
                 "Pelt", "Col", "M"),
  Percent = c(ICCovJ, LACovJ, DACovJ, ClCovJ, 
              PtCovJ, ColCovJ, MCovJ))
CrustCoverJ
  
CrustCoverW <- data.frame(
  Crust_Type = c("IC", "LAC", "DAC", "Clav",
                 "Pelt", "Col", "M"),
  Percent = c(ICCovW, LACovW, DACovW, ClCovW, 
              PtCovW, ColCovW, MCovW))
CrustCoverW

CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "LAC"] <- "LA"
CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "DAC"] <- "DA"
CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "Clav"] <- "CL"
CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "Pelt"] <- "PT"
CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "Col"] <- "CO"
CrustCoverA$Crust_Type[CrustCoverA$Crust_Type == "M"] <- "MO"

CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "LAC"] <- "LA"
CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "DAC"] <- "DA"
CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "Clav"] <- "CL"
CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "Pelt"] <- "PT"
CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "Col"] <- "CO"
CrustCoverJ$Crust_Type[CrustCoverJ$Crust_Type == "M"] <- "MO"

CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "LAC"] <- "LA"
CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "DAC"] <- "DA"
CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "Clav"] <- "CL"
CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "Pelt"] <- "PT"
CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "Col"] <- "CO"
CrustCoverV$Crust_Type[CrustCoverV$Crust_Type == "M"] <- "MO"

CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "LAC"] <- "LA"
CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "DAC"] <- "DA"
CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "Clav"] <- "CL"
CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "Pelt"] <- "PT"
CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "Col"] <- "CO"
CrustCoverW$Crust_Type[CrustCoverW$Crust_Type == "M"] <- "MO"



  ### Crust cover grapH ###

library(ggplot2)

library(ggpubr)
   

str(PlantCover)

CrustCover<-rbind(CrustCoverA, CrustCoverJ, CrustCoverV,CrustCoverW)
CrustCover$Site<-rep(c("AMT","JER","VOL","WHS"), each = 7)
CoTy<- function(x){factor(x$Cover_Type,levels=c("Crust","Mixed","Physical","Plant"))}
TypeCover$Cover_Type<-CoTy(TypeCover)

# COLOR ORDER NONSENSE AS IS IN LIST: DARKEST, 3RD LIGHTEST,2ND LIGHTEST, LIGHTEST #
COLSCALE3<-(levels=c("#FFF8D0","#FFCC99","#FF9966","#660066","#FF0066","#ccccff","#000044","#FFF8D0","#FFCC99","#FF9966","#660066","#FF0066","#ccccff","#000044","#FFF8D0","#FFCC99","#FF9966","#660066","#FF0066","#ccccff","#000044","#FFF8D0","#FFCC99","#FF9966","#660066","#FF0066","#ccccff","#000044"))


with(CrustCover, CrustCover[order(Site,Crust_Type,Percent),])
CrustCov<-ggplot(data=CrustCover,aes(x=Crust_Type,y=Percent)) + geom_bar(color=c(COLSCALE3),fill=c(COLSCALE3),stat="identity") +
  scale_x_discrete(limits = c("IC", "LA", "DA", "PT","CL", "CO", "MO"))+
  scale_y_continuous(limits = c(0,30)) + xlab("Crust Type") + ylab("Percent Base Cover") +
  facet_grid(~Site) +theme_bw()
dev.copy(png,"CrustCov.png",width=25,height=7.5,units="cm",res=300)
dev.off()

CCrustCover<-CrustCover %>% dplyr::select(Site,Crust_Type,Percent)
colnames(CCrustCover) = c("Site","Type","Percent Cover")
CCrustCover$`Percent Cover`<-round(CCrustCover$`Percent Cover`,digits=2)

write.table(CCrustCover, file = "CrustCover.txt", sep = ",", quote = FALSE, row.names = F)

TypeA2 <- ggplot(data=CrustCoverA, aes(x=Crust_Type, y=Percent)) + ggtitle("                    A Mountain") + 
  scale_y_continuous(limits=c(0,30)) +
  geom_bar(color= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                    "#99CC33", "#333333", "#00FF00"), 
           fill= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                   "#99CC33", "#333333", "#00FF00"), 
           stat="identity", position = "dodge") + 
  scale_x_discrete(limits = c("IC", "LA", "DA", "CL", "PT", "CO", "MO"))

TypeV2 <- ggplot(data=CrustCoverV,aes(x=Crust_Type,y=Percent)) + ggtitle("                       Volcanic") +
  scale_y_continuous(limits=c(0,30)) +
  geom_bar(color= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                    "#99CC33", "#333333", "#00FF00"), 
           fill= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                   "#99CC33", "#333333", "#00FF00"), 
           stat="identity",position = "dodge") + 
  scale_x_discrete(limits = c("IC", "LAC", "DAC", "Clav", "Pelt", "Col", "M"))

TypeJ2 <- ggplot(data=CrustCoverJ,aes(x=Crust_Type,y=Percent)) + ggtitle("                       Jornada") +
  scale_y_continuous(limits=c(0,30)) +
  geom_bar(color= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                    "#99CC33", "#333333", "#00FF00"), 
           fill= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                   "#99CC33", "#333333", "#00FF00"), 
           stat="identity",position = "dodge") + 
  scale_x_discrete(limits = c("IC", "LAC", "DAC", "Clav", "Pelt", "Col", "M"))

TypeW2 <- ggplot(data=CrustCoverW,aes(x=Crust_Type,y=Percent)) + ggtitle("                  White Sands") +
  scale_y_continuous(limits=c(0,30)) +
  geom_bar(color= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                    "#99CC33", "#333333", "#00FF00"), 
           fill= c("#FFFFCC", "#FFCC00", "#663300", "#990000", 
                   "#99CC33", "#333333", "#00FF00"), 
           stat="identity",position = "dodge") + 
  scale_x_discrete(limits = c("IC", "LAC", "DAC", "Clav", "Pelt", "Col", "M"))

ggarrange(TypeA2, TypeV2, TypeJ2, TypeW2, ncol=2, nrow=2)

  
            ### THE END ###


     #  #  # Tasks completeD #  #  #


#   #   # MISSION ACCOMPLISHED #   #   #

