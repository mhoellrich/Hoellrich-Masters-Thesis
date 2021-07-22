

### MISSION: VIEW DIVERSITY DATA ###





setwd("C:/Users/Mikaela/OneDrive/Documents/Masters Stuff/R Stuff")

library(ape)
library(vegan)
library(plyr)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(tidyr)
library(Biostrings)
library(phylotools)
library(readxl)

otu = read.table(file="TH16S.otu_table.txt", header=T, sep='\t',row.names = 1)
head(otu)
dim(otu)

taxo <- read.table(file="TH16Staxonomy.tsv", sep='\t', header=TRUE,row.names = 1)
head(taxo)
dim(taxo)

#Remove confidence column from taxonomy table

taxo <- select(taxo,-Confidence)

#Separate different taxonomy classes into individual columns

taxo_filtered = separate(taxo, Taxon, c("Kingdom","Phylum","Class","Order", "Family", "Genus","Species"), sep= ";", remove=TRUE)

##Convert ASV table and taxonomy tables to phyloseq object form

taxmat <- as(as.matrix(taxo_filtered),"matrix")
TAX = tax_table(taxmat)

otumat <- as(as.matrix(otu), "matrix")
mode(otumat) <- "numeric"
OTU = otu_table(otumat, taxa_are_rows = TRUE)

###STEP2.3: Import mapping file for analysis using Phyloseq

meta0 = read.csv("MappingTH16S.csv",
                 header=TRUE,row.names=1)
head(meta0)

##Convert mapping file to phyloseq object form
sampleData <- sample_data(meta0)

###STEP3: Construct Phyloseq object 

physeq = phyloseq(OTU,TAX,sampleData)

physeq

###STEP4: Process data further before analysis

###Remove singletons

physeq.prune = prune_taxa(taxa_sums(physeq) > 1, physeq)
physeq.prune

#Plot read counts to check dataset

readcount = data.table(as(sample_data(physeq.prune), "data.frame"),
                       TotalReads = sample_sums(physeq.prune), 
                       keep.rownames = TRUE)
setnames(readcount, "rn", "SampleID")

readcount = readcount[order(readcount$TotalReads), c("SampleID", "TotalReads")]
head(readcount)
sum(readcount$TotalReads)

readcount<-filter(readcount, TotalReads > 1500)
mean(readcount$TotalReads)

SeqDepth = ggplot(readcount, aes(TotalReads)) + geom_histogram() + ggtitle("Sequencing Depth")
SeqDepth

#Rarefy OTUs to a minimum number of reads

set.seed(711)
physeq.prune.rarefy = rarefy_even_depth(physeq.prune, sample.size = 10752, replace = FALSE, trimOTUs = TRUE)

#Subset for bacteria

AB.Bacteria0 <- subset_taxa(physeq.prune.rarefy, Kingdom == "Bacteria")
AB.Bacteria0
AB.Bacteria <- subset_taxa(AB.Bacteria0, Order != " Chloroplast")
AB.Bacteria

AB.cyanobacteria <- subset_taxa(physeq.prune.rarefy, Phylum == " Cyanobacteria")
AB.cyanobacteria

AB.cyanobacteria00 <- subset_taxa(AB.cyanobacteria, Order != " Chloroplast")
AB.cyanobacteria00

Ba.con <- subset_samples(AB.Bacteria, Rem != "Rem")
Cy.con <- subset_samples(AB.cyanobacteria00, Rem != "Rem")

TySubA<-subset_samples(Cy.con, Site=="AMT")
TySubJ<-subset_samples(Cy.con, Site=="JER")
TySubV<-subset_samples(Cy.con, Site=="VOL")
TySubW<-subset_samples(Cy.con, Site=="WHS")
BTySubA<-subset_samples(Ba.con, Site=="AMT")
BTySubJ<-subset_samples(Ba.con, Site=="JER")
BTySubV<-subset_samples(Ba.con, Site=="VOL")
BTySubW<-subset_samples(Ba.con, Site=="WHS")

tax.A <- as(tax_table(TySubA),"matrix")
tax.J <- as(tax_table(TySubJ),"matrix")
tax.V <- as(tax_table(TySubV),"matrix")
tax.W <- as(tax_table(TySubW),"matrix")
Btax.A <- as(tax_table(BTySubA),"matrix")
Btax.J <- as(tax_table(BTySubJ),"matrix")
Btax.V <- as(tax_table(BTySubV),"matrix")
Btax.W <- as(tax_table(BTySubW),"matrix")
tax.A[is.na(tax.A)] <- "Unknown"
tax.J[is.na(tax.J)] <- "Unknown"
tax.V[is.na(tax.V)] <- "Unknown"
tax.W[is.na(tax.W)] <- "Unknown"
Btax.A[is.na(Btax.A)] <- "Unknown"
Btax.J[is.na(Btax.J)] <- "Unknown"
Btax.V[is.na(Btax.V)] <- "Unknown"
Btax.W[is.na(Btax.W)] <- "Unknown"
TAX.A <- tax_table(tax.A)
TAX.J <- tax_table(tax.J)
TAX.V <- tax_table(tax.V)
TAX.W <- tax_table(tax.W)
BTAX.A <- tax_table(Btax.A)
BTAX.J <- tax_table(Btax.J)
BTAX.V <- tax_table(Btax.V)
BTAX.W <- tax_table(Btax.W)
A.con2 <- phyloseq(sample_data(TySubA),otu_table(TySubA),TAX.A)
J.con2 <- phyloseq(sample_data(TySubJ),otu_table(TySubJ),TAX.J)
V.con2 <- phyloseq(sample_data(TySubV),otu_table(TySubV),TAX.V)
W.con2 <- phyloseq(sample_data(TySubW),otu_table(TySubW),TAX.W)
BA.con2 <- phyloseq(sample_data(BTySubA),otu_table(BTySubA),BTAX.A)
BJ.con2 <- phyloseq(sample_data(BTySubJ),otu_table(BTySubJ),BTAX.J)
BV.con2 <- phyloseq(sample_data(BTySubV),otu_table(BTySubV),BTAX.V)
BW.con2 <- phyloseq(sample_data(BTySubW),otu_table(BTySubW),BTAX.W)
m.A.con2 <- merge_samples(A.con2, "Type")
m.J.con2 <- merge_samples(J.con2, "Type")
m.V.con2 <- merge_samples(V.con2, "Type")
m.W.con2 <- merge_samples(W.con2, "Type")
Bm.A.con2 <- merge_samples(BA.con2, "Type")
Bm.J.con2 <- merge_samples(BJ.con2, "Type")
Bm.V.con2 <- merge_samples(BV.con2, "Type")
Bm.W.con2 <- merge_samples(BW.con2, "Type")
sample_data(m.A.con2)$Type <- factor(sample_names(m.A.con2))
A.con3 = transform_sample_counts(m.A.con2, function(x) x / sum(x))
sample_data(m.J.con2)$Type <- factor(sample_names(m.J.con2))
J.con3 = transform_sample_counts(m.J.con2, function(x) x / sum(x))
sample_data(m.V.con2)$Type <- factor(sample_names(m.V.con2))
V.con3 = transform_sample_counts(m.V.con2, function(x) x / sum(x))
sample_data(m.W.con2)$Type <- factor(sample_names(m.W.con2))
W.con3 = transform_sample_counts(m.W.con2, function(x) x / sum(x))

sample_data(Bm.A.con2)$Type <- factor(sample_names(Bm.A.con2))
BA.con3 = transform_sample_counts(Bm.A.con2, function(x) x / sum(x))
sample_data(Bm.J.con2)$Type <- factor(sample_names(Bm.J.con2))
BJ.con3 = transform_sample_counts(Bm.J.con2, function(x) x / sum(x))
sample_data(Bm.V.con2)$Type <- factor(sample_names(Bm.V.con2))
BV.con3 = transform_sample_counts(Bm.V.con2, function(x) x / sum(x))
sample_data(Bm.W.con2)$Type <- factor(sample_names(Bm.W.con2))
BW.con3 = transform_sample_counts(Bm.W.con2, function(x) x / sum(x))

glom.A <- tax_glom(A.con3,taxrank = 'Species')
glom.J <- tax_glom(J.con3,taxrank = 'Species')
glom.V <- tax_glom(V.con3,taxrank = 'Species')
glom.W <- tax_glom(W.con3,taxrank = 'Species')
Bglom.A <- tax_glom(BA.con3,taxrank = 'Phylum')
Bglom.J <- tax_glom(BJ.con3,taxrank = 'Phylum')
Bglom.V <- tax_glom(BV.con3,taxrank = 'Phylum')
Bglom.W <- tax_glom(BW.con3,taxrank = 'Phylum')
data_glom.A <- psmelt(glom.A)
data_glom.A$SITE<-rep("AMT",length(data_glom.A$Site))
data_glom.J <- psmelt(glom.J)
data_glom.J$SITE<-rep("JER",length(data_glom.J$Site))
data_glom.V <- psmelt(glom.V)
data_glom.V$SITE<-rep("VOL",length(data_glom.V$Site))
data_glom.W <- psmelt(glom.W)
data_glom.W$SITE<-rep("WHS",length(data_glom.W$Site))
Bdata_glom.A <- psmelt(Bglom.A)
Bdata_glom.A$SITE<-rep("AMT",length(Bdata_glom.A$Site))
Bdata_glom.J <- psmelt(Bglom.J)
Bdata_glom.J$SITE<-rep("JER",length(Bdata_glom.J$Site))
Bdata_glom.V <- psmelt(Bglom.V)
Bdata_glom.V$SITE<-rep("VOL",length(Bdata_glom.V$Site))
Bdata_glom.W <- psmelt(Bglom.W)
Bdata_glom.W$SITE<-rep("WHS",length(Bdata_glom.W$Site))

data_glom.ALL<-rbind(data_glom.A,data_glom.J,data_glom.V,data_glom.W)
data_glom.ALL$Species <- as.character(data_glom.ALL$Species)

Sel0<-data_glom.ALL %>% dplyr::select(OTU,Sample,SITE,Type,Abundance,Kingdom,Phylum,Class,Order,Family,Genus,Species)

Bdata_glom.ALL<-rbind(Bdata_glom.A,Bdata_glom.J,Bdata_glom.V,Bdata_glom.W)
Bdata_glom.ALL$Phylum <- as.character(Bdata_glom.ALL$Phylum)

data_glom.ALL$Species[data_glom.ALL$Abundance < 0.01] <- "<1% abund."
Bdata_glom.ALL$Phylum[Bdata_glom.ALL$Abundance < 0.01] <- "<1% abund."

unique((Bdata_glom.ALL$Phylum))
data_glom.ALL$Species <- factor(data_glom.ALL$Species, levels = c(" Scytonema_hyalinum"," uncultured_cyanobacterium"," uncultured_bacterium","Unknown", " Microcoleus_sp.", " uncultured_Oscillatoriales", " Symplocastrum_torsivum",         " Microcoleus_paludosus",   " Oscillatoriales_cyanobacterium",      " Trichocoleus_desertorum", " Chroococcidiopsis_sp.",    " Nostoc_commune",  " Leptolyngbya_sp.", "<1% abund." ))
Bdata_glom.ALL$Phylum <- factor(Bdata_glom.ALL$Phylum, 
                                levels = c( " Proteobacteria"," Actinobacteriota" , " Cyanobacteria"," Chloroflexi", " Planctomycetota", " Bacteroidota", " Gemmatimonadota"," Acidobacteriota", " Verrucomicrobiota", " Abditibacteriota"," Myxococcota"," Armatimonadota", "Unknown","<1% abund."))

TyOrd<- function(x){factor(x$Type,levels=c("LA","DA","PT","CL","MO"))}
data_glom.ALL$Type<-TyOrd(data_glom.ALL)
Bdata_glom.ALL$Type<-TyOrd(Bdata_glom.ALL)

### FOR CYDRASIL ###

N<-data_glom.ALL[data_glom.ALL$Phylum == " Cyanobacteria",]
NNNo<-N %>% dplyr::select(OTU,SITE,Type,Abundance,Class,Order,Family,Genus,Species)
Count = length(unique(NNNo$OTU))

write.csv(NNNo,"AllCyanoAbund.csv")

### REMOVE UNKNOWNS ###

#Convert taxonomy table to dataframe

cyanotaxa <- as.data.frame(tax_table(AB.cyanobacteria))
head(cyanotaxa)

##install package for converting fasta file

#upload ASV sequences - you will first have to download your .fa file from Discovery

ASV.seqs <- readDNAStringSet("TH16S.ASVs.fa")
head(ASV.seqs)

#Create an ASV and sequence column

ASV = names(ASV.seqs)
sequence = paste(ASV.seqs)
seq <- data.frame(ASV, sequence)

##Change ASV column to row names

sequences <- data.frame(seq,row.names = 1)
head(sequences)

##merge cyano taxonomy table with sequences

cyano.tax.sequences <- merge(cyanotaxa,sequences,by=0)
cyano.tax.sequences

##Convert back to fasta file form

#Subset sequence and ASV name

unkseq <- subset(cyano.tax.sequences, select = c(1,9))

#rename columns to fit phylotools format
install.packages("phylotools")
library(phylotools)

colnames(unkseq)
names(unkseq)[names(unkseq) == "Row.names"] <- "seq.name"
names(unkseq)[names(unkseq) == "sequence"] <- "seq.text"

#Convert and save fasta file
dat2fasta(unkseq, outfile = "unkcyanoseqall.fasta")

write.csv(cyano.tax.sequences,"CyanoSeqAll.csv")

###Taxonomic Barplot

##Bacteria taxonomic barplots

N<-data_glom.ALL[data_glom.ALL$Species != "Unknown",]
NN<-N[N$Species != " uncultured_cyanobacterium",]
NNN<-NN[NN$Species != " uncultured_bacterium",]
NNNN<-NNN[NNN$Species != " uncultured_Oscillatoriales",]
NNNo<-NNNN %>% dplyr::select(OTU,SITE,Type,Abundance,Class,Order,Family,Genus,Species)

Unk0 <- read.csv("UnknownOver1percentFix.csv", header = TRUE)
NewCyano<-rbind(NNNo,Unk0)

Y<-NewCyano[NewCyano$Genus != " Aliterella",]
YY<-Y[Y$Genus != " Microcoleus_PCC-7113",]
YYY<-YY[YY$Genus != " Microcoleus_SAG_1449-1a",]
YYYY<-YYY[YYY$Genus != " Microcoleus_Es-Yyy1400",]
YYYYy<-YYYY[YYYY$Genus != " Symplocastrum_CPER-KK1",]
yy<-YYYYy[YYYYy$Genus != " Scytonema_UTEX_2349",]
y<-yy[yy$Genus != " Leptolyngbya_PCC-6306",]
yyy<-y[y$Genus != " Leptolyngbya_EcFYyyy-00",]
yyyy<-yyy[yyy$Genus != " Chroococcidiopsis_PCC_7203",]
yoo<-yyyy[yyyy$Genus != " Chroococcidiopsis_SAG_2023",]
yox<-yoo[yoo$Species !=" Scytonema_hyalinum",]

Unk00 <- read.csv("GenusFix.csv", header = TRUE)
NewCyano2<-rbind(yox,Unk00)
NewCyano2$Species[NewCyano2$Abundance < 0.01] <- "<1% abund."

Gen <- aggregate(NewCyano2$Abundance, by=list(NewCyano2$SITE,NewCyano2$Type,NewCyano2$Genus), FUN=sum)
colnames(Gen) = c("SITE","Type","Genus","Abundance")
Gen$Genus[Gen$Abundance < 0.01] <- "<1% abund."
Gen$Genus[Gen$Genus ==" uncultured"] <- "Unknown"

#Count the levels present in the Phylum column

Count = length(unique(Bdata_glom.ALL$Phylum))
Count
Count1 = length(unique(NewCyano$Species))
Count1

#Print out unique phyla names for insertion into barplots in next step.

unique((NewCyano$Species))
unique((data_glom.cya$Species))
unique((data_glom.bac$Phylum))

#Create levels of phyla represented in barplot. Levels appear on the barplot in the order they are listed.
##To order Phyla by relative abundance use command above and copy and paste the phyla in the order they are printed out


Bdata_glom.ALL$Phylum <- factor(Bdata_glom.ALL$Phylum, 
                               levels = c( " Proteobacteria"," Actinobacteriota" , " Cyanobacteria"," Chloroflexi", " Planctomycetota", " Bacteroidota", " Gemmatimonadota"," Acidobacteriota", " Abditibacteriota"," Verrucomicrobiota", " Myxococcota"," Armatimonadota", "Unknown", " Deinococcota","<1% abund."))

###Create barplot of phyla - use facet_grid to separate samples by a variable ie in mapping file. 
###Choose colors for plot in scale_fill_manual. 
##Color names can be found at http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf *amount of colors must match amount of level in phyla column

ggplot(data=Bdata_glom.ALL, aes(x=Type, y=Abundance, fill=Phylum)) + facet_grid(~SITE, scales = "free") +
  geom_bar(aes(), stat="identity", position="stack") + 
  scale_fill_manual(values = c("#FF3747","#FF8B0F","#FFD600","#FFED00","#EAE45F","#EEF5C2","#d1f1ac","#4FCBBB","#00E1DF","#1456b1","#962fbf","#d62976","#000000")) + 
  theme_bw() +  guides(fill=guide_legend(nrow=13)) + theme(axis.text.x = element_text(angle = 90))
dev.copy(jpeg,"AllAbund.jpeg",width=27,height=9,units="cm",res=300)
dev.off()

ggplot(data=Bdata_glom.ALL, aes(y=Abundance, x=Phylum,color=Type)) + ylab("Relative Abundance (%)")+xlab("Phylum")+geom_boxplot() +
  geom_point(size=1,alpha=.8,stat="identity",position=position_dodge(width=.75),aes(color=Type)) + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw()
dev.copy(jpeg,"16SrelDiff.jpeg",width=37,height=15,units="cm",res=300)
dev.off()

GGG<-Gen[order(Gen[,3]),]
ggplot(data=GGG, aes(y=Abundance, x=Genus,color=SITE)) + ylab("Relative Abundance (%)")+xlab("Genus")+geom_boxplot() +
  geom_point(size=1,alpha=.8,stat="identity",position=position_dodge(width=.75),aes(color=SITE)) + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3"))+ theme_bw()


Colllor <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","BA","CA","DA","EA")
Num<-c(3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22)
COnum<-as.data.frame(Colllor,Num)
COnum$Num<-c(3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22,3.22)
COnum$Type<-rep(c("LOL"),times=31)

ggplot(data=COnum, aes(x=Type, y=Num, fill=Colllor)) +scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FD7400","#FA9600","#FFBE00","#FFD10F","#FFE694","#EEF5C2","#BBFF6D","#68E17A","#15C286","#158F73","#165C60","#16294D","#0C63E7","#09A6F3","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "none") + theme(axis.text.x = element_text(angle = 90)) 

ggplot(data=GGG, aes(x=Type, y=Abundance, fill=Genus)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FD7400","#FA9600","#FFBE00","#FFD10F","#FFE694","#EEF5C2","#BBFF6D","#68E17A","#15C286","#158F73","#165C60","#16294D","#0C63E7","#09A6F3","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "none") +theme(axis.text.x = element_text(angle = 90)) 
dev.copy(png,"NewCyanGen2.png",width=27,height=9,units="cm",res=300)
dev.off()

ggplot(data=GGG, aes(x=Type, y=Abundance, fill=Genus)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FD7400","#FA9600","#FFBE00","#FFD10F","#FFE694","#EEF5C2","#BBFF6D","#68E17A","#15C286","#158F73","#165C60","#16294D","#0C63E7","#09A6F3","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "bottom")+guides(fill=guide_legend(nrow=6))+  theme(axis.text.x = element_text(angle = 90)) 
dev.copy(png,"NewCyanGen.png",width=35,height=25,units="cm",res=300)
dev.off()

ggplot(data=GGG, aes(y=Abundance, x=Genus,color=Type)) + ylab("Relative Abundance (%)")+xlab("Biomass Type")+geom_boxplot() +
  geom_point(size=3,alpha=.8,stat="identity",position=position_dodge(width=.75),aes(color=Type)) + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw()

CCC<-NewCyano2[order(NewCyano2[,8]),]

ggplot(data=CCC, aes(x=Type, y=Abundance, fill=Species)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FD7400","#FA9600","#FFBE00","#FFD10F","#FFE694","#EEF5C2","#BBFF6D","#68E17A","#15C286","#158F73","#165C60","#16294D","#0C63E7","#09A6F3","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "none")+guides(fill=guide_legend(nrow=12)) + theme(axis.text.x = element_text(angle = 90)) 
dev.copy(jpeg,"NewCyanSP2.jpeg",width=27,height=9,units="cm",res=300)
dev.off()

ggplot(data=CCC, aes(x=Type, y=Abundance, fill=Species)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FD7400","#FA9600","#FFBE00","#FFD10F","#FFE694","#EEF5C2","#BBFF6D","#68E17A","#15C286","#158F73","#165C60","#16294D","#0C63E7","#09A6F3","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "bottom")+guides(fill=guide_legend(nrow=12)) + theme(axis.text.x = element_text(angle = 90)) 
dev.copy(jpeg,"NewCyanSP.jpeg",width=35,height=25,units="cm",res=300)
dev.off()

CCC$Order[CCC$Order ==" RD011"] <- "Unclassified"
CCC$Order[CCC$Order ==" Sericytochromatia"] <- "Unclassified"
CCC$Order[CCC$Order ==" Thermosynechococcales"] <- "Unclassified"
CCC$Order[CCC$Order ==" Oxyphotobacteria_Incertae_Sedis"] <- "Unclassified"
CCC$Order[CCC$Order ==" Limnotrichales"] <- "Unclassified"
CCC$Order[CCC$Order ==" Leptolyngbyales"] <- "Unclassified"
CCC$Order[CCC$Order ==" Cyanobacteriales"] <- "Unclassified"

unique(CCC$Order)
CCC$Order <- factor(CCC$Order,
                    levels = c("Chroococcidiopsidales","Nostocales","Oscillatoriales","Synechococcales","Unclassified coocoid order","Unclassified"  ))

ggplot(data=CCC, aes(x=Type, y=Abundance, fill=Order)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FA9600","#000000","#FFD10F","#BBFF6D","#49ECC8","#15C286","#087353","#224749","#16294D","#285E83","#3FA1C8","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw() +theme (legend.position = "none")+guides(fill=guide_legend(nrow=4)) + theme(axis.text.x = element_text(angle = 90)) 
dev.copy(png,"NewCyanOrd2.png",width=27,height=9,units="cm",res=300)
dev.off()

ggplot(data=CCC, aes(x=Type, y=Abundance, fill=Order)) + facet_grid(~SITE, scales = "free")+scale_fill_manual(values = c("#022C7A","#700460","#BD0F57","#EC0F47","#FA9600","#000000","#FFD10F","#BBFF6D","#49ECC8","#15C286","#087353","#224749","#16294D","#285E83","#3FA1C8","#4CC9F0","#4895EF","#4361EE","#3F37C9","#3A0CA3","#560BAD","#7209B7","#68068E","#5F0465","#55013C","#8A0B37","#B30427","#000000","#B87D67","#DDC291","#B5CA99","#B5CA99","#000000"))+
  geom_bar(aes(), stat="identity", position="stack") + theme_bw() +theme (legend.position = "bottom")+guides(fill=guide_legend(nrow=3)) + theme(axis.text.x = element_text(angle = 90)) 
dev.copy(png,"NewCyanOrd.png",width=35,height=25,units="cm",res=300)
dev.off()

ggplot(data=Gen, aes(x=Type, y=Abundance, fill=Genus)) + facet_grid(~SITE, scales = "free")+ 
  geom_bar(aes(), stat="identity", position="stack") + theme_bw()+ theme (legend.position = "bottom")+guides(fill=guide_legend(nrow=12)) + theme(axis.text.x = element_text(angle = 90)) 
dev.copy(png,"NewCyan2.png",width=35,height=25,units="cm",res=300)
dev.off()

PhylumTab <- Bdata_glom.ALL %>% 
  dplyr::select(SITE, Type,Abundance,Kingdom,Phylum) %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

huxtable::quick_docx(PhylumTab, file = "Phylum.docx")

ggplot(data=data_glom.bac, aes(x=Sample, y=Abundance, fill=Phylum)) + facet_grid(~Site, scales = "free") +
  geom_bar(aes(), stat="identity", position="stack") + 
  scale_fill_manual(values = c("#FF3747","#FF8B0F","#FFD600","#FFED00","#EAE45F","#EEF5C2","#d1f1ac","#4FCBBB","#00E1DF","#1456b1","#5105d7","#4f5bd5","#962fbf","#d62976","#000000")) + 
  theme_bw() +  guides(fill=guide_legend(nrow=10)) + theme(axis.text.x = element_text(angle = 90))

#Alpha Diversity

A0S <- subset_samples(Ba.con, Site %in% c("AMT"))
J0S <- subset_samples(Ba.con, Site %in% c("JER"))
V0S <- subset_samples(Ba.con, Site %in% c("VOL"))
W0S <- subset_samples(Ba.con, Site %in% c("WHS"))
A0Sc <- subset_samples(Cy.con, Site %in% c("AMT"))
J0Sc <- subset_samples(Cy.con, Site %in% c("JER"))
V0Sc <- subset_samples(Cy.con, Site %in% c("VOL"))
W0Sc <- subset_samples(Cy.con, Site %in% c("WHS"))

ERA<-estimate_richness(A0S, measures = c("Observed","Shannon","Chao1","Simpson"))
ERA$Site<-rep("AMT",length(ERA$Observed))
ERA$Type<-rep(c("CL","CL","CL","CL","DA","DA","DA","DA","LA","LA","LA","LA","MO","MO","MO","MO","PT","PT","PT","PT"))
ERA$Type<-TyOrd(ERA)
ERJ<-estimate_richness(J0S, measures = c("Observed","Shannon","Chao1","Simpson"))
ERJ$Site<-rep("JER",length(ERJ$Observed))
ERJ$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","LA","LA","LA","LA","MO","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERJ$Type<-TyOrd(ERJ)
ERV<-estimate_richness(V0S, measures = c("Observed","Shannon","Chao1","Simpson"))
ERV$Site<-rep("VOL",length(ERV$Observed))
ERV$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","DA","LA","LA","LA","LA","LA","MO","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERV$Type<-TyOrd(ERV)
ERW<-estimate_richness(W0S, measures = c("Observed","Shannon","Chao1","Simpson"))
ERW$Site<-rep("WHS",length(ERW$Observed))
ERW$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","DA","LA","LA","LA","LA","LA","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERW$Type<-TyOrd(ERW)
ERall<-rbind(ERA,ERJ,ERV,ERW)

ERAc<-estimate_richness(A0Sc, measures = c("Observed","Shannon","Chao1","Simpson"))
ERAc$Site<-rep("AMT",length(ERAc$Observed))
ERAc$Type<-rep(c("CL","CL","CL","CL","DA","DA","DA","DA","LA","LA","LA","LA","MO","MO","MO","MO","PT","PT","PT","PT"))
ERAc$Type<-TyOrd(ERAc)
ERJc<-estimate_richness(J0Sc, measures = c("Observed","Shannon","Chao1","Simpson"))
ERJc$Site<-rep("JER",length(ERJc$Observed))
ERJc$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","LA","LA","LA","LA","MO","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERJc$Type<-TyOrd(ERJc)
ERVc<-estimate_richness(V0Sc, measures = c("Observed","Shannon","Chao1","Simpson"))
ERVc$Site<-rep("VOL",length(ERVc$Observed))
ERVc$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","DA","LA","LA","LA","LA","LA","MO","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERVc$Type<-TyOrd(ERVc)
ERWc<-estimate_richness(W0Sc, measures = c("Observed","Shannon","Chao1","Simpson"))
ERWc$Site<-rep("WHS",length(ERWc$Observed))
ERWc$Type<-rep(c("CL","CL","CL","CL","CL","DA","DA","DA","DA","DA","LA","LA","LA","LA","LA","MO","MO","MO","MO","PT","PT","PT","PT","PT"))
ERWc$Type<-TyOrd(ERWc)
ERallc<-rbind(ERAc,ERJc,ERVc,ERWc)

C1 <- aggregate(ERall$Chao1, by=list(ERall$Site,ERall$Type), FUN=mean)
colnames(C1) = c("Site","Type","Chao")
S1 <- aggregate(ERall$Simpson, by=list(ERall$Site,ERall$Type), FUN=mean)
colnames(S1) = c("Site","Type","Simp")

C1c <- aggregate(ERallc$Chao1, by=list(ERallc$Site,ERallc$Type), FUN=mean)
colnames(C1c) = c("Site","Type","Chao")
S1c <- aggregate(ERallc$Simpson, by=list(ERallc$Site,ERallc$Type), FUN=mean)
colnames(S1c) = c("Site","Type","Simp")

Csd <- aggregate(ERall$Chao, by=list(ERall$Site,ERall$Type), FUN=sd)
Ssd <- aggregate(ERall$Simp, by=list(ERall$Site,ERall$Type), FUN=sd)
P1r <- (Ssd[1:2,3]/sqrt(4))
P2r<- (Ssd[3:4,3]/sqrt(5))
P3r<- (Ssd[5:6,3]/sqrt(4))
P4r<- (Ssd[7:8,3]/sqrt(5))
P5r<- (Ssd[9,3]/sqrt(4))
P6r<- (Ssd[10:12,3]/sqrt(5))
P7r<- (Ssd[13,3]/sqrt(4))
P8r<- (Ssd[14:16,3]/sqrt(5))
P9r<- (Ssd[17,3]/sqrt(4))
P10r<- (Ssd[18:19,3]/sqrt(5))
P11r<- (Ssd[20,3]/sqrt(4))

Ssd<-cbind(c(P1r,P2r,P3r,P4r,P5r,P6r,P7r,P8r,P9r,P10r,P11r))

P1r <- (Csd[1:2,3]/sqrt(4))
P2r<- (Csd[3:4,3]/sqrt(5))
P3r<- (Csd[5:6,3]/sqrt(4))
P4r<- (Csd[7:8,3]/sqrt(5))
P5r<- (Csd[9,3]/sqrt(4))
P6r<- (Csd[10:12,3]/sqrt(5))
P7r<- (Csd[13,3]/sqrt(4))
P8r<- (Csd[14:16,3]/sqrt(5))
P9r<- (Csd[17,3]/sqrt(4))
P10r<- (Csd[18:19,3]/sqrt(5))
P11r<- (Csd[20,3]/sqrt(4))

Csd<-cbind(c(P1r,P2r,P3r,P4r,P5r,P6r,P7r,P8r,P9r,P10r,P11r))
S01<-S1%>% dplyr::select(Simp)
AlAll<-cbind(C1,Csd,S01,Ssd)

AlAll$Csd<-round(AlAll$Csd,digits=3)
AlAll$Ssd<-round(AlAll$Ssd,digits=3)
AlAll$Chao<-round(AlAll$Chao,digits=3)
AlAll$Simp<-round(AlAll$Simp,digits=3)

colnames(AlAll) = c("Site","Type","Chao1","SE","Simpson","SE2")
View(ERall)
SiOrd<- function(x){factor(x$Site,levels=c("AMT","JER","VOL","WHS"))}
AlAll$Site<-SiOrd(AlAll)


AlphaTab <- AlAll %>% 
  %>% 
  as_hux() %>%
  set_background_color(evens, everywhere, "grey95")%>%
  set_align(1, everywhere, "center") %>% 
  theme_article() %>% 
  set_tb_padding(2)

CH<-ggplot(data=ERallc, aes(x=Type, y=Chao1)) + ylab("Simpson")+geom_boxplot() +
  geom_point(size=5,alpha=.8,stat="identity",aes(color=Site)) + scale_color_manual(values=c(  "#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + theme_bw()
SI<-ggplot(data=ERallc, aes(x=Type, y=Simpson)) + ylab("Simpson")+geom_boxplot() +
  geom_point(size=5,alpha=.8,stat="identity",aes(color=Site)) + scale_color_manual(values=c(  "#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + theme_bw()

dev.copy(png,"PLFAdi.png",width=12,height=9,units="cm",res=300)
dev.off()

AlphAcC<-ggplot(ERallc, aes(x=Type, y=Chao1,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 
AlphAc<-ggplot(ERall, aes(x=Type, y=Chao1,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 
AlphAs<-ggplot(ERall, aes(x=Type, y=Shannon,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 
AlphAo<-ggplot(ERall, aes(x=Type, y=Observed,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 
AlphAsi<-ggplot(ERall, aes(x=Type, y=Simpson,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 
AlphAsiC<-ggplot(ERallc, aes(x=Type, y=Simpson,color=Type)) + geom_boxplot()+
  stat_summary(fun = "mean", geom = "point", shape = 1, size=1, color="Blue")+
  scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw() +facet_grid(~Site) 

ggarrange(AlphAc,AlphAsi, ncol=1, nrow=2)
dev.copy(jpeg,"16SAlpha.jpeg",width=20,height=12,units="cm",res=300)
dev.off()

ERall$Chao1c<-ERallc$Chao1
ERall$Simpsonc<-ERallc$Simpson


cor.test(ERall$Chao1,ERall$Chao1c,  method = "pearson", use = "complete.obs")
ThTBMt<-lm(Chao1c~Chao1,data=ERall)
summary(ThTBMt)

ggplot(ERall,aes(x=Chao1,y=Chao1c,color=Type,shape=Site)) + geom_point(size=4,alpha=.8) +  xlab("Bacterial Diversity (Chao1)") + ylab("Cyanobacterial Diversity (Chao1)") + theme_bw() + theme(legend.position="right") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8)))

ggplot(ERall,aes(x=Simpson,y=Simpsonc,color=Type,shape=Site)) + geom_point(size=4,alpha=.8) +  xlab("Bacterial Diversity (Simpson)") + ylab("Cyanobacterial Diversity (Simpson)") + theme_bw() + theme(legend.position="right") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044")) + scale_shape_manual(values=(c(17,15,19,8)))


ggarrange(AlphAcC,AlphAsiC, ncol=1, nrow=2)
dev.copy(jpeg,"16SAlphaCY.jpeg",width=20,height=12,units="cm",res=300)
dev.off()
library(lmPerm)
Alp.aov<-aovp(Chao1~Site*Type,data=ERall)
summary(Alp.aov)
Alp.aov<-aovp(Simpson~Site*Type,data=ERall)
summary(Alp.aov)
ERall$tname<-paste(ERall$Site,ERall$Type,sep="")

AAL<-ERallc %>% filter(Type=="LA")
AAD<-ERallc %>% filter(Type=="DA")
AAC<-ERallc %>% filter(Type=="CL")
AAP<-ERallc %>% filter(Type=="PT")
AAM<-ERallc %>% filter(Type=="MO")

AAA<-ERallc %>% filter(Site=="AMT")
AAJ<-ERallc %>% filter(Site=="JER")
AAV<-ERallc %>% filter(Site=="VOL")
AAW<-ERallc %>% filter(Site=="WHS")

tname0<-AAA %>% dplyr::select(tname)
type0<-AAV %>% dplyr::select(Type)
site0<-AAM %>% dplyr::select(Site)
chao0<-AAM %>% dplyr::select(Chao1)
simp0<-AAM %>% dplyr::select(Simpson)

set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
site0[,1]<-as.factor(site0[,1])
type0[,1]<-as.factor(type0[,1])
chao0<-unlist(chao0, recursive = TRUE, use.names = FALSE)
simp0<-unlist(simp0, recursive = TRUE, use.names = FALSE)
site0<-unlist(site0, recursive = TRUE, use.names = FALSE)
type0<-unlist(type0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=49)
pairwise.perm.t.test(simp0,site0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)

perm.anova(Chao1~Site*Type,nperm=49)
pairwise.perm.t.test(Net0,tname0,nperm=99)

#Beta Diversity

p3<-plot_ordination(Ba.con, bacteria.nmds, type = "samples", 
                color = "Site", shape="Type")  + scale_y_continuous(limits = c(-1.5,1.5)) +scale_x_continuous(limits = c(-1.5,1.5)) + scale_color_manual(values=c("#FD612C", "#62D26F", "#FFDD2B","#208EA3"))+
  scale_shape_manual(values=(c(17,15,1,19,5))) + theme_bw()
dev.copy(png,"NMDS16s.png",width=11,height=9,units="cm",res=300)
dev.off()

##Add Statistics - using adonis (Permanova) to compare beta diversity measure

mod.05 <- adonis(ps.dist.bacteria ~ Site*Type, as(sample_data(Ba.con),"data.frame"))
mod.05

### PERCENT SEPERATION ###

BCol <- read.csv("BiomassAllCol.csv", header = TRUE)

Bcat <- as.data.frame(BCol[1:220,])
Bcat$Amount <- as.numeric(Bcat$Amount)
Bex <- as.data.frame(BCol[221:401,])
Bcat$Amount <- as.numeric(Bcat$Amount)
Bex$Amount <- as.numeric(Bex$Amount)
Bcat$Type <- factor(Bcat$Type, levels=c("LA","DA","PT","CL","MO"))
Bex$Type <- factor(Bex$Type, levels=c("LA","DA","PT","CL","MO"))

Bcatb <- Bcat[Bcat$Biomass != "TotalBacteriaBiomass",]
Bcatt <- Bcatb[Bcatb$Biomass != "TotalFungiBiomass",]
Bcattt <- Bcatt[Bcatt$Biomass != "GramPosBiomassA",]

Bexx <- Bex[1:180,1:4]
BexD <- Bexx[Bexx$Biomass == "DiversityIndex",]
BexS <- Bexx[Bexx$Biomass == "Sat:Unsat",]
BexM <- Bexx[Bexx$Biomass == "Mono:Poly",]

BDI <- with(BexD, BexD[order(Site, Amount, Type),])
ggplot(data=BexD, aes(x=Type, y=Amount, fill=Biomass,color=Type)) + ylab("Simpson")+
  geom_point(size=5,alpha=.8,stat="identity") + scale_color_manual(values=c(  "#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ facet_grid(~Site) + theme_bw()
dev.copy(png,"PLFAalpha.png",width=25,height=7,units="cm",res=300)
dev.off()

DivMean <- aggregate(BexD$Amount, by=list(BexD$Type), FUN=mean)

# DIVERSITY INDEX ANOVA #
PPPPP<-aov(Amount ~ Type,data=BexD)
summary(PPPPP)

ggplot(data=BexD, aes(x=Type, y=Amount)) + ylab("Simpson")+geom_boxplot() +
  geom_point(size=5,alpha=.8,stat="identity",aes(color=Site)) + scale_color_manual(values=c(  "#FD612C", "#62D26F", "#FFDD2B","#208EA3")) + theme_bw()
dev.copy(png,"PLFAdi.png",width=12,height=9,units="cm",res=300)
dev.off()

BexDtab<-BexD %>% dplyr::select(Site,Type,Amount)
colnames(BexDtab) = c("Site","Type","Diversity Index")
write.table(BexDtab, file = "PLFADI.txt", sep = ",", quote = FALSE, row.names = F)

BSU <- with(BexS, BexS[order(Site, Amount, Type),])
ggplot(data=BexS, aes(x=Type, y=Amount, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#2A363B"))+ facet_grid(~Site) + theme_bw()

BM <- with(BexM, BexM[order(Site, Amount, Type),])
ggplot(data=BexM, aes(x=Type, y=Amount, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#2A363B"))+ facet_grid(~Site) + theme_bw()

Bcattt$Biomass[Bcattt$Biomass == "ActinomycetesBiomass"] <- "Actinomycetes"
Bcattt$Biomass[Bcattt$Biomass == "GramPosBiomass"] <- "Other Gram(+)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "GramNegBiomass"] <- "Other Gram(-)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "ArbuscularMycorrhizalBiomass"] <- "Arbuscular Mycorrhizae"
Bcattt$Biomass[Bcattt$Biomass == "RhizobiaBiomass"] <- "Rhizobia"
Bcattt$Biomass[Bcattt$Biomass == "SaprophytesBiomass"] <- "Saprophytes"
Bcattt$Biomass[Bcattt$Biomass == "ProtozoaBiomass"] <- "Protozoa"
Bcattt$Biomass[Bcattt$Biomass == "UndifferentiatedBiomass"] <- "Undifferentiated"

a1 <-Bcattt %>% filter(Biomass=="Actinomycetes")
a2 <-Bcattt %>% filter(Biomass=="Other Gram(+)Bacteria")
a3 <-Bcattt %>% filter(Biomass=="Other Gram(-)Bacteria")
a4 <-Bcattt %>% filter(Biomass=="Arbuscular Mycorrhizae")
a5 <-Bcattt %>% filter(Biomass=="Saprophytes")
a6 <-Bcattt %>% filter(Biomass=="Undifferentiated")

Net0<-a6 %>% dplyr::select(Biomass)
set.seed(1203)
tname0[,1]<-as.factor(tname0[,1])
Resp0<-unlist(Resp0, recursive = TRUE, use.names = FALSE)
Amax0<-unlist(Amax0, recursive = TRUE, use.names = FALSE)
Net0<-unlist(Net0, recursive = TRUE, use.names = FALSE)
tname0<-unlist(tname0, recursive = TRUE, use.names = FALSE)

perm.anova(Resp0~tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,nperm=99)
pairwise.perm.t.test(Net0,tname0,alternative = c("two.sided","less", "greater"),p.method="none",paired=FALSE,nperm=999)

MII<-aov(Percent ~ Type,data=a5)
MII<-aovp(Percent ~ Type,data=a3)
summary(MII)
TukeyHSD(MII)

Bcattt$Biomass <- factor(Bcattt$Biomass, 
                         levels = c("Actinomycetes","Other Gram(+)Bacteria","Rhizobia","Other Gram(-)Bacteria","Arbuscular Mycorrhizae","Saprophytes","Protozoa","Undifferentiated"))


BioComp <- with(Bcattt, Bcattt[order(Site, Biomass, Type),])
ggplot(data=Bcattt, aes(x=Type, y=Amount, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#FF3747","#FF8B0F","#FFD600","#FFED00","#EEF5C2","#4FCBBB","#00E1DF","#1456b1","#962fbf","#d62976","#000000"))+ facet_grid(~Site) + theme_bw()
dev.copy(png,"PLFABio.png",width=25,height=7,units="cm",res=300)
dev.off()


Pcomp <- with(Bcattt, Bcattt[order(Site, Percent, Type),])
ggplot(data=Bcattt, aes(x=Type, y=Percent, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#FF3747","#FF8B0F","#FFD600","#FFED00","#EEF5C2","#4FCBBB","#00E1DF","#1456b1","#962fbf","#d62976","#000000"))+ facet_grid(~Site) + theme_bw()
dev.copy(png,"PLFARel.png",width=25,height=7,units="cm",res=300)
dev.off()

ggplot(data=Bcattt, aes(y=Percent, x=Biomass,color=Type)) + ylab("Relative Abundance (%)")+xlab("Biomass Type")+geom_boxplot() +
  geom_point(size=3,alpha=.8,stat="identity",position=position_dodge(width=.75),aes(color=Type)) + scale_color_manual(values=c("#FFCC99","#FF9966","#FF0066","#660066","#000044"))+ theme_bw()
dev.copy(png,"BoxPrel.png",width=32,height=7,units="cm",res=300)
dev.off()



ggplot(data=Bcattt, aes(x=Type, y=Percent, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#FF3747","#FF8B0F","#FFD600","#FFED00","#EEF5C2","#4FCBBB","#00E1DF","#1456b1","#962fbf","#d62976","#000000"))+ facet_grid(~Site) + theme_bw()


Bcattt$Biomass[Bcattt$Biomass == "Actinomycetes"] <- "Gram(+)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "Other Gram(+)Bacteria"] <- "Gram(+)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "Other Gram(-)Bacteria"] <- "Gram(-)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "Arbuscular Mycorrhizae"] <- "Fungi"
Bcattt$Biomass[Bcattt$Biomass == "Rhizobia"] <- "Gram(-)Bacteria"
Bcattt$Biomass[Bcattt$Biomass == "Saprophytes"] <- "Fungi"
Bcattt$Biomass[Bcattt$Biomass == "Protozoa"] <- "Protozoa"
Bcattt$Biomass[Bcattt$Biomass == "Undifferentiated"] <- "Undifferentiated"

Bcattt$Biomass <- factor(Bcattt$Biomass, 
                         levels = c("Gram(+)Bacteria","Gram(-)Bacteria","Fungi","Protozoa","Undifferentiated"))

mAb <- aggregate(Bcattt$Amount, by=list(Bcattt$Site,Bcattt$Type,Bcattt$Biomass), FUN=sum)

FB<-mAb[1:20,1:2]
colnames(FB) = c("Site","Type")
FB$Fungi<-mAb[1:20,4]
FB$GramPos<-mAb[21:40,4]
FB$GramNeg<-mAb[41:60,4]
FB$Ftrans<-FB$Fungi/(FB$Fungi+FB$GramPos+FB$GramNeg)
FB$GPtrans<-FB$GramPos/(FB$Fungi+FB$GramPos+FB$GramNeg)
FB$GNtrans<-FB$GramNeg/(FB$Fungi+FB$GramPos+FB$GramNeg)

ggplot(data=Bcattt, aes(x=Type, y=Percent, fill=Biomass)) + 
  geom_bar(stat="identity") + scale_fill_manual(values=c("#FFED00","#FFD600","#EEF5C2","#00E1DF","#1456b1","#962fbf","#d62976","#000000"))+ facet_grid(~Site) + theme_bw()
dev.copy(png,"FtoB.png",width=25,height=7,units="cm",res=300)
dev.off()
