# Hoellrich-Masters-Thesis
R code for analyzing C-flux and Sequencing data for my masters thesis

PullPhotoLicor.R provides the code to pull out the Photo (the C-flux data) line from raw licor files along with the code to process that information (finding quantum yield, curve shape, light saturation point, calcuating C fixaiton),

     - This file assumes you are working with biocrust communities, not plant communities 
     - A Plant=1 entity, biocrust=community of autotrophs and heterotrophs

Transect info.R provides the code for calculating % cover from site transects and was written when I was just starting out with R. So the code is a bit inelegant. 

     - AllTheTransects0.csv contains the transect data
     
TH_Cfix_Code.R contains the analysis of carbon flux data as well as PCA plot of soil chemical data.

     - JRN_metadata_Study549_CfixBiomass.csv (includes all carbon flux data, thickness, soil chemical data, PLFA data) 
     - NFixRready.csv (includes additional thickness measurements, and % water content)

TH_Diversity_Code.R provides the code for 16S microbial analysis and PLFA biomass and relitive abundance analysis. 

     - TH16S.otu_table.txt, TH16S.ASVs.fa and MappingtH16S.csv include 16S data
     - UnknownOver1percentFix.csv and GenusFix.csv include reclassification of cyanobacteria taxonomy
     - BiomassAllCol.csv include the PLFA biomass and relative abundance data
