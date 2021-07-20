# Hoellrich-Masters-Thesis
R code for analyzing C-flux and Sequencing data for my masters thesis

PullPhotoLicor.R provides the code to pull out the Photo (the C-flux data) line from raw licor files along with the code to process that information (finding quantum yield, curve shape, light saturation point, calcuating C fixaiton)
     - This file assumes you are working with biocrust communities, not plant communities (Plant=1 entity, biocrust=community of autotrophs and heterotrophs)


Transect info.R provides the code for calculating % cover from site transects and was written when I was just starting out with R. So the code is a bit inelegant. 
