
#################################################################################################
#				Projet de macroéconométrie : Effet d'un choc de désinflation dans l'UE et aux US				#
#################################################################################################


# Commençons par charger quelques packages
library(fUnitRoots)
library(urca)
library(tseries)
require(tseries)
require(urca)
require(MASS)
require(fUnitRoots)

#Baptiste : 
setwd("~/Documents/MiE2/Macroeconometrics")

# Eva : 
setwd("/home/eva/ENSAE/macroeconometrie/macroenometrics")

# Chargeons les données (fichier Stata .dta)
library(haven)
data_us <- read_dta("data_us.dta")


# View(data_euro)


# Observons les caractéristiques principales des données
head(data_euro)
tail(data_euro)
names(data_euro)
summary(data_euro)
