
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
data_euro <- read_dta("data_euro.dta")


# View(data_euro)


# Observons les caractéristiques principales des données
head(data_euro)
tail(data_euro)
names(data_euro)
summary(data_euro)

# On va à présent analyser les variables d'intérêt, et
# plus particulièrement leur stationnarité. Si k
# variables ne sont pas stationnaires, on va chercher
# à "forcer", dans le VAR, k-1 relations de cointégration
# à partir de la théorie économique


##############
# Désinflation


# On transforme l'indice des prix en inflation
data_euro$infla <- NA
for (i in 5:length(data_euro$hicp)) data_euro$infla[i] <- ((data_euro$hicp[i]/data_euro$hicp[i-4])-1)*100

# Et désormais, l'inflation en désinflation
data_euro$desinfla <- NA
for (i in 6:length(data_euro$desinfla)) data_euro$desinfla[i] <- data_euro$infla[i]-data_euro$infla[i-1]

# On regarde l'allure des courbes, surtout celle de "desinfla"
plot(data_euro$infla, main="", col="red", type = "l")
plot(data_euro$desinfla, main="", col="red", type = "l")

# La désinflation a l'air stationnaire, et centrée. Plutôt que d'estimer le 3ème
# modèle, le plus contraint, de la procédure emboîtée de DF, on la mène dans son
# intégralité. On introduit des lags pour blanchir au maximum les résidus,
# et le nombre de lags est celui qui minimise le BIC (qui est convergent)
a<-ur.df(data_euro$desinfla[6:length(data_euro$desinfla)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
# On conclut à la stationnarité de la série (-8.909<-3.43) à 5%, mais le modèle n'est pas le bon :
# en testant la signif du trend en utilisant le t de Student (car cas stationnaire), on voit que
# celui-ci n'est pas significatif. Il ne s'agit donc pas du "bon" modèle
a<-ur.df(data_euro$desinfla[6:length(data_euro$desinfla)], type = "drift", selectlags = "BIC", lags=10)
summary(a)
# Mêmes remarques, avec la constante : attendu, puisque la série "désinfla" ne semble pas avoir
# de trend.
a<-ur.df(data_euro$desinfla[6:length(data_euro$desinfla)], type = "none", selectlags = "BIC", lags=10)
summary(a)

# On mène un test de KPSS pour s'assurer que la série est bien stationnaire
# Graphiquement, on voit qu'elle l'est très sûrement, on vérifie par précaution
# avec un autre test que l'ADF, considéré peu puissant.
# La série ne présentant pas de tendance, on applique l'option "mu", testant la
# stationnarité de la série en niveau. On utilise les 2 méthodes de correction
# de l'auto-corrélation des résidus.
a<-ur.kpss(data_euro$desinfla, type="mu", lags="long")
summary(a)
a<-ur.kpss(data_euro$desinfla, type="mu", lags="short")
summary(a)

# On mène également le test de Phillips Perron, là aussi à partir des 2 méthodes 
# de correction de l'auto-corrélation des résidus :
a<-ur.pp(data_euro$desinfla, type="Z-tau", model="constant", lags="long")
summary(a)
a<-ur.pp(data_euro$desinfla, type="Z-tau", model="constant", lags="short")
summary(a)


##############
# Consommation


plot(data_euro$pcr, type="l")

# La conso n'est pas stationnaire, mais étant à LT liée au revenu,
# on peut forcer une relation de cointégration conso-revenu.

# Calcul de la part de la conso dans le revenu
data_euro$part_conso <- (data_euro$pcr/data_euro$yer)*100
data_euro$log_part_conso<-log(data_euro$part_conso)
plot(data_euro$log_part_conso, main="", col="red", type = "l")

a<-ur.df(data_euro$log_part_conso, type = "trend", selectlags = "BIC", lags=10)
summary(a)
# -3.2682 > -3.43 : on ne rejette pas l'hypothèse nulle de racine unitaire
# 5.6321  < 6.49  : ce modèle n'est pas le "bon" pour tester cela
a<-ur.df(data_euro$log_part_conso, type = "drift", selectlags = "BIC", lags=10)
summary(a)
# -1.8398 > -2.88 : on ne rejette pas l'hypothèse nulle de racine unitaire
# 1.783   < 4.63  : ce modèle n'est toujours pas le bon pour tester cela
# Comme le plot de la série le suggérait (il ne semble pas y avoir de trend
# dans la série en niveau), le modèle le plus contraint est le "bon"
a<-ur.df(data_euro$log_part_conso, type = "none", selectlags = "BIC", lags=10)
summary(a)

a<-ur.kpss(data_euro$log_part_conso, type="mu", lags="long")
summary(a)
a<-ur.kpss(data_euro$log_part_conso, type="mu", lags="short")
summary(a)

a<-ur.pp(data_euro$log_part_conso, type="Z-tau", model="constant", lags="long")
summary(a)
a<-ur.pp(data_euro$log_part_conso, type="Z-tau", model="constant", lags="short")
summary(a)

# Les tests ADF, PP et KPSS concluent tous à la non-stationnarité
# de la série. On se raccroche toutefois à la théorie.

# On calcule par ailleurs la différence première de la série log(consommation)
data_euro$log_conso<-log(data_euro$pcr)
plot(data_euro$log_conso, main="", col="red", type = "l")
data_euro$diff_log_conso<-NA
for(i in 2:length(data_euro$diff_log_conso)) data_euro$diff_log_conso[i] <- data_euro$log_conso[i] - data_euro$log_conso[i-1]
plot(data_euro$diff_log_conso, type="l")

# Et on teste sa stationnarité
a<-ur.df(data_euro$diff_log_conso[2:length(data_euro$diff_log_conso)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$diff_log_conso, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$diff_log_conso, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$diff_log_conso, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_log_conso, type="Z-tau", model="trend", lags="short")
summary(a)


############
# Output gap


plot(data_euro$yer, type="l")

# Le PIB n'est évidemment pas stationnaire. On va créer 2 variables
# : l'une à partir du filtre HP (le cycle), et l'autre étant la diff
# première de son log, plus facile pour ensuite poser des conditions
# d'identification

install.packages("mFilter")
install.packages("quantmod")
library(mFilter)
library(quantmod)

# On prend la composante cyclique du PIB
# On utilise le filtre HP, avec lambda=1600 car séries 
# trimestrielles (Ravn et Uhlig, 2002)

data_euro$log_gdp <- log(data_euro$yer)
data_euro$og_euro <- NA
data_euro$og_euro <- hpfilter(data_euro$log_gdp,freq=1600)$cycle
plot(data_euro$og_euro, main="", col="red", type = "l")

# On va également prendre la différence première du PIB

data_euro$diff_log_gdp<-NA
for(i in 2:length(data_euro$diff_log_gdp)) data_euro$diff_log_gdp[i] <- data_euro$log_gdp[i] - data_euro$log_gdp[i-1]
plot(data_euro$diff_log_gdp, type="l")

a<-ur.df(data_euro$diff_log_gdp[2:length(data_euro$diff_log_gdp)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
a<-ur.df(data_euro$diff_log_gdp[2:length(data_euro$diff_log_gdp)], type = "drift", selectlags = "BIC", lags=10)
summary(a)
# Remarque : la constante est significative, mais très faible
a<-ur.kpss(data_euro$diff_log_gdp, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$diff_log_gdp, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$diff_log_gdp, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_log_gdp, type="Z-tau", model="trend", lags="short")
summary(a)


################
# Investissement


data_euro$part_invest <- (data_euro$itr/data_euro$yer)*100
plot(data_euro$part_invest, main="", col="red", type = "l")

data_euro$log_part_invest<-log(data_euro$part_invest)
plot(data_euro$log_part_invest, main="", col="red", type = "l")

a <- ur.df(data_euro$log_part_invest, type = "trend", selectlags = "BIC", lags=10)
summary(a)
a <- ur.df(data_euro$log_part_invest, type = "drift", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$log_part_invest, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$log_part_invest, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$log_part_invest, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$log_part_invest, type="Z-tau", model="trend", lags="short")
summary(a)
# Ils ne concluent pas tous à la stationnarité de la série : ADF dit qu'elle est
# I(0), et KPSS + PP qu'elle est I(1)

# Différence du log de l'investissement

data_euro$log_invest<-log(data_euro$itr)
data_euro$diff_log_invest<-NA
for(i in 2:length(data_euro$diff_log_invest)) data_euro$diff_log_invest[i] <- data_euro$log_invest[i] - data_euro$log_invest[i-1]
plot(data_euro$diff_log_invest, type="l")

a <- ur.df(data_euro$diff_log_invest[2:length(data_euro$diff_log_invest)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
a <- ur.df(data_euro$diff_log_invest[2:length(data_euro$diff_log_invest)], type = "drift", selectlags = "BIC", lags=10)
summary(a)
a <- ur.df(data_euro$diff_log_invest[2:length(data_euro$diff_log_invest)], type = "none", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$diff_log_invest, type="mu", lags="long")
summary(a)
a<-ur.kpss(data_euro$diff_log_invest, type="mu", lags="short")
summary(a)
a<-ur.pp(data_euro$diff_log_invest, type="Z-tau", model="constant", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_log_invest, type="Z-tau", model="constant", lags="short")
summary(a)


#########
# Chômage


plot(data_euro$urx, type="l")
data_euro$chomage_euro_cycle <- hpfilter(data_euro$urx,freq=1600)$cycle

# On va également calculer la différence première du taux de chômage

data_euro$diff_chom<-NA
for(i in 2:length(data_euro$diff_chom)) data_euro$diff_chom[i] <- data_euro$urx[i] - data_euro$urx[i-1]
plot(data_euro$diff_chom, type="l")

a <- ur.df(data_euro$diff_chom[2:length(data_euro$diff_chom)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
a <- ur.df(data_euro$diff_chom[2:length(data_euro$diff_chom)], type = "drift", selectlags = "BIC", lags=10)
summary(a)
a <- ur.df(data_euro$diff_chom[2:length(data_euro$diff_chom)], type = "none", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$diff_chom, type="mu", lags="long")
summary(a)
a<-ur.kpss(data_euro$diff_chom, type="mu", lags="short")
summary(a)
a<-ur.pp(data_euro$diff_chom, type="Z-tau", model="constant", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_chom, type="Z-tau", model="constant", lags="short")
summary(a)


#############
# Salaire réel

data_euro$sal_reel<-data_euro$wrn/data_euro$pcd
plot(data_euro$sal_reel, type="l")
plot(data_euro$wrn, type="l")
data_euro$log_sal_reel<-log(data_euro$sal_reel)
data_euro$log_sal_reel_cycle<-hpfilter(data_euro$log_sal_reel, freq=1600)$cycl
plot(data_euro$log_sal_reel_cycle, type="l")

# On prend aussi la différence du salaire réel 
data_euro$diff_log_sal_reel<-NA
for(i in 2:length(data_euro$diff_log_sal_reel)) data_euro$diff_log_sal_reel[i] <- data_euro$log_sal_reel[i] - data_euro$log_sal_reel[i-1]
plot(data_euro$diff_log_sal_reel, type="l")

a <- ur.df(data_euro$diff_log_sal_reel[2:length(data_euro$diff_chom)], type = "trend", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$diff_log_sal_reel, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$diff_log_sal_reel, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$diff_log_sal_reel, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_log_sal_reel, type="Z-tau", model="trend", lags="short")
summary(a)


################
# Taux d'intérêt

plot(data_euro$ltn, type="l")
plot(data_euro$stn, type="l")

# Les séries ne semblent pas stationnaires, on le vérifie tout de même

a<-ur.df(data_euro$ltn, type = "trend", selectlags = "BIC", lags=10)
summary(a)
# On est proche de la valeur critique
a<-ur.df(data_euro$ltn, type = "drift", selectlags = "BIC", lags=10)
summary(a)
a<-ur.df(data_euro$ltn, type = "none", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$ltn, type="mu",lags="long")
summary(a)
a<-ur.kpss(data_euro$ltn, type="mu",lags="short")
summary(a)
a<-ur.pp(data_euro$diff_log_sal_reel, type="Z-tau", model="constant", lags="long")
summary(a)
a<-ur.pp(data_euro$diff_log_sal_reel, type="Z-tau", model="constant", lags="short")
summary(a)


a<-ur.df(data_euro$stn, type = "trend", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$stn, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$stn, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$stn, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$stn, type="Z-tau", model="trend", lags="short")
summary(a)


data_euro$spread <- data_euro$ltn-data_euro$stn
plot(data_euro$spread, type="l")
a<-ur.df(data_euro$spread, type="trend", selectlags = "BIC", lags=10)
summary(a)
a<-ur.kpss(data_euro$spread, type="tau", lags="long")
summary(a)
a<-ur.kpss(data_euro$spread, type="tau", lags="short")
summary(a)
a<-ur.pp(data_euro$spread, type="Z-tau", model="trend", lags="long")
summary(a)
a<-ur.pp(data_euro$spread, type="Z-tau", model="trend", lags="short")
summary(a)



install.packages("MSBVAR")
library("MSBVAR")
require(MSBVAR)

# Un autre package avec plusieurs priors : MSBSVAR