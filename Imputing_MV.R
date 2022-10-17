######################
#  Data Imputation   #
# fur fehlende Werte #
######################

#####
# Pakete
# install.packages('mice')
library(mice)

#####
# Pfad setzen
setwd("D:/OneDrive - Technische Hochschule Deggendorf/Materialen - Uni/4. Semester/Maschinelles Lernen/Prufungsstudienarbeit")

bikes <- read.csv(
  "BIKE DETAILS.csv",
  header=TRUE,
  sep=",",
  fill=TRUE,
  stringsAsFactors=TRUE,
  na.strings=" ") # ersetzt die Leerzeichen in den Daten durch R verstandliche "NA" Werte

summary(bikes)
    
    
#####
# Spalten als Faktor definieren, die nicht als Faktor erkannt wurden
# denke alles kommt richtig aus


#####
# Auflistung aller Spalten und Summieren der Zellen mit Wert NA
sapply(bikes, function(bikes) sum(is.na(bikes)))


#####
# MICE Setup
#init = mice(bikes, method=meth, predictorMatrix=predM, m=5) 
meth = init$method
predM = init$predictorMatrix

# to skip a variable from imputation 
meth[c("name")]=""
meth[c("selling_price")]=""
meth[c("year")]=""
meth[c("seller_type")]=""
meth[c("owner")]=""
meth[c("km_driven")]=""

meth[c("ex_showroom_price")]="pmm" # pmm = predictive mean matching


# Precisely, the methods used by this package are:
# - PMM (Predictive Mean Matching) – For numeric variables.
# - logreg(Logistic Regression) – For Binary Variables( with 2 levels)
# - polyreg(Bayesian polytomous regression) – For Factor Variables (>= 2 levels)
# - Proportional odds model (ordered, >= 2 levels)

# print(init)
# Werte mit MICE berechnen
imputed = mice(bikes, method=meth, predictorMatrix=predM, m=5)

imputed <- complete(imputed)

# Prufen, ob Daten vollstandig sind

sapply(imputed, function(x) sum(is.na(x)))

summary(imputed)

# Schreiben der Daten in neues CSV Datei.
write.csv(imputed,"bikes_imputed.csv", row.names = FALSE)