###############################################################################
#L2-Boosting
###############################################################################
# Laden des R-Pakets 'mboost'
    library(mboost)
#Pfad setzen
setwd("D:/OneDrive - Technische Hochschule Deggendorf/Materialen - Uni/4. Semester/Maschinelles Lernen/motorbikes_Gutoranska_Likhacheva_Solodovnyk")

#ersetzt die Leerzeichen in den Daten durch R verstandliche "NA" Werte
Daten <- read.csv("bikes_imputed.csv", header=TRUE, sep=",", fill=TRUE, stringsAsFactors=TRUE, na.strings=" ") 


# Entfernung Ausrei?er
quartiles <- quantile(Daten$ex_showroom_price, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(Daten$ex_showroom_price)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

Daten <- subset(Daten, Daten$ex_showroom_price > Lower & Daten$ex_showroom_price < Upper)

quartiles <- quantile(Daten$km_driven, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(Daten$km_driven)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

Daten <- subset(Daten, Daten$km_driven > Lower & Daten$km_driven < Upper)

Daten[1:10,]

# Kontrolle der Datentypen durch Ausgabe der Summary
summary(Daten)

# Vertauschung der Reihenfolge der Daten
n<-length(Daten[,1])
n
index <- sample(1:n,n,replace=FALSE)
Daten <- Daten[index,]

# Aufteilung 910 Daten in Training und Test  
Daten.train <- Daten[1:637,]
Daten.test <- Daten[638:910,] 
   

 # Berechnung des Modells (noch nicht kreuzvalidiert)

    model <- gamboost(selling_price ~ year + km_driven + ex_showroom_price, data=Daten.train, dfbase = 4, control = boost_control(mstop = 1000))
    model
    par(mfrow=c(1,3)) # Weil es 3 Einflussvariablen sind
    plot(model)

 # Kreuzvalidierung:
  
    cv10f <- cv(model.weights(model), type = "kfold")
    cvm <- cvrisk(model, folds = cv10f, papply = lapply)
    print(cvm)
    mstop(cvm)
    plot(cvm)

  # Berechnung des Modells (nun kreuzvalidiert), 
  # daher steht in den Optionen: 'mstop = mstop(cvm)'

    model <- gamboost(selling_price ~ year + km_driven + ex_showroom_price, data=Daten.train, dfbase = 4, control = boost_control(mstop = mstop(cvm)))
    model
    par(mfrow=c(1,3))  # Weil es 3 Einflussvariablen sind
    plot(model)


  # Berechnung der Prognoseergebnisse auf den Testdaten:

    X.test <- Daten.test[,c("year" , "km_driven" , "ex_showroom_price")]
    prognosen <- predict(model,X.test)

# Berechnung des mittleren Prognosefehlers (MAD)
y.test <- Daten.test[,"selling_price"]
    mean(abs((y.test-prognosen)/y.test))*100

