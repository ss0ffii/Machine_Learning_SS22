#################################################################
# Maschinelles Lernen mit SVMs
#################################################################

# F?r maschinelles Lernen muss erst ein Zusatzpaket in R installiert werden
# Wir verwenden im folgenden das Paket "e1071"
# Pakete muss man (einmalig) mit folgendem Befehl installieren
# (Internetverbindung n?tig!):

# install.packages("e1071")

# Damit wird das Paket auf dem eigenen Computer installiert. Immer wenn
# man das Paket verwenden will, muss man es aber in der aktuellen
# R-Sitzung aus der Bibliothek laden. Das geht folgenderma?en:


library(e1071)

##################
# Regression mit SVR (Support Vector Regression)
##################

#Pfad setzen
setwd("D:/OneDrive - Technische Hochschule Deggendorf/Materialen - Uni/4. Semester/Maschinelles Lernen/motorbikes_Gutoranska_Likhacheva_Solodovnyk")

#ersetzt die Leerzeichen in den Daten durch R verstandliche "NA" Werte
Daten <- read.csv("bikes_imputed.csv", header=TRUE, sep=",", fill=TRUE, stringsAsFactors=TRUE, na.strings=" ") 
Daten[1:10,]

# Kontrolle der Datentypen durch Ausgabe der Summary
summary(Daten)

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


# Definition der Tuning-Parameter
cc <- seq(-5,10,1)       # f?r m?gliche Werte von "Cost" (Tuningparameter)
cg <- seq(-4,1,0.5)      # f?r m?gliche werte von "gamma" (Tuningparameter)

# Berechnung des Modells
tuning <- tune.svm(selling_price ~ name + year + km_driven + ex_showroom_price, data=Daten, scale = TRUE, type = "eps-regression", kernel = "radial", gamma = 10^cg, cost = 2^cc, epsilon = 0.1, tunecontrol = tune.control(sampling = "cross",cross=5))

print(tuning)

# Speichern des Modells mit den besten Parametern:
model <- tuning$best.model      # das Model mit optimalen Tuningparametern

# Berechnen von Prognosen

# Berechnung der Prognosen f?r die Motorr?der aus dem Datensatz:
    
# Auswahl aller Input-Variablen und Speicherung unter 'X'
X <- Daten[,c("name","year","km_driven","ex_showroom_price")]

# Berechnung der Prognosen
predict(model,X)

# Berechnung einer Prognose f?r einen neuen Datenpunkt
# Hinzuf?gen eines neuen Datenpunkts
x.neu <- data.frame("Honda CB Hornet 160R", 2018, 10000, 95000)
names(x.neu) <- names(X)
X <- rbind(X,x.neu)
      
      
# Berechnung der Prognosen
predict(model,X)
  
#############################
# Nun wiederholen wir die Berechnung nochmal, wobei wir den Datensatz
# vorher in Test- und Trainingsdaten aufteilen:
  
# Aufteilen der Daten:  
  # Anzahl der Daten  
   length(Daten[,1])     # 910 (70% zu 30% Aufteilung - 637 Datenpunkte als Trainingsdaten)
   Daten.train <- Daten[1:637,]
   Daten.test <- Daten[638:910,] 

# Definition der Tuning-Parameter 
cc <- seq(-5,10,1)         # f?r m?gliche Werte von "Cost" (Tuningparameter)
cg <- seq(-4,1,0.5)        # f?r m?gliche werte von "gamma" (Tuningparameter)

# Berechnung des Modells
tuning <- tune.svm(selling_price ~ name + year + km_driven + ex_showroom_price, data=Daten, scale = TRUE, type = "eps-regression", kernel = "radial", gamma = 10^cg, cost = 2^cc, epsilon = 0.1, tunecontrol = tune.control(sampling = "cross",cross=5))

print(tuning)

# Speichern des Modells mit den besten Parametern:
model <- tuning$best.model      # das Model mit optimalen Tuningparametern

# Berechnung der Prognoseergebnisse auf den Testdaten:
X.test <- Daten.test[,c("name", "year","km_driven","ex_showroom_price")] 
prognosen <- predict(model,X.test)


# Berechnung des mittleren Prognosefehlers (MAD)
y.test <- Daten.test[,"selling_price"]
    mean(abs((y.test-prognosen)/y.test))*100