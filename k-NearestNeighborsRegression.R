#####################################
#  k-Nearest Neighbors: Regression  #
#####################################


#################################################################
# Pakete
# Install die FNN Package für k-Nearest Neighbors -> install.packages("FNN")
library(FNN)


#################################################################
# Pfad setzen
setwd("D:/OneDrive - Technische Hochschule Deggendorf/Materialen - Uni/4. Semester/Maschinelles Lernen/Prüfungsstudienarbeit")

bikes <- read.csv(
  "bikes_imputed.csv",
  header=TRUE,
  sep=",",
  fill=TRUE,
  stringsAsFactors=TRUE)

bikes[1:10,]
summary(bikes)

# Reduzierung der Kategorien / Zusammenfassen von Kategorien 
bikes[,"owner"] <- ifelse(bikes[,"owner"] == "1st owner" | bikes[,"owner"] == "2nd owner", bikes[,"owner"], 2)
bikes[,"owner"] <- as.factor(bikes[,"owner"])

summary(bikes)


#################################################################
# Wir betrachten zunaechst nur den Zusammenhang zwischen selling_price und year
x <- bikes[,"year"]
y <- bikes[,"selling_price"]

plot(x,y,pch=19, xlab = "Jahr", ylab = "Preis")


#################################################################
# Aufteilung des Datensatzes in Trainings- und Validierungsdatensatz bzw. Testdatensatz

# Vertauschung der Reihenfolge der Daten
n <- length(bikes[,1])
n
index <- sample(1:n,n,replace=FALSE)
bikes <- bikes[index,]
# Aufteilung in Training/Validierung und Test
bikes.train.val <- bikes[1:743,]
bikes.test <- bikes[744:1061,]

# Anlage von Vektoren fuer die Guete
guete <- 0
guete.temp <- 0


# Start der for-Schleife fuer verschiedene Werte von k

for(k in 1:20){
  
  # Start der for-Schleife fuer Leave-One-Out
  
  m <- length(bikes.train.val[,1])
  for(i in 1:m){
    
    # Aufteilen in Trainingsdaten und Validierungsdatenpunkt
    bikes.train <- bikes.train.val[-i,]
    bikes.val <- bikes.train.val[i,]
    
    # Berechnung von k-Nearest-Neighbors und der Prognoseguete
    x.train <- bikes.train[,"year"]
    y.train <- bikes.train[,"selling_price"]
    x.val <- bikes.val[,"year"]
    y.val <- bikes.val[,"selling_price"]
    
    x.train <- matrix(x.train)
    x.val <- matrix(x.val)
    
    model <- knn.reg(train=x.train, test = x.val, y=y.train, k = k)
    prognosen <- model$pred
    
    guete.temp[i] <- abs(prognosen-y.val)        
    
  }
  
  # Berechnung der durchschnittlichen Prognoseguete fuer k
  guete[k] <- mean(guete.temp)
}

# Ausgabe der Gueten fuer die verschiedenen Werte von k
guete
plot(1:20,guete,pch=19, xlab = "Jahr", ylab = "Preis")

# Ausgabe des optimalen k mit minimalen Prognosefehler  
which.min(guete)


# Berechnung des Modells auf dem Trainings- und Validierungsdatensatz fuer das optimale k 

x.train <- bikes.train.val[,"year"]
y.train <- bikes.train.val[,"selling_price"]

# Testdaten
x.test <- bikes.test[,"year"]
y.test <- bikes.test[,"selling_price"]

# Weil x.train und x.test im Moment Vectoren sind, muessen diese zunaechst in
# Matrizen umgewandelt werden (das ist hier noetig, weil wir nur 1 Einflussvariable haben) 
x.train <- matrix(x.train)
x.test <- matrix(x.test)

# Berechnung der Prognosen mit k-Nearest Neighbors fuer k=11 und des Prognosefehlers 

model <- knn.reg(train=x.train, test = x.test, y=y.train, k = 1)
prognosen <- model$pred

mean(abs(prognosen-y.test))
