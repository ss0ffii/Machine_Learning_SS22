##########################
#  Deskriptive Analyse   #
##########################


#################################################################
# Pakete
# ...


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
# Mittelwerte
mean(bikes[,"selling_price"])
mean(bikes[,"year"])
mean(bikes[,"km_driven"])
mean(bikes[,"ex_showroom_price"])

# Mediane
median(bikes[,"selling_price"])
median(bikes[,"year"])
median(bikes[,"km_driven"])
median(bikes[,"ex_showroom_price"])

#Standardabweichungen
sd(bikes[,"selling_price"])
sd(bikes[,"year"])
sd(bikes[,"km_driven"])
sd(bikes[,"ex_showroom_price"])


#################################################################
# Boxplots fuer die metrischen Variablen
par(mfrow = c(2,2))
boxplot(bikes[,"selling_price"], main="Verkaufspreis")
boxplot(bikes[,"year"], main="Jahr")
boxplot(bikes[,"km_driven"], main="Kilometer")
boxplot(bikes[,"ex_showroom_price"], main="Ausstellungspreis")


# Boxplots ohne Ausreisser 
par(mfrow = c(2,2))
boxplot(bikes[,"selling_price"], main="Verkaufspreis", outline=FALSE)
boxplot(bikes[,"year"], main="Jahr", outline=FALSE)
boxplot(bikes[,"km_driven"], main="Kilometer", outline=FALSE)
boxplot(bikes[,"ex_showroom_price"], main="Ausstellungspreis", outline=FALSE)


# Vergleichende Boxplots fuer die metrischen Variablen, das sind die Variablen 3 bis 6
par(mfrow=c(2,2))
for(j in 3:6){
  boxplot(bikes[,j] ~ bikes[,"selling_price"],main=names(bikes)[j])
}

# Wegen den Ausreissern sieht man nicht viel, deswegen nochmal ohne Ausreisser
par(mfrow=c(2,2))
for(j in 3:6){
  boxplot(bikes[,j] ~ bikes[,"selling_price"],main=names(bikes)[j],outline=FALSE)
}


#################################################################
# Histogramme
par(mfrow = c(2,2))
hist(bikes[,"selling_price"], main="selling_price", xlab = "Preis", ylab = "Anzahl", breaks=50)
hist(bikes[,"year"], main="year", xlab = "Jahr", ylab = "Anzahl", breaks=20)
hist(bikes[,"km_driven"], main="km_driven", xlab = "Kilometer", ylab = "Anzahl", breaks=50)
hist(bikes[,"ex_showroom_price"], main="ex_showroom_price", xlab = "Preis", ylab = "Anzahl", breaks=50)


# Histogramme ohne Ausreisser 

# Löscht die Ausreisser in "selling_price"
sp_no <- bikes[,"selling_price"]
quartiles <- quantile(sp_no, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(sp_no)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
sp_no_outlier <- subset(sp_no, sp_no > Lower & sp_no < Upper)

# Löscht die Ausreisser in "year"
y_no <- bikes[,"year"]
quartiles <- quantile(y_no, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(y_no)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
y_no_outlier <- subset(y_no, y_no > Lower & y_no < Upper)

# Löscht die Ausreisser in "km_driven"
km_no <- bikes[,"km_driven"]
quartiles <- quantile(km_no, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(km_no)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
km_no_outlier <- subset(km_no, km_no > Lower & km_no < Upper)

# Löscht die Ausreisser in "ex_showroom_price"
shP_no <- bikes[,"ex_showroom_price"]
quartiles <- quantile(shP_no, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(shP_no)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
shP_no_outlier <- subset(shP_no, shP_no > Lower & shP_no < Upper)

par(mfrow = c(2,2))
hist(sp_no_outlier, main="Verkaufspreis", xlab = "Verkaufspreis", ylab = "Anzahl", breaks=30)
hist(y_no_outlier, main="Jahr", xlab = "Jahr", ylab = "Anzahl", breaks=20)
hist(km_no_outlier, main="Kilometer", xlab = "Kilometer", ylab = "Anzahl", breaks=30)
hist(shP_no_outlier, main="Ausstellungspreis", xlab = "Ausstellungspreis", ylab = "Anzahl", breaks=30)


#####
# Balkendiagramm für seller_type und owner
barplot(height=c(6,1055),names.arg=c("Dealer","Individual"),main="seller_type", xlab = "Verkaufstyp", ylab = "Anzahl")
barplot(height=c(924,137),names.arg=c("1st owner","2nd owner"),main="owner", xlab = "Besitzer", ylab = "Anzahl")


#################################################################
# Fuer einfache Zusammenhaenge betrachten wir als Output Variable nun nur "selling_price"
# Streudiagramme zwischen metrische Input Variablen und Output Variable
par(mfrow = c(1,3))
plot(bikes[,"ex_showroom_price"], bikes[,"selling_price"], pch=19, xlab = "Ausstellungspreis", ylab = "Verkaufspreis")
plot(bikes[,"year"],bikes[,"selling_price"],pch=19, xlab = "Jahr", ylab = "Verkaufspreis")
plot(bikes[,"km_driven"],bikes[,"selling_price"],pch=19, xlab = "Kilometers", ylab = "Verkaufspreis")

# Alle Streudiagramme
plot(bikes)


#################################################################
# Boxplots fuer die Variablen mit Typ factor
# Boxplots fuer "selling_price" in Abhaengigkeit von "owner"
owner.1 <- subset(bikes, owner == 1)
owner.2 <- subset(bikes, owner == 2)

boxplot(owner.1[,"selling_price"],owner.2[,"selling_price"], outline=FALSE)
boxplot(selling_price ~ owner, data=bikes, outline=FALSE)
      
# Nun alle Boxplots mit der "praktischen Alternative"
par(mfrow = c(1,3))
boxplot(selling_price ~ ex_showroom_price, data=bikes)
boxplot(selling_price ~ owner, data=bikes)
boxplot(selling_price ~ km_driven, data=bikes)

# Wegen den Ausreissern sieht man nicht viel, deswegen nochmal ohne Ausreisser 
par(mfrow = c(1,3))
boxplot(selling_price ~ ex_showroom_price, data=bikes, xlab = "Ausstellungspreis", ylab = "Verkaufspreis", outline=FALSE)
boxplot(selling_price ~ year, data=bikes, xlab = "Jahr", ylab = "Verkaufspreis", outline=FALSE)
boxplot(selling_price ~ km_driven, data=bikes, xlab = "Kilometers", ylab = "Verkaufspreis", outline=FALSE)


#################################################################
s_p <- bikes[,"selling_price"]
y <- bikes[,"year"]
s_t <- bikes[,"seller_type"]
o <- bikes[,"owner"]
k_d <- bikes[,"km_driven"]
e_s_p <- bikes[,"ex_showroom_price"]

# Einfache Zusammenhange: metrisch-metrisch 
plot(s_p,e_s_p,main="ex_showroom_price in Abhaengigkeit von selling_price",xlab="selling_price",ylab="ex_showroom_price",pch=19)

plot(s_p,k_d,main="km_driven in Abhaengigkeit von selling_price",xlab="selling_price",ylab="km_driven",pch=19)

plot(s_p,y,main="year in Abhaengigkeit von selling_price",xlab="selling_price",ylab="year",pch=19)
 

################################################################# 
# Einfache Zusammenhaenge: metrisch-nominal
# Trennung der Daten nach Owner 
owner1 <- subset(bikes,owner==1)
owner2 <- subset(bikes,owner==2)
# Berechnung der Mittelwerte
mean(owner1[,"km_driven"])
mean(owner2[,"km_driven"])


#################################################################
# Zusammenhaenge: metrisch-metrisch-nominal
# Trennung der Daten nach Owner 
owner1 <- subset(bikes,owner==1)
owner2 <- subset(bikes,owner==2)
# Berechnung von max und min bei selling_price und ex_showroom_price  
# selling_price in Abhaengigkeit von ex_showroom_price:
# owner 1 (in blau)
x <- owner1[,"ex_showroom_price"]
y <- owner1[,"selling_price"]
plot(x,y,main="selling_price in Abhaengigkeit von ex_showroom_price",xlim=c(30490,1278000),ylim=c(5000,760000),xlab="ex_showroom_price",ylab="selling_price",pch=19,col="blue")
# owner 2 (in red)
x <- owner2[,"ex_showroom_price"]
y <- owner2[,"selling_price"]
points(x,y,pch=19,col="red")


#################################################################
# Zusammenhang: metrisch-nominal
# Trennung der Daten nach Owner 
owner1 <- subset(bikes,owner==1)
owner2 <- subset(bikes,owner==2)
# Herausgreifen des jeweiligen Verbrauchs
x <- owner1[,"year"]  
y <- owner2[,"year"]  
# Malen der Boxplots
boxplot(x,y,main="year",names=c("owner1","owner2"))


#################################################################
# Korrelationen
# Herausgreifen von selling_price, year und ex_showroom_price
x <- bikes[,"selling_price"]
y <- bikes[,"year"]
z <- bikes[,"ex_showroom_price"]
# Berechnung der Korrelationen
cor(x,y)
cor(x,z)
