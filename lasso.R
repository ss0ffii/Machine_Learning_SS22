#######################################################################
################################ LASSO ################################
#######################################################################

setwd("D:/OneDrive - Technische Hochschule Deggendorf/Materialen - Uni/4. Semester/Maschinelles Lernen/motorbikes_Gutoranska_Likhacheva_Solodovnyk")
Data <- read.csv("bikes_imputed.csv",header=TRUE,sep=",",fill=TRUE,stringsAsFactors=TRUE)

# Explore and prepare dataset
Data[1:10,]

Data[,"owner"] <- as.factor(Data[,"owner"])
Data[,"seller_type"] <- as.factor(Data[,"seller_type"])
Data[,"selling_price"] <- as.numeric(Data[,"selling_price"])
Data[,"year"] <- as.numeric(Data[,"year"])
Data[,"km_driven"] <- as.numeric(Data[,"km_driven"])
Data[,"ex_showroom_price"] <- as.numeric(Data[,"ex_showroom_price"])

bikes_with_outliers = Data[,c("owner", "seller_type", "year", "km_driven", "ex_showroom_price", "selling_price")]
summary(bikes_with_outliers)

# Remove outliers from dataset 
quartiles <- quantile(bikes_with_outliers$year, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(bikes_with_outliers$year)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
year_outliers <- subset(bikes_with_outliers, bikes_with_outliers$year > Lower & bikes_with_outliers$year < Upper)

quartiles <- quantile(year_outliers$km_driven, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(year_outliers$km_driven)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
km_outliers <- subset(year_outliers, year_outliers$km_driven > Lower & year_outliers$km_driven < Upper)

quartiles <- quantile(km_outliers$ex_showroom_price, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(km_outliers$ex_showroom_price)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
bikes <- subset(km_outliers, km_outliers$ex_showroom_price > Lower & km_outliers$ex_showroom_price < Upper)

summary(bikes)

# Randomize the dataset and divide into training and test data
n <- length(bikes[,1])
Index <- sample(seq(1,n,1), replace=FALSE)
bikes <- bikes[Index,]
rownames(bikes) <- 1:n

nrow(bikes)
bikes.train <- bikes[1:710,]
bikes.test <- bikes[711:887,]

# Prepare package for LASSO
install.packages("glmnet")
library(glmnet)

#LASSO with automatic cross-validation
X <- model.matrix(selling_price ~. , bikes.train)
X <- X[,-1]  
y <- bikes.train[,"selling_price"]

model.lasso <- cv.glmnet(X,y)

model.lasso <- cv.glmnet(X,y)
coef(model.lasso,s="lambda.1se")

coef(model.lasso,s="lambda.min")
# LASSO with 100 iterations
X <- model.matrix(selling_price ~. , bikes.train)
X <- X[,-1]
y <- bikes.train[,"selling_price"]
m <- length(X[1,])
total.numbers <- rep(0,m)

RUNS <- 100
for( run in 1:RUNS ){
  model.lasso <- cv.glmnet(X,y)
  beta <- coef(model.lasso,s="lambda.1se")[-1,1]  
  total.numbers <- total.numbers + ifelse( beta != 0, 1, 0)  
}

total.numbers <- as.matrix(total.numbers)
rownames(total.numbers) <- names(beta)
total.numbers
