rm(list=ls(all=T))

getwd()

setwd("C://Users//Sheik Basha//Desktop//test//Housing")

housing_train <-  read.csv("train.csv", header = T, sep = ",")

str(housing_train)

basicLinearRegr <- lm(formula =  SalePrice ~ . , data = housing_train)

#lapply(housing_train, unique)

str(housing_train)
#housing_train[is.na(housing_train),]
#testNA <- na.omit(housing_train)
library(DMwR)

manyNAs(housing_train, nORp = 0.1)

sum(is.na(housing_train$Alley))
#Alley column has 1369 records with NA out of total 1460
#but as per description NA does not mean its a missing value,it means no alley access
#PoolQC:  NA means no pool, its not missing value
#Fence : NA means NoFence, its not missing value
#MiscFeature: NA means none, its not missing value


na_count <- sapply(housing_train, function(x){
    sum(length(which(is.na(x))))
})

na_count <- data.frame(na_count)

#housing_train_filteredRemovedNABlindly <- subset(housing_train, select=-c(Alley,PoolQC,Fence,MiscFeature))

# we should convert NA to factor to above variables Alley,PoolQC,Fence,MiscFeature

#housing_train$Alley[is.na(housing_train$Alley)] <- as.factor("None")
levels(housing_train$Alley)
levels(housing_train$Alley) = c("Grvl","Pave","NAA")
housing_train$Alley[is.na(housing_train$Alley)] <- as.factor("NAA")

levels(housing_train$PoolQC)
levels(housing_train$PoolQC) <- c("Ex","Fa","Gd","NP")
housing_train$PoolQC[is.na(housing_train$PoolQC)] <- as.factor("NP")

levels(housing_train$Fence)
levels(housing_train$Fence) <- c("GdPrv","GdWo","MnPrv","MnWw","NF")
housing_train$Fence[is.na(housing_train$Fence)] <- as.factor("NF")

levels(housing_train$MiscFeature)
levels(housing_train$MiscFeature) <- c("Gar2","Othr","Shed","TenC","None")
housing_train$MiscFeature[is.na(housing_train$MiscFeature)] <- as.factor("None")

levels(housing_train$FireplaceQu)
levels(housing_train$FireplaceQu) <- c("Ex","Fa","Gd","Po","TA","NFP")
housing_train$FireplaceQu[is.na(housing_train$FireplaceQu)] <- as.factor("NFP")

levels(housing_train$GarageCond)
levels(housing_train$GarageCond) <- c("Ex","Fa","Gd","Po","TA","NG")
housing_train$GarageCond[is.na(housing_train$GarageCond)] <- as.factor("NG")

levels(housing_train$GarageQual)
levels(housing_train$GarageQual) <- c("Ex","Fa","Gd","Po","TA","NG")
housing_train$GarageQual[is.na(housing_train$GarageQual)] <- as.factor("NG")

str(housing_train)

levels(housing_train$GarageFinish)
levels(housing_train$GarageFinish) <- c("Fin","RFn","Unf","NG")
housing_train$GarageFinish[is.na(housing_train$GarageFinish)] <- as.factor("NG")

levels(housing_train$GarageType)
levels(housing_train$GarageType) <- c("2Types","Attchd","Basment","BuiltIn","CarPort","Detchd","NG")
housing_train$GarageType[is.na(housing_train$GarageType)] <- as.factor("NG")

levels(housing_train$BsmtQual)
levels(housing_train$BsmtQual) <- c("Ex","Fa","Gd","TA","NB")
housing_train$BsmtQual[is.na(housing_train$BsmtQual)] <- as.factor("NB")

levels(housing_train$BsmtCond)
levels(housing_train$BsmtCond) <- c("Fa","Gd","Po","TA","NB")
housing_train$BsmtCond[is.na(housing_train$BsmtCond)] <- as.factor("NB")

levels(housing_train$BsmtExposure)
levels(housing_train$BsmtExposure) <- c( "Av","Gd","Mn","No","NB")
housing_train$BsmtExposure[is.na(housing_train$BsmtExposure)] <- as.factor("NB")

levels(housing_train$BsmtFinType1)
levels(housing_train$BsmtFinType1) <- c( "ALQ","BLQ","GLQ","LwQ","Rec","Unf","NB")
housing_train$BsmtFinType1[is.na(housing_train$BsmtFinType1)] <- as.factor("NB")

levels(housing_train$BsmtFinType2)
levels(housing_train$BsmtFinType2) <- c( "ALQ","BLQ","GLQ","LwQ","Rec","Unf","NB")
housing_train$BsmtFinType2[is.na(housing_train$BsmtFinType2)] <- as.factor("NB")

# variables  MasVnrType and MasVnrArea have NA's but just 8 in count. doing imputation to replace
str(housing_train)

table(which.max(housing_train$MasVnrType))
unique(housing_train$MasVnrType)

rm(Mode)

names(which.max(table(housing_train$MasVnrType)))
housing_train$MasVnrType[is.na(housing_train$MasVnrType)] <- names(which.max(table(housing_train$MasVnrType)))

housing_train$MasVnrArea[is.na(housing_train$MasVnrArea)] <- median(housing_train$MasVnrArea, na.rm = T)

housing_train$Electrical[is.na(housing_train$Electrical)] <- names(which.max(table(housing_train$Electrical)))

housing_train$LotFrontage[is.na(housing_train$LotFrontage)] <- median(housing_train$LotFrontage, na.rm = T)
housing_train$GarageYrBlt[is.na(housing_train$GarageYrBlt)] <- median(housing_train$GarageYrBlt, na.rm = T)


basicHousingLR <- lm(SalePrice ~ ., data=housing_train)
plot(basicHousingLR)

library(MASS)

Step1 <- stepAIC(basicHousingLR, direction = "both")
summary(Step1)


