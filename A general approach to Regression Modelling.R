#Read the file in to R and analyse the data for suitable operations to run soa s to make sene of the information. 
df_homes <- read.csv("C:\\Users\\ND.COM\\Downloads\\train.csv", header = TRUE)
View(df_homes)
library(mltools)
library(data.table)
library(olsrr)
library(GGally)
library(car)
#Check column names 
colnames(df_homes)
#Check the missing values in the dataset. 
is.na(df_homes)
#Replace all the missing values using the appropriate categories given in the text document. 
df_homes$Alley[is.na(df_homes$Alley)] <- "No Alley Way"
df_homes$MiscFeature[is.na(df_homes$MiscFeature)] <- "NONE"
df_homes$Fence[is.na(df_homes$Fence)] <- "No Fence"
df_homes$PoolQC[is.na(df_homes$PoolQC)] <- "No Pool"
df_homes$FireplaceQu[is.na(df_homes$FireplaceQu)] <- "No Fireplace"
df_homes$GarageType[is.na(df_homes$GarageType)] <- "No Garage Type"
df_homes$GarageFinish[is.na(df_homes$GarageFinish)] <- "No Garage Finish"
df_homes$GarageQual[is.na(df_homes$GarageQual)] <- "No Garage"
df_homes$GarageCond[is.na(df_homes$GarageCond)] <- "No Garage"
df_homes$BsmtQual[is.na(df_homes$BsmtQual)] <- "No Basement"
df_homes$BsmtCond[is.na(df_homes$BsmtCond)] <- "No Basement"
df_homes$BsmtExposure[is.na(df_homes$BsmtExposure)] <- "No Basement"
df_homes$BsmtFinType1[is.na(df_homes$BsmtFinType1)] <- "No Basement"
df_homes$BsmtFinType2[is.na(df_homes$BsmtFinType2)] <- "No Basement"
df_homes$LotFrontage[is.na(df_homes$LotFrontage)] <- 0


#Using the linear model we can prepare the equation
#Grouping the data then applying the model to individual groups. 
lm_homes_1 <- lm(SalePrice ~  MSZoning + LotFrontage, data = df_homes)
summary(lm_homes_1)
ols_plot_resid_fit(lm_homes_1)
ols_plot_resid_stud(lm_homes_1)
vif(lm_homes_1)

lm_home_2 <- lm(SalePrice ~  LotArea  + MasVnrArea , data = df_homes)
summary(lm_home_2)
ols_plot_resid_fit(lm_home_2)
ols_plot_resid_stud(lm_home_2)
vif(lm_home_2)


lm_home_3 <- lm(SalePrice ~  TotalBsmtSF +HalfBath, data = df_homes)
summary(lm_home_3)
ols_plot_resid_fit(lm_home_3)
ols_plot_resid_stud(lm_home_3)
vif(lm_home_3)


lm_home_4 <- lm(SalePrice ~ SaleCondition + LotShape, data = df_homes)   **
summary(lm_home_4)
ols_plot_resid_fit(lm_home_4)
ols_plot_resid_stud(lm_home_4)
vif(lm_home_4)

lm_home_5 <- lm(SalePrice ~ OverallQual + Neighborhood , data = df_homes)
summary(lm_home_5)
ols_plot_resid_fit(lm_home_5)
ols_plot_resid_stud(lm_home_5)
vif(lm_home_5)

lm_home_6 <- lm(SalePrice ~ ExterQual +KitchenQual + WoodDeckSF , data = df_homes)
summary(lm_home_6)
ols_plot_resid_fit(lm_home_6)
ols_plot_resid_stud(lm_home_6)
vif(lm_home_6)

lm_home_7 <- lm(SalePrice ~ YearBuilt+ YearRemodAdd, data = df_homes)
summary(lm_home_7)
ols_plot_resid_fit(lm_home_7)
ols_plot_resid_stud(lm_home_7)
vif(lm_home_7)

lm_home_8 <- lm(SalePrice ~  Foundation + GarageType+ FullBath+ HalfBath, data = df_homes)
summary(lm_home_8)
ols_plot_resid_fit(lm_home_8)
ols_plot_resid_stud(lm_home_8)
vif(lm_home_8)

lm_home_9 <- lm(SalePrice ~   BsmtFinType1+ BsmtFinSF1 +BsmtFinSF2+ BsmtUnfSF, data = df_homes)
summary(lm_home_9)
ols_plot_resid_fit(lm_home_9)
ols_plot_resid_stud(lm_home_9)
vif(lm_home_9)

lm_home_10 <- lm(SalePrice ~   GarageFinish+ GarageCars +GarageArea , data = df_homes)
summary(lm_home_10)
ols_plot_resid_fit(lm_home_10)
ols_plot_resid_stud(lm_home_10)
vif(lm_home_10)

lm_home_11 <- lm(SalePrice ~   OpenPorchSF+ ScreenPorch , data = df_homes)
summary(lm_home_11)
ols_plot_resid_fit(lm_home_11)
ols_plot_resid_stud(lm_home_11)
vif(lm_home_11)
##################################################################################################################################
lm_home_12 <- lm(SalePrice ~   LotArea  + MasVnrArea+ MSZoning + LotFrontage , data = df_homes)
summary(lm_home_12)
ols_plot_resid_fit(lm_home_12)
ols_plot_resid_stud(lm_home_12)
vif(lm_home_12)

lm_home_34 <- lm(SalePrice ~   TotalBsmtSF +HalfBath + LotShape  , data = df_homes)
summary(lm_home_34)
ols_plot_resid_fit(lm_home_34)
ols_plot_resid_stud(lm_home_34)
vif(lm_home_34)

lm_home_56 <- lm(SalePrice ~  OverallQual + Neighborhood +ExterQual +KitchenQual + WoodDeckSF , data = df_homes)
summary(lm_home_56)
ols_plot_resid_fit(lm_home_56)
ols_plot_resid_stud(lm_home_56)
vif(lm_home_56)

lm_home_79 <- lm(SalePrice ~  YearBuilt+ YearRemodAdd + BsmtFinType1+ BsmtFinSF1 +BsmtFinSF2+ BsmtUnfSF, data = df_homes)
summary(lm_home_79)
ols_plot_resid_fit(lm_home_79)
ols_plot_resid_stud(lm_home_79)
vif(lm_home_79)

lm_home_81011 <- lm(SalePrice ~  OpenPorchSF+ FullBath +GarageArea, data = df_homes)
summary(lm_home_81011)
ols_plot_resid_fit(lm_home_81011)
ols_plot_resid_stud(lm_home_81011)
vif(lm_home_81011)
##################################################################################################################################3

lm_home_final <- lm(SalePrice ~    LotArea+ MSZoning +TotalBsmtSF +HalfBath + LotShape, data = df_homes)
summary(lm_home_final)
ols_plot_resid_fit(lm_home_final)
ols_plot_resid_stud(lm_home_final)
vif(lm_home_final)

lm_home_f2 <- lm(SalePrice ~   Neighborhood +ExterQual +KitchenQual + WoodDeckSF+ YearRemodAdd, data = df_homes)
summary(lm_home_f2)
ols_plot_resid_fit(lm_home_f2)
ols_plot_resid_stud(lm_home_f2)
vif(lm_home_f2)
########################################################################################################################################3
lm_home_f3 <- lm(SalePrice ~ GarageCars +GarageArea+ OverallQual+ FullBath + Neighborhood +ExterQual +KitchenQual + YearRemodAdd+ LotArea+TotalBsmtSF , data = df_homes[-1299, -524,])
summary(lm_home_f3)
ols_plot_resid_fit(lm_home_f3)
#The irrigularity of the scatter plot indicates that we're on the right track. 
ols_plot_resid_stud(lm_home_f3)
ols_plot_cooksd_chart(lm_home_f3)
ols_plot_hadi(lm_home_f3)
ols_plot_resid_lev(lm_home_f3)
#Removing outliers
#and re running the equaiton. 
lm_home_f3 <- lm(SalePrice ~ GarageCars +GarageArea+ OverallQual+ FullBath + Neighborhood +ExterQual +KitchenQual + YearRemodAdd+ LotArea+TotalBsmtSF , data = df_homes[-1299, -524,])
# measures of influence
vif(lm_home_f3)
#The summary indicates that: 
# R-suared value of 82.42% is obtained through this model.



