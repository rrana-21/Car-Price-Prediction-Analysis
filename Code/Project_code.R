#libraries

library(GGally)
library(MASS);
library(dplyr);
library(lars);
library(leaps);
library(corrplot);
library(pls);
library(RColorBrewer);
library(tidyverse);
library(visdat);
library(ggplot2);
#read in file 
options(scipen=999)
df = read.csv("Car details v3.csv", header = T)
head(df)
#Explanatory Data Analysis (EDA)
#Check for NA's
colSums(is.na(df) | df == "N/A")
vis_miss(df)
df <- df[!is.na(df$seats) & df$seats != "N/A", ]
#Create dummy variables
#Default is CNG
# Calculate the percent of rows with 'selling_price' over 1M
price_condition <- df$selling_price > 1000000
price_over_million_percent <- sum(price_condition) / nrow(df) * 100

# Calculate the percent of rows with 'km_driven' over 200K
km_condition <- df$km_driven > 200000
km_over_200K_percent <- sum(km_condition) / nrow(df) * 100

# Print out the percentages
cat("Percentage of rows with 'selling_price' over 1,000,000:", price_over_million_percent, "%\n")
cat("Percentage of rows with 'km_driven' over 200000:", km_over_200K_percent, "%\n")

# Create new df by removing rows that satisfy either condition
df <- df[!(price_condition | km_condition), ]

# Print out the number of rows in updated df & that we removed
cat("Number of rows in 'cleaned df':", nrow(df), "\n")
cat("Number of rows removed:", nrow(df[(price_condition | km_condition), ]), "\n")

fuel_dummies <- model.matrix(~fuel, data=df)[,-1]
#default is Dealer
seller_type_dummies <- model.matrix(~seller_type, data=df)[,-1]
#transmissionManual
transmission_manual <- model.matrix(~transmission, data=df)[,-1]
#default is First Owner
owner_dummies <- model.matrix(~owner, data=df)[,-1]
#Bind to DF 
df <- cbind(df, fuel_dummies, seller_type_dummies, transmission_manual, owner_dummies)
head(df)
# Create interaction terms
#Have this here but Commenting out as this may be more transparent to compute within the regression model 
#Interaction between year and fuel dummies:
#interaction_fuel <- df$year * fuel_dummies
#Interaction between year and seller_type dummies:
#interaction_seller_type <- df$year * seller_type_dummies
#Interaction between year and transmission dummies:
#interaction_transmission <- df$year * transmission_manual
#Bind to DF 
#df <- cbind(interaction_fuel, interaction_seller_type, interaction_transmission)
#Scatter & box plots
columns <- c("name", "year", "selling_price", "km_driven", "fuel", "seller_type", "transmission", "owner")
for (col_name in columns) 
  
  # For numeric variables - scatter
  if (is.numeric(df[[col_name]])) {
    plot(df[[col_name]], df$selling_price, main=paste("Selling Price vs", col_name), 
         xlab=col_name, ylab="Selling Price", col="blue", pch=20)
  } else {
    # For categorical variables- boxplot 
    boxplot(selling_price ~ df[[col_name]], data=df, main=paste("Selling Price vs", col_name), 
            xlab=col_name, ylab="Selling Price", col=rainbow(length(unique(df[[col_name]]))))
  }
#Heat maps
p <- ggplot(df, aes(x = selling_price, y = km_driven)) + 
  geom_bin2d(bins=30) + 
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Selling Price vs KM Driven", 
       x = "Selling Price", 
       y = "KM Driven", 
       fill = "Count") 
y <- ggplot(df, aes(x = selling_price, y = km_driven)) + 
  geom_bin2d(bins=30) + 
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Selling Price vs Year", 
       x = "Selling Price", 
       y = "year", 
       fill = "Count") 
# Display the plots
print(p)
print(y)  
str(df)
#fill up N/A values
df$seats[is.na(df$seats)]<-mean(df$seats,na.rm=TRUE)
#Create log variables for dependent and indep variables 
head(df)
df <- df %>% mutate(selling_price_ln = log(selling_price+1)) %>% mutate(km_driven_ln = log(km_driven+1))  %>% mutate(year_ln = log(year))
head(df)
str(df)
summary(df)
# correlation matrix
h1 <- data.frame(df$selling_price, df$year, df$km_driven, df$selling_price_ln, df$year_ln, df$km_driven_ln)
head(h1)
ggpairs(h1, 
        upper = list(continuous = wrap("cor", size = 8))) 
## plots between dependent and key independent variables
#plot b/w selling price and km_driven
plot(df$km_driven, df$selling_price, main=paste("Selling Price vs km_driven"), 
     xlab="km_driven", ylab="Selling Price", 
     #xlim=c(0,300000), 
     col="blue", pch=20)
#plot b/w selling price and year
plot(df$year, df$selling_price, main=paste("Selling Price vs year"), 
     xlab="year", ylab="Selling Price", col="blue", pch=20)
#Insights
## 1 high correlation bw selling_price_ln and year
## 2 selling_price_ln and year_ln seem to have a linear relationship as seen by scatterplots
## 3 slight linear relationship observed bw selling_price_ln and km_driven_ln
## no other non-linear relationships observed bw dependent variable and the key independent variables
###For use to remove outliers in selling price
str(df)
quartiles <- quantile(df$selling_price, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(df$selling_price)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
df_no_outlier <- subset(df, df$selling_price > Lower & df$selling_price < Upper)
summary(df_no_outlier)

test = df_no_outlier %>% separate(mileage, into=c("mileage_num", "trash"), sep=" ", convert=TRUE)
test = test %>% separate(engine, into=c("engine_num", "trash"), sep=" ", convert=TRUE)
test = test %>% separate(max_power, into=c("maxpower_num", "trash"), sep=" ", convert=TRUE)

test = test %>% mutate(mileage_num = ifelse(is.na(mileage_num ), 0, mileage_num ))
test = test %>% mutate(engine_num = ifelse(is.na(engine_num ), 0, engine_num ))
test = test %>% mutate(maxpower_num = ifelse(is.na(maxpower_num ), 0, maxpower_num ))
test$OwnerFourthAndAbove = test[["ownerFourth & Above Owner"]]
test=test[,-21]

finaldf = test %>% select("selling_price","selling_price_ln", "km_driven_ln", "year_ln",
                          "mileage_num", "engine_num", "maxpower_num", "seats", "fuelDiesel", "fuelLPG",
                   "fuelPetrol", "seller_typeIndividual", "seller_typeTrustmark Dealer",
                   "transmission_manual", "OwnerFourthAndAbove", "ownerSecond Owner",
                   "ownerThird Owner","year","km_driven");
colnames(finaldf) <- sub(" ", "_", colnames(finaldf))

split=20;

n=dim(finaldf)[1];
n1=round((split/100)*n)

#set seed for randomization
set.seed(6302);

#number of MCCV loops
k=100;

#final matricies for MSE and adjusted rsquared
final.MSE=NULL;
final.rsq=NULL;

for (i in 1:k) {
  
  error = NULL;
  rsq = NULL;
  #randomly split full table into testing and train split
  flags <- sort(sample(1:n, n1));
  train <- finaldf[-flags,];
  test <- finaldf[flags,];
  
  
  #fit the models, calcualte the prediction error and extract the rsqured
  
  #linear
  predictors = colnames(train)[5:length(colnames(train))]
  linear.formula= as.formula(paste("selling_price ~", (paste(predictors, collapse="+"))))
  linear.model = lm(linear.formula, data=train)

  
  pred.linear = predict(linear.model, newdata=test[,c(5:length(colnames(finaldf)))])
  linear.mse = sqrt(mean((pred.linear - test$selling_price)**2))

  error=c(error, linear.mse)
  rsq = c(rsq, summary(linear.model)$r.squared)
  
  
  #stepwise regression
  stepwise = stepAIC(linear.model, direction="both")
  step_pred = predict(stepwise, newdata=test[,c(5:length(colnames(finaldf)))])
  step.mse = sqrt(mean(((step_pred-test$selling_price)**2)))
  
  error=c(error, step.mse)
  rsq = c(rsq, summary(stepwise)$r.squared)
  
  #log.log
  new.predictors = colnames(train)[5:length(colnames(train))-2]
  log.predictors = as.formula(paste("selling_price_ln ~", (paste(new.predictors, collapse="+"))))
  log.log = lm(log.predictors,data=train)
  summary(log.log)
  log.pred = predict(log.log, newdata=test[,c(5:length(colnames(train))-2)])
  log.mse = sqrt(mean((log.pred-test$selling_price_ln )**2))
  
  error=c(error, log.mse)
  rsq = c(rsq, summary(log.log)$r.squared)
  
  final.MSE= rbind(final.MSE, error)
  final.rsq=rbind(final.rsq, rsq)
}

#should have K rows, one for each loop
dim(final.MSE)
dim(final.rsq)
#average the rsquared and MSE for the K runs
colnames(final.MSE) = c("Linear", "StepWise", "Log.Log")
colnames(final.rsq) = c("Linear", "StepWise", "Log.Log")

round(apply(final.MSE, 2, mean), digit=2);
round(apply(final.rsq, 2, mean), digit=5);

#since the relationship between selling price and km driven and year is not linear, the log transformations of 
#selling price, km driven and year has a better rsquared on average and a lower Mean Absolute Deviations. 
