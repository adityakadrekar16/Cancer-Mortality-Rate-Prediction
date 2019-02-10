# Created on Sat Nov 17 20:26:44 2018
# @author: Aditya Kadrekar
# To predict cancer mortality rates by United States counties using Multiple Linear Regression
# Source: https://data.world/exercises/linear-regression-exercise-1

df1 = read.csv('avg-household-size.csv')
df2 = read.csv('cancer_reg.csv')

# joining both the datasets by "geography"
merge_df = merge(df1, df2, by ="geography", all.df1 = TRUE)
# dropping binnedinc as the median income contains the value within this range
merge_df <- subset(merge_df, select = -c(binnedinc)) 
attach(merge_df)

# to find missing values in the dataset
colSums(is.na(merge_df))

# replacing missing values with average
merge_df$pctsomecol18_24 = ifelse(is.na(merge_df$pctsomecol18_24),
                                  ave(merge_df$pctsomecol18_24, FUN = function(x) median(x, na.rm = TRUE)),
                                  merge_df$pctsomecol18_24)

merge_df$pctemployed16_over = ifelse(is.na(merge_df$pctemployed16_over),
                                     ave(merge_df$pctemployed16_over, FUN = function(x) mean(x, na.rm = TRUE)),
                                     merge_df$pctemployed16_over)

merge_df$pctprivatecoveragealone = ifelse(is.na(merge_df$pctprivatecoveragealone),
                                          ave(merge_df$pctprivatecoveragealone, FUN = function(x) mean(x, na.rm = TRUE)),
                                          merge_df$pctprivatecoveragealone)

# To create a PDF file
pdf("test.pdf")
# To create a JPEF file
jpeg("test.jpeg")

# histogram plots of all indepdendent variables to check skewness
# no need to prove assumptions as it's already given in the question
plot(medincome)
hist(medincome)
# taking log because it is right skewed
hist(log(medincome))

hist(avghouseholdsize)
hist(log(avghouseholdsize))

hist(avganncount)
hist(log(avganncount))

hist(avgdeathsperyear)
hist(log(avgdeathsperyear))

hist(popest2015)
hist(log(popest2015))

hist(povertypercent)
hist(log(povertypercent))

hist(incidencerate) # slightly normal
hist(percentmarried) # left skewed
hist(pctmarriedhouseholds) # slightly normal
hist(pctotherrace) # right skewed
hist(pctemployed16_over) #left skewed or slightly normal
hist(medianagefemale) # normal
hist(pcths25_over) # left skewed
hist(pctbachdeg25_over) # right skewed

# Target variable
hist(target_deathrate)

# indicates to R that we are done creating the plot
# execute this function once you are done creating all the plots you need
dev.off() 

# to check the significance of binnedinc just to check the linearity
test = lm(target_deathrate~binnedinc, data=merge_df)
summary(test)

# splitting the dataset into training and test set
split = sample.split(merge_df$target_deathrate, SplitRatio = 0.8)
training_set = subset(merge_df, split == TRUE)
test_set = subset(merge_df, split == FALSE)

# checking all the variables
lm.fit = lm(target_deathrate~.-geography, data =merge_df)
fit = lm(target_deathrate~ incidencerate, data =merge_df)
mlr_gvlma = gvlma(fit)
summary(mlr_gvlma)
summary(lm.fit)
plot(lm.fit)
lm.fit = lm(target_deathrate~incidencerate)
plot(lm.fit)
abline(target_deathrate,incidencerate, col="red")

# only adding variables with high significance (this is wrong as these variables were taken before replacing missing data)
lm.fit = lm(target_deathrate~incidencerate + percentmarried + pctmarriedhouseholds + pctotherrace
           + pctemployed16_over + medianagemale + pcths25_over + pctbachdeg25_over, data = training_set)
summary(lm.fit)
summary(lm.fit)$r.squared
summary(lm.fit)$adj.r.squared

library(wle) # for mallow cp
mle.cp(lm.fit)

# Predicting the Test set results
y_pred = predict(lm.fit, newdata = test_set)

#plot(pctmarriedhouseholds,target_deathrate,pch=20, col="red")
par(mfrow=c(1,1))

#ols_step_forward_p(lm.fit)
#ols_regress(lm.fit)
#ols_step_both_p(lm.fit)

# another method to find significant dependent variables
install.packages("caret") # run install only if you've never installed it before
library(caret) 
fit<-lm(target_deathrate~.-geography, data=merge_df)
varImp(fit, scale = FALSE)

