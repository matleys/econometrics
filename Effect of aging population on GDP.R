rm(list = ls())

if(!require(pastecs)){install.packages("pastecs")}
if(!require(psych)){install.packages("psych")}
if(!require(moments)){install.packages("moments")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(sandwich)){install.packages("sandwich")}
if(!require(AER)){install.packages("AER")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(nlme)){install.packages("nlme")}
if(!require(orcutt)){install.packages("orcutt")}

library(pastecs)  ## Descriptive Statistics
library(psych)    ## Correlation plot
library(moments)  ## Testing for Normality check up
library(lmtest)   ## Tests for heteroscedasticity
library(sandwich) ## Newey-West HAC estimators
library(AER)      ## 2SLS
library(stargazer)## Stargazer
library(orcutt)   ## Cochrane-Orcutt EGLS
library(nlme)     ## Linear models with autocorrelated error terms
library(EconometricsUGent)  ## Additional functions

data = read.table("Aging.csv",header = TRUE, sep = ";")
dim(data)             ## Number of variables and sample size
n = length(data[,1])  ## Sample size

pop1990 = data$Pop1990        # ln(Pop)
gdp1990 = data$GDP1990        # ln(GDP)
change_gdp = data$change_gdp  # Change in ln(GDP)
change_age = data$change_age  # Change in the old age dependency ratio
age1990 = data$Age1990        # Initial old age dependency ratio 
birthrate1960_1965 = data$birthrate1960_1965
birthrate1965_1970 = data$birthrate1965_1970
birthrate1970_1975 = data$birthrate1970_1975
birthrate1975_1980 = data$birthrate1975_1980
birthrate1980_1985 = data$birthrate1980_1985
birthrate1985_1990 = data$birthrate1985_1990
MNA = data$Region_MNA         # Middle East and North Africa
AFR = data$Region_AFR         # Africa
LAC = data$Region_LAC         # Latin America
DEV = data$Region_DEV         # Developed countries
SAS = data$Region_SAS         # South Asia
EAS = data$Region_EAS         # East Asia
ECA = data$Region_ECA         # Eastern Europe and Central Asia 

dataWithoutBirthrates = data[-c(13:18)]

#DATA DESCRIPTIVES
stat.desc(data)
cor(data)

#OLS REGRESSION AND HYPOTHESIS TESTING
model = lm(dataWithoutBirthrates$change_gdp~., data = dataWithoutBirthrates)
summary(model)

#NORMALITY ERROR TERMS
jarque.test(model$residuals)

#MULTICOLLINEARITY
cor(data)

#van laag naar hoog change_age (voor autocorrelation en specification error?)
ordered = dataWithoutBirthrates[order(dataWithoutBirthrates$change_age),]
lmfit = lm(ordered$change_gdp~., data = ordered)

#HETEROSKEDASTICITY
#Goldfeld-Quandt test (niet parametrisch)
gqtest(lmfit, alternative = "two.sided")
#White's General Heteroskedasticity test
bptest(model,~ change_age +gdp1990 +age1990 +pop1990 + LAC + EAS + SAS + AFR +MNA + ECA + I(change_age^2) + I(gdp1990^2)+ I(age1990^2) + I(pop1990^2))

#AUTOCORRELATION
#Graphical autocorrelation
laggedResPlot(lmfit$residuals, lag = 1)
#Runs test
runs(lmfit)
N1 = 76
N2 = 93
N = N1 + N2
mean = ((2*N1*N2)/N) + 1
mean
variance = (2*N1*N2*(2*N1*N2 - N))/((N^2)*(N-1))
variance
stddev = sqrt(variance)
stddev
Lowertail = mean - 1.96 * stddev
Lowertail
Uppertail = mean + 1.96 * stddev
Uppertail
#Durbin-Watson test (X stochastic?)
dwtest(lmfit, alternative = "two.sided")
#Breusch-Godfrey LM test
bgtest(lmfit)

#SPECIFICATION ERRORS
#Ramsey's RESET test
resettest(lmfit, power = 2:3, type = 'fitted')
# Lagrange Multipliers test 
aux = lm(lmfit$residuals~data$change_age +I(data$change_age^2) + I(data$change_age^3)+ data$GDP1990 +I(data$GDP1990^2) + I(data$GDP1990^3) + data$Pop1990 +I(data$Pop1990^2) + I(data$Pop1990^3)+data$Age1990 +I(data$Age1990^2) + I(data$Age1990^3) + data$Region, data = data)
summary(aux)
LM = 169*summary(aux)$r.squared
LM 
k = 6
qchisq(0.95, df = k)
qchisq(0.99, df = k)
#Forecast ChiSquared test
NumbersInFirstSet = 1:100
NumbersInSecondSet = 101:n
firstSet = ordered[NumbersInFirstSet,]
secondSet = ordered[NumbersInSecondSet,]
auxiliaryModelTrain = lm(dataWithoutBirthrates$change_gdp~., data = dataWithoutBirthrates, subset = NumbersInFirstSet)
auxiliaryModelTest = lm(dataWithoutBirthrates$change_gdp~., data = dataWithoutBirthrates, subset = NumbersInSecondSet)
sumOfSquaredResiduals = sum(auxiliaryModelTrain$residuals^2)
esimateVariance = var(auxiliaryModelTest$residuals)

ratio = sumOfSquaredResiduals/estimateVariance
ratio
qchisq(0.95, df = length(NumbersInSecondSet))
qchisq(0.99, df = length(NumbersInSecondSet))