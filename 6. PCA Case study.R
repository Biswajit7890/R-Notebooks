# Generic script to run PCA on any dataset

##################################################################
# Uncomment and use below lines for Reading Data from a csv file
#FilePath='C:/Users/Farukh Hashmi/Downloads/DiamondPricesData.csv'
#DataForPCA=read.csv(FilePath)
#str(DataForPCA)

##################################################################
# Uncomment and use below lines for Choosing a data.frame 
# which was already present in workspace

library(MASS)
DataForPCA=Boston
str(DataForPCA)

##################################################################
# Specifying the target variable present in above data
# If this is a classification problem then convert the Target variable into Factor
TargetVariableName='medv'

##################################################################

# All Commands below are generic
# Just change the data and specify the target variable above
#################################################################
# Checking the missing values in Data and removing
# BEFORE deleting missing data ###
dim(DataForPCA)
colSums(is.na(DataForPCA))

DataForPCA=na.omit(DataForPCA)

# AFTER deleting missing data ###
dim(DataForPCA)
colSums(is.na(DataForPCA))

##################################################################

# Separating the target and predictor variables
TargetVariable=DataForPCA[ ,c(TargetVariableName)]

# Use below command ONLY if the TargetVariable is a factor
#TargetVariableName=as.factor(TargetVariableName)
str(TargetVariable)

PredictorVariables=DataForPCA[ , !names(DataForPCA) %in%  TargetVariableName]
str(PredictorVariables)
head(PredictorVariables)

##################################################################
# Converting Predictor variables into numeric format by using dummies
library(dummies)
PredictorVariablesDummies=dummy.data.frame(PredictorVariables, sep='_')

# Making sure the column names for predictors are valid R names
names(PredictorVariablesDummies)=make.names(names(PredictorVariablesDummies))
head(PredictorVariablesDummies)

##################################################################
# Creating unsupervised PCA model on Predictors
PredictorPCA=prcomp(PredictorVariablesDummies,scale=T)

# Taking out the summary of PCA
PCASummary=data.frame(summary(PredictorPCA)$importance)
PCASummary

# Column Names of PCASummary data.frame is Principal component names
PrincipalComponents=1:ncol(PCASummary)
# Third row of the data frame are cumulative variances
IndividualVarianceExplained=PCASummary[2,] * 100
CumulativeVarianceExplained=PCASummary[3,] * 100

# Splitting the Plot window into two sections
par(mfrow=c(2,1))
plot(PrincipalComponents,IndividualVarianceExplained, main='Individual Variances')
lines(PrincipalComponents,IndividualVarianceExplained)
plot(PrincipalComponents,CumulativeVarianceExplained,main='Cumulative Variances')
lines(PrincipalComponents,CumulativeVarianceExplained)

# Changing the setting of plot window to normal
par(mfrow=c(1,1))
##################################################################
# Choosing Principal components based on above plots
# In this example of DiamondPricesData
# Individual plot suggests to choose 5 Principal Components
# Cumulative plot suggests to choose 7 Principal Components
# We will choose both number of Principal Components one by one
# Generate Predictions after machine learning 
# Choose that option which give better accuracy

# Generating principal components values
PrincipalComponentValues=predict(PredictorPCA, PredictorVariablesDummies)
head(PrincipalComponentValues)

# Choosing Best number of Principal Components
PrincipalComponentValuesTop=PrincipalComponentValues[,c(1:5)]
head(PrincipalComponentValuesTop)

##################################################################
# Machine Learning
# Adding the Target variable to Best Principal components which are new predictors

DataForML=data.frame(TargetVariable, PrincipalComponentValuesTop)
head(DataForML)
str(DataForML)

library(randomForest)
PredictiveModel=randomForest(TargetVariable~. , data=DataForML, ntree=10) 
PredictiveModel

# Run Below command to check which Principal Component is more important
varImpPlot(PredictiveModel)

