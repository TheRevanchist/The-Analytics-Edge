# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer

library(mice)

train = read.csv("train2016.csv")
train = complete(mice(train))

test = read.csv("test2016.csv")
test = complete(mice(test))

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTION = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

library(caret)
limitedTrain = train
limitedTrain$Party = NULL
preproc = preProcess(limitedTrain)
limitedTrain = predict(preproc, limitedTrain)
k = 2
distances = dist(limitedTrain, method = "euclidean")
clusterTrain = hclust(distances, method = "ward.D")
clusterTrain = cutree(clusterTrain, k = 2)
train$clustering = clusterTrain

limitedTest = test
preproc = preProcess(limitedTest)
distances = dist(limitedTest, method = "euclidean")
clusterTest = hclust(distances, method = "ward.D")
clusterTest = cutree(clusterTest, k = 2)
test$clustering = clusterTest

library(randomForest)
Forest = randomForest(Party ~., data = train, ntree = 10000)
PredictForest = predict(Forest, newdata = test)
MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTION = PredictForest)
write.csv(MySubmission, "SubmissionSimpleLogRFCluster4.csv", row.names=FALSE)