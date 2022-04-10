
# Note: 
# ~ You can skip running Data Exploration part if you just want to see the classification process.
# ~ But Data Preprocessing part is a must.

install.packages("e1071")
install.packages("ggplot2")
install.packages("vtable")
install.packages("magrittr") 
install.packages("dplyr")    
install.packages("plotly")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("caret")
install.packages("rpart")
install.packages("rattle")
install.packages("scales")
install.packages("formattable")

library(e1071)
library(ggplot2)
library(vtable)
library(magrittr) 
library(dplyr)    
library(plotly)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(caret)
library(rpart)
library(rattle)
library(scales)
library(formattable)

##############################
#~~ 1.0 DATA PREPROCESSING ~~#
##############################

#~ 1.1. General Processing ~#

setwd(" ") #set your own working directory path
cvd_1 <- read.csv2("cardio_train.csv", sep = ";")
vtable(cvd_1)
summary(cvd_1)

# From the summary() we can see there are impossible values in height, weight, ap_hi and ap_lo which indicate outliers
# Eg. Min and max values of both height and weight. Negative values of blood presssure as well as their min and max

# Check for NULL values. The result will show there's no null values in our data
sum(is.na(cvd_1))

# Store data frame cdv_1 into cdv_2 so that if we messed up the data processing onwards we can just initialize this back
cvd_2 <- cvd_1

#~ 1.1. Data Transformation 1 ~#

# Change data type of categorical attributes to factor
cols_to_change_fac = c(3,8:13)
for (i in cols_to_change_fac) {
  cvd_2[, i] <- as.factor(cvd_2[, i])
}

# Change data type of numerical attributes to numeric
cols_to_change_num = c(2,4:7)
for (i in cols_to_change_num) {
  cvd_2[, i] <- as.numeric(cvd_2[, i])
}

# Using absolute function to convert negative values of blood pressure into positive
cvd_2$ap_hi <- abs(cvd_2$ap_hi)
cvd_2$ap_lo <- abs(cvd_2$ap_lo)

#~ 1.2. Outliers ~#

# Visualize outliers in height, weight, ap_hi, ap_lo using boxplots
# Output: Outliers folder
boxplot(cvd_2$height ~ cvd_2$cardio, main="Height by cardio", ylab = "Height blood pressure", xlab = "cardio", col=(c("gold", "darkgreen")))
boxplot(cvd_2$weight ~ cvd_2$cardio, main="Weight by cardio", ylab = "Weight blood pressure", xlab = "cardio", col=(c("gold", "darkgreen")))
boxplot(cvd_2$ap_hi ~ cvd_2$cardio, main="Systolic blood pressure (ap_hi) by cardio", ylab = "ap_hi", xlab = "cardio", col=(c("gold", "darkgreen")))
boxplot(cvd_2$ap_lo ~ cvd_2$cardio, main="Diastolic blood pressure (ap_lo) by cardio", ylab = "ap_lo", xlab = "cardio", col=(c("gold", "darkgreen")))

# Getting rid of outliers in height, weight, ap_hi, ap_lo
outliers1 <- boxplot(cvd_2$height, plot=FALSE)$out
outliers2 <- boxplot(cvd_2$weight, plot=FALSE)$out
outliers3 <- boxplot(cvd_2$ap_hi, plot=FALSE)$out
outliers4 <- boxplot(cvd_2$ap_lo, plot=FALSE)$out

# Store the new data without the outliers into the cvd_2 data frame
cvd_2 <- cvd_2[-which(cvd_2$height %in% outliers1),]
cvd_2 <- cvd_2[-which(cvd_2$weight %in% outliers2),]
cvd_2 <- cvd_2[-which(cvd_2$ap_hi %in% outliers3),]
cvd_2 <- cvd_2[-which(cvd_2$ap_lo %in% outliers4),]

#~ 1.3. Data Transformation 2 ~#

# Drop the id column as it gives us no valuable info whatsoever about the data
cvd_2 <- cvd_2[-1] 

# Convert age metric from days to years
cvd_2$age <- format(round((cvd_2$age/365), 0), nsmall = 0)
cvd_2$age <- as.numeric(cvd_2$age)

# Convert height metric from cm to m
cvd_2$height <- cvd_2$height/100

# Calculate BMI
bmi <- format(round(cvd_2$weight/(cvd_2$height^2)), 0, nsmall = 0)
cvd_2$bmi <- bmi
cvd_2$bmi <- as.numeric(cvd_2$bmi)

# Rearrange the columns so BMI column is placed beside weight for easy viewing
cvd_2 <- cvd_2[,c(1,2,3,4,13,5,6,7,8,9,10,11,12)]
sapply(cvd_2, class)

# Constructing variable table
# Objective: factual information
# Examination: results of medical examination
# Subjective: information given by the patient
label <- data.frame(
  id = "Objective Feature",
  age = "Objective Feature | Age | (days)",
  gender = "Objective Feature | Gender | 1: women, 2: men",
  height = "Objective Feature | Height (cm)",
  weight = "Objective Feature | Weight (kg)",
  bmi = "Objective Feature | Body mass index",
  ap_hi = "Examination Feature | Systolic blood pressuree",
  ap_lo = "Examination Feature | Diastolic blood pressure",
  cholesterol = "Examination Feature | Cholesterol | 1: normal, 2: above normal, 3: well above normal",
  gluc = "Examination Feature | Glucose | 1: normal, 2: above normal, 3: well above normal",
  smoking = "Subjective Feature | Smoking | 0: not smoking, 1: smoking",
  alco = "Subjective Feature | Alcohol intake | 0: not taking alcohol, 1: take alcohol",
  active = "Subjective Feature | Physical activity | 0: not active, 1: active",
  cardio = "Target Variable | Presence or absence of cardiovascular disease | 0: Absent, 1: Present"
)

# vtable function will output a descriptive variable table that can be viewed continuously while working with data
# Output: Variable Table of Cardiovascular Disease Data Set After Removing Outliers.png
vtable::vtable(cvd_2, labels = label)
summary(cvd_2)

#~ 1.4. General Processing ~#

# The current theme is automatically applied to every plot we draw
theme_set(theme_bw()) 

# Store cvd_2 data frame into cdv_3
cvd_3 <- cvd_2

# Recode label value to the categorical/binary values for better understanding and presentation
cvd_3 <- cvd_3 %>%
  mutate(cardio = fct_recode(cardio, absent = "0", present = "1"))
cvd_3 <- cvd_3 %>%
  mutate(gender = fct_recode(gender, women = "1", men = "2"))
cvd_3 <- cvd_3 %>%
  mutate(cholesterol = fct_recode(cholesterol, normal = "1", above_normal = "2", well_above_normal = "3"))
cvd_3 <- cvd_3 %>%
  mutate(gluc = fct_recode(gluc, normal = "1", above_normal = "2", well_above_normal = "3"))
cvd_3 <- cvd_3 %>%
  mutate(smoke = fct_recode(smoke, not_smoking = "0", smoke = "1"))
cvd_3 <- cvd_3 %>%
  mutate(alco = fct_recode(alco, not_taking_alcohol = "0", take_alcohol = "1"))
cvd_3 <- cvd_3 %>%
  mutate(active = fct_recode(active, not_active = "0", active = "1"))

############################
#~~ 2.0 DATA EXPLORATION ~~#
############################

#~ 2.1. Univariate Analysis ~#

# Show the distribution of target variable
# Output: Univariate - Cardio Class Distribution.png
chart_target <-
  cvd_3 %>%
  count(cardio) %>%
  mutate(pct = round(n / sum(n) * 100)) %>%
  ggplot(aes(x = cardio, y = n, fill = cardio)) +
  geom_bar(stat = "identity", width = 0.4, show.legend = FALSE) +
  labs(
    x = "Heart disease presence", y = "Number of patients",
    title = str_c("Class distribution of cardio")
  ) +
  scale_fill_manual(values = c("present" = "red", "absent" = "green"), aesthetics = "fill") +
  geom_text(aes(label = str_c(pct, "%")), vjust = 4.5, size = 2.5, colour = "black") +
  theme(
    legend.position = "top", axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 50, vjust = 0.3, face = "bold")
  )

ggplotly(chart_target, tooltip = c("x", "y"))

#~ 2.2. Bivariate Analysis ~#

# Visualize class separation by numeric features
# Numeric features: age, height, weight, bmi, ap_hi, ap_lo
# Output: In numeric folder

plot_box <- function(df, cols, col_x = "cardio") {
  for (col in cols) {
    p <- ggplot(df, aes(x = .data[[col_x]], y = .data[[col]], fill = .data[[col_x]])) +
      geom_boxplot(
        show.legend = FALSE, outlier.colour="blue", 
        outlier.fill="blue", outlier.size=0.5
      ) +
      scale_fill_manual(
        values = c("present" = "red", "absent" = "green"), aesthetics = "fill"
      ) +
      labs(
        x = "Heart disease presence", y = str_c(col),
        title = str_c("Box plot of", col, "for heart disease", sep = " ")
      ) +
      theme(
        axis.text.x = element_text(face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")
      )
    
    print(p)
  }
}

num_cols <-
  cvd_3 %>%
  select_if(is.numeric) %>%
  colnames()

plot_box(cvd_3, num_cols)

# Visualize class separation by categorical features
# Categorical features: gender, cholesterol, gluc, smoke, alco, active
# Output: In category folder

plot_bars <- function(df, cat_cols, facet_var) {
  for (col in cat_cols) {
    p <- ggplot(df, aes(x = .data[[col]], fill = .data[[col]])) +
      geom_bar(show.legend = F) +
      labs(
        x = col, y = "Number of patients",
        title = str_c("Bar plot of", col, "for heart disease", sep = " ")
      ) +
      facet_wrap(vars({{ facet_var }}), scales = "free_y") +
      theme(
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold")
      )
    
    print(p)
  }
}

cat_cols <-
  cvd_3 %>%
  select_if(is.factor) %>%
  colnames()

cat_cols <- cat_cols[-7] # removing the class label
cat_cols
plot_bars(cvd_3, cat_cols, cardio)

#~ 2.3. Multivariate Analysis ~# 

# Correlation matrix
# Output: Multivariate - Correlation Matrix Cardiovascular Disease Data Set.png
cvd_2$height <- cvd_2$height*100
par(mfrow=c(1,1))
dCorr <- cvd_2
dCorr[] <- lapply(dCorr,as.integer)
correlation = cor(dCorr[,1:13])

title <- "Correlation matrix of cardiovascular disease dataset"
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation, method="color", col=col(150),  
         type="upper",
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         sig.level = 0.01, insig = "blank",
         #title=title,
         diag=FALSE,
         title = title,
         mar=c(0,0,1,0)
)

# Scatter plot of blood pressure relationship by cardio
# Output: Multivariate - Blood Pressure Relationship by Cardio.png
ggplot(cvd_3, 
       aes(x = ap_hi, 
           y = ap_lo,
           color= cardio)) +
  geom_jitter(alpha = 0.7,
              size = 1.5) +
  labs(x = "Systolic Blood Pressure (ap_hi)",
       y = "Diastolic Blood Pressure (ap_lo)",
       title = "Blood pressure relationship by cardio"
  )

############################
#####~~ 3.0 MODELING ~~#####
############################ 

# It'll be redundant if we do classification using both height and weight together with BMI.
# In data exploration, we found out that height has no correlation with the target variables. 
# So it's better if we drop BMI for two reasons:
# 1) It's calculated using height and weight 
# 2) More precise to classify using those two variables seperately in order to see the feature importance later 

cvd_4 <- select(cvd_3, -c(5))

# Create training and test data sets for cardio_train.csv
# Split the data set into training and test samples using the following method
partition <- createDataPartition(cvd_4$cardio, p = 0.8, list = FALSE)

# Create the training and test samples
train_cvd <- cvd_4[partition, ]
test_cvd <- cvd_4[-partition, ]

cat("The dimension of the training set is (", dim(train_cvd), ")")
cat("The dimension of test set is (", dim(test_cvd), ")")

# We'll compare first between Decision Tree, KNN and Naive Bayes which is the optimal model for our classification problem
# Setting all arguments the same for all model with exception of the method 
# Using 10 cross-validation as a standard to achieve good accuracy and avoiding overfitting

#~ 3.1. Decision Tree Model ~#

dt.model <- train(cardio ~ ., 
                  method = "rpart", 
                  data = train_cvd,
                  trControl = trainControl(
                    method = "cv", 
                    number = 10,
                    returnResamp = "all", 
                    classProbs = TRUE, 
                    summaryFunction = twoClassSummary
                  ),
                  metric="ROC"
)

dt.predict <- predict(dt.model, test_cvd, type = "raw")

Predicted <-  dt.predict
Actual <- test_cvd$cardio
xtab <- table(Predicted, Actual)
cfm.dt <- confusionMatrix(xtab)

#~ 3.2. KNN Model ~#

knn.model <-
  train(cardio ~ .,
        data = train_cvd, 
        method = "knn",
        trControl = trainControl(
          method = "cv", 
          number = 10,
          returnResamp = "all", 
          classProbs = TRUE, 
          summaryFunction = twoClassSummary
        ),
        metric="ROC"
  )

knn.predict <- predict(knn.model, test_cvd, type = "raw")

Predicted <-  knn.predict
Actual <- test_cvd$cardio
xtab <- table(Predicted, Actual)
cfm.knn <- confusionMatrix(xtab)

#~ 3.3. Naive Bayes Model ~#

naiveBayes.model <- train(cardio ~ .,
                          data = train_cvd,
                          method = "naive_bayes", 
                          trControl = trainControl(
                            method = "cv",
                            number = 10, 
                            returnResamp = "all",
                            classProbs = TRUE, 
                            summaryFunction = twoClassSummary
                          ),
                          metric="ROC"
)

naiveBayes.predict <- predict(naiveBayes.model, test_cvd, type = "raw")

Predicted <-  naiveBayes.predict
Actual <- test_cvd$cardio
xtab <- table(Predicted, Actual)
cfm.nb <- confusionMatrix(xtab)

# Tabulate and compare the accuracies between models. Arrange in increasing accuracy order
# Output: Classification Models Accuracy Comparison.png
Accuracy = list()
Accuracy$DecisionTree <- cfm.dt$overall[1]
Accuracy$KNN <- cfm.knn$overall[1]
Accuracy$NaiveBayes <- cfm.nb$overall[1]  

evaluation <- tibble(
  DecisionTree = Accuracy$DecisionTree, KNN = Accuracy$KNN, NaiveBayes = Accuracy$NaiveBayes
)

Accuracy <- t(evaluation)
Model <- rownames(Accuracy)
accuracy_table <- as_tibble(Accuracy) %>%
  add_column(Model) %>%
  rename(Accuracy = "V1") %>%
  arrange(Accuracy) %>%
  dplyr::select(Model, Accuracy)
formattable(accuracy_table)

# DT model has the highest accuracy so it's the most optimal model for classifying this data set

#~ 3.4. Pruning and Optimizing Decision Tree Model ~#

# Fully grown Decision Tree, cp = 0
dt.model.full <- rpart(cardio ~ ., data = train_cvd, method = "class", cp = 0)

#dev.off()
printcp(dt.model.full)

#plotcp(dt.model.full, lyt = 3, col = 2, upper = "splits")

bestcp <- dt.model.full$cptable[which.min(dt.model.full$cptable[,"xerror"]),"CP"]

# Pruning and plotting DT based on bestcp
# Output: Pruned Decision Tree.png
dt.model.pruned <- prune(dt.model.full, cp = bestcp)
fancyRpartPlot(dt.model.pruned, palettes = c("Greens", "Reds"), sub = "")   

# The plotted Decision Tree is too big and illegible so it's better to observe the summary of the data frame
# Output: Variable Importance from Pruned Decision Tree.png
summary(dt.model.pruned)

dt.predict.fit <- predict(dt.model.pruned, test_cvd, type = "class")
Predicted <-  dt.predict.fit
Actual <- test_cvd$cardio
xtab <- table(Predicted, Actual)
cfm.dt.fit <- confusionMatrix(xtab)

# Comparing accuracy between full grown DT and pruned DT
# Output: Decision Trees Accuracy Comparison.png
DT.Accuracy = list()
DT.Accuracy$DecisionTree <- cfm.dt$overall[1]
DT.Accuracy$PrunedDesicionTree <- cfm.dt.fit$overall[1]
row.names <- names(DT.Accuracy)
col.names <- "Accuracy"
cbind(as.data.frame(matrix(DT.Accuracy,nrow = 2, ncol = 1, dimnames = list(row.names, col.names))))


# Plotting confusion matrix
# Output: Pruned Decision Tree Confusion Matrix.png
# Legend: Orange quadrants = True label. Yellow quadrants = Misclassification
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Decision tree accuracy:", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Actual, y = Predicted)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "yellow", high = "orange") +
    geom_text(aes(x = Actual, y = Predicted, label = Freq)) +
    theme_update(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

ggplotConfusionMatrix(cfm.dt.fit)

# END