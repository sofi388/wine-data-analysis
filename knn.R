library(tidyverse)
library(icesTAF)
library(readr)
library(lubridate)
library(caret)

# Import the datasets.
# 'red' is the red wine dataset
# 'white' is the white wine dataset.
red   <- read_delim("data/winequality-red.csv", 
                    delim = ";", 
                    locale = locale(decimal_mark = ".", 
                                    grouping_mark = ","), 
                    col_names = TRUE)
white <- read_delim("data/winequality-white.csv", 
                    delim = ";", 
                    locale = locale(decimal_mark = ".", 
                                    grouping_mark = ","), 
                    col_names = TRUE)

# Set column names
cnames <- c("fixed_acidity", "volatile_acidity", "citric_acid",
            "residual_sugar", "chlorides", "free_sulfur_dioxide",
            "total_sulfur_dioxide", "density", "pH",
            "sulphates", "alcohol", "quality")

# Columns used for prediction are all columns
# except 'quality'.
xcol <- c("fixed_acidity", "volatile_acidity", "citric_acid",
          "residual_sugar", "chlorides", "free_sulfur_dioxide",
          "total_sulfur_dioxide", "density", "pH",
          "sulphates", "alcohol")

colnames(red)   <- cnames
colnames(white) <- cnames

# Add the column 'type' to define the type of wine
red   <- mutate(red,   type = "red")
white <- mutate(white, type = "white")

# Join 'red' and 'white' datasets
wine <- rbind(red, white)
wine <- mutate(wine, 
               quality = as.factor(quality),
               type = as.factor(type))
levels(wine$quality) <- paste0("Q", levels(wine$quality))





# Test set will be 10% of the entire dataset
set.seed(2020, sample.kind = "Rounding")
test_index <- createDataPartition(y = wine$type, 
                                  times = 1, 
                                  p = 0.2, 
                                  list = FALSE)

# Train and test sets for wine type
train_set <- wine[-test_index,]
test_set  <- wine[test_index,]

# Train and test sets for red wine quality
train_set_red <- train_set[which(train_set$type == "red"),]
test_set_red  <- test_set[which(test_set$type == "red"),]

train_set_red$quality <- factor(train_set_red$quality)
test_set_red$quality  <- factor(test_set_red$quality)


summary(train_set)

# Formula used in predictions
fml <- as.formula(paste("type", "~", 
                        paste(xcol, collapse=' + ')))
# K = 5
# Train
fit_knn <- knn3(formula = fml, data = train_set, k = 20)

# Predict
y_knn <- predict(object = fit_knn, 
                 newdata = test_set, 
                 type ="class")

# Compare the results: confusion matrix
caret::confusionMatrix(data = y_knn, 
                       reference = test_set$type, 
                       positive = "red")



# Distribution of red and white wines
ggplot(data = train_set) + 
  geom_bar(aes(type, fill = type)) +
  labs(title = "Red and white wines") +
  theme(legend.position = 'none')+
  scale_fill_manual(values = c("red" = "violetred4", "white" = "beige"))
