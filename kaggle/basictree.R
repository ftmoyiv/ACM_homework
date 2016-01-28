library(rpart)
if (!require("rattle")) install.packages("rattle"); library(rattle)
if (!require("rpart.plot")) install.packages("rpart.plot"); library(rpart.plot)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)

trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)
testData <- read.csv("news_popularity_test.csv", stringsAsFactors = T)

# Function to predict labels. Can supply own variable selection with vars. Uses all by
# default. Default to save tree graph and csv for submission. Return dataframe
predLabs <- function(train, test, vars = NA , pdf = TRUE, csv = TRUE){
  
  # Making the formulae
  if(is.na(vars)){
    n <- length(names(train)) - 1
    form <- as.formula(paste0("popularity~",
                              paste(names(trainData)[4:n],collapse = "+")))
  } else {
    form <- as.formula(paste0("popularity~",
                              paste(names(trainData)[vars],collapse = "+")))
  }
  
  # Creating the tree
  tree <- rpart(formula = form, data = train, method = "class")
  
  # Saving the diagram to pdf
  if(pdf){
    pdf("treediagram.pdf")
    fancyRpartPlot(tree)
    dev.off()
  }
  
  # Getting predicted labels
  Prediction <- predict(tree, test, type = "class")
  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  if(csv){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  return(popularityClass)
}

predLabs(trainData, testData)

# Idea could be to make a new set of variables depending on the date
# Maybe get some that are in summer months / winter months
