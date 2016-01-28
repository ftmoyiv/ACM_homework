# The script is the prediction process so far. CSV files are formatted
# for submission.
# Further ideas are to improve the actual random forest generation using different
# functions as well as parameters, because I think this will help the most.
# Otherwise there is a thing of variable selection, but inclusion of all of them
# seems to be the best. Creation of new variables is another shout.




library(rpart)
if (!require("rattle")) install.packages("rattle"); library(rattle)
if (!require("rpart.plot")) install.packages("rpart.plot"); library(rpart.plot)
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)

trainData <- read.csv("news_popularity_training.csv", stringsAsFactors = T)
testData <- read.csv("news_popularity_test.csv", stringsAsFactors = T)

### Variable creation section ###

# This function pulls the year and months from the URL included in the data
# Will probably add all other "designed" features here
getDates <- function(train,test){
  test$popularity <- NA
  n <- nrow(train)
  comb <- rbind(train,test) # Combining the two to make easier
  
  # Looks for the year and month inside the URL
  year <- as.vector(sapply(as.character(comb$url), function(x){
    word <- strsplit(x,"/")[[1]][4]
    return(word)
  }))
  
  months <- as.vector(sapply(as.character(comb$url), function(x){
    word <- strsplit(x,"/")[[1]][5]
    return(word)
  }))
  
  comb$Year <- as.factor(year)
  comb$Month <- as.factor(months)
  
  # Re-arranging the columns otherwise trees aren't happy
  a <- ncol(comb)
  comb <- comb[, c(1:(a-3),a,(a-1),(a-2))]
  
  # Returns a list containing the changed train and test sets
  reslist <- list(train = comb[1:n,],
                  test = comb[(n+1):nrow(comb),])
  
  return(reslist)
}

reslist <- getDates(trainData, testData)

train <- reslist$train # this is required to properly put things back in place
test <- reslist$test

# Function to just return the column indices of the variables you want
# Made this just to clean up some code. The idea is that you have a vector
# With all variables, then use this to get the indices of the ones you want to
# remove.
# e.g.
# vec <- c(1:(length(names(train))-1)) ->>> all the variables
# variables <- c(names(train)[1:2], "Year") ->>> exclude these variables
# newVec <- vec[-getVar(variables,train)] ->>> variables want to include
getVar <- function(varNames, frame){
  col.names <- names(frame)
  
  indices <- sapply(varNames, function(word,y){
    a <- as.numeric(grep(word, y)[1])
  }, y = col.names)
  
  return(indices)
}

### A basic decision tree ###

# Function to predict labels. Can supply own variable selection with vars. Uses all by
# default. Default to save tree graph and csv for submission. Return dataframe
predLabs <- function(train, test, vars = NA , pdf = TRUE, csv = TRUE){
  set.seed(seed)
  
  # Getting the variables. Included some print functions just to de-bug
  if(is.na(vars)){
    n <- length(names(train)) - 1
    form <- as.formula(paste0("as.factor(popularity)~",
                              paste(names(train)[4:n],collapse = "+")))
    print(paste0("popularity~", paste(names(train)[4:n],collapse = "+")))
  } else {
    variables <- names(train)[vars]
    features <- paste(variables,collapse = "+")
    form <- as.formula(paste0("as.factor(popularity)~",
                              features))
    print(paste0("as.factor(popularity)~",
                 features))
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
  print("Getting labels")
  Prediction <- predict(tree, test, type = "class")
  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  if(csv){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  return(popularityClass)
}



### RANDOM TREES ###
if (!require("randomForest")) install.packages("randomForest"); library(randomForest)
if (!require("party")) install.packages("party"); library(party)
if (!require("unbalanced")) install.packages("unbalanced"); library(unbalanced)

# A function that does a similar thing to above, but now using random trees.
# Two options of method, changed by setting "rand" to T or F - F is a bit buggy
# dummytest will allow you to do a cross validation type thing
# Returns the MODEL instead, for the "importance" plot
# Set NT for the number of trees - 100 provides a pretty good result
# PDF = TRUE CURRENTLY DOES NOT WORK
# NOTE that dummytest = T will not provide a "submittable" CSV file
predLabsRF <- function(train, test, vars = NA, NT = 100, seed = 123, 
                       pdf = FALSE, csv = TRUE, rand = TRUE, dummytest = FALSE,
                       DATA = FALSE){
  set.seed(seed)
  if(is.na(vars)){
    n <- length(names(train)) - 1
    form <- as.formula(paste0("as.factor(popularity)~",
                              paste(names(train)[4:n],collapse = "+")))
    print(paste0("popularity~", paste(names(train)[4:n],collapse = "+")))
  } else {
    variables <- names(train)[vars]
    features <- paste(variables,collapse = "+")
    form <- as.formula(paste0("as.factor(popularity)~",
                              features))
    print(paste0("as.factor(popularity)~",
                 features))
  }

  # Cross validating step
  if(dummytest){
    m <- ceiling(nrow(train)/3)
    subset <- sample(c(1:2*m))
    test <- train[-subset,]
    train <- train[subset,]
    
  }
  
  # Creating the random forests. The second method only works for certain seeds.
  if(rand){
    randomTree <- randomForest(form, data = train, importance = TRUE, ntree = NT, 
                               mtry = 7)
    Prediction <- predict(randomTree, test, type = "class")
    
  } else {
    randomTree <- cforest(form, data = train, controls = cforest_unbiased(ntree = NT))
    Prediction <- predict(randomTree, test, OOB=TRUE, type = "response")
  }
  
  # Saving the diagram to pdf
  if(pdf){
    pdf("treediagram.pdf")
    fancyRpartPlot(tree)
    dev.off()
  }
  
  # Getting predicted labels
  print("Getting labels")
  popularityClass <- data.frame(id = test$id, popularity = Prediction)
  
  # Doing the cross validating to see how accurate out of sample.
  if(dummytest){
    popularityClass$real.popularity <- test$popularity
    print(mean(ifelse(popularityClass$popularity == test$popularity, 1, 0)))
  }
  
  if(csv){
    write.csv(popularityClass,"kagglesub.csv", row.names = F, quote = F)
  }
  
  if(!DATA) return(randomTree)
  if(DATA) return(popularityClass)
  
}

# Example run with "importance plot"
vec <- c(1:(length(names(train))-1)) # all the variables

variables <- c(names(train)[1:2])
newVec <- vec[-getVar(variables,train)]
randomFor <- predLabsRF(train,test, csv = TRUE, rand = T, dummytest = T,
                        vars = newVec, NT = 100, seed = 100)



# The plot of which variables are important
varImpPlot(randomFor)
