trainData <- read.csv("news_popularity_training.csv")

plotCompare <- function(var1,var2,data){
  if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
  
  ggplot(data = data, aes_string(x = var1, y = var2)) +
    geom_point(aes(colour = as.factor(popularity))) +
    theme_bw() +
    scale_colour_manual("popularity",
                        values = c("blue", "red", "black", "green", "yellow")) # change colour scale here if you want
}

# Put the names of the columns you want to graph as var1 and var2
plotCompare(var1 = "n_tokens_content", var2 = "num_imgs", data = trainData)
