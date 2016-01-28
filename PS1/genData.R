library(ggplot2)
genData <- function(numA=150, numB=150, freqA=6, freqB=3, shiftA=0.5, shiftB=0.5, jitA=5000, jitB=5000, csv=TRUE, pdf=TRUE){
  a <-runif(numA, 0, pi)
  b <-runif(numB, 0, pi)
  time <- c(a, b)
  setA <-jitter(cos(freqA*a)+shiftA*a, jitA)
  setB <-jitter(cos(freqB*b)+shiftB*b, jitB)
  amplitude <- c(setA, setB)
  data <- data.frame(time, amplitude)
  label <- c(rep("A", numA), rep("B", numB))
  target = c(rep(0, numA), rep(1, numB))
  data <- data.frame(data, label, target)
  colnames(data) <- c("time", "amplitude", "label", "target")
  
  if(csv){
    write.csv(data, "dataset.csv")
  }
  if(pdf){
    pdf("dataPlot.pdf")
    print(ggplot(data = data, 
           aes(x = time, y = amplitude, colour=label, fill=label)) + 
      geom_point() +
      xlab("time") +
      ylab("amplitude") +
      theme_bw())
    dev.off()
  }
  return(data)
}

genData()
