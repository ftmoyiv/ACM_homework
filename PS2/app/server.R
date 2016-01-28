library(mvtnorm)
library(extrafont)
library(ggplot2)

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

# correlation is slightly negative
sigmaXY(rho=-0.1, sdX=1, sdY=20)

# highly positive
sigmaXY(rho=0.8, sdX=2, sdY=30)

# creating a function for all of this
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved,
                     sdDenied, rhoApproved, rhoDenied, seed=1111) {
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  loanDf <- as.data.frame(rbind(approved,denied))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
  target = c(rep(0, noApproved), rep(1, noDenied))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

shinyServer(function(input, output, session) {
  ## create new df with your data generating function from above
  selData <- reactive({
    loanDF <- loanData( 50, 50, c(input$muApp.Solvency, input$muApp.PIratio), 
                       c(input$muDeny.Solvency, input$muDeny.PIratio),
                       c(input$stdApp.Solvency, input$stdApp.PIratio), 
                       c(input$stdDeny.Solvency, input$stdDeny.PIratio),
                       - 0.05, 0.2, 935)  
        return(loanDF)
  })
 
  ## create the boundary line
  boundary <- reactive({
    #Run lm for coeffs
    datafit <- lm(target ~ solvency + PIratio + 1, data=selData())
    
    #grab coeff
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    
    #set up data for boundary line
    x <- seq(min(selData()$PIratio), max(selData()$PIratio),
             length.out = nrow(selData()))
    y <- -(weights["PIratio"]/weights["solvency"])*x +
      (0.5-bias)/weights["solvency"]
    
    # df for boundary line
    boundaryDf <- data.frame(PIratio=x, solvency=y,
                             deny=rep("Boundary", length(x)))
    
    list(boundaryDf=boundaryDf, datafit=datafit)
  })
  
  ##  compute misclassification
  prediction <- reactive({
    
    #assign labels for approved and denied
    predictedLabels <- ifelse(predict(boundary()$datafit) < 0.5, "Approved", "Denied")
    
    return(predictedLabels)
    
  })

  ##  load font and create plot  
  loadfonts()
  output$plot1 <- renderPlot({
    ggplot(data = selData(),
           aes(x = solvency, y = PIratio, colour=deny, fill=deny)) +
      geom_point() +
      xlab("solvency") +
      ylab("PIratio") +
      theme_bw() +
      theme(text=element_text(family="Arial")) +
      # add boundary line
      geom_line(data=boundary()$boundaryDf) 
  })
 
  ##  create contingency table 
  output$table1 <- renderTable({
    contMatrix <- table(selData()$deny, prediction())
    contMatrix
  })
})