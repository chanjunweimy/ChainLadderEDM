test.FourTriangles <- function() {
    ## by Eric Dal Moro
    ## Check the quantile IFRS 17 coming from the function
    ## Results are in the excel sheet available at: 
    ## https://drive.google.com/open?id=0B6piPKdUSkYISWRWMEkzZ3VlWmc
    ## The corresponding article is: 
    ## "PROBABILITY OF SUFFICIENCY OF SOLVENCY II RESERVE RISK MARGINS: PRACTICAL APPROXIMATIONS"
    ## by Eric Dal Moro and Yuriy Krvavych
    ## Available at SSRN: https://ssrn.com/abstract=2652088 
   
    
    RiskMargin=30000000
  
    MRT <- QuantileIFRS17(FourTriangles, Correl4, RiskMargin)
  
    Quantile <- 0.633
    GammaX<-0.375
    
    ## test output from MackChainLadder
    checkEquals(MRT$QuantileIFRS_17, Quantile,tol=0.0015, checkNames = FALSE)
    checkEquals(MRT$Skewness, GammaX,tol=0.0015, checkNames = FALSE)
}

test.ThreeTriangles <- function() {
  ## by Eric Dal Moro
  ## Check the quantile IFRS 17 coming from the function
  ## Results are in the excel sheet available at: 
  ## https://drive.google.com/open?id=0B6piPKdUSkYISWRWMEkzZ3VlWmc
  ## The corresponding article is: 
  ## "PROBABILITY OF SUFFICIENCY OF SOLVENCY II RESERVE RISK MARGINS: PRACTICAL APPROXIMATIONS"
  ## by Eric Dal Moro and Yuriy Krvavych
  ## Available at SSRN: https://ssrn.com/abstract=2652088 
  

  RiskMargin=1000000
  
  MRT <- QuantileIFRS17(ThreeTriangles, Correl3, RiskMargin)
  
  Quantile <- 0.669
  Gamma<-0.2144
  
  ## test output from MackChainLadder
  checkEquals(MRT$QuantileIFRS_17, Quantile,tol=0.0015, checkNames = FALSE)
  checkEquals(MRT$Skewness, Gamma, tol=0.0015, checkNames = FALSE)
}
