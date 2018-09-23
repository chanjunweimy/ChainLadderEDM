
## Authors: Eric Dal Moro / Yuriy Krvavych
## Copyright: Eric Dal Moro / Yuriy Krvavych
## Added 1 August 2018

QuantileIFRS17<-function(
  Triangles,
  Correlations,
  RiskMargin
)
{
MyData <- Triangles
Mycorrel <- Correlations

RM<-RiskMargin

ncol <- dim(MyData)[2]
nlin <- dim(MyData)[1]
nbTriangle <- nlin/ncol

Skew<-c(nbTriangle)
Stdev<-c(nbTriangle)
Reserve<-c(nbTriangle)
Gamma<-c(nbTriangle)
Phi<-c(nbTriangle)
bi<-c(nbTriangle)
ai<-c(nbTriangle)

for (k in c(1:nbTriangle)) {
   Infe<-1+(k-1)*ncol
   Supe<-k*ncol
   Tri<-MyData[c(Infe:Supe),]
   MackCL<-MackChainLadder(Tri)
   Skew[k]<-MackCL$OverSkew
   Stdev[k]<-MackCL$Total.Mack.S.E
   Latest<-getLatestCumulative(MackCL$Triangle)
   Reserve[k]<-sum(MackCL$FullTriangle[,ncol]-Latest)
}

Gamma<-Skew/Stdev^3
Phi<-acos(-Gamma/sqrt(8))
bi<-sqrt(2)*cos(Phi/3+4*pi/3)
ai<-sqrt(1-2*bi^2)

Variance<-sum(Stdev^2)
Skewness<-sum(Skew)

for (j in c(1:nbTriangle))  {
  for (k in c(1:nbTriangle))  {
    if (j != k) {
    Variance<-Variance+Stdev[j]*Stdev[k]*Mycorrel[j,k]*(ai[j]*ai[k]+2*bi[j]*bi[k]*Mycorrel[j,k])
    Skewness<-Skewness+3*Stdev[j]^2*Stdev[k]*2*Mycorrel[j,k]*(2*ai[j]*ai[k]*bi[j]+(ai[j]^2+4*bi[j]^2)*bi[k]*Mycorrel[j,k])
    }
  }
}


for (i in c(1:nbTriangle))  {
  for (j in c(1:nbTriangle))  {
    for (l in c(1:nbTriangle)) {
    if ((i != j)& (i !=l) & (j !=l)) {
    Skewness<-Skewness+Stdev[i]*Stdev[j]*Stdev[l]*(2*(ai[j]*ai[l]*bi[i]*Mycorrel[i,j]*Mycorrel[i,l]+ai[j]*ai[i]*bi[l]*Mycorrel[j,l]*Mycorrel[i,l]+ai[i]*ai[l]*bi[j]*Mycorrel[i,j]*Mycorrel[j,l])+8*bi[i]*bi[j]*bi[l]*Mycorrel[i,j]*Mycorrel[i,l]*Mycorrel[j,l])
         }
    }
  }  
}

GammaX <- Skewness/Variance^1.5
q <- RM/Variance^0.5

Za <- -3/GammaX+sqrt(9/GammaX^2+6*q/GammaX+1)
quantileIF <- pnorm(Za)
CoV<-Variance^0.5/sum(Reserve)
Res<-sum(Reserve)

Totals <-  c(quantileIF, GammaX, CoV, Res)
Totals <- formatC(Totals,digits=3, format="f")
Totals <- as.data.frame(Totals)

colnames(Totals)=c("Totals")

rownames(Totals) <- c("Quantile IFRS 17:","Skewness","Coefficient of Variation:",
                      "Reserve:")

output <- list()
output[["QuantileIFRS_17"]]<-quantileIF
output[["CoV"]]<-CoV
output[["Skewness"]]<-GammaX
output[["Reserve"]]<-Res
output[["Totals"]]<-Totals

return(output)
}