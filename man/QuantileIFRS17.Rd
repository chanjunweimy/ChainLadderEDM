\name{QuantileIFRS17}
\alias{QuantileIFRS17}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile estimation for the IFRS 17 Risk Adjustment
}
\description{
The Quantile IFRS 17 function provides an estimate of the quantile attained on the reserve risk distribution that corresponds to the booked Risk Adjustment amount on top of the Best Estimate.
}
\usage{
QuantileIFRS17(Triangles, Correlations, RiskMargin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Triangles}{
  Import triangles of the same size (number of columns and lines) one below the other. Several triangles should be imported.
}
  \item{Correlations}{
  Correlation matrix depicting the correlations between each triangle imported
}
  \item{RiskMargin}{
  Input the risk margin as a single number
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
QuantileIFRS17 returns a list with the following elements
\item{QuantileIFRS_17}{Quantile attained on the reserve risk distribution with the booked Risk Adjustment}
\item{Skewness}{Skewness of the overall aggregated risk distribution across all triangles input}
\item{CoV}{Coefficient of Variation of the overall aggregated risk distribution across all triangles input}
\item{Res}{Best Estimate reserve using a chain-ladder model applied on all input triangles}
\item{Totals}{A summary of the above elements: Quantile attained on the reserve risk distribution with the booked Risk Adjustment, Skewness, Coefficient of Variation and Total Reserves}
}
\references{
\cite{Thomas Mack. Distribution-free calculation of the standard error of chain ladder reserve estimates. \emph{Astin Bulletin}. Vol. 23. No 2. 1993. pp.213:225}

\cite{Thomas Mack. The standard error of chain ladder reserve estimates: Recursive calculation and inclusion of a tail factor. \emph{Astin Bulletin}. Vol. 29. No 2. 1999. pp.361:366}

\cite{Dal Moro, Krvavych.
  Probability of sufficiency of Solvency II Reserve risk margins: Practical approximations. \emph{ASTIN Bulletin}, 47(3), 737-785
  }
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

library(ChainLadder)
MyData <- read.csv(file="threeTriangles.csv", header=FALSE,  sep=";")
Mycorrel <- read.csv(file="correl1.csv", header=FALSE, sep=";")
RiskMargin=1000000

QuantileIF<-QuantileIFRS17(MyData, Mycorrel, 10000000)
QuantileIF$Totals
}