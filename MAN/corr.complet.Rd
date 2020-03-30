\name{corr.complet}
\alias{corr.complet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detailed correlation analysis
}
\description{
GUI for detailed correlation analysis. May be used in command line
}
\usage{
corr.complet(X=NULL, Y=NULL, Z=NULL,data=NULL,  group=NULL,
          param=c("param", "non param", "Bayes" "robust"), 
           save=F, outlier=c("complete", "id", "removed"),  
           z=NULL, info=T, n.boot=NULL, rscale=0.353, html=T)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X}{ character or vector. First set of variables. These variables will be on the X axis of the plot}
\item{Y}{ character or vector. Second set of variables. These variables will be on the Y axis of the plot}
\item{Z}{ character or vector. Variables that mut be controlled in the analysis}
\item{data}{ name of dataframe in which to find variables }
\item{group}{ character or vector. Factor variable for which analyses must be done by subgroup.}
\item{param}{character. One or several among "param", "bayes", "non param", and "robust"}
\item{save}{logical. Save the results in ms word document}
\item{outlier}{character. One or several among "complete", "id", or "removed"}
\item{z}{numeric value. Default for outlier is Grubbs test. If z is not NULL, scaled values are used with z as cutoff}
\item{info}{Logical. Should information be printed in the console when using dialog boxes?}
\item{n.boot}{number of bootstrap for bayes and robust analyses. Default is 1000}
\item{rscale}{rscale for the Bayes analyses. Default = 0.353 see details}  
\item{html}{logical. Do you want easieR to output the results in nice html document ?}
}

\details{
corr.complet relies on the packages 'psych', 'BayesFactor' and 'ppcor' for performing parmatric 
  and non parametric analyses, as well as Bayes and robust analyses.
  rscale is based on Morey's recommendation. The value "medium", "wide" and "ultrawide" are also possible.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Descriptive }{Descriptive statistics}
\item{normality}{Shapiro test, Lilliefors test, graphical distribution of residuals and QQplot}
\item{Plot}{The ggplot scatter plot}
\item{Bravais Pearson correlation}{The Bravais-Pearon correlation, if robust has been selected, bootstrapped BCa CI are provided}
\item{Spearman rho}{Spearman's rho, if robust has been selected, bootstrapped BCa CI are provided}
\item{Kendal tau}{Kendall's tau}
\item{Bayes factor}{Bayes factor for the analysis}
\item{Grubbs test}{Grubbs' test allowing to determine whether there are outlier in the analysis}  
  %% ...
}
\references{
%% ~put references to the literature/web site here ~
Canty, A, & Ripley, B. (2019). boot: Bootstrap R (S-Plus) Functions. R package version 1.3-23.
Efron, B., & Gong, G. (1983). A Leisurely Look at the Bootstrap, the Jackknife, and Cross-Validation. *The American Statistician, 371*, 36-48. 
Jeffreys, H. (1961). Theory of probability, 3rd edn. Oxford, UK: Oxford University Press.
Richard D. Morey and Jeffrey N. Rouder (2018). BayesFactor: Computation of Bayes Factors for Common Designs. R package version 0.9.12-4.2.
  https://CRAN.R-project.org/package=BayesFactor
}
\author{
Nicolas Stefaniak
}
\note{
%%  ~~further notes~~
}\section{Details }{....} 
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
corr.complet() # open GUI

corr.complet(X=c('Sepal.Length','Sepal.Width'), Y=c('Petal.Length','Petal.Width'), 
    Z =NULL,data=iris, group=c('Species'), param=c('param','non param','robust','bayes'), 
    save=FALSE,outlier=c('complete','id','removed'),z=NULL, info=T, rscale=0.353553390593274, n.boot=1000, html=TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
