\name{ez.anova}
\alias{ez.anova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Anova 
}
\description{
GUI for analysis of variance and covariance, with contrasts
}
\usage{
ez.anova<-function(data=NULL, DV=NULL, between=NULL, within=NULL,id=NULL, cov=NULL, RML=NULL, 
                   RML.factor=NULL, param=c("param","bayes"),outlier=c("complete","id", "removed"), 
                   ES="ges", SumS="3", save=F , html=T, contrasts="none",p.adjust="none", n.boot=1000, rscaleFixed = 0.5,
                   rscaleRandom = 1 )

}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{data}{ name of dataframe in which to find variables }
\item{DV}{ character or vector. Dependant variable - the measured variable name }
\item{between}{ character or vector. Between group variable. Must be a factor variable}
\item{within}{ character or vector. Within group variable. Must be a factor variable}
\item{id}{ character or vector. Variable used to identify subjects. Must be a factor variable}
\item{cov}{ character or vector. Variable used as covariable. Must be a numeric/integer variable}
\item{RML}{ character with length >= 2. Repeated measure levels. All the columns that corresponds to repeated measures in the wide format}
\item{RML.factor}{ list. The names in the list corresponds to the names of the factors and the values 
                 at each level correspond to the levels of each factor. The product of the number of levels must equal the length of RML. 
                 If RML is not NULL and RML.factor is NULL, it is assumed that there is only one factor and the name of the factor is "variable.1"}
\item{param}{character. One or several among "param", "bayes", "non param", and "robust"}
\item{outlier}{character. One or several among "complete", "id", or "removed"}
\item{ges}{one among "ges" or "pes"}
\item{SumS}{Type of sum of squares, one among "2" or "3"} 
\item{save}{logical. Do you want to save the results}
\item{html}{logical. Do you want easieR to output the results in nice html document ?}
\item{contrast}{list. The names in the list corresponds to the names of the factors and the values is a matrix of coefficients for the contrasts. "pairs" or "none" are also possible}
\item{p.adjust}{adjust p values for multiples comparisons. see <code>p.adjust</code>}
\item{n.boot}{number of bootstrap for bayes and robust analyses. Default is 1000}
\item{rscaleFixed}{rscale for the fixed effect in Bayes analyses. Default = 0.5. It is not possible to state it in dialog box}  
\item{rscaleRandom}{ rscale for the fixed effect in Bayes analyses. Default = 1. It is not possible to state it in dialog box}  

}
}
\details{
ez.anova relies on the packages 'afex', 'emmeans', 'WRS2', 'WRS', 'BayesFactor' and 'PMCMRplus' for performing parmatric 
  and non parametric analyses, as well as Bayes and robust analyses
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Model}{The model that has been tested}
\item{Descriptive }{Descriptive statistics}
\item{normality}{Shapiro test, Lilliefors test, graphical distribution of residuals and QQplot}
\item{Homogenity of variance}{Levene test testing the homogenity of variance}
\item{Plot}{The ggplot that summarizes the analysis}
\item{Mauchly test}{Mauchly test that allows to assess the sphericity of the covariance matrix}
\item{principal analysis}{The main anova table}
\item{Contrast}{Give the matrix of contrasts for each variable and the contrasts computed by emmeans}
\item{Bayes factor}{Bayes factor for the analysis}
\item{Non paramtric test}{Non parametric test. The test of Kruskall-Wallis or anova of Friedman depending on the nature of the variable}
 \item{Robust analyses}{Anova on trimmed means or on medians}
\item{Grubbs test}{Grubbs' test allowing to determine whether there are outlier in the analysis}  
  %% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Nicolas Stefaniak
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ez.anova() # open GUI
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
