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
                   rscaleRandom = 1 ){

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


\item{type}{ Which estimate for skew and kurtosis. Value between 1 and 3. See ?describe }
\item{plot}{ Logical. Should the plot be printed ? }
\item{ref}{ Logical. Should references be printed ? }
\item{save}{ Logical. Should results be saved ? }
\item{html}{Logical. Should results be knit in html? 

}
}
\details{
Results from describeBy from psych package or summary function, depending on variable type.
Violin plots are created by ggplot2. The red point is the mean and point range one SD. Each value is also plotted
If saved is true, an rtf document and compatible R file are created in the working directory.
The value 'FALSE' for ref is used in internal easieR function, in order to avoid packages used for this function to be printed.
Note : If the groupes argument is not a factor variable, a message box asks whether you want to convert the variable in a factor.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
require(psych)
data(sat.act)
stat.desc(X=c("education","age","ACT","SATV","SATQ"), groupes="gender", data=sat.act, tr=.1, type=3, plot=T, ref=T, save=F)
### is the same as 
psych::describeBy(sat.act,sat.act$gender) #just one grouping variable
### can also be used as follow 
stat.desc() # open GUI
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
