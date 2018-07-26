\name{stat.desc}
\alias{stat.desc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive statistics
}
\description{
GUI for descriptive statistics.
}
\usage{
stat.desc(X = NULL, groupes = NULL, data = NULL, tr = 0.1, type = 3, plot = T, ref = T, save = F, html = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{X}{ character or vector. Variable names for which descriptive must be done }
\item{groupes}{ character or vector. Grouping variable. Must be a factor variable}
\item{data}{ name of dataframe in which to find variables }
\item{tr}{ trim value. The value must be between 0 and 0.5 }
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
