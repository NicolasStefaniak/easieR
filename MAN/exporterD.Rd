\name{exporterD}
\alias{exporterD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GUI for exporting data.frame in csv file. 
}
\description{
Wrapper function for \code{\link{ write.csv}} allowing to export data.frame in a csv file by using dialog box. 
}
\usage{
exporterD(data = NULL, nom = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data frame to be written.
%%     ~~Describe \code{data} here~~
}
  \item{nom}{
Character string naming a file or a connection open for writing. "" indicates output to the console.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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

exporterD(data = cars, nom = "cars.csv")
# is the same as 
write.csv(x=cars, file="cars.csv")
# but can also be used with GUI
exporterD()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
