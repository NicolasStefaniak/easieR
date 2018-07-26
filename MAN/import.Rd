\name{import}
\alias{import}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GUI for importing data
}
\description{
This function is a shotcut for the following functions: \code{\link{read.table}},  \code{\link{read.csv}},  \code{\link{read.spss}},
\code{\link{read_excel}}
}
\usage{
import(file=NULL, dir=NULL, type=NULL, header=T, info=TRUE, dec=".", 
       sep=";",na.strings="NA",sheet=NULL, name="dataset")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{file}{
Character. Name of the file to import
}
\item{dir}{
Character. Directory in which the file is stored
}
\item{type}{
Type of file. One among "csv", "txt", "excel", "SPSS"
}
\item{header}{
Logical. Is the name of the variables on the first row ? 
}
\item{info}{
Should information about dialog boxes be printed in the console?

}
\item{dec}{
Character. The character used in the file for decimal points.
}
\item{sep}{
The field separator character. Values on each line of the file are separated by this character.
}
\item{na.strings}{
a character vector of strings which are to be interpreted as NA values. Blank fields are also considered to be missing values in logical, integer, numeric
}
  \item{sheet}{
 Character. The name of the sheet for excel files.   
}
\item{name}{
Character. Name of the R object in which to store the data. 
}
}
\details{
The function imports data from text files, csv files, SPSS files and excel files. The function check variable names and correct them if necessary. Moreover, variable names are too long (> 30 characters), a dialog box suggest to shorten them.
The function allows to deal accented letters in directory name and/or in file name.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The data frame is stored in R memory with the name given. 
The function also returns the structure of the data frame, the number of missing values for each variable when some values are missing. 

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

# use with dialog box
import()

# use by command line 
# create a csv file 
write.csv2(cars, "cars.csv") 
dir<-getwd()
# import the file 
import(file="cars.csv",dir=dir,Type='csv',dec=',',sep=';',na.strings='NA',sheet=NULL,name="ez.cars")

# remove the file from directory 
file.remove("cars.csv")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
