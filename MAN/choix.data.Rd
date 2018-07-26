\name{choix.data}
\alias{choix.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GUI for choosing data.frame. This function is mainly an internal function of easieR.GUI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
choix.data(data = NULL, info = TRUE, nom = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
 Character. Name of a dataframe 

}
  \item{info}{
Should information about the argument be printed in the console?

}
  \item{nom}{
Logical. Should the name of the dataframe be returned by the function ? 
}
}
\details{
This function is mainly an internal function of easieR.GUI. When the name of a data.frame is provided, the function checks whether the data.frame exists in R memory. If not, a dialog box makes the list of all data.frames in R memory. If data is not stated, a dialog box gives the list of all data.frames in R memory. 
If no data.frame is in R memory, the function call the import function. 
If there is only one data.frame in R memory, it is assumed that user wants to work this data.frame.
}
\value{
it returns either the data.frame that has been choosen or a list with the data.frame and the original name of the data.frame depending on whether name is set to FALSE or TRUE. 
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
data(mtcars)
# return only the data.frame
choix.data() 
# return the name of the data.frame and the data.frame
choix.data(nom=T)
# return the name of the data.frame and the data.frame without dialog box
choix.data(data="mtcars", nom=T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
