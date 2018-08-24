\name{ez.reshape}
\alias{ez.reshape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reshape data frame from large to long format
}
\description{
Wrapper function for reshaping data frame from large to long format
}
\usage{
ez.reshape(data=NULL, varying = NULL, v.names = NULL, idvar = "id", IV.names=NULL, IV.levels=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{data}{a data frame}
\item{varying}{ names of sets of variables in the wide format that correspond to single variables in long format (‘time-varying’). }
\item{v.names}{ names of variables in the long format that correspond to multiple variables in the wide format.}
\item{idvar}{Names of one or more variables in long format that identify multiple records from the same group/individual. }
\item{IV.names}{list with the name of the independant variables names created in the long format }
\item{IV.levels}{ list with the levels of the independant variables created in the long format   }

}
}
\details{
This function is mainly based on the reshape function from base. 
It allows to return a data.frame in a long format. 
It is not neccessary to state the name of the data frame given that the data.frame in the long format is automatically assigned 
to the .GlobalEnv with the name of the dataframe which is extended with ".long". 
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
df<-data.frame(student=c(rep(1,5),rep(2,5)), month=c(1:5,1:5),  
                               quiz1p1=seq(20,20.9,0.1),quiz1p2=seq(30,30.9,0.1),  
                              quiz2p1=seq(80,80.9,0.1),quiz2p2=seq(90,90.9,0.1))
df
ez.reshape() # GUI
# or in command line
ez.reshape(data=df, varying =list(c('quiz1p1','quiz1p2','quiz2p1','quiz2p2')), 
v.names =c('Score'),idvar =c('student','month'),IV.names=list('Quizz','Part'), 
IV.levels=list( c('Quizz1','Quizz2'), c('Part1','Part2')))
 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
