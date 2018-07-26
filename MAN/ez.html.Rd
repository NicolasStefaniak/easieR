\name{ez.html}
\alias{ez.html}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an HTML report
}
\description{
This function generate an nice html report for most common object in R. 
}
\usage{
ez.html(ez.results = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ez.results}{
Name of the object for which a HTML repot must be done. 
}
}
\details{
ez.html is mainly used internally by function which returns results. It is also used for the generation of the report of a session. Nevertheless, it can be used for any most common R object (i.e., vector, data.frame, list, ggplot, matrix, character)
}
\value{
The function return the HTML page of the object produced by knitr. 
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
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)
ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y))

p<-ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

result<-list()
result$df<-df
result$p<-p 
ez.html(result)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
