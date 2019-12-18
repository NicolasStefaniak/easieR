\name{ez.report}
\alias{ez.report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an HTML report or MS DOC report on all the analyses performed with easieR during the session
}
\description{
This function generate an nice html/docx report from ez.results. 
}
\usage{
ez.report(html = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{html}{
Logical. Must the output be an html document (TRUE) or a MS word document (FALSE)
}
}
\details{
ez.report is used for the generation of the report of a session. 
}
\value{
The function return the HTML/MS word page of the object produced by knitr. 
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

ez.results<-list()
ez.result$df<-df
ez.result$p<-p 
ez.report(html=F)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
