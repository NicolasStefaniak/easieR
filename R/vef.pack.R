vef.pack <-
function(){
    pack.to.inst <- c("afex", "akima",  "Amelia", "asbio","BayesFactor", "bibtex","car", "cobs", "corpcor", "DAAG","deldir", "DescTools","devtools", "doBy","dplyr", "epitools", "emmeans",
                      "foreign", "ggplot2", "ggplotgui","gmodels", "GPArotation", "gsl","knitr"  ,"lars", "lsr", "MBESS", "mc2d","mgcv", "mlogit", "nFactors", "nortest", 
                      "outliers","pander", "pgirmess", "phia", "pkgmaker", "plyr", "ppcor", "psych", "pwr", "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", 
                      "reshape2", "rmarkdown", "rms", "robust", "robustbase", "rtf", "rrcov","rmarkdown", "scatterplot3d","semPlot", "sos", "sp", "stringi", "stringr", "svDialogs", "TeachingDemos",
                      "trimcluster", "wle", "WRS","WRS2")

          list()->Resultats
    Resultats$packages.installés.correctement<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==TRUE) ]
    Resultats$Package.mal.installés<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==FALSE) ]
    return(Resultats)
}
