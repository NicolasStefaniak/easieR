vef.pack <-
function(){
    pack.to.inst <- c('afex',
'akima',
'Amelia',
'asbio',
'BayesFactor',
'bibtex',
'car',
'cobs',
'corpcor',
'DAAG',
'deldir',
'DescTools',
'doBy',
'dplyr',
'emmeans',
'epitools',
'foreign',
'ggplot2',
"ggplotgui",
'gmodels',
'GPArotation',
'gsl',
'knitr',
'lars',
'lsr',
'MBESS',
'mc2d',
'mgcv',
'mlogit',
'nFactors',
'nortest',
'olsrr',
'outliers',
'pander' ,                     
'pgirmess',
'phia',
'pkgmaker',
'plyr',
'ppcor',
'psych',
'pwr',
'QuantPsyc',
'quantreg',
'Rcpp',
'readxl',
'reshape2',
'Rfit',
'RGtk2',
'RGtk2Extras',
'rmarkdown',
'rms',
'robust',
'robustbase',
'rpivotTable',
'rrcov',
'rtf',
'scatterplot3d',
'semPlot',
'sos',
'sp',
'stringi',
'stringr',
'svDialogs',
'TeachingDemos',
'trimcluster',
 'WRS',                   
'WRS2'
)


        
        
        
        
        )

          list()->Resultats
    Resultats$"packages.installés.correctement"<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==TRUE) ]
    Resultats$"Package.mal.installés"<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==FALSE) ]
    return(Resultats)
}
