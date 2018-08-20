ez.install <-
function(){
  require(tcltk)
  
  # # 2. installer les packages nécessaires et MAJ des packages installés
  # # 2a. packages à installer, par ordre alphabétique
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
'pander',
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
  
  # 2b. packages manquants
  pack.uninst <- pack.to.inst[!(pack.to.inst %in% rownames(installed.packages()))]
  
  # 2c. installer packages manquants si nécessaires et si utilisateur le souhaite
  if(length(pack.uninst)>0){
    inst <- menu(choices=c("oui","non"), graphics=TRUE, title="Voulez-vous installer les packages manquants ?")
    if(length(inst)==0 || inst==2){
      tk_messageBox(type="ok", caption="Attention", message="Vous avez choisi de ne pas installer les packages manquants, cela peut gêner l'exécution de certaines fonctions. Relancez easieR() si vous souhaitez installer les packages.")
    } else {
      writeLines("Installation des packages")
      print(pack.uninst)
      flush.console()
      install.packages(pack.uninst, quiet=TRUE)
      #WRS is a special case because it is not on CRAN
      if (!("WRS" %in% rownames(installed.packages()))) {
        # third: install an additional package which provides some C functions
        library("devtools")
        install_github("nicebread/WRS", subdir="pkg")
      }
    }
  } 
  
  library(rmarkdown)
  if(is.null(pandoc_version())){
     if(grepl("mac",  .Platform$pkgType)){
     return(easieR.msg(msg=1))
  }else{
  
    install.packages("installr")
    library(installr)
    install.pandoc()
  }
}
  
 
  flush.console()
  vef.pack()->Resultats
  return(Resultats)
  
}
