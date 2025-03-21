ez.install <-
  function(){


    # # 2. installer les packages necessaires et MAJ des packages installes
    # # 2a. packages a installer, par ordre alphabetique
    pack.to.inst <- c('afex',
                      'akima',
                      'Amelia',
                      'BayesFactor',
                      'bibtex',
                      'car',
                      'cobs',
                      'corpcor',
                      'deldir',
                      'DescTools',
                      'doBy',
                      'dplyr',
                      'emmeans',
                      'epitools',
                      'foreign',
                       'ggcorrplot',
                      'ggmosaic',
                      'ggplot2',
                      "ggplotgui",
                      'gmodels',
                      'GPArotation',
                      'gsl',
                      'huxtable',
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
                      "PMCMRplus",
                      'plyr',
                      'ppcor',
                      'psych',
                      'pwr',
                      'quantreg',
                      'Rcpp',
                      'readxl',
                      'reshape2',
                      'Rfit',
                      'rmarkdown',
                      'rms',
                      'robustbase',
                      'rpivotTable',
                      'rrcov',
                      'scatterplot3d',
                      'semPlot',
                      "sjstats",
                      'sos',
                      'sp',
                      'stringi',
                      'stringr',
                      'svDialogs',
                      'TeachingDemos',
                      'WRS2'
    )

    # 2b. packages manquants
    pack.uninst <- pack.to.inst[!(pack.to.inst %in% rownames(installed.packages()))]

    # 2c. installer packages manquants si necessaires et si utilisateur le souhaite
    if(length(pack.uninst)>0){
        writeLines(.dico[["txt_packages_install"]])
        print(pack.uninst)
        flush.console()
        install.packages(pack.uninst, quiet=TRUE)
        
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
    Resultats<- list()
    Resultats[[.dico[["desc_install_correct_packages"]]]]<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==TRUE) ]
    Resultats[[.dico[["desc_install_bad_packages"]]]]<-pack.to.inst[ which(lapply(pack.to.inst, require, character.only=T)==FALSE) ]
    return(Resultats)
  }




