ez.html <-
  function(ez.results=NULL){
    
    packages<-c("rmarkdown", "knitr","ggplot2","stringr" )
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    dir.create(path= paste0(tempdir(),"\\easieR") , showWarnings = FALSE)
    
    outputb<-c("---","title: 'Resultats de vos analyses'",
               "author: 'Genere automatiquement par easieR'",
               paste("date:","'", date(),"'"),
               "output:",
               "  html_document:",
               "    toc: true",
               "    toc_float: true",
               "    toc_depth: 5",
               "---")
    
    im<-c("```{r, echo=F}","options(digits = 4)", "library('pander')","library('knitr')",
          "library('bibtex')", "library('flextable')","library('tibble')", "data.results<-dget('ez.results.txt')", "i<-0","```")
    outputb<-c(outputb, im)
    
    to.html<-function(Resultats, X=1){
      listes<-list()
      output<-c()
      for(i in 1:length(Resultats)){
        
        if(length(Resultats[[i]])!=0){
          names(Resultats)[[i]]->titres
          level<-paste0(rep("#", times=X+1), collapse="")
          output<-c(output, " ",paste(level, titres) , " ") 
          
          if(any(class(Resultats[[i]])=="chr")|any(class(Resultats[[i]])=="character")) {
            output<-c(output, Resultats[[i]])
          }
          if(any(class(Resultats[[i]])=="matrix") && any(class(Resultats[[i]])=="table")) class(Resultats[[i]])<-"matrix"
          if(any(class(Resultats[[i]])=="matrix")) {
            data.frame(Resultats[[i]]) ->essai
            listes[[length(listes)+1]]<-essai
            essai<-c("```{r, echo=F, results='asis'}", 
                     "i<-i+1", "tableau<-data.results[[i]]",
                     "tableau<-data.frame(tableau)", "if(has_rownames(tableau)) tableau<-rownames_to_column(tableau) ",
                     "ft <- flextable(tableau)",
                     "ft<-theme_booktabs(ft)", "ft<-fontsize(ft, size=14)",
                     "if(any(grepl('valeur.p', names(tableau)))) ft <- color( ft, i = which(any(tableau[, which(grepl('valeur.p', names(tableau)))]<0.05)), j = 1:ncol(tableau), color = 'red' )", 
                     "ft","```")
            output<-c(output, essai)
          }
          if(any(class(Resultats[[i]])=="ftable")) {
            Resultats[[i]] ->essai
            listes[[length(listes)+1]]<-essai
            essai<-c("```{r, echo=F, results='asis'}", "i<-i+1", "tableau<-data.results[[i]]", 
                     "pandoc.table(tableau, style='simple',split.tables=150)","```")
            output<-c(output, essai)
          }
          if(any(class(Resultats[[i]])=="data.frame") && length(Resultats[[i]])!=0) {
            essai<-data.frame(Resultats[[i]])
            
            listes[[length(listes)+1]]<-essai
            essai<-c("```{r, echo=F, results='asis'}", "i<-i+1", "tableau<-data.results[[i]]",
                     "tableau<-data.frame(tableau)", "if(has_rownames(tableau)) tableau<-rownames_to_column(tableau)",
                     "ft <- flextable(tableau)",
                     "ft<-theme_booktabs(ft)", "ft<-fontsize(ft, size=14)",
                     "if(any(grepl('valeur.p', names(tableau)))) ft <- color( ft, i = which(any(tableau[, which(grepl('valeur.p', names(tableau)))]<0.05)), j = 1:ncol(tableau), color = 'red' )", 
                     "ft","```")
            output<-c(output, essai)
          }
          if(any(class(Resultats[[i]])=="bibentry")) {
            listes[[length(listes)+1]]<-Resultats[[i]]
            essai<-c("```{r, echo=F, results='asis'}","i<-i+1",
                     "invisible(write.bib(data.results[[i]], file='references'))",
                     "bibtex::read.bib('references.bib')",
                     "invisible(file.remove('references.bib'))","```")
            
            output<-c(output, essai)
          }
          
          if(any(class(Resultats[[i]])=="table")) {
            essai<-as.data.frame.matrix(Resultats[[i]])
            listes[[length(listes)+1]]<-essai
            
            essai<-c("```{r, echo=F}", "i<-i+1", "tableau<-data.results[[i]]", 
                     " kable(data.frame(tableau))","```")
            output<-c(output, essai)
          }
          
          
          if(any(class(Resultats[[i]])=="ggplot")) {
            essai<-Resultats[[i]]
            if(Sys.info()[[1]]=="Windows"){
              dire<-dir(paste0(tempdir(), "\\easieR\\"))
              if(any(str_detect(dire, "ezplot"))) {
                ezplot<-str_detect(dire, "ezplot")
                n<-length(which(ezplot==TRUE))
                nom<-paste0(tempdir(), "\\easieR\\ezplot", n+1, ".png")
              }else{nom<-paste0(tempdir(), "\\easieR\\ezplot1.png")}
              ggsave(filename=nom, plot=essai)
              essai<-paste0("<img src='", nom, "'alt='Drawing' style='width: 700px;'/>")
              output<-c(output, essai)
            }else{
              dir.create(paste0(tempdir(), "/easieR/"))
              dire<-dir(paste0(tempdir(), "/easieR/"))
              if(any(str_detect(dire, "ezplot"))) {
                ezplot<-str_detect(dire, "ezplot")
                n<-length(which(ezplot==TRUE))
                nom<-paste0(tempdir(), "/easieR/ezplot", n+1, ".png")
              }else{nom<-paste0(tempdir(), "/easieR/ezplot1.png")}
              ggsave(filename=nom, plot=essai)
              essai<-paste0("<img src='", nom, "'alt='Drawing' style='width: 700px;'/>")
              output<-c(output, essai)
            }
            
          }
          
          
          
          if(any(class(Resultats[[i]])=="numeric") ) {
            if(length(Resultats[[i]])==1) {
              output<-c(output, Resultats[[i]])
            }else{
              
              round(matrix(Resultats[[i]],nrow=1),4)->essai
              essai<-data.frame(essai)
              names(essai)<-names(Resultats[[i]])
              listes[[length(listes)+1]]<-essai
              
              essai<-c("```{r, echo=F, results='asis'}", "i<-i+1", "tableau<-data.results[[i]]",
                       "tableau<-data.frame(tableau)", "if(has_rownames(tableau)) tableau<-rownames_to_column(tableau)",
                       "ft <- flextable(tableau)",
                       "ft<-theme_booktabs(ft)", "ft<-fontsize(ft, size=14)",
                       "if(any(grepl('valeur.p', names(tableau)))) ft <- color( ft, i = which(any(tableau[, which(grepl('valeur.p', names(tableau)))]<0.05)), j = 1:ncol(tableau), color = 'red' )", 
                       "ft","```")
              output<-c(output, essai)
            }
          }
          
          if(any(class(Resultats[[i]])=="list") ){
            Resultats[[i]]->Y
            output2<-to.html(Y, X=X+1)
            listes<-c(listes, output2$listes)
            output<-c(output, output2$output)
          }
        }
      }
      tot<-list()
      tot$output<-output
      tot$listes<-listes
      return(tot)
    }
    output2<-to.html(Resultats=ez.results)
    listes<-c(output2$listes)
    
    wd<-getwd() 
    if(grepl("[^[:alnum:]]", wd)) {
      
    }
    
    if(Sys.info()[[1]]=="Windows"){
      file.nametxt<-paste0(tempdir(), "\\easieR\\ez.results.txt")
    } else {
      dir.create(paste0(tempdir(), "/easieR/"))
      file.nametxt<-paste0(tempdir(), "/easieR/ez.results.txt")
    }
    dput(listes, file.nametxt )
    
    
    
    output<-c(outputb, output2$output)
    if(Sys.info()[[1]]=="Windows"){
      file.nameRmd<-paste0(tempdir(), "\\easieR\\Rapport.easieR.Rmd")
    } else {
      file.nameRmd<-paste0(tempdir(), "/easieR/Rapport.easieR.Rmd")
    }
    writeLines(output, file.nameRmd)
    #writeLines(output, file.nameRmd)
    render(file.nameRmd)
    #    render("Rapport.easieR.Rmd" )
    if(Sys.info()[[1]]=="Windows"){
      browseURL(file.path("file:\\", tempdir(), "easieR\\Rapport.easieR.html"))
    } else {
      browseURL(file.path("file:/", tempdir(), "easieR/Rapport.easieR.html"))
    }
    
    
    
    dire<-dir()
    if(any(str_detect(dire, "ezplot"))) {
      ezplot<-str_detect(dire, "ezplot")
      ezplot<-dire[which(ezplot==TRUE)]
      file.remove(ezplot)
    }
  }
