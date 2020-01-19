ez.html <-
  function(ez.results=NULL, html=T){
    
    packages<-c("rmarkdown", "knitr","ggplot2","stringr","reshape2", "readr","stringi") 
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
      require(packages)}
    dir.create(path= paste0(tempdir(),"\\easieR") , showWarnings = FALSE)
    
outputb<-c("---","title: 'Resultats de vos analyses'",
               "author: 'Genere automatiquement par easieR'",
               paste("date:","'", date(),"'"),
               if(html) {c("output:",
               "  html_document:",
               "    toc: true",
               "    toc_float: true",
               "    toc_depth: 5")}else{
			         "output: word_document"},
               "---")
    a<-c("```{r global options, include = FALSE}",
         "knitr::opts_chunk$set(echo=FALSE, include=TRUE, warning=FALSE, message=FALSE)",
         "```")
    
    im<-c("```{r, echo=F}","options(digits = 4)", "library('pander')","library('knitr')",
          "library('bibtex')", "library('flextable')","library('tibble')", "data.results<-dget('ez.results.txt')", "i<-0","```")
    round<-c("```{r, echo=F}",
             "round.ps<-function (x) { substr(as.character(ifelse(x < 0.0001, ' <.0001', ifelse(round(x, 2) == 1, ' >.99', formatC(x, digits = 4, format = 'f')))), 2, 7)}",
             "myf<-function(x){which(x<0.05)}"
                  ,"```")

    outputb<-c(outputb,a, im,round)
    
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
          
          if(any(class(Resultats[[i]])=="bibentry")) {
            listes[[length(listes)+1]]<-Resultats[[i]]
            essai<-c("```{r, echo=F, results='asis'}","i<-i+1",
                     "invisible(write.bib(data.results[[i]], file='references'))",
                     "bibtex::read.bib('references.bib')",
                     "invisible(file.remove('references.bib'))","```")
            
            output<-c(output, essai)
          }        
          if(any(class(Resultats[[i]])%in%c("ggplot","arrangelist"))) {
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
          
          if(any(class(Resultats[[i]])=="matrix") | any(class(Resultats[[i]])=="table") |  any(class(Resultats[[i]])=="data.frame")|  any(class(Resultats[[i]])=="ftable")) {
            
            essai<-Resultats[[i]]
            if(any(class(essai)=="ftable")) {
		    if(length(attributes(essai[[1]])$row.vars)!=0 & length(attributes(essai[[1]])$col.vars)!=0) {
              essai<-dcast(as.data.frame(essai), as.formula(paste(paste(names(attr(essai, 'row.vars')), collapse='+'), '~', paste(names(attr(essai, 'col.vars'))))))
			    }else{  essai<-as.data.frame(essai)
      }
            }
            
            listes[[length(listes)+1]]<-essai
            
            essai<-c("```{r, echo=F, results='asis'}", 
                     "i<-i+1",
                     "table<-data.results[[i]]",
                     "tableau<-table",
                    # "tableau<-as.data.frame.matrix(tableau)",
                     "if(has_rownames(tableau) & any(rownames(tableau)!=' ')) tableau<-rownames_to_column(tableau, var = ' ')", 
                     "if(any(grepl('valeur.p', names(tableau)))) {", 
                     "col<-which(grepl('valeur.p', names(tableau)))",
                     "if(length(col)>1) {is<-unique(unlist(apply(tableau[,col], 2,myf )))",
                     "tableau[,col]<-apply(tableau[,col], 2, round.ps) }else{",
                     "is<-which(tableau[, which(grepl('valeur.p', names(tableau)))]<0.05)",
                     "tableau[, which(grepl('valeur.p', names(tableau)))]<-round.ps(tableau[, which(grepl('valeur.p', names(tableau)))])}}",
                     "ft<-flextable(tableau)", 
                     "if(!is.null(names(dimnames(table)))){ft<-add_header_row(ft, top=T, values=c(names(dimnames(table)), rep(' ',times= length(tableau)-2)))}",
                     "ft<-theme_booktabs(ft)",
                     "ft<-fontsize(ft, size=14, part='all')",
                     "if(any(grepl('valeur.p', names(tableau)))) {",
                         "ft <- color( ft, i = is, j = 1:ncol(tableau), color = 'red' )",
                         "}", 
                     "if(any(class(table)=='p.value')){",
                     "for(j in 1:ncol(tableau)){ft <- color(ft, i = which(tableau[,j]<0.05) , j = j, color='red')}}",
                     "ft","```")
            output<-c(output, essai)
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
                       "tableau<-data.frame(tableau)", 
                       "if(has_rownames(tableau) & rownames(tableau)!=' ') tableau<-rownames_to_column(tableau,  var = ' ')",
                       "ft <- flextable(tableau)",
                       "ft<-theme_booktabs(ft)", "ft<-fontsize(ft, size=14, part='all')",
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

    writeLines(enc2utf8(output), file.nameRmd, useBytes = TRUE)
    render(file.nameRmd, quiet=T, encoding="UTF-8")
    if(html){	  
    if(Sys.info()[[1]]=="Windows"){
      browseURL(file.path("file:\\", tempdir(), "easieR\\Rapport.easieR.html"))
    } else {
      browseURL(file.path("file:/", tempdir(), "easieR/Rapport.easieR.html"))
    }
    }else { 
    if(Sys.info()[[1]]=="Windows"){
      browseURL(file.path("file:\\", tempdir(), "easieR\\Rapport.easieR.docx"))
    } else {
      browseURL(file.path("file:/", tempdir(), "easieR/Rapport.easieR.docx"))
    }
    }
    
    
    
    dire<-dir()
    if(any(str_detect(dire, "ezplot"))) {
      ezplot<-str_detect(dire, "ezplot")
      ezplot<-dire[which(ezplot==TRUE)]
      file.remove(ezplot)
    }
  }
