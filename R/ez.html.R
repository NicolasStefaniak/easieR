ez.html <-
  function(ez.results=NULL, html=T){
    # Loading packages
    packages<-c('rmarkdown', 'knitr','ggplot2','stringr','reshape2', 'readr','stringi')
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    # create the easieR directory so that resultats can be saved
    dir.create(path= paste0(tempdir(),"\\easieR") , showWarnings = FALSE)

    # create metadata for the output document

    outputb<-c("---",
               .dico[["desc_title"]],
               .dico[["desc_author"]],
               paste("date:","'", date(),"'"),
               if(any(html)) {
                 c("output:",
                   "  html_document:",
                   "    theme: default",
                   "    toc: true",
                   "    toc_float: true",
                   "    toc_depth: 5")
               } else {
                 "output: word_document"
               },
               "---"

    )
    # put general options

    a<-c("```{r globaloptions, include = FALSE, message=F, warning=F}",
         "knitr::opts_chunk$set(echo=FALSE, include=TRUE, warning=FALSE, message=FALSE)",
         "```")
  # load packages for the rmarkdown document
    im<-c("```{r, echo=F, message=F, warning=F}","options(digits = 4)",
          "library('pander')",
          "library('knitr')",
          "library('bibtex')",
          "library('tibble')",
          "library(flextable)",
          "data.results<-dget('ez.results.txt')",
          "i<-0",
          "```")
   # create a function to round p values

    round<-c("```{r, echo=F}",
             "round.ps<-function (x) { substr(as.character(ifelse(x < 0.0001, ' <.0001', ifelse(round(x, 2) == 1, ' >.99', formatC(x, digits = 4, format = 'f')))), 2, 7)}",
             "myf<-function(x){which(x<0.05)}",
             "```")
   # combine the different previous elements
    outputb<-c(outputb,a, im,round)

   # add the different results from z.results
    output2<-to.html(Resultats=ez.results)

    # create a file in the temp dir/easieR with all the results
    listes<-c(output2$listes)


    if(Sys.info()[[1]]=='Windows'){
      file.nametxt<-paste0(tempdir(), "\\easieR\\ez.results.txt")
    } else {
      dir.create(paste0(tempdir(), "/easieR/"))
      file.nametxt<-paste0(tempdir(), "/easieR/ez.results.txt")
    }
    dput(listes, file.nametxt )


    # Combine the rmarkdown metadata document with the results from easieR
    output<-c(outputb, output2$output)
    # create the Rmd document
    if(Sys.info()[[1]]=='Windows'){
      file.nameRmd<-paste0(tempdir(), "\\easieR\\Rapport.easieR.Rmd")
    } else {
      file.nameRmd<-paste0(tempdir(), "/easieR/Rapport.easieR.Rmd")
    }

    # render the rmarkdown document document
    writeLines(enc2utf8(output), file.nameRmd, useBytes = TRUE)
    render(file.nameRmd, quiet=T, run_pandoc = TRUE)
    if (Sys.info()[[1]]=='Darwin') {
      options(browser = 'open')
    } else if (Sys.info()[[1]]=='Linux') {
      options(browser = 'xdg-open')
    }
    # open the html or docx document
    if(any(html)){
      if(Sys.info()[[1]]=='Windows'){
        browseURL(file.path("file:\\", tempdir(), "easieR\\Rapport.easieR.html"))
      } else {
        browseURL(file.path("file:/", tempdir(), "easieR/Rapport.easieR.html"))
      }
    } else {
      if(Sys.info()[[1]]=='Windows') {
        browseURL(file.path("file:\\", tempdir(), "easieR\\Rapport.easieR.docx"))
      } else {
        browseURL(file.path("file:/", tempdir(), "easieR/Rapport.easieR.docx"))
      }
    }


    # remove from the easieR directory the ez plots.
if(Sys.info()[[1]]=='Windows'){
  directory<-paste0(tempdir(), "\\easieR")
} else {

  directory<-paste0(tempdir(), "/easieR")
}

dire<-dir(directory)
if(any(str_detect(dire, "ezplot"))) {
  ezplot<-str_detect(dire, "ezplot")
  ezplot<-dire[which(ezplot==TRUE)]
  ezplot<-paste0(directory,"\\",ezplot)
  file.remove(ezplot)
}
}


to.html<-function(Resultats, X=1){
  listes<-list()
  output<-c()
  for(i in 1:length(Resultats)){
    if(length(Resultats[[i]])!=0){
      names(Resultats)[[i]]->titres
      level<-paste0(rep("#", times=X+1), collapse="")

      # Convert "titres" to its correct "string" in the report
      #print("------TRANSLATION_START-------")
      #print(titres)
      if (length(titres) != 0) {
        if (exists(titres,inherits=FALSE)) {
          # Should avoid conflicts between built-in functions/variables and user-defined variables
          # https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
          output<-c(output, " ",paste(level, get(titres)) , " ")
        } else if (exists(titres,inherits=TRUE)) {
          # Check the type of the object contained inside "titres"
          # If it is a string, extract it as the title (defined in translation file)
          # else use its own identifier as the string to be displayed.
          title_type <- typeof(eval(parse(text = titres)))
          if (title_type == "character") {
            output<-c(output, " ",paste(level, get(titres)) , " ")
          } else {
            output<-c(output, " ",paste(level, titres) , " ")
          }
        } else {
          output<-c(output, " ",paste(level, titres) , " ")
        }
      }

      #print(Resultats[[i]])
      #print(class(Resultats[[i]]))
      #print("------TRANSLATION_END---------")

      # if results are character, they are probably information to report
      if(any(class(Resultats[[i]])=="chr")|any(class(Resultats[[i]])=="character")) {
        output<-c(output, Resultats[[i]])
      }
      # if resultats are bibentry create the bibliography
      if(any(class(Resultats[[i]])=="bibentry")) {
        listes[[length(listes)+1]]<-Resultats[[i]]
        essai<-c("```{r, echo=F, results='asis', message=F, warning=F}","i<-i+1",
                 "invisible(write.bib(data.results[[i]], file='references'))",
                 "bibtex::read.bib('references.bib')",
                 "invisible(file.remove('references.bib'))","```")

        output<-c(output, essai)
      }
      # if resultats is a plot, create the plot in the directory and import it in the document
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
          essai<-paste0("![](" ,basename(nom),")")
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
          essai<-paste0("![](", basename(nom),")")
          output<-c(output, essai)
        }

      }

      # if the result is a numeric the value can be added to the output
      # but if there are several values, they should be processed like a data.frame a data.frame
      if(any(class(Resultats[[i]])=="numeric") ) {
        if(length(Resultats[[i]])==1) {
          output<-c(output, Resultats[[i]])
        }else{

          round(matrix(Resultats[[i]],nrow=1),4)->essai
          essai<-data.frame(essai)

          names(essai)<-names(Resultats[[i]])
          listes[[length(listes)+1]]<-essai # we must add the data.frame to the ez.results.txt


          essai <- c(
"```{r, echo=FALSE,  message=F, warning=F, results='asis'}",
"i <- i + 1",
"tableau <- data.results[[i]]",
"tableau <- as.data.frame(tableau)",

"# Ajout des noms de lignes si présents",
"if (!is.null(dimnames(tableau)[[1]]))",
"  tableau <- data.frame(' ' = dimnames(tableau)[[1]], tableau, check.names = FALSE)",

"# Détection des colonnes de p-valeur",
paste0("col_p <- grep('", .dico[['txt_p_dot_val']], "', names(tableau))"),

"# Mise en forme des p-valeurs",
"if (length(col_p) > 0) {",
"  if (length(col_p) > 1) {",
"    is <- unique(unlist(apply(tableau[, col_p], 2, myf))) ",
"    tableau[, col_p] <- apply(tableau[, col_p], 2, round.ps)",
"  } else {",
"    is <- which(tableau[, col_p] < 0.05) ",
"    tableau[, col_p] <- round.ps(tableau[, col_p])",
"  }",
"}",


"ft <- flextable::flextable(tableau)",
"ft<-flextable::align(ft, align = 'center', part = 'all')",
"ft<-flextable::color(ft, i = is, j = NULL, color='red', part = 'body')",
"# 6. Si objet 'table' (certaines statistiques), idem",
"if (any(class(table) == 'p.value')) {",
"  for (j in seq_len(ncol(tableau))) {",

"ft <- flextable::flextable(table)",
"ft<-flextable::align(ft, align = 'center', part = 'all')",
"flextable::color(ft, i = is, j = NULL, color='red', part = 'body')","  }",
"}",
"ft",

"```"
)

          output<-c(output, essai) # add to output
        }
      }

      if(any(class(Resultats[[i]])=="matrix") | any(class(Resultats[[i]])=="table") |  any(class(Resultats[[i]])=="data.frame")|
         any(class(Resultats[[i]])=="ftable")) {

        essai<-Resultats[[i]]
        if(any(class(essai)=="ftable")) {
          if(length(attributes(essai[[1]])$row.vars)!=0 & length(attributes(essai[[1]])$col.vars)!=0) {
            essai<-dcast(as.data.frame(essai), as.formula(paste(paste(names(attr(essai, 'row.vars')), collapse='+'), '~',
                                                                paste(names(attr(essai, 'col.vars'))))))
          }else{  essai<-as.data.frame(essai)
          }
        }
        if(any(class(essai)=="p.value" ))  {
          essai<-data.frame(essai)
          class(essai)<-c("p.value", "data.frame")
        }else{
          essai<-data.frame(essai)
        }
        listes[[length(listes)+1]]<-essai

        essai<-c("```{r, echo=F, results='asis'}",
                 "i<-i+1",
                 "tableau<-data.results[[i]]",
                 "prob<-if(any(class(tableau)=='p.value')) T else F",
                 # "if(any(class(tableau )=='p.value')){ ",
                 # "tableau<-as.data.frame(tableau)",
                 # "class(tableau)<-c('data.frame', 'p.value')}else{tableau<-as.data.frame(tableau)}",
                 "if(!is.null(dimnames(tableau)[[1]])) tableau<-data.frame(' '=dimnames(tableau)[[1]], tableau, check.names=F)",
                 paste0("if(any(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))) {"),
                 paste0("col<-which(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))"),
                 "if(length(col)>1) {is<-unique(unlist(apply(tableau[,col], 2,myf )))",
                 "tableau[,col]<-apply(tableau[,col], 2, round.ps) }else{",
                 paste0("is<-which(tableau[, which(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))]<0.05)"),
                 "is<-is",
                 paste0("tableau[, which(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))]<-round.ps(tableau[,  which(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))])}}"),
                 "ft <- flextable::flextable(tableau)",
                 "ft<-flextable::align(ft, align = 'center', part = 'all')",

                 paste0("if(any(grepl('",.dico[["txt_p_dot_val"]],"', names(tableau)))) {"),
                "ft<-flextable::color(ft, i = is, j = NULL, color='red', part = 'body')",
                 "}",
                 "if(prob){",

                 "for(j in 1:ncol(tableau)){",
                 "is<-which(tableau[,j]<.05)",
                 "is<-is",
                 "ft<-flextable::color(ft, i = is, j = NULL, color='red', part = 'body')","}}",
                 "ft",
                 "```"
)

        output<-c(output, essai)
      }

      # if Resultats is a list, to.html must extract the list by using the function recurvively

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
