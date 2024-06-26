ez.report<-function(html=NULL){
  options (warn=-1)
  require(svDialogs)
  if(is.null(html)){  choix<- c("html", "MS WORD")
  title<-.dico[["ask_which_output"]]
  choix<-dlgList(choix, preselect=NULL, multiple = FALSE,
          title=title)$res
  if(length(choix)==0) return(donnees())
  if(choix=="html") html<-T else html<-F
    }
  ez.html(ez.results, html= html)

}
