ez.report<-function(html=NULL){
  options (warn=-1)
  require(svDialogs)
  if(is.null(html)){  choix<- c("html", "MS WORD")   
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
    
    title<-"Quel format souhaitez-vous ?"
  }else{
       title<-"Which output do you want?"
  }
  choix<-dlgList(choix, preselect=NULL, multiple = FALSE, 
          title=title)$res
  if(length(choix)==0) return(donnees())
  if(choix=="html") html<-T else html<-F
    }
  ez.html(ez.results, html= html)
 
}
