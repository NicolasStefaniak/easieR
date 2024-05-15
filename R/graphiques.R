graphiques <-
function(){

  c('ggplotgui', 'svDialogs')->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== 'try-error') return(ez.install())
  data<-choix.data(nom=TRUE)
  if(length(data)==0) return(easieR())
  nom<-data[[1]]
  data<-data[[2]]

  msgBox(.dico[["desc_close_browser_to_come_back"]])
   print(ref1(packages))
  if (Sys.info()[[1]]=='Darwin') {
    options(browser = 'open')
  } else if (Sys.info()[[1]]=='Linux') {
    options(browser = 'xdg-open')
  }
  ggplot_shiny(dataset=data)

}
