graphiques <-
function(){
  
  c("ggplotgui", "svDialogs")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  data<-choix.data(nom=TRUE)
  if(length(data)==0) return(easieR())
  nom<-data[[1]]
  data<-data[[2]]
  
  msgBox("Ne pas oublier de fermer la fenetre htmlt (firexfox, chrome, internet explorer...) pour revenir à la session R")
   print(ref1(packages))
  ggplot_shiny(dataset=data)
 
}
