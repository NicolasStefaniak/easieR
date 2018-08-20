view.results <-
function(){
  c("svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  list()->Resultats
  Resultats$Call<-"view.results()"
  ref1(packages)->Resultats$"Packages utilises pour cette fonction"
  if(!exists("ez.results")) return("Aucune analyse sauvegardee n'a pu etre trouvee") else get("ez.results")
  TkListView(ez.results)
  return(Resultats)
}
