choix.reg <-
function(){
  try(library(svDialogs), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  dlgList(c("Régressions", 
            "Effets de mediation", 
            "Régressions logistiques"), preselect="Régressions", multiple = FALSE, title="Quel type de régression ?")$res->choix
  if(length(choix)==0) return(analyse())
  if(choix=="Régressions") regressions()->Resultats
  if(choix=="Effets de mediation") ez.mediation()->Resultats
  if(choix=="Régressions logistiques") regressions.log()->Resultats
  return(Resultats)
  
}
