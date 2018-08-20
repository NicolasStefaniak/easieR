choix.reg <-
  function(){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    dlgList(c("Regressions", 
              "Effets de mediation", 
              "Regressions logistiques"), preselect="Regressions", multiple = FALSE, title="Quel type de regression ?")$res->choix
    if(length(choix)==0) return(analyse())
    if(choix=="Regressions") regressions()->Resultats
    if(choix=="Effets de mediation") ez.mediation()->Resultats
    if(choix=="Regressions logistiques") regressions.log()->Resultats
    return(Resultats)
    
  }

