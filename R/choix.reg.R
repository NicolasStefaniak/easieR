choix.reg <-
  function(html=T){
    try(library(svDialogs), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    dlgList(c("Regressions", 
              "Effets de mediation", 
              "Regressions logistiques"), preselect="Regressions", multiple = FALSE, title="Quel type de regression ?")$res->choix
    if(length(choix)==0) return(analyse())
    if(choix=="Regressions") regressions(html=html)->Resultats
    if(choix=="Effets de mediation") ez.mediation(html=html)->Resultats
    if(choix=="Regressions logistiques") regressions.log(html=html)->Resultats
    return(Resultats)
    
  }

