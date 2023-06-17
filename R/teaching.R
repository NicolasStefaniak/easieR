teaching <-
  function(){
    tcl<-function(){
      clt.examp(1)
      msgBox("etes-vous pret?")
      for(i in 1:50){
        clt.examp(i*2)
        Sys.sleep(1)
      }
    }
    
    c("psych", "svDialogs", "TeachingDemos", "tkrplot")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    choix <- dlgList(c("Comprendre un intervalle de confiance", "Comprendre alpha et la puissance",
                       "Comprendre la correlation",
                       "Comprendre le theorem central limit","Comprendre une correlation 2",
                       "Comprendre la prevalence, la sensibilite et la specificite",
                       "Comprendre la prevalence, la sensibilite et la specificite 2",
                       "Comprendre le pouvoir predictif positif et le pouvoir predictif negatif",
                       "Comprendre une inference bayesienne",
                       "Comprendre le maximum de vraisemblance",
                       "Comprendre les effets de variances heterogenes"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
    if(length(choix)==0) return(easieR())
    
    switch(choix, 
           "Comprendre un intervalle de confiance"=ci.examp(), # peut etre completer par des arguments
           "Comprendre le theorem central limit"=tcl(),
           "Comprendre la prevalence, la sensibilite et la specificite"= plotFagan2(),
           "Comprendre une inference bayesienne"=plotFagan(),
           "Comprendre le maximum de vraisemblance"=mle.demo() , #des arguments peuvent etre utilises
           "Comprendre alpha et la puissance"=run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE), 
           "Comprendre la correlation" = put.points.demo(),
           "Comprendre les effets de variances heterogenes"={
             writeLines("Avec deux moyennes egales, ou pratiquement egales, le taux d'erreurs doit etre de 5%.
                        Modifiez progressivement l'ecart entre les ecart-types et voyez comment le taux d'erreur alpha va etre modifie")
             run.Pvalue.norm.sim()
           },
           "Comprendre la prevalence, la sensibilite et la specificite 2"= roc.demo(),
           "Comprendre une correlation 2"=run.cor2.examp(),
           "Comprendre le pouvoir predictif positif et le pouvoir predictif negatif"= {
             for(i in seq(1,11,2)) {
               SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i) # on peut modifier sensibilite et specificite
               if( interactive() ) {
                 readline("Press Enter to continue")  
               }
               
               
             }
           }
           
           
    )
    ref1(packages)->Resultats
    return(Resultats)
    
           }
