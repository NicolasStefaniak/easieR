teaching <-
function(){
  tcl<-function(){
    clt.examp(1)
    msgBox("êtes-vous prêt?")
    for(i in 1:50){
      clt.examp(i*2)
      Sys.sleep(1)
    }
  }
  
  c("psych", "svDialogs", "TeachingDemos")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  choix <- dlgList(c("Comprendre un intervalle de confiance", "Comprendre alpha et la puissance",
                     "Comprendre la corrélation",
                     "Comprendre le theorem central limit","Comprendre une corrélation 2",
                     "Comprendre la prévalence, la sensibilité et la spécificité",
                     "Comprendre la prévalence, la sensibilité et la spécificité 2",
                     "Comprendre le pouvoir prédictif positif et le pouvoir prédictif négatif",
                     "Comprendre une inférence bayesienne",
                     "Comprendre le maximum de vraisemblance",
                     "Comprendre les effets de variances hétérogènes"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
  if(length(choix)==0) return(easieR())
  
  switch(choix, 
         "Comprendre un intervalle de confiance"=ci.examp(), # peut être compléter par des arguments
         "Comprendre le theorem central limit"=tcl(),
         "Comprendre la prévalence, la sensibilité et la spécificité"= plotFagan2(),
         "Comprendre une inférence bayesienne"=plotFagan(),
         "Comprendre le maximum de vraisemblance"=mle.demo() , #des arguments peuvent être utilisés
         "Comprendre alpha et la puissance"=run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE), 
         "Comprendre la corrélation" = put.points.demo(),
         "Comprendre les effets de variances hétérogènes"={
           writeLines("Avec deux moyennes égales, ou pratiquement égales, le taux d'erreurs doit être de 5%.
                      Modifiez progressivement l'écart entre les écart-types et voyez comment le taux d'erreur alpha va être modifié")
           run.Pvalue.norm.sim()
         },
         "Comprendre la prévalence, la sensibilité et la spécificité 2"= roc.demo(),
         "Comprendre une corrélation 2"=run.cor2.examp(),
         "Comprendre le pouvoir prédictif positif et le pouvoir prédictif négatif"= {
           for(i in seq(1,11,2)) {
             SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i) # on peut modifier sensibilité et spécificité
             if( interactive() ) {
               readline("Press Enter to continue")  
             }
             
             
           }
         }
         
         
  )
  ref1(packages)->Resultats
  return(Resultats)
  
         }
