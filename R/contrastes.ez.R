contrastes.ez <-
function(longdata, inter=NULL, intra=NULL){
  Resultats<-list()
  writeLines("Les contrastes a priori correspondent aux contrastes sans correction de la probabilité en suivant les règles de contrastes.
             Les contrastes 2 à 2 permettent de faire toutes les comparaisons 2 à 2 en appliquant ou non une correction à la probabilité")
  type.cont<- dlgList(c("a priori",  "Comparaison 2 à 2", "aucun"), preselect="a priori",multiple = FALSE, title="Quel types de contraste voulez-vous ?")$res
  if(length(type.cont)==0) return(NULL)
  Resultats$type.cont<-type.cont
  c(inter, unlist(intra))->interintra
  if(type.cont=="a priori") {
    contrastes<-list()
    writeLines("Vous pouvez choisir les contrastes que vous souhaitez. Néanmoins les règles concernant l'application des contrastes doivent être respectées.
               Les contrastes peuvent etre specifiés manuellement. Dans ce cas, veuillez choisir spécifier les contrastes")
    cont.exemple<-list()
    contr.helmert(3)->cont.exemple$Orthogonaux
    apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inversés
    contr.poly(3)->cont.exemple$Polynomiaux
    contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
    print(cont.exemple)
    
    for (i in 1:length(interintra)){
      if(i>1) {
        type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", "spécifier les contrastes"), 
                             preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res} else {
                               type.cont2<- dlgList(c("orthogonaux", "orthogonaux inversés", "polynomiaux","comparaison à une ligne de base", 
                                                      "spécifier les contrastes"),preselect=c("orthogonaux"), multiple = FALSE, title=paste("Quels contrastes pour la variable",names(longdata[interintra])[i],"?"))$res                      
                             }
      if(length(type.cont2)==0) return(contrastes.ez()) 
      if(type.cont2=="orthogonaux") contr.helmert(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
      if(type.cont2=="orthogonaux inversés") apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrastes[[i]]
      if(type.cont2=="polynomiaux")  contr.poly(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
      if(type.cont2=="comparaison à une ligne de base") { 
        base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                       multiple = FALSE, title="Quelle est la ligne de base?")$res
        which(levels(longdata[, interintra[i]])==base)->base
        contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
      } 
      if(type.cont2=="spécifier les contrastes"){
        ortho<-FALSE
        while(ortho!=TRUE){
          matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrastes3
          dimnames(contrastes3)[[1]]<-levels(longdata[,interintra[i]])
          dimnames(contrastes3)[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
          fix(contrastes3)->contrastes3
          if(any(colSums(contrastes3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrastes3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
            test.out<-rep(1, length(contrastes3[,1]))
            for(j in 1:length(contrastes3[1,])) {contrastes3[,j]*test.out->test.out}
            if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
          if(ortho==FALSE) {dlgMessage("Les contrastes doivent respecter l orthogonalité. Voulez-vous continuer ?", "yesno")$res->cont
            if(cont=="no") return(contrastes.ez(longdata=longdata, inter=inter, intra=intra ))  }
          contrastes[[i]]<-contrastes3
          
        }
        
      }
      
      dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
    }
    names(contrastes)<-interintra
    Resultats$contrastes<-contrastes
    
  }
  if(type.cont== "Comparaison 2 à 2"){
    list()->p.adjust
    writeLines("Quelle correction de la probabilité voulez-vous appliquer ? Pour ne pas appliquer de correction, choisir +none+")
    dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title="Type de correction ?")$res->p.adjust
    if(length(p.adjust)==0) return(contrastes.ez())
    Resultats$p.adjust<-p.adjust
  }
  return(Resultats)
}
