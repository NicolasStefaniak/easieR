fiabilite <-
  function(X=NULL,Y=NULL, data=NULL, choix=NULL, ord=NULL,outlier="Donnees completes", keys=NULL, n.boot=NULL, sauvegarde=F, info=T, imp=NULL){options (warn=-1)
    packages<-c("svDialogs", "psych", "lavaan")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    
    .e<- environment()
    Resultats<-list()
    if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
    if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c("Alpha de Cronbach","alpha","ICC","CCK","Correlation intra-classe","Coefficient de concordance de Kendall")==FALSE){
      dial<-T  
      if(info) writeLines("Veuillez choisir l'analyse que vous desirez realiser.")
      dlgList(c("Alpha de Cronbach", "Correlation intra-classe","Coefficient de concordance de Kendall"), preselect=NULL, multiple = FALSE, title="Quelle analyse voulez-vous realiser?")$res->choix
      if(length(choix)==0) return(analyse())
    }
    
    
    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(analyse())
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    if(choix=="CCK" | choix=="Coefficient de concordance de Kendall"){
      msg3<-"Veuillez choisir le premier juge"
      type<-"factor"
      title<-"Juge 1"
      multiple<-T
    } else{
      multiple<-T
      msg3<-"Veuillez choisir les variables que vous desirez analyser."
      type<-"numeric"
      title<-"variables"
    }
    
    X<-.var.type(X=X, info=info, data=data, type=type, check.prod=F, message=msg3,  multiple=multiple, title=title, out=NULL)
    if(is.null(X)) {
      Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
      return(Resultats)}
    data<-X$data
    X<-X$X
    
    if(choix %in% c("Alpha de Cronbach","Correlation intra-classe","ICC","alpha") ){
      if(dial || length(outlier)>1 || outlier %in% c("Donnees completes", "Donnees sans valeur influente") ==FALSE){
        if(info) writeLines("Desirez-vous l'analyse sur les donnees completes ou sur les donnees pour lesquelles les valeurs influentes ont ete enlevees ?")
        if(info) writeLines("les valeurs influentes sont identifiees sur la base de la distance de Mahalanobis avec un seuil du chi a 0.001")
        outlier<- dlgList(c("Donnees completes", "Donnees sans valeur influente"), preselect="Donnees completes",multiple = FALSE, title="Quels resultats voulez-vous obtenir ?")$res
        if(length(outlier)==0) { Resultats<-fiabilite()
        return(Resultats)}
      }
      
      if(outlier=="Donnees sans valeur influente"){
        inf<-VI.multiples(data[,X])
        Resultats$"Valeurs considerees comme influentes"<-inf$"Valeurs considerees comme influentes"
        data<-inf$data
      }
      
      
      if(choix %in% c("Alpha de Cronbach","alpha"))  {
        if(dial){
          if(info) writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables ordinales et Bravais-Pearson sur les variables continues")
          type<-dlgList(c("dichotomiques/ordinales", "continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res
          if(length(type)==0) {Resultats<-fiabilite()
          return(Resultats)
          }} else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"}
        
        if(dial){
          if(info) writeLines("Y a-t-il des items inverses ?") 
          rev<-dlgList(c(TRUE,FALSE), multiple = TRUE, title="items inverses?")$res
          if(length(rev)==0) {Resultats<-fiabilite()
          return(Resultats)
          }  } else rev<-FALSE
          
          if(rev=="TRUE" || !is.null(keys) && any(keys %in% X==FALSE)){
            if(info) writeLines("Veuillez preciser les items inverses")
            keys<-dlgList(X, multiple = TRUE, title="items inverses?")$res
            if(length(keys)==0) {Resultats<-fiabilite()
            return(Resultats)
            }else keys<-NULL
          }
          
          
          
          if(type=="continues"){
            if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
              msgBox("Le nombre de bootstrap doit etre un nombre entier positif") 
              n.boot<-NULL
            }
            while(is.null(n.boot)){
              writeLines("Veuillez preciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
              n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
              if(length(n.boot)==0) {Resultats<-fiabilite()
              return(Resultats)}
              strsplit(n.boot, ":")->n.boot
              tail(n.boot[[1]],n=1)->n.boot
              as.numeric(n.boot)->n.boot
              if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
                msgBox("Le nombre de bootstrap doit etre un nombre entier positif") 
                n.boot<-NULL
              }
            }
            psych::alpha(data[,X], keys=keys, n.iter=n.boot)->cron
          }else{
            n.boot<-0
            if(type=="mixte") {
              if(info) writeLines("Veuillez preciser les variables ordinales ?") 
              ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
              if(length(ord)==0){
                Resultats<-fiabilite()
                return(Resultats)
              }
            }else ord<-X
            Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp=imp,html=F)[[1]]
            if(all(class(Matrice)!="matrix")) {
              sortie<-dlgMessage("Vous essayez de faire un alpha sur autre chose qu'un matrice. Voulez-vous sortir de cette analyse?", type="yesno")$res
              if(sortie=="yes") return(analyse()) else Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]]
            }
            
            psych::alpha(Matrice, keys=keys,n.obs=length(data[,1]))->cron
          }
          
          round(cron$total,3)->Resultats$"Alpha de Cronbach sur la totalite de l'ehelle"
          if(n.boot>1) cron$boot.ci->Resultats$"Intervalle de confiance base sur le bootstrap"
          cron$total[,1]->a1
          cron$total[,6]->ase
          data.frame(Lim.inf.IC.95=a1-1.96*ase, alpha=a1, Lim.sup.IC.95=a1+1.96*ase)->Resultats$"Intervalle de confiance base sur l'erreur standard de l'alpha"
          round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Resultats$"fiabilite par item supprime"
          
      }
      
      if(choix=="Correlation intra-classe"| choix=="ICC"){ICC(data[,X], missing=FALSE)->ICC.out
        ICC.out[[1]]->Resultats$"correlation intra-classe"
        Resultats$"informations"<-paste("le nombre de juge =", length(X), "et le nombre d'observations =", ICC.out$n.obs) } 
    }
    
    
    if(choix=="Coefficient de concordance de Kendall"){  
      msg4<-"Veuilez choisir le second juge"
      Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=F, title="Juge 2", out=X)
      if(is.null(Y)) {
        Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
        return(Resultats)}
      data<-Y$data
      Y<-Y$X
      cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
      dimnames(CK.out$confid)<-list(c("Coefficient kappa non pondere","Coefficient kappa pondere"),c("lim.inf","estimation","lim.sup"))
      round(CK.out$confid,3)->Resultats$"Coefficient de concordance de Kendall"
      CK.out$agree->Resultats$"Accord"
      Resultats$information<-paste("le nombre d'observations =", CK.out$n.obs)
    }
    
    if(dial) dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez-vous sauvegarder?")$res->sauvegarde
    if(length(sauvegarde)==0) {
      Resultats<-fiabilite(data=NULL,X=NULL, sauvegarde=F, info=T, rev=NULL)
      return(Resultats)
    }
    
    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    if(!is.null(keys)) paste(ord, collapse="','", sep="")->keys
    
    Resultats$Call<-paste0("fiabilite(X=c('", X,"'),Y=", ifelse(is.null(Y), "NULL", paste0("'",Y,"'")), ",data=", nom, ",choix='", choix,"',ord=", 
                           ifelse(!is.null(ord),paste0("c('", ord, "')"), "NULL" ), ",outlier='", outlier, "', keys=", ifelse(!is.null(keys), paste0("c('",keys,"')"), "NULL"),
                           ",n.boot=", ifelse(!is.null(n.boot), n.boot, "NULL"), ", sauvegarde=", sauvegarde, ", info=T)")
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))  
    
    
    if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
    ref1(packages)->Resultats$"References"
    ez.html(Resultats)
    return(Resultats)
  }
