ez.cfa <-
function(modele=NULL, X=NULL, data=NULL,ord=NULL, outlier="Données complètes",imp="rm", output="default", info=T, sauvegarde=F, mimic=NULL, fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                 estimator="ML",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                 link="probit",int.ov.free=FALSE, int.lv.free=FALSE, std.lv=FALSE, n.boot=1000, group.w.free=F,
                 group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", "lv.variances" , "lv.covariances")){
  # modele : lavaan modele if X is null
  # data : dataframe
  # X : character. names of the variables if modele is null
  # LV : Vector. names of LV=atent Variables
  # ord: Character. Vector of ordered variables among X
  # outlier : should outliers be detected and removed on Mahalanobis distance ? ("Données sans valeur influente") or not ("Données complètes")
  # imp : How must missing data be dealt :"rm"= remove, "mean" = impute mean, "median"=impute median, "amelia"=use amelia algorithm for imputation. 
  # output : character vector. List of output that has to be shown. 
  # info : logical. Should information be printed in the console ? 
  # sauvegarde : logical. Must the output be saved in external file ? 
  # mimic : forced argument to determine whether to use or not dialog boxes in specifying options
  # for other options, see lavOptions
  
  options (warn=-1)
  Lav.modele<-function(X=NULL, modele=NULL, LV=NULL, info=T){
    # X : character. Names pf tje manifest variables
    # LV : character. Vector of latent variable names. 
    # modele : lavaan modele
    if(!is.null(modele)){
      semPlot.modele<-try(semPlotModel_lavaanModel(modele))
      if(class(semPlot.modele)=="try-error"){
        msgBox("Le modèle semble incorrect et n'a pas pu être créé.")
        return(NULL)
      }
      semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Est-ce que votre modèle est correct ?", "yesno")$res->suppression
      if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
      return(modele)
    }
    
    if(is.null(LV) && length(X)>3) {
      if(info)   writeLines("Veuillez préciser le nombre de variables latentes")
      nF<-NA
      while(!is.numeric(nF)) {
        if(info) writeLines("Veuillez préciser le nombre de variables latentes") 
        nF <- dlgInput("Nombre de facteurs ?", 2)$res
        if(length(nF)==0) return(NULL)
        strsplit(nF, ":")->nF
        tail(nF[[1]],n=1)->nF
        as.numeric(nF)->nF
        if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>length(X) ){
          msgBox("Le nombre de facteur doit être un entier positif inférieur au nombre de variables")
          nF<-NA }
      }} else if(!is.null(LV)) nF<-length(LV) else nF<-1
      
      O2<-c()
      X->reste
      list()->modele2
      for(i in 1:nF){
        if(is.null(LV[i]))  {dlgInput(paste("Nom de la variable latente",i,  "?"), paste("Facteur",i, sep="."))$res->noms
          if(length(noms)==0) return(Lav.modele(X=X, LV=NULL))
          strsplit(noms, ":")->noms
          tail(noms[[1]],n=1)->noms} else noms<-LV[i]
          title<-paste("Variables manifestes de", noms)
          if(i==nF) O1<-reste else O1<- dlgList(reste, preselect=NULL, multiple = TRUE, title=title)$res
          O2<-c(O2,O1)	
          setdiff(reste,O2)->reste
          paste(noms, "=~", O1[1])->modele
          for(j in 2 :(length(O1))){paste(modele, "+", O1[j])->modele}
          modele2[[i]]<-modele
          modele2[[1]]->modele
          if(i>1) {
            for(j in 2 : i){paste(modele,"\n", modele2[[j]])->modele   }
          }
          semPlot.modele<-semPlotModel_lavaanModel(modele)
          semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
      }
      
      cat ("Appuyez [entree] pour continuer")
      line <- readline()
      dlgMessage("Est-ce que votre modèle est correct ?", "yesno")$res->suppression
      if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T)) 
      return(modele)
  }
  .ez.lavaan.options<-function(modele=NULL, data=NULL, X=NULL, info=TRUE, opt.list=NULL, dial=T, imp=NULL, outlier=NULL,output=NULL){
    if(dial || is.null(opt.list$mimic) || !opt.list$mimic%in% c("default", "Mplus", "EQS")){dial<-T
    if(info) writeLines("Voulez-vous spécifier tous les paramètres [default] ou imiter un logiciel particulier ?")
    opt.list$mimic<-dlgList(c("default", "Mplus", "EQS"), preselect="default", multiple = FALSE, title="Imiter ?")$res
    if(length(opt.list$mimic)==0) return(NULL)
    }
    
    if(dial){ 
      if(opt.list$mimic=="default"){ 
        options2<-c("Variables exogènes fixées [fixed.x=default]", "information [information=default]", "correction de continuité [zero.keep.margins=default]",
                    "Vraisemblance (seulement pour estimator=ML) [likelihood=default]") 
      } else options2<-c()
      options<-c("estimateur [estimator=ml])", "groupes [group=NULL]", "test [test=standard]", "erreur standard [se=standard]", "standardisation des variables observées [std.ov=T]", 
                 "Orthogonalité des facteurs [orthogonal=FALSE]", "Lien (seulement pour estimator=MML) [link=probit]",
                 "Intercept des variables observées [int.ov.free=FALSE]", "Intercept des variables latentes [int.lv.free=FALSE]", "Variables exogènes fixées [fixed.x=default]",
                 "Estimation des indicateurs des variables latentes [std.lv=FALSE]", options2)
      
      if(info) writeLines("Quelles options voulez-vous spécifier ?")
      options<-dlgList(c("Garder les valeurs par défaut", options), preselect=c("estimateur [estimator=ml])","test [test=standard]", "erreur standard [se=standard]"), multiple = TRUE, title="Quelles options ?")$res
      if(length(options)==0) return(NULL)
      if(options=="Garder les valeurs par défaut") return(list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                                                               estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                                                               link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                                                               group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                                                             "lv.variances" , "lv.covariances")))
    } else options<-NULL
    
    
    
    if(any(options=="estimateur [estimator=ml])")|is.null(opt.list$estimator) || length(opt.list$estimator)!=1|| 
       try(opt.list$estimator %in%c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ),silent=T)!=T){
      if(info){  writeLines("[WLS] correspond à [ADF]. Les estimateurs avec les extensions [M],[MV],[MVSF],[R] 
                            sont des versions robustes des estimateurs classiques [MV],[WLS], [DWLS], [ULS]")
        abb<-data.frame(abb=c("ML","GLS", "WLS", "ULS", "DWLS"), nom=c("maximum de vraisemblance","moindre carré généralisés","moindre carré pondéré","moindre carré non pondéré","moindre carré  pondéré diagonalement"))
        print(abb)    }
      opt.list$estimator<-dlgList(c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ), multiple = FALSE, title="Quelles estimateur ?")$res
      if(length(opt.list$estimator)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    if(any(options=="test [test=standard]") || length(opt.list$test)!=1 || !opt.list$test%in% c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted",
                                                                                                "scaled.shifted", "bootstrap","Bollen.Stine")){
      if(info) writeLines("Quel test voulez-vous utiliser ?")
      opt.list$test<-dlgList(c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted","scaled.shifted", "bootstrap","Bollen.Stine"), multiple = FALSE, title="Quelles estimateur ?")$res
      if(length(opt.list$test)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine") &&!is.null(opt.list$n.boot) && ((class(opt.list$n.boot)!="numeric" & class(opt.list$n.boot)!="integer") ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1)){
      msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
      opt.list$n.boot<-NULL
    }
    if(dial & opt.list$test%in%c("boot","bootstrap","Bollen.Stine") || is.null(opt.list$n.boot) & opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) {
      while(is.null(opt.list$n.boot)){
        writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
        n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
        if(length(n.boot)==0) {Resultats<-Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
        strsplit(n.boot, ":")->n.boot
        tail(n.boot[[1]],n=1)->n.boot
        as.numeric(n.boot)->opt.list$n.boot
        if(is.na(opt.list$n.boot) ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1){
          msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
          opt.list$n.boot<-NULL
        }
      }
    } 
    
    if( any(is.na(data[,X])) & opt.list$estimator=="ml" & opt.list$mimic=="default") opt.list$missing<-"fiml" else opt.list$missing<-"default"
    
    if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) se1<-c("standard","first.order", "robust", "bootstrap","none" ) else se1<-c("standard","first.order", "robust", "none" )
    if(any(options=="erreur standard [se]") || is.null(opt.list$se) || !opt.list$se%in%se1)  {
      if(info) writeLines("Comment l'erreur standard doit-elle être estimée ?")
      opt.list$se<-dlgList(se1, multiple = FALSE, title="Erreur standard ?")$res
      if(length(opt.list$se)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="groupes [group=NULL]") || !is.null(opt.list$group)){
      msg2<-"Veuillez choisir la définissant les groupes"
      .var.type(X=opt.list$group, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable [groupes] ?", out=X)->group
      if(is.null(group)){
        Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      } 
      group$data->data
      group$X->opt.list$group
      if(dial|| any(opt.list$group.equal %in% c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))==FALSE){
        if(info) writeLines("Quels sont les paramètres que vous désirez maintenir constants ?")
        opt.list$group.equal<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), multiple = T, 
                                      preselect=c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"), title="Paramètres constants ?")$res
        if(length(opt.list$group.equal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
        }}
      # écrase group equal puisque àa libère les group sur cette contraintes ==> utilité ? 
      #group.partial<-dlgList(c("loadings", "intercepts","means","thresholds","regressions","residuals","residual.covariances","lv.variances", "lv.covariances"))
      if(info) writeLines("est-ce que les fréquences des différents group est un paramètre libre ? ") 
      opt.list$group.w.free<-dlgList(c(TRUE, FALSE), multiple=F, preselect=FALSE, title="Constance de la fréquence ?")$res
      if(length(opt.list$group.w.free)==0) {Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      }
    }
    ### zero.keep.margins
    if(any(options=="correction de continuité [zero.keep.margins]") || is.null(opt.list$zero.keep.margins)||(!is.logical(opt.list$zero.keep.margins) & opt.list$zero.keep.margins!="default")){
      if(info) writeLines("Faut-il ajouter une valeur aux cellules vides pour les corrélations polychorique ? Pour spécifier les valeurs,choisissez TRUE, sinon choisissez [default]")
      opt.list$zero.keep.margins<-dlgList(c(TRUE, FALSE,"default"), preselect="default", multiple = FALSE, title="Cellules vides ?")$res
      if(length(opt.list$zero.keep.margins)==0) {
        Resultats<-Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)
      }
    } 
    
    if( opt.list$zero.keep.margins==TRUE){
      if(!is.null(opt.list$zero.add) && ((class(opt.list$zero.add)!="numeric" ) || any( opt.list$zero.add<0) || any(opt.list$zero.add>1))){
        msgBox("La correction pour le calcul de corrélations polycoriques doit être comprise entre 0 et 1.") 
        opt.list$zero.add<-NULL
      }
      while(is.null(opt.list$zero.add)){
        writeLines("Veuillez préciser la valeur pour les tableaux 2x2")
        zero.add1<-dlgInput("tableau 2x2 ?", 0.5)$res
        if(length(zero.add1)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
        strsplit(zero.add1, ":")->zero.add1
        tail(zero.add1[[1]],n=1)->zero.add1
        as.numeric(zero.add1)->zero.add1
        if(is.na(zero.add1) ||  zero.add1<0 || zero.add1>1){
          msgBox("La valeur doit être comprise entre 0 et 1") 
          opt.list$zero.add<-NA} else{
            writeLines("Veuillez préciser la valeur pour les tableaux plus grand que 2x2")
            zero.add2<-dlgInput("tableau > 2x2 ?", 0)$res
            if(length(zero.add2)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
            return(Resultats)}
            strsplit(zero.add2, ":")->zero.add2
            tail(zero.add2[[1]],n=1)->zero.add2
            as.numeric(zero.add2)->zero.add2
            if(is.na(zero.add2) ||  zero.add2<0 || zero.add2>1){
              msgBox("La valeur doit être comprise entre 0 et 1") 
              opt.list$zero.add<-NA}
          }
        opt.list$zero.add<-c(zero.add1,zero.add2)
        
      }
    } 
    
    
    ### fin zero.keep.margins
    if(any(options=="Vraisemblance (seulement pour estimator=ML) [likelihood=default]") & opt.list$mimic=="default" & opt.list$estimator=="ML" ||is.null(opt.list$likelihood) || length(opt.list$likelihood)!=1 || try(opt.list$likelihood%in%c("wishart","normal", "default" ),silent=T)!=T) {
      if(info) writeLines("Veuillez préciser la vraisemblance.")
      opt.list$likelihood<-dlgList(c("wishart","normal", "default" ), multiple=F, preselect="default", title="Vraisemblance ?")$res # dépend de mimic
      if(length(opt.list$likelihood)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="Lien (seulement pour estimator=MML) [link=probit]") & opt.list$estimator=="MML" ||length(opt.list$link)!=1 || try(opt.list$link%in%c("logit","probit" ),silent=T)!=T ){
      if(info) writeLines("Veuillez préciser la famille (i.e. forme de la distribution).")
      opt.list$link<-dlgList(c("logit","probit" ), multiple=F, preselect=FALSE, title="Distribution ?")$res
      if(length(opt.list$link)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    
    if(any(options=="information [information=default]") ||is.null(opt.list$information) || try(opt.list$information%in%c("expected","observed", "default" ),silent=T)!=T ){
      if(info) writeLines("Sur quelle matrice d'information doit se réaliser l'estimation des erreurs standards ?")
      opt.list$information<-dlgList(c("expected","observed", "default" ), multiple=F, preselect=FALSE, title="Matrice d'information ?")$res
      if(length(opt.list$information)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    if(any(options=="Variables exogènes fixées [fixed.x=default]") ||length(opt.list$fixed.x)!=1 || (!is.logical(opt.list$fixed.x) & opt.list$fixed.x!="default") ){
      if(info) writeLines("Si vrai, on considère les covariés exogènes comme fixés, sinon on les considère comme aléatoires et leurs paramètres sont libres")
      opt.list$fixed.x<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Covariables fixées ?")$res
      if(length(opt.list$fixed.x)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }  
    
    if(any(options=="Orthogonalité des facteurs [orthogonal=FALSE]") ||length(opt.list$orthogonal)!=1 || !is.logical(opt.list$orthogonal) ){
      if(info) writeLines("Est-ce que les facteurs sont corrélés (FALSE) ou sont-ils orthogonaux (TRUE)?")
      opt.list$orthogonal<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Orthogonalité des facteurs ?")$res
      if(length(opt.list$orthogonal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    if(any(options=="standardisation des variables observées [std.ov=T]") ||length(opt.list$std.ov)!=1 || !is.logical(opt.list$std.ov) ){
      if(info) writeLines("Faut-il standardisé (i.e. centrer réduire) les variables observées au prélable (TRUE) ou non (FALSE) ?")
      opt.list$std.ov<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Standardisation ?")$res
      if(length(opt.list$std.ov)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    #####
    if(any(options=="Intercept des variables observées [int.ov.free=FALSE]") ||length(opt.list$int.ov.free)!=1 || !is.logical(opt.list$int.ov.free) ){
      if(info) writeLines("Faut-il fixer l'intercept des variables observées à 0 ?")
      opt.list$int.ov.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VO=0 ?")$res
      if(length(opt.list$int.ov.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }
    
    
    if(any(options=="Intercept des variables latentes [int.lv.free=FALSE]") ||length(opt.list$int.lv.free)!=1 || !is.logical(opt.list$int.lv.free) ){
      if(info) writeLines("Est-ce que l'intercept des variables latentes doit être fixé à 0 ?")
      opt.list$int.lv.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Intercept VL=0 ?")$res
      if(length(opt.list$int.lv.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    }
    
    
    
    if(any(options=="Estimation des indicateurs des variables latentes [std.lv=FALSE]") ||length(opt.list$std.lv)!=1 || !is.logical(opt.list$std.lv) ){
      if(info) writeLines("Si vrai, les résidus des variables latentes sont fixés à 1, sinon les paramètres de la variable latente sont estimés en fixant le premier indicateur à 1")
      opt.list$std.lv<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title="Standardisation VL?")$res
      if(length(opt.list$std.lv)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
      return(Resultats)}
    } 
    
    
    return(opt.list)
  }
  
  
  
  cfa.in<-function(modele=NULL,X=NULL,LV=NULL, data=NULL, ord=NULL, outlier=NULL,imp=NULL,output=NULL, info=T, opt.list=list(), sauvegarde=F){
    
    Resultats<-list()
    if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL) 
      nom<-data[[1]]
      data<-data[[2]]  
    }else{
      deparse(substitute(data))->nom  
    }
    
    
    if(is.null(modele)){ 
      msg3<-"Veuillez choisir les variables manifestes que vous désirez analyser. Vous devez choisir au moins 3 variables" 
      
      X<-.var.type(X=X, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=T, title="Variables", out=NULL)
      data<-X$data
      X<-X$X
      if(is.null(X) || length(X)<3) return(NULL)
      
      
      if(dial || length(outlier)>1 || outlier %in% c("Données complètes", "Données sans valeur influente") ==FALSE){
        if(info) writeLines("Désirez-vous l'analyse sur les données complètes ou sur les données pour lesquelles les valeurs influentes ont été enlevées ?")
        if(info) writeLines("les valeurs influentes sont identifiées sur la base de la distance de Mahalanobis avec un seuil du chi à 0.001")
        outlier<- dlgList(c("Données complètes", "Données sans valeur influente"), preselect="Données complètes",multiple = FALSE, title="Quels résultats voulez-vous obtenir ?")$res
        if(length(outlier)==0) { Resultats<-cfa.in()
        return(Resultats)}
      }
      
      if(outlier=="Données sans valeur influente"){
        inf<-VI.multiples(data[,X])
        Resultats$"Valeurs considérées comme influentes"<-inf$"Valeurs considérées comme influentes"
        data<-inf$data
      }
      
      if(dial){
        if(info) writeLines("Veuillez préciser le type de variables. Des corrélations tétra/polychoriques seront réalisées sur les variables dichotomiques/ordinales et Bravais-Pearson sur les variables continues")
        if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c("dichotomiques/ordinales","continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res}else {
          type<-dlgList(c("continues", "mixte"), preselect=NULL, multiple = FALSE, title="Nature des variables ?")$res 
        }
        
        if(length(type)==0) {Resultats<-cfa.in()
        return(Resultats)}
      } else{if(is.null(ord)) type<-"continues" else type<-"dichotomiques/ordinales"
      }
      
      if(type!="continues"){ 
        if(type=="mixte") {
          if(info) writeLines("Veuillez préciser les variables ordinales ?") 
          ord<-dlgList(X, multiple = TRUE, title="Variables ordinales ?")$res
          if(length(ord)==0) {Resultats<-cfa.in()
          return(Resultats)}
        }else ord<-X
      }
      
      modele<-Lav.modele(X=X, LV=LV)
      if(is.null(modele)) {
        Resultats<-cfa.in()
        return(Resultats) 
      }}else{
        modele<-Lav.modele(modele=modele)
        if(is.null(modele)) {
          Resultats<-cfa.in()
          return(Resultats)
        }
      }
    if(any(is.na(data[,X]))) {
      if(is.null(imp))  {msgBox("Des valeurs manquantes ont été détectées. Comment voulez-vous les traiter ?")
        imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes","Remplacer par la médiane","Multiple imputation - Amelia"), 
                      preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes ?")$res}
      if(length(imp)==0){
        Resultats<-cfa.in()
        return(Resultats)
      }
      data1<-ez.imp(data[, X], imp=imp, ord= ord)
      diff<-setdiff(names(data), X)
      data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),diff])
    }  
    
    
    
    Resultats$opt.list<-.ez.lavaan.options(data=data, X=X, info=TRUE, opt.list=opt.list, dial=dial) 
    if(is.null( Resultats$opt.list)) {
      Resultats<-cfa.in()
      return(Resultats)
    }
    
    
    if(dial || class(output)!="character"|| any(!output%in% c("default", "Sorties par défaut", "parEst", "Paramètres estimés", "parSt", "Paramètres standardisés","Matrice de covariance ajustée", "fitted.cov",
                                                              "Résidus standardisés", "res.St","res.Unst","Résidus non standardisés","vcov","Matrice de covariance estimée",
                                                              "AIC", "BIC", "Mesures d'adéquation","fitM", "Inspecter les valeurs de départ", "start", "Inspecter les matrices du modèle",
                                                              "modmat", "Inspecter la représentation du modèle", "modrep"))==TRUE){
      if(info) writeLines("Quels résultats souhaitez-vous ? Attention : les sorties par défaut ne peuvent être sauvegrdées. Si vous voulez une sauvarde, choisissez le détail")
      output<-c( "Sorties par défaut", "Paramètres estimés", "Paramètres standardisés","Matrice de covariance ajustée", 
                 "Résidus standardisés", "Résidus non standardisés","Matrice de covariance estimée","AIC", "BIC", "Mesures d'adéquation", 
                 "Inspecter les valeurs de départ",  "Inspecter les matrices du modèle", "Inspecter la représentation du modèle")
      if(info) writeLines("Quelles sorties de résultats souhaitez-vous ?")
      output<- dlgList(output, preselect="Sorties par défaut", multiple = TRUE, title="Sorties de résultats ?")$res
      if(is.null( Resultats$opt.list)) {
        Resultats<-cfa.in()
        return(Resultats)
      }
    }
    
    
    if(dial || length(sauvegarde)!=1 || !is.logical(sauvegarde)){
      sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Voulez-vous sauver les résultats ?")$res
      if(length(sauvegarde)==0) {
        Resultats<-cfa.in()
        return(Resultats)}
    }   
    
    Resultats$ord<-ord
    Resultats$data<-data
    Resultats$nom<-nom
    Resultats$modele<-modele 
    Resultats$output<-output 
    Resultats$sauvegarde<-sauvegarde
    
    return(Resultats)  
  }
  cfa.out<-function(cfa.options){
    .e <- environment()
    list()->Resultats
    
    data<-cfa.options$data   
    modele<-cfa.options$modele
    nom.v<-strsplit(modele, split="[\\|,+,='\n'~' ']+")
    var.mod<-which(names(data)%in% nom.v[[1]])
    ord<-cfa.options$ord
    output<-cfa.options$output
    sauvegarde<-cfa.options$sauvegarde
    cfa.options$opt.list->opt.list   
    
    opt.list$mimic->mimic
    opt.list$fixed.x->fixed.x
    opt.list$missing->missing
    opt.list$information->information
    opt.list$zero.keep.margins->zero.keep.margins
    opt.list$zero.add->zero.add
    if(is.null(zero.add)) zero.add<-"default"
    opt.list$estimator->estimator
    if(estimator=="ML" & (!is.null(ord)|any( unlist(sapply(data[,var.mod], class))=="factor") )) estimator<-"WLSMV"
    opt.list$group->group
    opt.list$test->test
    opt.list$se->se
    opt.list$std.ov->std.ov
    opt.list$orthogonal->orthogonal
    opt.list$likelihood->likelihood
    if(estimator!="ML")likelihood<-"default"
    opt.list$link->link
    opt.list$int.ov.free->int.ov.free
    opt.list$int.lv.free->int.lv.free
    opt.list$fixed.x->fixed.x
    opt.list$std.lv->std.lv
    opt.list$n.boot->n.boot
    opt.list$group.w.free->group.w.free
    if(is.null(group.w.free)) group.w.free<-F
    opt.list$group.equal->group.equal
    
    
    fit<-try( lavaan::cfa(modele, data = data, ordered=ord,estimator=estimator, test=test,
                          bootstrap=n.boot,meanstructure="default", check="start",zero.cell.warn=F, 
                          missing=missing, group=group, ifelse(!is.null(group), group.equal=group.equal,group.equal="mean"),
                          group.w.free= group.w.free,fixed.x=fixed.x,information=information,se=se,std.ov=as.logical(std.ov),
                          orthogonal=as.logical(orthogonal),likelihood=likelihood, link=link, int.ov.free=as.logical(int.ov.free),
                          int.lv.free=as.logical(int.lv.free),std.lv=as.logical(std.lv),zero.add=zero.add, zero.keep.margins=zero.keep.margins), silent=T)
    if(class(fit)=="try-error") {msgBox("Nous n'avons pas pu terminer correctement l'analyse. Veuillez tenter de respécifier les paramètres")
      return(ez.cfa())}
    
    if(any(output== "default") | any(output== "Sorties par défaut"))  {summary(fit, fit.measures = TRUE, standardized=T)->Resultats$"Résultats de l'analyse factorielle confirmatoire"
      if(length(output)==1) fit->>modele.cfa}
    if(any(output== "parEst") | any(output=="Paramètres estimés")) parameterEstimates(fit)->Resultats$"Paramètres estimés non standardisés"
    if(any(output== "parSt") | any(output=="Paramètres standardisés")) standardizedSolution(fit)->Resultats$"Paramètres estimés standardisés"
    if(any(output== "Matrice de covariance ajustée") | any(output=="fitted.cov")) fitted(fit)->Resultats$"Matrice de covariance ajustée"
    if(any(output== "Résidus standardisés") | any(output=="res.St")) resid(fit, type="standardized")->Resultats$"Résidus standardisés"
    if(any(output== "Résidus non standardisés") | any(output=="res.Unst")) resid(fit)->Resultats$"Résidus non standardisés"
    if(any(output== "vcov") | any(output=="Matrice de covariance estimée")) vcov(fit)->Resultat$"Matrice de covariance estimée"
    if(any(output== "AIC") ) AIC(fit)->Resultats$AIC
    if(any(output== "BIC") ) BIC(fit)->Resultats$BIC
    if(any(output== "Mesures d'ajustement") | any(output=="fitM")) fitMeasures(fit)->Resultat$"Mesure d'ajustement"
    if(any(output== "Inspecter les valeurs de départ") | any(output=="start"))inspect(fit, what=start)->Resultats$"Valeurs de départ"
    if(any(output== "Inspecter les matrices du modèle") | any(output=="modmat")) inspect(fit)->Resultats$"Matrices du modèles"
    if(any(output== "Inspecter la représentation du modèle") | any(output=="modrep"))inspect(fit, what=list)->Resultats$"Représentation du modèle"
    semPaths(fit, what="path", whatLabels="std", edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T)
    
    
    return(Resultats)
    
    
    
  } 
  
  
  packages<-c("svDialogs", "psych","lavaan","semPlot")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  try( windows(record=T), silent=T)->win
  if(class(win)=="try-error") quartz()    
  
  Resultats<-list()
  opt.list<-list(mimic=mimic,fixed.x=fixed.x,missing=missing,information=information,zero.keep.margins=zero.keep.margins,zero.add=zero.add,
                 estimator=estimator,group=group,test=test,se=se,std.ov=std.ov,orthogonal=orthogonal,likelihood=likelihood,
                 link=link,int.ov.free=int.ov.free,int.lv.free=int.lv.free,fixed.x=fixed.x,std.lv=std.lv,n.boot=n.boot,group.w.free=group.w.free,group.equal=group.equal)
  cfa.options<-cfa.in(modele=modele,X=X, data=data, ord=ord, outlier=outlier,imp=imp,output=output, info=T, opt.list=opt.list, sauvegarde=sauvegarde)
  if(is.null(cfa.options)) return(analyse())
  AFC<-cfa.out(cfa.options)
  if(!is.null(AFC)) Resultats$AFC<-AFC
  
  
  
  def.values<-list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                   estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                   link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                   group.equal=c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", 
                                 "lv.variances" , "lv.covariances"))
  
  if(!is.null(cfa.options$ord)) paste(cfa.options$ord, collapse="','", sep="")->ord
  paste(cfa.options$output, collapse="','", sep="")->output
  call<-paste0("ez.cfa(modele='", cfa.options$modele, "',data=", cfa.options$nom, ",ord=", ifelse(is.null(cfa.options$ord), "NULL",paste0("c('",ord,"')")),",outlier='", outlier, 
               "', imp='",imp,"',output=c('", output,"'), sauvegarde=", cfa.options$sauvegarde, ", mimic='", cfa.options$opt.list$mimic, "'")
  
  for(i in 1:length(def.values)){
    if(names(def.values)[i]!="group" & names(def.values)[i]!="mimic") n<-which(names(cfa.options$opt.list) == names(def.values)[i]) else n<-NULL
    if(is.null(def.values[[i]])) call<-ifelse(is.null(cfa.options$opt.list$group),paste0(call, ", group=NULL"), paste0(call,", group =",cfa.options$opt.list$group)) 
    if(length(n)==1){
      if( def.values[[i]] !=cfa.options$opt.list[[n]]){
        if(is.logical(def.values[[i]]) ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"=",cfa.options$opt.list[[n]])
        if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))==1 ) call<-paste0(call, ",", names(cfa.options$opt.list)[n],"='",cfa.options$opt.list[[n]],"'")
        if(is.character(def.values[[i]]) & length(is.character(def.values[[i]]))>1 ){
          paste(cfa.options$opt.list[[n]], collapse="','", sep="")->param
          call<-paste0(call, ",", names(cfa.options$opt.list)[i],"=c('",param,"')") 
        } 
      }} 
  }
  call<-paste0(call,")")
  Resultats$Call<-call
  
  .add.history(data=cfa.options$data, command=Resultats$Call, nom=cfa.options$nom)
  .add.result(Resultats=Resultats, name =paste("AFC", Sys.time() ))  
  
  if(sauvegarde) save(Resultats=Resultats, choix="AFC", env=.e)
  Resultats$ref<-ref1(packages)
  ez.html(Resultats)
  return(Resultats)
  }
