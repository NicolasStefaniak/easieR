regressions.log <-
  function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
           sauvegarde=F,proba=F){
    
    logisticPseudoR2s <- function(LogModel) {
      dev <- LogModel$deviance
      nullDev <- LogModel$null.deviance
      modelN <-  length(LogModel$fitted.values)
      R.l <-  1 -  dev / nullDev
      R.cs <- 1- exp ( -(nullDev - dev) / modelN)
      R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
      return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
    } 
    reg.log.in<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
                         sauvegarde=F,proba=F){
      
      options (warn=-1) 
      Resultats<-list()
      if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F 
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL) 
      nom<-data[[1]]
      data<-data[[2]]  
      
      
      if(dial && is.null(modele)){
        if(info) writeLines("Veuillez choisir le(s) type(s) de relations entre les variables. Les effets additifs prennent la forme de
                            y=X1+X2 tandis que les effets d'interaction prennent la forme de Y=X1+X2+X1:X2")
        dlgList(c("Effets additifs", "Effets d'interaction", "Specifier le modele"), preselect="Regressions", multiple = TRUE, title="Quel type de regression ?")$res->link
        if(length(link)==0) return(NULL)} else link<-"none"
      
      if(length(Y)>1){
        msgBox("Il ne peut y avoir qu'une seule variable dependante.")
        Y<-NULL }
      if(any(link %in% c("Effets additifs", "Effets d'interaction"))){
        msg3<-"Veuillez choisir la variable dependante."
        Y<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=FALSE, title="Variable dependante", out=NULL)
        if(is.null(Y)) {
          reg.log.in()->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X
        
        if(length(unique(data[,Y]))!=2) {
          msg1<-paste("Votre veriable dependante a", length(unique(data[,Y])), "modalites. Elle est incompatible avec une regression logistique. Elle doit etre dichotomique" )
          msgBox(msg1)
          if(class(data[,Y]) %in%c("numeric","integer")){
            dlgMessage("voulez-vous convertir la variable dependante en une variable dichotomique,  ?","yesno")$res->conv
            
            if(conv=="no") return(reg.log.in())  else{
              if(info) writeLines("Veuillez specifier le critere sur lequel vous souhaitez dichotomiser votre variable.Vous pouvez utiliser la mediane ou choisir un seuil specifique.")
              dlgList(c("Mediane", "Seuil"), preselect="Mediane", multiple = FALSE, title="Quel critere de codage voulez-vous ?")$res->codage
              if(length(codage)==0) return(reg.log.in())
              if(codage=="Mediane") data[,Y]<-ifelse(data[,Y]>median(data[,Y]),1, 0)
              View(data)
              readline()
              if(codage=="Seuil") {
                seuil<-NA
                while(is.na(seuil)){
                  seuil<-dlgInput("Veuillez preciser la valeur de separation", median(data[,Y]))$res 
                  if(length(seuil)==0) return(reg.log.in())
                  strsplit(seuil, ":")->seuil
                  tail(seuil[[1]],n=1)->seuil
                  as.numeric(seuil)->seuil
                  if(is.na(seuil) || seuil>max(data[,Y]) || seuil<min(data[,Y])) {msgBox("La valeur doit etre numerique et comprise entre le minimum et le maximum de la variable dependante.")
                    Y<-NA}
                }
                data[,Y]<-ifelse(data[,Y]>seuil,1, 0)
                
              } # seuil
            }
          }
          if(class(data[,Y]) %in%c("factor","character")){
            dlgMessage("Voulez-vous faire des regroupements entre les modalites ?","yesno")$res->reg
            if(reg=="no") return(reg.log.in()) else {
              if(info) writeLines("Veuillez specifier la/les modalite(s) qui serviront pour la ligne de base (e.g. 0). Les autres modalites seront regroupes dans la categorie 1.")
              reg<- dlgList(levels(data[,Y]), preselect=NULL, multiple = TRUE, title="Modalites a regrouper")$res
              setdiff(levels(data[,Y]),reg)->reste
              data[,Y]<-ifelse(data[,Y]%in%reg, 0,1) 
              data[,Y]<-factor(data[,Y])
            }
          }      
        }
        
        
        if(any(link=="Effets additifs") || !null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-"Veuillez choisir la variable dependante."
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modele additif", out=Y)
          if(is.null(X_a)) {
            reg.log.in()->Resultats
            return(Resultats)}
          data<-X_a$data
          X_a<-X_a$X
          
        }else X_a<-NULL 
        
        if(any(link=="Effets d'interaction") || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
          msg3<-"Veuillez choisir les predicteurs a entrer dans le modele d'interaction. Il est necessaire d'avoir au moins deux variables"
          X_i<-c()
          while(length(X_i)<2){
            X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modele interactif", out=c(X_a,Y))
            if(is.null(X_i)) {
              reg.log.in()->Resultats
              return(Resultats)}
            data<-X_i$data
            X_i<-X_i$X
          }
        }else X_i<-NULL
        
        
        
        paste0(Y," ~ ")->modele
        if(!is.null(X_a ))  {
          X_a.mod<-X_a[1]
          if(length(X_a)>1) for(i in 2 : length(X_a)) paste0(X_a.mod, "+", X_a[i])-> X_a.mod
        } else X_a.mod<-NULL
        
        if(!is.null(X_i)){
          X_i.mod<-X_i[1]
          if(length(X_i)>1) for(i in 2 : length(X_i)) paste0(X_i.mod, "*", X_i[i])-> X_i.mod
        } else X_i.mod<-NULL
        
        if(!is.null(X_a.mod) & !is.null(X_i.mod)) {
          paste0(modele, X_a.mod, "+", X_i.mod)->modele
        } else paste0(modele, X_a.mod, X_i.mod)->modele
        
      }
      
      
      
      if(any(link=="Specifier le modele")) modele<-fix(modele)
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels
      if(any(ftable(data[,which(lapply(sapply(data,unique),length)<5)])<3)) {
        msgBox("Les observations sont en nombre insuffisant (<3) pour certaines combinaisons de niveaux des variables du modele")
        return(ftable(data[,which(lapply(sapply(data,unique),length)<5)]))
      }
      if(dial){
        if(length(pred>1)){
          pred.ord<-c()
          while(length(pred)!=0){
            if(info)  writeLines("L'ordre d'entree des variables est important pour le calcul du maximum de vraisemblance. Veuillez
                                 preciser l'ordre d'entree des variables") 
            V1<-dlgList(pred, multiple = FALSE,title="Quelle variable a cette etape")$res
            c(pred.ord,V1)->pred.ord
            setdiff(pred,V1)->pred}
        }else pred.ord<-pred
        
        paste0(Y," ~ ", pred.ord[1])->modele
        if(length(pred.ord)>1) for(i in 2 : length(pred.ord)) paste0(modele, "+", pred.ord[i])-> modele
        modele<-as.formula(modele)}
      
      model.test<-try(model.matrix(modele, data), silent=T)
      if(class(model.test)=="try-error") {
        msgBox("Le modele specifie est incorrect. Verifiez vos variables et votre modele")
        return(reg.log.in())
      }
      
      
      data[complete.cases(data[,variables]),]->data
      options<-.ez.options(options=c("outlier"), n.boot=NULL,param=F, non.param=F, robust=F, Bayes=F, msg.options1=NULL, msg.options2=NULL, info=info, dial=dial, 
                           choix=NULL,sauvegarde=sauvegarde, outlier=outlier, rscale=NULL)
      if(is.null(options)) return(reg.log.in())
      
      reg.options<- .regressions.options(data=data, modele=modele, CV=FALSE, inf=inf, select.m=select.m, method=NULL, criteria=NULL, step=step, group=group, scale=scale, dial=dial,info=info)
      if(is.null(reg.options)) return(reg.log.in())
      
      if(dial){
        if(info) writeLines('voulez-vous integrer les probabilites a votre base de donnees ?')
        dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Probabilites ?")$res->proba
        
      }
      
      Resultats$proba<-proba
      Resultats$data<-data
      Resultats$nom<-nom
      Resultats$modele<-modele
      Resultats$options<-options
      Resultats$reg.options<-reg.options
      return(Resultats)   
      
    }
    
    reg.log.out<-function(data=NULL, modele=NULL,  select.m="none", step=NULL, scale=T, nom=NULL,proba=F){
      
      Resultats<-list()
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels
      Resultats$"Statistiques descriptives"<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)
      
      if(scale==T || scale=="Centre") {Resultats$info<-"En accord avec les recommandations de Schielzeth 2010, les donnees ont ete prealablement centrees"
      fun<-function(X){X-mean(X)}
      variables[-1]->pred2
      sapply(X=data[, names(which(sapply(data[,pred2],class)!="factor"))], fun)->data[,names(which(sapply(data[,pred2],class)!="factor"))]
      }
      if(class(data[,variables[1]])=="character") factor(data[,variables[1]])->data[,variables[1]]
      
      if(!is.null(step)){
        
        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
        list()->modele.H1
        list()->formule.H1
        for(i in 1:length(step)){
          
          for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
          formule.H1[[i]]<-modele.H
          glm(modele.H, data=data, na.action=na.exclude , family="binomial")->lm.H
          lm.H->modele.H1[[i]]}
        
        hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
        if(length(modele.H1)>2){
          for(i in 3: length(modele.H1)){
            hier<-paste0(hier, ",modele.H1[[", i, "]]")
          }
        }
        hier<-paste0(hier,")")
        hier<-eval(parse(text=hier))
        
        attributes(hier)$heading[1]<-"Table de l'analyse de la deviance des modeles hierarchiques"
        round(1-pchisq(hier$Deviance,hier$Df,lower.tail=F),4)->hier$valeur.p
        names(hier)<-c("ddl.resid", "Deviance.resid","ddl.effet", "Deviance", "valeur.p")
        Resultats$"Analyse hierarchique des modeles "<-hier
      }
      
      
      
      
      
      mod<-list()
      modele1<-as.formula(paste0(variables[1], "~", pred[1]))
      glm(modele1, data=data, family="binomial")->glm.r1
      mod <- list() 
      glm.r1->mod[[1]] 
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(glm.r1, as.formula(paste0(".~.+",pred[i])))->glm.r1
          glm.r1->mod[[i]]}
      }
      
      anova(mod[[length(mod)]])->Amelioration_du_MV
      
      summary(mod[[length(mod)]])->resultats
      as(resultats$call,"character")->texte
      paste("le modele teste est" , texte[2])->Resultats$"Modele teste"
      
      cbind(rms::vif(mod[[length(mod)]]), 1/rms::vif(mod[[length(mod)]]))->MC
      dimnames(MC)[[2]]<-c("Facteur d'inflation de la variance", "Tolerance")
      round(MC,4)->Resultats$"Test de multicolinearite"
      
      sum(Amelioration_du_MV$Df[2:length(Amelioration_du_MV$Df)])->ddl
      Amelioration_du_MV$`Resid. Dev`[1]-Amelioration_du_MV$`Resid. Dev`[length(Amelioration_du_MV$`Resid. Dev`)]->chi.carre.modele
      round(1-pchisq(chi.carre.modele,ddl),4)->valeur.p
      logisticPseudoR2s(mod[[length(mod)]])->Pseudo.R.carre
      data.frame(chi.carre.modele, ddl, valeur.p,Pseudo.R.carre[1],Pseudo.R.carre[2],Pseudo.R.carre[3])->mod.glob
      names(mod.glob)<-c("chi.2.modele", "ddl", "valeur.p","Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
      mod.glob->Resultats$"Significativite du modele global"
      
      
      Amelioration_du_MV$chi.deux.prob<-1-pchisq(Amelioration_du_MV$Deviance, Amelioration_du_MV$Df)
      round(Amelioration_du_MV,4)->Amelioration_du_MV
      names(Amelioration_du_MV)<-c("ddl predicteur", "MV","ddl.residuels","MV residuel","valeur.p")
      Resultats$"Amelioration de la vraisemblance pour chaque variable"<-data.frame(Amelioration_du_MV)
      
      data.frame(resultats$coefficients)->table
      (table$z.value)^2->table$Wald.statistic
      exp(table$Estimate)->table$Odd.Ratio
      round(table,4)->table
      names(table)<-c("b","Erreur.standard","valeur.Z","p.Wald", "Wald","Odd.ratio")
      cbind(table, round(exp(confint(mod[[length(mod)]])),4))->table
      table$interpretation<-ifelse(table$Odd.ratio>=1,paste(table$Odd.ratio, "fois plus"), paste(round(1/table$Odd.ratio,4), "fois moins"))
      table->Resultats$"Table des coefficients"
      
      R_sq<-NULL
      for(i in 1:length(mod)){logisticPseudoR2s(mod[[i]])->R_squared
        rbind(R_sq, R_squared)->R_sq}
      diff(R_sq,lag=1)->R_sq[2.]
      dimnames(R_sq)[[1]]<-pred
      dimnames(R_sq)[[2]]<-c("Hosmer and Lemeshow R^2","Cox and Snell R^2","Nagelkerke R^2")
      R_sq->Resultats$"Delta du pseudo R carre"
      
      if(proba=="TRUE")	{ 
        round(fitted(mod[[length(mod)]]),4)->data$"Probabilites predites"
        head(data)
        print(nom)
        assign(x=nom, value=data, envir=.GlobalEnv)}
      
      if(select.m!="none"){
        select.m<-switch(select.m,"Forward - pas-a-pas ascendant"="forward", "Backward- pas-a-pas descendant"="backward", "Bidirectionnel"="both",
                         "forward"="forward", "bidirectional"="both","backward"="backward" )
        glm(modele, data=data, family="binomial")->glm.r1
        
        steps<-stepAIC(glm.r1, direction=select.m) 
        Resultats$"Methode de selection - criteres d'information d'Akaike"<-steps$anova
        modele<-as.formula(attributes(steps$anova)$heading[5])
      }
      
      return(Resultats)
      
      
      
    }
    
    
    c("boot","car","psych", "mlogit","svDialogs","rms","MASS")->packages
    if(class(data)=="data.frame") deparse(substitute(data))->data 
    options (warn=-1) 
    .e <- environment()
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    Resultats<-list() 
    reg.in.output<-reg.log.in(data=data, modele=modele, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                              select.m=select.m,  step=step, group=group,  scale=scale, info=info, sauvegarde=sauvegarde, proba=proba)
    if(is.null(reg.in.output)) return(choix.reg())
    data<-reg.in.output$data
    nom<-reg.in.output$nom
    modele<-reg.in.output$modele
    outlier<-reg.in.output$options$desires
    sauvegarde<-reg.in.output$options$sauvegarde
    scale<-reg.in.output$reg.options$scale
    inf<-reg.in.output$reg.options$inf
    step<-reg.in.output$reg.options$step
    select.m<-reg.in.output$reg.options$select.m
    group<-reg.in.output$reg.options$group
    proba<-reg.in.output$proba
    
    if(!is.null(reg.in.output$reg.options$CV) && reg.in.output$reg.options$CV==TRUE) print("La validation croisee n'est pas encore disponible.")
    
    if(any(outlier==  "Donnees completes")){
      Resultats$"Donnees completes"<-  reg.log.out(data=data, modele=modele,  select.m=select.m, step=step, scale=scale, proba=proba, nom=nom)
      if(!is.null(group))   {  
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-  try(reg.log.out(data=G[[i]], modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba), silent=T)
          if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"Le nombre d'observations est insuffisant pour mener a bien les analyses pour ce groupe" else R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Resultats$"Donnees completes"$"Analyse par groupe"<-R1
      } 
      
    } 
    if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Donnees sans valeur influente")|inf==T){
      
      lm.r1<-glm(modele, data, na.action=na.exclude ,family="binomial")
      as.character(attributes(terms(modele))$variables)->variables
      variables[2:length(variables)]->variables
      plot(lm.r1, which = 5)
      if(inf) {
        influence.measures(lm.r1)->mesure_influence
        data<-data.frame(data, round(mesure_influence$infmat,3))
        rstandard(lm.r1)->data$res.stand
        rstudent(lm.r1)->data$res.student # idem avec le residu studentise
        data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residual, lower.tail=F)
        data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
        data$est.inf<-" "
        data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
        
        data[order(data$res.student.p.Bonf), ]->data
        writeLines("Les observations marquees d'un asterisque sont considerees comme influentes au moins sur un critere")
        View(data)
        suppression<-"yes"
        outliers<-data.frame()
        nettoyees<-data
        while(suppression=="yes"){
          
          cat ("Appuyez [entree] pour continuer")
          line <- readline()
          sup<-NA
          while(is.na(sup)){
            sup <- dlgInput("Quelle observation souhaitez-vous retirer des analyses ? 0=aucune", 0)$res
            if(length(sup)==0) return(regressions())
            strsplit(sup, ":")->sup
            tail(sup[[1]],n=1)->sup
            as.numeric(sup)->sup
            if(is.na(sup)) msgBox("Vous devez entrer le numero de l'observation")  
          }
          if(sup==0) suppression<-"no" else {
            rbind(outliers, nettoyees[sup,])->outliers
            nettoyees[-sup,]->nettoyees
          }
          
        }
        if(length(outliers)!=0) outliers<-outliers[,variables]
        assign(nom, data, envir=.GlobalEnv)
      } else {
        4/length(data[,1])->seuil_cook # fixe le seuil pour les valeurs aberrantes 
        cooks.distance(lm.r1)->data$cook.d  
        data[which(data$cook.d<= seuil_cook), ]->nettoyees 
        data[which(data$cook.d>= seuil_cook), ]->outliers
        cbind(outliers[,variables],outliers$cook.d)->outliers
        Resultats$"information"$"les valeurs influentes sont identifiees sur la base de 4/n"
      }
      nettoyees->>nettoyees   
      
      if(any(outlier== "Identification des valeurs influentes")){
        length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retirees"=N_retire, "Pourcentage.obs.retirees"=Pourcentage_retire)->Resultats$"Synthese du nombre d'observations considerees comme influentes"
        if(length(outliers)!=0) Resultats$"Identification des valeurs influentes"$"Observations considerees comme influentes"<-outliers
        
      }
      if(any(outlier== "Donnees sans valeur influente")) {
        if(N_retire!=0 | all(outlier!="Donnees completes")){
          so<- try(reg.log.out(data=nettoyees,modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba, nom=paste0(nom,".nettoyees")),silent=T)
          if(class(so)=="try-error") Resultats$"Donnees sans valeur influente"<-"La suppression des valeurs influentes entraîne un effectif trop faible sur certaines modalites pour mener a bien l'analyse" else{
            Resultats$"Donnees sans valeur influente"<-so 
            
            if(!is.null(group))   {  
              R1<-list()
              G<-nettoyees[,group]
              if(length(group)>1) G<-as.list(G)
              G<-split(nettoyees, G)
              for(i in 1:length(G)){
                resg<- try( reg.log.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group,  scale=scale,proba=proba), silent=T)
                
                if(class(resg)=="try-error")   R1[[length(R1)+1]]<-"Le nombre d'observations est insuffisant pour mener a bien les analyses pour ce groupe" else R1[[length(R1)+1]]<-resg
                names(R1)[length(R1)]<-names(G)[i]
              }
              Resultats$"Donnees sans valeur influente"$"Analyse par groupe"<-R1
            } 
          } 
          
        }
      }
    }
    
    
    paste(outlier, collapse="','", sep="")->outlier
    as.character(modele)->m1
    modele<-paste0(m1[2],"~", m1[3])
    if(!is.null(group)) paste(group, collapse="','", sep="")->group
    if(!is.null(step)) {
      paste0("list(")->step.call
      for(i in 1:length(step)){
        if(i>1) n.step<-paste0(", step",i) else n.step<-paste0("step",i)
        paste(step[[i]], collapse="','", sep="")->var.step
        step.call<-paste0(step.call,n.step,"=c('", var.step, "')")
      }
      step.call<-paste0(step.call, ")")
    }
    Resultats$Call<-paste0("regressions.log(data=", nom, ",modele=",  modele, ",outlier=c('", outlier, "'),inf=", inf, ",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                           ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),",dial=T, info=T,sauvegarde=", sauvegarde,",proba=",proba ,")")
    
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("Regressions.logistique", Sys.time() ))  
    if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix="Regressions.logistique", env=.e)
    Resultats$"References"<-ref1(packages)
    if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
    
  }
