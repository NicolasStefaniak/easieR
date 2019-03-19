
regressions <-
  function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=0.15 , scale=T, dial=T, info=T,
           sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
    
    
    
    regressions.in<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=F, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=NULL , scale=T, dial=T, info=T,
                             sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
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
        if(length(link)==0) return(NULL) } else link<-"none"
      
      if(length(Y)>1){
        msgBox("Il ne peut y avoir qu'une seule variable dependante.")
        Y<-NULL }
      if(any(link %in% c("Effets additifs", "Effets d'interaction"))){
        msg3<-"Veuillez choisir la variable dependante."
        Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=FALSE, title="Variable dependante", out=NULL)
        if(is.null(Y)) {
          regressions.in()->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X
        
        if(any(link=="Effets additifs") || !is.null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-"Veuillez choisir la variable dependante."
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title="Variables modele additif", out=Y)
          if(is.null(X_a)) {
            regressions.in()->Resultats
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
              regressions.in()->Resultats
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
      
      if(any(link=="Specifier le modele")) {
        if(is.null(modele)) modele<-" "
        modele<-fix(modele)}
      modele<-as.formula(modele)
      variables<-terms(modele)
      variables<-as.character( attributes(variables)$variables)[-1]
      
      
      model.test<-try(model.matrix(modele, data), silent=T)
      if(class(model.test)=="try-error") {
        msgBox("Le modele specifie est incorrect. Verifiez vos variables et votre modele")
        return(regressions.in())
      }
      
      
      data[complete.cases(data[,variables]),]->data
      msg.options1<-"Le test parametrique est la regression classique et les tests robustes sont une estimation sur un M estimeur ainsi qu'un bootstrap."
      
      options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=F, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                           choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
      if(is.null(options)) return(regressions.in())
      
      reg.options<- .regressions.options(data=data, modele=modele, CV=CV, inf=inf, select.m=select.m, method=method, criteria=criteria, step=step, group=group, scale=scale, dial=dial,info=info)
      if(is.null(reg.options)) return(regressions.in())
      
      
      Resultats$data<-data
      Resultats$nom<-nom
      Resultats$modele<-modele
      Resultats$options<-options
      Resultats$reg.options<-reg.options
      return(Resultats)   
      
    }
    
    regressions.out<-function(dtrgeasieR=NULL, modele=NULL,  VC=F, select.m="none", method=NULL, step=NULL, group=NULL, criteria=NULL , scale=T,
                              sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
      
      Resultats<-list()
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels
      Resultats$"Statistiques descriptives"<-.stat.desc.out(X=variables, groupes=NULL, data=dtrgeasieR, tr=.1, type=3, plot=T)
      
      if(scale==T || scale=="Centre") {Resultats$info<-"En accord avec les recommandations de Schielzeth 2010, les donnees ont ete prealablement centrees"
      
      which(sapply(dtrgeasieR[,pred[which(pred %in% variables)]],class)!="factor")->centre
      if(length(centre)==1) dtrgeasieR[,names(centre)]-mean(dtrgeasieR[,names(centre)],na.rm=T)->dtrgeasieR[,names(centre)] else{
        sapply(X=dtrgeasieR[,names(centre)], fun<-function(X){X-mean(X, na.rm=T)})->dtrgeasieR[,names(centre)]
      }
      }
      
      
      mod<-list()
      modele1<-as.formula(paste0(variables[1], "~", pred[1]))
      lm( modele1,na.action=na.exclude, data=dtrgeasieR)->lm.r1
      lm.r1->mod[[1]]
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(lm.r1, as.formula(paste0(".~.+",pred[i])))->lm.r1
          lm.r1->mod[[i]]}
      }
      assign("lm.r1",lm.r1, env= .GlobalEnv)
      resid(lm.r1)->dtrgeasieR$residu
      Resultats$"Tests de normalite"<-.normalite(data=dtrgeasieR, X="residu", Y=NULL)
      if(length(variables)>1)  {
        cont<-variables[which(sapply(dtrgeasieR[,variables],class)!="factor")]
        Resultats$"Normalite multivariee"<-.normalite(data=dtrgeasieR, X=cont, Y=NULL)
        ols_plot_resid_fit(lm.r1)
        FIV<-ols_coll_diag(lm.r1) # calcul du facteur d inflation de la variance 
        names(FIV)<-c("Test de multicolinearite", "Indice des valeurs propres")
        names(FIV$`Test de multicolinearite`)<-c("variables", "Tolerance", "FIV")
        Resultats$"Tests de multicolinearite"<-FIV$`Test de multicolinearite`
        if(FIV$`Test de multicolinearite`$Tolerance==0) {
            msgBox("La multicolinearite est trop importante. Le modele est instable")
            return(Resultats)
            }

        Resultats$"Graphique testant la linearite entre les predicteurs et la variable dependante"<-ols_plot_comp_plus_resid(lm.r1)
        Resultats$"Indice des valeurs propres"<-FIV$`Indice des valeurs propres`
        dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
        Resultats$"Test de Durbin-Watson - autocorrelations"<-round(data.frame("Autocorrelation"=DWT.results[1],"statistique de D-W"=DWT.results[2],"valeur p"=DWT.results[3]),4)->DWT.results
        var.err<-bptest(model)
        Resultats$"Verification de la non-constance de la variance d'erreur (test de Breusch-Pagan)"<-data.frame(chi=var.err$statistic,
                                                                                                                 ddl=var.err$parameter,valeur.p=var.err$p.value)
        
        
        try(ceresPlots(lm.r1, main="Graphique de Ceres testant la linearite"), silent=T)
      }
      if(select.m!="none"){
        dtrgeasieR<<-dtrgeasieR
        if(method %in% c("F", "valeur du F", "p", "valeur de la probabilite")){
          select.m<-switch(select.m,"Forward - pas-a-pas ascendant"="Forward", "Backward- pas-a-pas descendant"="Backward", "Bidirectionnel"="Both",
                           "forward"="Forward", "bidirectional"="Stepwise","backward"="Both" )
          
          if(select.m=="Forward") t<-capture.output({  ols.out <- ols_step_forward_p(lm.r1,penter = criteria, details=F)})
          if(select.m=="Backward") t<-capture.output({  ols.out <- ols_step_backward_p(lm.r1, prem=criteria, details=F)})
          if(select.m=="Both") t<-capture.output({  ols.out <- ols_step_both_p(lm.r1,pent=criteria, details=F)})
          predname<-if(!is.null(ols.out$predictors)) rep(TRUE, length(ols.out$predictors)) else rep(FALSE,length(ols.out[[1]]) )
          methodname<-if(!is.null(ols.out$method)) rep(TRUE, length(ols.out$method)) else rep(select.m,length(ols.out[[1]]) )
          ols.frame<-data.frame(etape=1:ols.out$steps,
                                predicteurs=ifelse(predname,ols.out$predictors,ols.out$removed) ,
                                mallows_cp=ols.out$ mallows_cp,
                                AIC=ols.out$aic,
                                BIC=ols.out$sbc,
                                RMSE=ols.out$rmse,
                                r.carre=ols.out$rsquare,
                                r.carre.adj=ols.out$adjr,
                                Method=ifelse(methodname==T, ols.out$method, ifelse(methodname=="Forward" , "Variable ajoutee", "variable supprimee"))
          )
          Resultats$"Methode de selection"<-ols.frame 
        }
        
        if(method %in% c("AIC - Akaike Information criterion","AIC")){ 
          select.m<-switch(select.m,"Forward - pas-a-pas ascendant"="Forward", "Backward- pas-a-pas descendant"="Backward", "Bidirectionnel"="Both",
                           "forward"="Forward", "bidirectional"="Both","backward"="Backward" )
          lm.r1<-lm(modele, data=dtrgeasieR)
          if(select.m=="Forward") t0<-capture.output({  ols.out <- ols_step_forward_aic(lm.r1, details=T)}) 
          if(select.m=="Backward") t0<-capture.output({  ols.out <- ols_step_backward_aic(lm.r1, details=T)})
          if(select.m=="Both")     t0<-capture.output({  ols.out <- ols_step_both_aic(lm.r1, details=T)})
          
          predname<-if(select.m!="Backward") rep(TRUE, length(ols.out$predictors)) else rep(FALSE,length(ols.out[[1]])+1 )
          methodname<-if(!is.null(ols.out$method)) rep(TRUE, length(ols.out$method)) else rep(select.m,length(ols.out[[4]]) )
          ols.frame<-data.frame(etape=1:ols.out$steps,
                                predicteurs=ifelse(predname,ols.out$predictors, c("Modele complet", ols.out$predictor)) ,
                                Somme.Carre=ols.out$rss,
                                AIC=ols.out$aic,
                                SC.res=ols.out$ess,
                                r.carre=ols.out$rsq,
                                r.carre.adj=ols.out$arsq,
                                Method=ifelse(methodname==T, ols.out$method, ifelse(methodname=="Forward" , "Variable ajoutee", c(" ","variable supprimee")))
          )
          
          Resultats$"Methode de selection - criteres d'information d'Akaike"<-ols.frame
          
        }
        
        if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
          BF.out<-try(regressionBF(modele, data=dtrgeasieR,progress=F, rscaleCont=rscale), silent=T)
          if(class(BF.out)!="try-error") {
            try(plot(BF.out) , silent=T)
            BF.out<-extractBF(BF.out)
            BF.out<-head(BF.out[order(BF.out[,1], decreasing=T), ])
            BF.out<-BF.out[,1:2]
            Resultats$"Methodes de selection : facteurs bayesiens"<-BF.out
          } else Resultats$"Methodes de selection : facteurs bayesiens"<-"Les methodes de selection pour les facteurs bayesiens ne s'appliquent pas pour des modeles complexes."
        }
        rm( "dtrgeasieR", envir = .GlobalEnv)
      }
      
      if(!is.null(step)){
        
        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
        list()->modele.H1
        list()->formule.H1
        for(i in 1:length(step)){
          
          for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
          formule.H1[[i]]<-modele.H
          lm(modele.H, data=dtrgeasieR, na.action=na.exclude )->lm.H
          lm.H->modele.H1[[i]]}
        
        if(any(param=="param")|any(param=="Test parametrique")) {
          hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
          if(length(modele.H1)>2){
            for(i in 3: length(modele.H1)){
              hier<-paste0(hier, ",modele.H1[[", i, "]]")
            }
          }
          hier<-paste0(hier,")")
          hier<-eval(parse(text=hier))
          attributes(hier)$heading[1]<-"Table de l'analyse de variance des modeles hierarchiques"
          names(hier)<-c("ddl.resid", "SC.resid","ddl.effet", "SC", "F", "p")
          Resultats$"Analyse hierarchique des modeles "<-hier
          
          
          
          c(summary(modele.H1[[1]])$sigma, summary(modele.H1[[1]])$r.squared, summary(modele.H1[[1]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
          pf(summary(modele.H1[[1]])$fstatistic[1], summary(modele.H1[[1]])$fstatistic[2],summary(modele.H1[[1]])$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
          c(significativite_modele , p.value)->modele_avec_outliers 
          
          for(i in 2:(length(modele.H1))){
            c(summary(modele.H1[[i]])$sigma, summary(modele.H1[[i]])$r.squared, summary(modele.H1[[i]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
            pf(summary(modele.H1[[i]])$fstatistic[1], summary(modele.H1[[i]])$fstatistic[2],summary(modele.H1[[i]])$fstatistic[3], lower.tail=F)->valeur.p #permet de savoir si le F est significatif
            rbind(modele_avec_outliers, c(significativite_modele ,valeur.p))->modele_avec_outliers  
          }
          round(modele_avec_outliers,3)->modele_avec_outliers 
          c("Erreur residuelle", "R.deux", "F", "Ddl(1)", "Ddl(2)","valeur.p")->dimnames(modele_avec_outliers)[[2]]
          paste("etape", 1:length(modele_avec_outliers[,1]))->dimnames(modele_avec_outliers)[[1]]
          Resultats$"Modeles hierarchique - significativite du modele complet a chaque etape"<-modele_avec_outliers
          
        }
        
        if(any(param=="Bayes")|any(param=="Facteurs bayesiens")) {
          BF<-lmBF(formula= as.formula(formule.H1[[1]]), data=dtrgeasieR, rscaleFixed=rscale)
          BF.modele<-extractBF(BF, onlybf=T)
          BF.hier<-c(NA)
          for(i in 2:length(formule.H1)){
            numBF<-lmBF(formula= as.formula(formule.H1[[i]]), data=dtrgeasieR, rscaleFixed=rscale)
            BF.modele<-c(BF.modele, extractBF(numBF, onlybf=T))
            denomBF<-lmBF(formula= as.formula(formule.H1[[i-1]]), data=dtrgeasieR, rscaleFixed=rscale)
            OddBF<-numBF/denomBF
            BF.hier<-c(BF.hier, extractBF(OddBF, onlybf=T))}

          BF.hier<-data.frame("Rapport des FB entre les modeles"=BF.hier, "FB du modele"= BF.modele)
          dimnames(BF.hier)[[1]]<- unlist(as.character(formule.H1))
          Resultats$"Approche bayesienne des modeles hierarchique"<-BF.hier
        }
        
      }
      # "test parametrique", "test non parametrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens"   
      if(any(param=="param")|any(param=="Test parametrique")) {
        c(summary(lm.r1)$sigma, summary(lm.r1)$r.squared, summary(lm.r1)$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
        pf(summary(lm.r1)$fstatistic[1], summary(lm.r1)$fstatistic[2],summary(lm.r1)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
        c(significativite_modele , p.value)->modele.F # on combine les precedents 
        round(modele.F,3)->modele.F # on arrondit les nombres a la 3e decimale
        c("Erreur residuelle", "R.deux", "F", "Ddl (num)", "Ddl (dnom)","valeur.p")->names(modele.F)# attribue le nom aux colonnes
        modele.F->Resultats$"Estimation  du modele global"
        
        
        data.frame(summary(lm.r1)$coefficients)->table # fournit le b, le t et la valeur de la probabilite. On le stocke dans table
        round(table[,1:4],3)->table # on arrondit les valeurs a 3 decimales 
        
        beta<-coef(lm.r1)*sapply(data.frame(model.matrix(lm.r1)),sd) /sd(data[,variables[1]])
        c("",round(beta[-1],5))->table$beta # fournit les betas qu on inclut a la table 
        names(table)<-c("b","erreur.standard","t","valeur.p","beta")
        
        r_carre<- matrix(c(0,0,0),1)
        for(i in 1:length(mod)){
          rep(summary(mod[[i]])$r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre2
          summary(mod[[i]])$r.squared-r_carre[length(r_carre[,2]),1]->diff
          rep(diff, (length(coef(mod[[i]]))-length(r_carre[,1])))->diff
          rep(summary(mod[[i]])$adj.r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre_adj
          
          round(cbind(r_carre2, diff, r_carre_adj), 4)->r_carre2
          rbind(r_carre,r_carre2 )->r_carre
          
        }
        
        dimnames(r_carre)<-list(ligne=NULL, c("R.deux", "Delta R.deux", "R.deux.aj"))
        data.frame(table,r_carre)->table
        table[is.na(table)]<-""
        table->Resultats$"table des betas"
        if(length(pred)>1){
        ols.corr<-ols_correlations(lm.r1)
        Resultats$"Contribution des variables au modele"<-ols.corr
        Resultats$"Graphe des variables ajoutees" <-ols_plot_added_variable(lm.r1)
          }
      }
      
      if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
        
        lmBF(modele1, data=dtrgeasieR)->BF.out
        BF.table<-extractBF(BF.out)[1:2]
        if(length(pred)>1) { for(i in 2:length(pred)){
          modele1<-update(modele1, as.formula(paste0(".~.+",pred[i])))
          lmBF(modele1, data=dtrgeasieR)->BF.out
          BF.table<-rbind(BF.table, extractBF(BF.out)[1:2])
        }
        } 
        Resultats$"Facteurs bayesiens"<-BF.table
        
      }
      
      if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
        
        rlm(formula=modele, data=dtrgeasieR)->modele_robuste
        summary(modele_robuste)->res_modele_robuste
        (1-pt(abs(res_modele_robuste$coefficients[,3]), (length(dtrgeasieR[,1])-1-length(pred)), lower.tail=TRUE))*2->proba
        round(cbind(res_modele_robuste$coefficients, proba),3)->M_estimator
        data.frame(M_estimator)->M_estimator
        noms<-c("b (M estimator)", "SE", "t.value", "p.valeur")
        
        
        if(n.boot>100){ 
          bootReg<-function(formula, dtrgeasieR, i)
          {  d <- dtrgeasieR[i,]
          fit <- lm(formula, data = d)
          return(coef(fit))}
          bootResults<-boot(statistic=bootReg, formula= modele , data=dtrgeasieR, R=n.boot) # cree le bootstrap
          intervalle<-c()
          try(for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "bca", index = i)$bca[,4:5]->IC1
            rbind(intervalle, IC1)->intervalle}, silent=T)
          if(is.null(intervalle)){
            for(i in 1: length(lm.r1$coefficients)){boot.ci(bootResults, type = "perc", index = i)$percent[,4:5]->resultats
              rbind(intervalle, resultats)->intervalle}
            noms<-c(noms, "Percentile.lim.inf", "Percentile.lim.sup")
          } else{
            noms<-c(noms, "Bca.lim.inf", "Bca.lim.sup")
          }
          data.frame(M_estimator, round(intervalle,4))->M_estimator
        }
        names(M_estimator)<-noms
        Resultats$"Statistiques robustes"<-M_estimator
      }  
      
      
      if(CV) CVlm(data=dtrgeasieR, form.lm=modele, m=2, plotit=FALSE)
      
      return(Resultats) 
      
    }
    options (warn=-1) 
    .e <- environment()
    c("BayesFactor","boot","car","DAAG","ggplot2","gsl","lmtest", "MBESS","olsrr","nortest","psych","QuantPsyc","svDialogs")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    Resultats<-list() 
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(class(data)=="data.frame") deparse(substitute(data))->data 
    reg.in.output<-regressions.in(data=data, modele=modele, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf, 
                                  CV=CV, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale, info=info,
                                  sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
    if(is.null(reg.in.output)) return(choix.reg()) 
    data<-reg.in.output$data
    nom<-reg.in.output$nom
    modele<-reg.in.output$modele
    param<-reg.in.output$options$choix
    n.boot<-reg.in.output$options$n.boot
    if(reg.in.output$options$rscalei) rscale<-reg.in.output$options$rscale/2 else rscale<-reg.in.output$options$rscale
    outlier<-reg.in.output$options$desires
    sauvegarde<-reg.in.output$options$sauvegarde
    scale<-reg.in.output$reg.options$scale
    inf<-reg.in.output$reg.options$inf
    CV<-reg.in.output$reg.options$CV
    step<-reg.in.output$reg.options$step
    select.m<-reg.in.output$reg.options$select.m
    method<-reg.in.output$reg.options$method
    criteria<-reg.in.output$reg.options$criteria
    group<-reg.in.output$reg.options$group
    
    
    
    
    
    
    
    
    if(any(outlier==  "Donnees completes")){
      Resultats$"Donnees completes"<-regressions.out(dtrgeasieR=data, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
      if(!is.null(group))   {  
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-regressions.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
          
          R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Resultats$"Donnees completes"$"Analyse par groupe"<-R1
      } 
      
    } 
    if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Donnees sans valeur influente")|inf==T){
      lm.r1<-lm(modele, data)
      as.character(attributes(terms(modele))$variables)->variables
      variables[2:length(variables)]->variables
      plot(lm.r1, which = 5)
      if(inf) {
        influence.measures(lm.r1)->mesure_influence
        data<-data.frame(data, round(mesure_influence$infmat,3))
        data$leverage<-ols_leverage(lm.r1)
        rstandard(lm.r1)->data$res.stand
        rstudent(lm.r1)->data$res.student # idem avec le residu studentise
        data$res.student.p<-2*pt(abs(data$res.student), df=lm.r1$df.residual, lower.tail=F)
        data$res.student.p.Bonf<-p.adjust(data$res.student.p,"bonferroni")
        data$est.inf<-" "
        data[which(apply(mesure_influence$is.inf, 1, any)),"est.inf"]<-"*"
        ols_plot_dfbetas(lm.r1)
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
            if(is.na(sup)) msgBox("Vous devez entrer le numero permettant de savoir quelle observation doit etre supprimee.")  
          }
          if(sup==0) suppression<-"no" else {
            rbind(outliers, nettoyees[which(dimnames(nettoyees)[[1]]==sup),])->outliers
            nettoyees[-which(dimnames(nettoyees)[[1]]==sup),]->nettoyees
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
      length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      if(any(outlier== "Identification des valeurs influentes")){
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retire"=N_retire, "Pourcent.obs.retirees"=Pourcentage_retire)->Resultats$"Synthese du nombre d'observations considerees comme influentes"
        if(length(outliers)!=0) Resultats$"Identification des valeurs influentes"$"Observations considerees comme influentes"<-outliers
        
      }
      if(any(outlier== "Donnees sans valeur influente")) {
        if(N_retire!=0 | all(outlier!="Donnees completes")){
          Resultats$"Donnees sans valeur influente"<-regressions.out(dtrgeasieR=nettoyees, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
          
          if(!is.null(group))   {  
            R1<-list()
            G<-nettoyees[,group]
            if(length(group)>1) G<-as.list(G)
            G<-split(nettoyees, G)
            for(i in 1:length(G)){
              resg<-regressions.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                    sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
              
              R1[[length(R1)+1]]<-resg
              names(R1)[length(R1)]<-names(G)[i]
            }
            Resultats$"Donnees sans valeur influente"$"Analyse par groupe"<-R1
          } 
          
          
        }
      }
    }
    
    
    paste(outlier, collapse="','", sep="")->outlier
    paste(param, collapse="','", sep="")->param
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
    Resultats$Call<-paste0("regressions(data=", nom, ",modele=",  modele, ",outlier=c('", outlier, "'),inf=", inf, ",CV=", CV,",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                           ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),
                           ",criteria=", criteria, ",scale=", scale, ",dial=T, info=T,sauvegarde=", sauvegarde, ",n.boot=", n.boot, ",param=c('", param, "'),rscale=", round(rscale,3), ")")
    
    
    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("regressions.multiples", Sys.time() ))  
    if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix="Regressions.multiples", env=.e)
    Resultats$"References"<-ref1(packages)
    ez.html(Resultats)
    return(Resultats)
  }





.regressions.options<-function(data=NULL, modele=NULL, CV=F, inf=F, select.m="none", method="p", criteria=NULL, step=NULL, group=NULL, scale=T, dial=T,info=T){
  # data : dataframe 
  # modele : formula as it is used in lm
  # CV : logical. Should a cross validation to be performed ? 
  # inf : Logical. Should influential observations be checked ? 
  # select.m : character specifying method of selection. One among "none", "forward", "backward" and "bidirectional"
  # method : if select is different of "none", one among "AIC", "F", or "p"
  # criteria : if method is "F", specify F value to use. If method is "p", specify p value to use as cutoff criteria. 
  # step : list. Each element of the list is a vector with the effect to test at the specific step (see details)
  # group : character. Name of the factor variable definying the groups
  # scale : Logical. Should the predictor be scaled before the analysis (recommended) ? 
  
  Resultats<-list()
  step1<-terms(as.formula(modele))
  
  step2<-as.character( attributes(step1)$variables)[-1]
  step1<-attributes(step1)$term.labels
  if(dial || !is.logical(scale)){
    if(info)   writeLines("Voulez-vous centrer les variables numeriques ? Centrer est generalement conseille (e.g., Schielzeth, 2010).")
    scale<-dlgList(c("Centre", "Non centre"), multiple = FALSE, title="Centrer?")$res
    if(length(scale)==0) return(NULL)
    scale<-ifelse(scale=="Centre",T,F) 
  }
  Resultats$scale<-scale
  if(dial || !is.logical(inf) || !is.logical(CV)) {
    writeLines("Voulez-vous preciser d'autres options ? Vous pouvez en selectionner plusieurs.
               Les methodes de selection permettent de selectionner le meilleur modele sur la base de criteres statistiques.
               Les modeles hierarchiques permettent de comparer plusieurs modeles. 
               Les validations croisees permettent de verifier si un modele n'est pas dependant des donnees. Cette option est a utiliser notamment 
               avec les methodes de selection. L'analyse par groupe permet de realiser la meme regression pour des sous-groupes.
               Les mesures d'influences sont les autres mesures habituellement utilisees pour identifier les valeurs influentes.")
    autres.options<-c("Validation croisee","Mesure d influence",  "aucune")
    if(dim(model.matrix(modele, data))[2]>2) autres.options<-c("Methodes de selection", "Modeles hierarchiques", autres.options)
    if(length(step2)<length(data))  autres.options<-c("analyse par groupes",autres.options)
    
    autres.options<- dlgList( autres.options, preselect=c("aucune"), multiple = TRUE, title="Autres options?")$res 
    if(length(autres.options)==0) return(.regressions.options(data=data, modele=modele))
    # if(any(autres.options=="aucune")) return(Resultats)   
    if(any(autres.options=="Mesure d influence") ) Resultats$inf<-T else  Resultats$inf<-F
    if(any(autres.options=="Validation croisee") ) Resultats$CV<-T else Resultats$CV<-F
  }else{Resultats$inf<-inf
  Resultats$CV<-CV 
  autres.options<-"aucune"
  }
  
  
  if(any(autres.options=="analyse par groupes") || !is.null(group)) {
    
    msg5<-"Veuillez choisissez le facteur de classement categoriel."
    group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=T, message=msg5,  multiple=FALSE, title="Variable-s groupes", out=step2)
    if(length(group)==0) { return(.regressions.options(data=data, modele=modele))}
    data<-group$data
    group<-group$X 
    ftable(data[,group])->groupe.check
    if(any(is.na(groupe.check)) || min(groupe.check)<(length(dimnames(model.matrix(as.formula(modele), data))[[2]])+10)) {
      msgBox("Il faut au moins 10 observations plus le nombre de variables pour realiser l'analyse. Verifiez vos donnees.")
      return(groupe.check)
    }
  }
  
  if(any(autres.options=="Methodes de selection") || select.m!="none" & length(select.m)!=1 | !select.m%in%c("none","forward", "backward", "bidirectional","Forward - pas-a-pas ascendant",
                                                                                                             "Backward- pas-a-pas descendant", "Bidirectionnel")){
    if(info) writeLines("Veuillez choisir la methode de selection que vous souhaitez utiliser")
    select.m<- dlgList(c("Forward - pas-a-pas ascendant","Backward- pas-a-pas descendant", "Bidirectionnel"), 
                       preselect=NULL, multiple = FALSE, title="Choix de la methode")$res
    if(length(select.m)==0) return(.regressions.options(data=data, modele=modele))
  } 
  if(!is.null(method)){
    if(any(autres.options=="Methodes de selection")   || (select.m!="none" && !method%in%c("AIC", "p", "F", "valeur du F","valeur de la probabilite", "AIC - Akaike Information criterion")) ){
      if(info) writeLines("Quel methode faut-il appliquer pour la methode de selection ?")
      method<- dlgList(c("valeur du F","valeur de la probabilite", "AIC - Akaike Information criterion"), 
                       preselect=c("valeur du F"), multiple = FALSE, title="Choix de la methode")$res
      if(length(method)==0) return(.regressions.options(data=data, modele=modele)) 
    }
    
    if(select.m!="none" & (method=="valeur du F" | method=="F")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<1)) {msgBox("Vous devez specifier la valeur du F. Cette valeur doit etre superieure a 1")
        criteria<-NULL}
      
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur du F voulez-vous utiliser ?", 4)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria<1) {criteria<-NULL
          msgBox("Vous devez specifier la valeur du F. Cette valeur doit etre superieure a 1")
          }
          criteria<-df(criteria, df1=1, df2=(length(data[,1])-1-length(step1)), log = FALSE)
        }
      }
    }
    
    if(select.m!="none" & (method=="valeur de la probabilite" | method=="p")){
      if(dial | !is.null(criteria) && (!is.numeric(criteria) || criteria<0 || criteria>1)) {msgBox("Vous devez specifier la valeur de la probabilite. Cette valeur doit etre entre 0 et 1")
        criteria<-NULL}
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur de la probabilite voulez-vous utiliser ?", 0.15)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria>1 || criteria<0 ) {criteria<-NULL
          msgBox("Vous devez specifier la valeur de la probabilite. Cette valeur doit etre entre 0 et 1")}
        }
      }
      
    }
  }
  if(any(autres.options=="Modeles hierarchiques")| !is.null(step)) {
    
    if(!is.null(step) ){
      st1<-unlist(step)
      if(any(table(st1>1))) st1<-"erreur"
      if(any(!st1%in%step1 ))st1<-"erreur"
      if(st1=="erreur"){
        msgBox("Un probleme a ete identifie dans les etapes de votre regression hierarchique")
        step<-NULL
      }
    }         
    if(is.null(step)){
      if(info) writeLines("Veuillez choisir les variables a utiliser pour chaque etape")      
      step<-list()
      step[[1]]<- dlgList(step1, preselect=NULL, multiple = TRUE, title="Variable(s) de cette etape")$res
      if(length(step[[1]])==0) return(.regressions.options(data=data, modele=modele))
      setdiff(step1,step[[1]])->step1
      
      while(length(step1!=0)){
        step[[length(step)+1]]<-dlgList(step1, multiple = TRUE,title="Variable(s) de cette etape")$res
        if(length(step[[length(step)]])==0) return(.regressions.options(data=data, modele=modele))
        setdiff(step1,step[[length(step) ]])->step1
      } 
    }
  } 
  
  Resultats$step<-step
  Resultats$select.m<-select.m
  Resultats$method<-method
  Resultats$criteria<-criteria
  Resultats$group<-group 
  return(Resultats) 
}
