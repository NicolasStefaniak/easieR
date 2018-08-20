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
    
    regressions.out<-function(data=NULL, modele=NULL,  VC=F, select.m="none", method=NULL, step=NULL, group=NULL, criteria=NULL , scale=T,
                              sauvegarde=F, n.boot=NULL, param=NULL, rscale=0.353){
      
      Resultats<-list()
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels
      Resultats$"Statistiques descriptives"<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)
      
      if(scale==T || scale=="Centre") {Resultats$info<-"En accord avec les recommandations de Schielzeth 2010, les donnees ont ete prealablement centrees"
      
      which(sapply(data[,pred[which(pred %in% variables)]],class)!="factor")->centre
      if(length(centre)==1) data[,names(centre)]-mean(data[,names(centre)],na.rm=T)->data[,names(centre)] else{
        sapply(X=data[,names(centre)], fun<-function(X){X-mean(X, na.rm=T)})->data[,names(centre)]
      }
      }
      
      
      mod<-list()
      modele1<-as.formula(paste0(variables[1], "~", pred[1]))
      lm( modele1,na.action=na.exclude, data=data)->lm.r1
      lm.r1->mod[[1]]
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(lm.r1, as.formula(paste0(".~.+",pred[i])))->lm.r1
          lm.r1->mod[[i]]}
      }
      resid(lm.r1)->data$residu
      Resultats$"Tests de normalite"<-.normalite(data=data, X="residu", Y=NULL)
      if(length(variables)>1)  {
        cont<-variables[which(sapply(data[,variables],class)!="factor")]
        Resultats$"Normalite multivariee"<-.normalite(data=data, X=cont, Y=NULL)
        vif(lm.r1)->FIV # calcul du facteur d inflation de la variance 
        Resultats$"Tests de multicolinearite"$Tests<-data.frame("Tolerance"=round(1/FIV,4) , FIV= round(FIV,4))
        Resultats$"Tests de multicolinearite"$Information<-"FIV : facteur d'inflation de la variance"
        dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
        Resultats$"Test de Durbin-Watson - autocorrelations"<-round(data.frame("Autocorrelation"=DWT.results[1],"statistique de D-W"=DWT.results[2],"valeur p"=DWT.results[3]),4)->DWT.results
        ncvTest(lm.r1)->var.err
        Resultats$"Verification de la non-constance de la variance d'erreur (test de Breusch-Pagan)"<-data.frame(chi=var.err$ChiSquare,
                                                                                                                 ddl=var.err$Df,valeur.p=var.err$p)
        
        
        try(ceresPlots(lm.r1, main="Graphique de Ceres testant la linearite"), silent=T)
      }
      if(select.m!="none"){
        if(method %in% c("F", "valeur du F", "p", "valeur de la probabilite")){
          select.m<-switch(select.m,"Forward - pas-a-pas ascendant"="Forward", "Backward- pas-a-pas descendant"="Backward", "Bidirectionnel"="Stepwise",
                           "forward"="Forward", "bidirectional"="Stepwise","backward"="Backward" )
          select.m.out<-mle.stepwise(modele, data, type=select.m, model=T,f.in=criteria, x=T, y=T) 
          select.m.out<-select.m.out$step
          if(any(select.m.out!=0)){
            if(!is.null(dim( select.m.out))) {data.frame(select.m.out)->select.m.out}else t(as.matrix(select.m.out1$step))->select.m.out
            
            names(select.m.out)[length(select.m.out)]<-"F d'entree"
            dimnames(select.m.out)[[1]]<-paste("etape", 1:length(select.m.out[,1]))
            Resultats$"Methode de selection"<-select.m.out 
          } else  Resultats$"Methode de selection"<-"Aucune variable n'a ete retenue par la methode de selection. L'analyse est realisee sur l'ensemble des predicteurs."
        }
        
        if(method %in% c("AIC - Akaike Information criterion","AIC")){ 
          select.m<-switch(select.m,"Forward - pas-a-pas ascendant"="forward", "Backward- pas-a-pas descendant"="backward", "Bidirectionnel"="both",
                           "forward"="forward", "bidirectional"="both","backward"="backward" )
          lm.r1<-lm(modele, data=data)
          steps<-stepAIC(lm.r1, direction=select.m) 
          Resultats$"Methode de selection - criteres d'information d'Akaike"<-steps$anova
          modele<-as.formula(attributes(steps$anova)$heading[5])
          pred<-attributes(terms(modele))$term.labels
          
        }
        
        if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
          BF.out<-try(regressionBF(modele, data=data,progress=F, rscaleCont=rscale), silent=T)
          if(class(BF.out)!="try-error") {
            plot(BF.out) 
            Resultats$"Methodes de selection : facteurs bayesiens"<-head(BF.out)
          } else Resultats$"Methodes de selection : facteurs bayesiens"<-"Les methodes de selection pour les facteurs bayesiens ne s'appliquent pas pour des modeles complexes."
        } 
      }
      
      if(!is.null(step)){
        
        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
        list()->modele.H1
        list()->formule.H1
        for(i in 1:length(step)){
          
          for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
          formule.H1[[i]]<-modele.H
          lm(modele.H, data=data, na.action=na.exclude )->lm.H
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
          BF<-lmBF(formula= as.formula(formule.H1[[1]]), data=data, rscaleFixed=rscale)
          BF.modele<-extractBF(BF, onlybf=T)
          BF.hier<-c(NA)
          for(i in 2:length(formule.H1)){
            numBF<-lmBF(formula= as.formula(formule.H1[[i]]), data=data, rscaleFixed=rscale)
            BF.modele<-c(BF.modele, extractBF(numBF, onlybf=T))
            denomBF<-lmBF(formula= as.formula(formule.H1[[i-1]]), data=data, rscaleFixed=rscale)
            OddBF<-numBF/denomBF
            BF.hier<-c(BF.hier, extractBF(OddBF, onlybf=T))}
          
          # BF.out[formule.H1[[i]]]/BF.out[formule.H1[[i-1]]]->BF.comp
          
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
        
      }
      
      if(any(param=="Bayes")|any(param=="Facteurs bayesiens")){
        
        lmBF(modele1, data=data)->BF.out
        BF.table<-extractBF(BF.out)[1:2]
        if(length(pred)>1) { for(i in 2:length(pred)){
          modele1<-update(modele1, as.formula(paste0(".~.+",pred[i])))
          lmBF(modele1, data=data)->BF.out
          BF.table<-rbind(BF.table, extractBF(BF.out)[1:2])
        }
        } 
        Resultats$"Facteurs bayesiens"<-BF.table
        
      }
      
      if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
        
        rlm(formula=modele, data=data)->modele_robuste
        summary(modele_robuste)->res_modele_robuste
        (1-pt(abs(res_modele_robuste$coefficients[,3]), (length(data[,1])-1-length(pred)), lower.tail=TRUE))*2->proba
        round(cbind(res_modele_robuste$coefficients, proba),3)->M_estimator
        data.frame(M_estimator)->M_estimator
        noms<-c("b (M estimator)", "SE", "t.value", "p.valeur")
        
        
        if(n.boot>100){ 
          bootReg<-function(formula, data, i)
          {  d <- data[i,]
          fit <- lm(formula, data = d)
          return(coef(fit))}
          bootResults<-boot(statistic=bootReg, formula= modele , data=data, R=n.boot) # cree le bootstrap
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
      
      
      if(CV) CVlm(data=data, form.lm=modele, m=2, plotit=FALSE)
      
      return(Resultats) 
      
    }
    options (warn=-1) 
    .e <- environment()
    c("BayesFactor","boot","car","DAAG","ggplot2","gsl","MASS", "MBESS","nortest","psych","QuantPsyc","svDialogs")->packages
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
      Resultats$"Donnees completes"<-regressions.out(data=data, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
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
          Resultats$"Donnees sans valeur influente"<-regressions.out(data=nettoyees, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
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
