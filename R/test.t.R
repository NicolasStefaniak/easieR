test.t <-
  function(X=NULL, Y=NULL, group=NULL, choix=NULL,
           sauvegarde=F, outlier=c("Donnees completes",  "Identification des valeurs influentes","Donnees sans valeur influente"),  z=NULL, data=NULL,
           alternative="two.sided", mu=NULL, formula=NULL, n.boot=NULL, 
           param=c("test parametrique", "test non parametrique","Test robustes - impliquant des bootstraps", 
                   "Facteurs bayesiens"), info=TRUE, rscale=0.707, html=T){
    # X : Character specifying the dependant variable in dataframe. 
    # Y : character specifying either a two levels factor in dataframe or a numeric variable if paired is TRUE
    # group : Factor vector allowing to decompose analysis by group in one sample t test
    # choix : Character. One among c("Comparaison a une norme", "Deux echantillons apparies","Deux echantillons independants")
    # sauvegarde : logical. Should the results be saved ? 
    # outlier : character. One or several possibilities among c("Donnees completes",   "Identification des valeurs influentes", "Donnees sans valeur influente")
    # z : if NULL and the identification/exclusion of outlier is desired, outlier are identified on Grubbs' test. If z is numeric, outliers are identified on abs(z)
    # data : data on which analysis has to be performed. 
    # alternative : one among c("greater", "lower", "two.sided"). Two sided is default. 
    # formula : a formula of the form dependant.variable~independant.variable
    # n.boot : number of bootstrap. Must be a positive value
    # param : character vector with one or several choices among c("test parametrique", "test non parametrique","Test robustes - impliquant des bootstraps", "Facteurs bayesiens")
    # info : logical. If dialog box are used, Should information be printed in the console
    # rscale : if "Facteurs bayesiens is choosen in "param", rscale is the prior scale. See t.testBF for more information
    
    #### 5 fonctions qui seront appelees pour realiser l'analyse
    test.t.in<-function(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL, mu=NULL){
      
      Resultats<-list()
      if(!is.null(choix)) dial<-F else dial<-T
      if(is.null(choix) || (choix %in%c("Comparaison a une norme", "Deux echantillons apparies","Deux echantillons independants")==FALSE)){
        if(info) writeLines("Veuillez preciser le type de test t que vous souhaitez realiser.")
        choix<-dlgList(c("Comparaison a une norme", "Deux echantillons apparies",
                         "Deux echantillons independants"), preselect=NULL, multiple = FALSE, title="Choix du test t")$res
        if(length(choix)==0) return(NULL)
      }
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]
      if(is.null(Y) || class(data[,Y]) == "factor") format<-"long" else format<-"large"
      
      if(is.null(formula)){
        if(choix=="Deux echantillons apparies"){
          if(dial){
            if(info==TRUE){
              temps1<-1:3
              temps2<-4:6
              data.frame("temps1"=temps1,"temps2"=temps2)->large
              data.frame(c(rep("temps1",3),rep("temps2", 3)), 1:6)->long
              names(long)<-c("moment","mesure")
              writeLines("ceci est le format large")
              print(large)
              writeLines("ceci est le format long")
              print(long)}
            format<-dlgList(c("large", "long"), preselect="large", multiple = FALSE, title="Quel est le format de vos donnees?")$res
            if(length(format)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
          }}  
        if(format=="large") {
          msg3<-"Veuillez choisir le temps 1."
          msg4<-"Veuillez choisir le temps 2."
          title1<-"temps 1"
          title2<-"temps 2"
        } else{
          msg3<-"Veuillez choisir la variable dependante."
          msg4<-"Veuillez choisir la variable independante."
          title1<-"Variable-s dependante-s"
          title2<-"Variable independante"
          
        }
        
        if(choix=="Deux echantillons apparies") {multiple<-F 
        if(length(X)>1){
          msgBox("Il ne peut y avoir qu'une seule variable dependante pour les t de student pour echantillons apparies")
          X<-NULL }}else multiple<-T
          X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=multiple, title=title1, out=NULL)
          if(is.null(X)) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          data<-X$data
          X1<-X$X
          
          if(choix!="Comparaison a une norme"){
            if(choix=="Deux echantillons apparies" && format=="large") type<-"numeric" else type<-"factor"
            Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=FALSE, title=title2, out=X1)
            if(is.null(Y)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)}
            data<-Y$data
            Y<-Y$X 
            if(class(data[,Y])=="factor" && nlevels(data[,Y])!=2) {
              msgBox("Vous devez utiliser une variable independante categorielle a 2 modalites")
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)
            }
          } 
      } else {
        X1<-as.character(formula[2])
        Y<-as.character(formula[3])
      }
      
      
      
      
      
      if(choix=="Deux echantillons apparies"){
        if(format=="large"){
          if(dial){
            if(info==TRUE)writeLines("Veuillez donner un nom a la variable independante. Donner un nom explicite a la variable independante rendra la lecture des resultats plus lisible")
            nomVI <- dlgInput("Quel est le nom de la variable independante?", "Moment")$res
            if(length(nomVI)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
            strsplit(nomVI, ":")->nomVI
            tail(nomVI[[1]],n=1)->nomVI
            if(info==TRUE) writeLines("Veuillez donner un nom a la variable dependante. Donner un nom explicite a la variable dependante rendra la lecture des resultats plus lisible")
            nomVD <- dlgInput("Quel est le nom de la variable dependante?", "Resultat")$res
            if(length(nomVD)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
          } else {
            nomVD<-"Resultat"
            nomVI<-"Moment"
          }
          strsplit(nomVD, ":")->nomVD
          tail(nomVD[[1]],n=1)->nomVD
          data[complete.cases(data[,c(X1, Y)]),]->data
          data$IDeasy<-paste0("p", 1:length(data[,X1]))
          melt(data=data, measure.vars=c(X1,Y) , variable.name=nomVI, value.name=nomVD)->data
          assign(x=paste0(nom,".format.long"), value=data, envir=.GlobalEnv)
          X1<-nomVD
          Y<-nomVI
        }
        if(format=="long") {
          if( length(unique(table(data[,Y])))!=1) {
            msgBox("Le nombre d'occurrence pour chaque modalite de votre variable independante n'est pas identique. Veuillez choisir un identifiant participant") 
            msg4<-"Veuillez choisir la variable identifiant les participants"
            ID<-.var.type(X=NULL, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=multiple, title="Variable *Identifiant*", out=c(X1,Y))
            if(is.null(ID)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)}
            ID<-ID$X 
            ID.fail<-names(which(table(data[,ID])!=2))
            data<-data[which(data[,ID]!=ID.fail),]
            data<-data[order(data[,c(Y,ID)]), ]
          } else {
            data[order(data[,Y]),]->data
            data$IDeasy<-rep(paste0("p", 1:(length(data[,X1])/2)), 2) 
          }
        }
        
      }
      
      if(choix=="Comparaison a une norme"){
        writeLines("Veuillez specifier la valeur de la norme")
        if(class(mu) !="numeric") mu<-NA
        while(is.na(mu)){
          mu <- dlgInput("Quelle est la valeur de la norme ?", 0)$res
          if(length(mu)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          strsplit(mu, ":")->mu
          tail(mu[[1]],n=1)->mu
          as.numeric(mu)->mu
          if(is.na(mu)) msgBox("La norme doit etre une valeur numerique.")  
        }
        if(dial){
          
          
          if(info==TRUE) writeLines("Une analyse bilaterale teste l'existence d'une difference. Le choix superieur teste si la moyenne est strictement superieure
                                    \n Le choix inferieur teste l'existence d'une difference strictement inferieure")
          dlgList(c("Bilateral", "Superieur", "Inferieur"), preselect=NULL, multiple = FALSE, title="Comparaison de moyennes")$res->alternative
          if(length(alternative)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)
          } else car::recode(alternative, "'Bilateral'= 'two.sided';'Superieur'='greater'; 'Inferieur'='less'")->alternative
          
          if(info==TRUE) writeLines("Si vous souhaitez realiser l'analyse pour differents sous-echantillons en fonction d'un critere categoriel (i.e; realiser une analyse par groupe)
                                    \n choisissez oui. Dans ce cas, l'analyse est realisee sur l'echantillon complet et sur les sous-echantillons.
                                    \n Si vous desirez l'analyse pour l'echantillon complet uniquement, chosissez non.
                                    \n l'analyse par groupe ne s'appliquent pas aux statistiques robustes.")
          dlgList(c("oui", "non"), preselect="non", multiple = FALSE, title="Analyse par groupe?")$res->par.groupe
          if(length(par.groupe)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)
          } 
          msg5<-"Veuillez choisissez le facteur de classement categoriel."
          if(par.groupe=="oui"){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=FALSE, title="Variable-s", out=X1)
          if(length(group)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                                           formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          data<-group$data
          group<-group$X 
          }
        }
      }
      msg.options1<-"Le test parametrique est le test t classique"
      msg.options2<- "Le test non parametrique est le test de Wilcoxon (ou Mann-Whitney)"
      
      options<-.ez.options(options=c("choix","outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, 
                           choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
      if(is.null(options)){
        test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided", 
                  formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
        return(Resultats)
      }
      Resultats$choix<-choix
      Resultats$nom<-ifelse(format=="large", paste0(nom,".format.long"), nom)
      Resultats$data<-data
      Resultats$X<-X1
      if(exists("Y")) Resultats$Y<-Y
      if(exists("mu")) Resultats$mu<-mu
      if(exists("alternative")) Resultats$alternative<-alternative
      if(exists("group")) Resultats$group<-group
      Resultats$options<-options
      return(Resultats)
      }
    
    norme<-function(X, mu, data, param=c("param", "non param", "robustes"), group=NULL, alternative="two.sided", n.boot=NULL, rscale=0.707){
      if(class(data)!="data.frame") {data<-data.frame(data)
                                     names(data)[1]<-X}
      Resultats<-list()
      .e <- environment()
      Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=NULL, data=data, tr=.1, type=3, plot=F)
      cutoff <- data.frame(x = c(-Inf, Inf), y = mu, cutoff = factor(mu) )
      p2<- ggplot(data)
      p2<-p2+ eval(parse(text=paste0("aes(x=factor(0), y=", X,")"))) + geom_violin()
      p2<-p2+geom_line(aes( x, y, linetype = cutoff ), cutoff)
      p2<-p2+ labs( x=" ")
      p2<-p2 + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
      p2<-p2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)
      p2<-p2 + theme(legend.position="none")
      p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("Moyenne et ecart-type")
      # print(p2)
      Resultats$"statistiques descriptives"$Graphique<-p2
      
      if(!is.null(group)) {Resultats$"statistiques descriptives par groupe"<-.stat.desc.out(X=X, groupes=group, data=data, tr=.1, type=3, plot=T) }
      if(any(param=="param") | any(param=="Test parametrique")){
        Resultats$"Tests de normalite"<-.normalite(data=data, X=X, Y=NULL)
        t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(data[,X], mu=mu)->dc
        data.frame("t test"=round(ttest$statistic,3), "ddl"=ttest$parameter, "valeur.p"=round(ttest$p.value,4), "Lim.inf.IC"=ttest$conf.int[[1]], "Lim.sup.IC"=ttest$conf.int[[2]], 
                   "R.carre"=round(R_carre,4), "D Cohen"=round(dc,3))->ttest
        dimnames(ttest)[1]<-" "
        ttest->Resultats$"Test de Student - comparaison a une norme"
        if(!is.null(group)){
          data<-data[complete.cases(data[,group]),]
          func <- function(data, moy=mu){ 
            t.test(data, mu = moy)->ttest
            ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
            cohensD(data[,1], mu=moy)->dc
            return(data.frame(test.t=round(ttest$statistic,3), 
                              ddl=ttest$parameter, 
                              valeur.p=round(ttest$p.value,4), 
                              IC.inf=ttest$conf.int[[1]], 
                              IC.sup=ttest$conf.int[[2]], 
                              "R.carre"=round(R_carre,4), 
                              D.Cohen=round(dc,3)))}
          data.frame(data[,X])->Y
          
          ddply(.data=Y, .(data[,group]), func)->t.groupes
          t.groupes->Resultats$"t de Student par groupe"}}
      
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
        if(all(param!="param") & all(param!="Test parametrique")) Resultats$"Tests de normalite"<-.normalite(data=data, X=X, Y=NULL)
        
        BF<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("En faveur de l'hypothese alternative", "En faveur de l'hypothese nulle")
        Resultats$"Facteurs Bayesiens"<-BF
        if(!is.null(group)){
          func <- function(data, moy=mu, scale=rscale){ 
            ttestBF(data, mu = moy, rscale=scale)->BF
            BF<-extractBF(BF, onlybf=F)
            return(data.frame("Facteur bayesien"=round(BF$bf,5), "Erreur"=round(BF$error,5)))
          }
          BFgroup<-tapply(X=data[,X], data[,group], func,scale=rscale, moy=mu)
          BFgroup<-matrix(unlist(BFgroup), ncol=2, byrow=T)
          dimnames(BFgroup)<-list(levels(data[,group]), c("FB", "erreur"))
          BFgroup->Resultats$"Facteur bayesien par groupe"
        }
        samples<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,"mu"])
        
        
        bfs<-c()
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=0.707)
          bfl <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1)
          bful <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs, 
                        "rscale"=factor(rep(c("moyen", "large", "ultra large"), length.out= 3*(length(data[,X])-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats$"Facteurs bayesiens sequentiels"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      
      if(any(param=="non param")| any(param=="Test non parametrique")){
        
        wilcox.test(x= data[,X], y = NULL, alternative = alternative, mu = mu, paired = FALSE, exact = T,  
                    conf.int = TRUE, conf.level = 0.95)
        WT<-wilcox.test(data[,X],y=NULL, mu=mu, alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
        
        if(!is.null(group)){
          func <- function(data,Y=X, moy=mu, alt=alternative){
            WT<-wilcox.test(data[,Y],mu=moy, alternative=alt)
            if(alt!="two.sided") abs( qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
            r<-z/(length(data[,X]))^0.5
            return(data.frame(Wilcoxon.W=WT$statistic, valeur.p=round(WT$p.value,4), z=round(z,4), r=round(r,4)))
          }
          
          ddply(.data=data, .(data[, group]), func)->Wilcox.groupes
          Wilcox.groupes->Resultats$"Wilcoxon par groupe"
        }
      }
      
      if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps"))){
        try( round(unlist(WRS::trimci(data[,X],tr=.2,alpha=.05, null.value=mu)),4), silent=T)->m.tr
        if(m.tr!="try-error"){
          names(m.tr)<-c("lim.inf.IC","lim.sup.IC", "M.tronquee","test.t", "se","valeur.p","n")
          m.tr->Resultats$'Test sur la moyenne tronquee a 0.2' 
          data[,X]->x
          try(WRS::trimcibt(x, tr=.2,alpha=.05,nboot=n.boot,plotit=T,op=3)$ci, silent=T)->trimci
          try(WRS::mestci(x,alpha=.05,nboot=n.boot,bend=1.28,os=F),silent=T)->M.estimator
          try(WRS:: momci(x,alpha=.05,nboot=n.boot),silent=T)->MoM
          IC.robustes<-data.frame()
          if(class(trimci)!="try-error") {IC.robustes<-rbind(IC.robustes,trimci)
          dimnames(IC.robustes)[[1]][1]<-"bootstrap-t method"}
          if(class(M.estimator)!="try-error") {IC.robustes<-rbind(IC.robustes,M.estimator$ci)
          dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator"}
          if(class(MoM)!="try-error") {IC.robustes<-rbind(IC.robustes,MoM$ci)
          dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator modifie"}
          if(all(dim(IC.robustes)!=0)) names(IC.robustes )<-c("lim.inf.IC", "lim.sup.IC")
          Resultats$Robustes<-IC.robustes
          c("Le bootstrap-t method est un bootstrap adapte au calcul de la moyenne tronquee", 
            " Cet indice est adapte dans la plupart des situations. Le M-estimator modifie doit etre prefere pour N<20",
            "La troncature sur le M-estimator s'adapte en fonction des caracteristiques de l'echantillon.")->Resultats$infos
        } else Resultats$Robustes<-"Les statistiques robustes n'ont pu etre realisees. Verifiez que le packages WRS est correctement installe"
      }
   
      return(Resultats)
    }
    apparies<-function(X, Y, data=NULL, param=c("param", "non param", "robustes"),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Resultats<-list()
      .e <- environment()
      Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      large<-data.frame("t1"=data[which(data[,Y]==levels(data[,Y])[1]), X], "t2"=data[which(data[,Y]==levels(data[,Y])[2]), X])
      if(any(param=="param") | any(param=="Test parametrique")){
        large$diff<--large$t2-large$t1
        Resultats$"Tests de normalite"<-.normalite(data=large, X="diff", Y=NULL)
        t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(x= large[,1], y=large[,2], method="paired")->dc
        data.frame("t test"= round(ttest$statistic,3), "ddl"= ttest$parameter, "valeur.p"= round(ttest$p.value,4), "Lim.inf.IC"= ttest$conf.int[[1]], 
                   "Lim.sup.IC"=ttest$conf.int[[2]], "R.carre"=round(R_carre,4), "D de Cohen"=round(dc,3))->ttest
        dimnames(ttest)[1]<-" "
        ttest->Resultats$"Test de Student - comparaison de deux echantillons apparies"}
      if(any(param=="param") | any(param=="Test parametrique", any(param=="Bayes") | any(param=="Facteurs bayesiens"))) {
        # realisation du graphique
        X1<-which(names(data)==X)
        nonaj<-ggplot(data) 
        nonaj<- nonaj+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        # aes(x=data[,Y], y=data[,X1]))+labs(x=Y, y=X)+
        nonaj<- nonaj+ stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        nonaj<-nonaj+theme(plot.title = element_text(size = 12))+ggtitle("Donnees non ajustees")
        # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
        Resultats$"Moyenne et ecart-type pour les donnees non ajustees"<-nonaj
        large$meanD2<-(large[ ,1]+large[ ,2])/2
        mean(large$meanD2)->GMean
        GMean-large$meanD2->large$adj
        large$adjM1<-large[ ,1]+large$adj
        large$adjM2<-large[ ,2]+large$adj
        data[,paste0(X, ".ajustee")]<-c(large$adjM1,large$adjM2)
        
        aj<-ggplot(data)
        aj<-aj+eval(parse(text=paste0("aes(x=", Y, ", y=", names(data)[length(data)],")")))
        aj<-aj+labs(x=Y, y=X)+stat_summary(fun.y=mean, geom="bar", 
                                           fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        aj<-aj+theme(plot.title = element_text(size = 12))+ggtitle("Donnees ajustees (Loftus & Masson, 1994)")
        Resultats$"Moyenne et ecart-type pour les donnees ajustees"<-aj
        .multiplot(nonaj,aj, cols=2 )
      }
      
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
        if(all(param!="param") & all(param!="Test parametrique")) Resultats$"Tests de normalite"<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("En faveur de l'hypothese alternative", "En faveur de l'hypothese nulle")
        Resultats$"Facteurs Bayesiens"<-BF
        
        samples<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])
        
        
        bfs<-c()
        for (i in 5:(length(data[,X])/2)) {
          bfm <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE, rscale=0.707)
          bfl <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1)
          bful <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:(length(data[,X])/2), each=3 ),"BF"= bfs, 
                        "rscale"=factor(rep(c("moyen", "large", "ultra large"), length.out= 3*((length(data[,X])/2)-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats$"Facteurs bayesiens sequentiels"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      if(any(param=="non param")| any(param=="Test non parametrique")) {
        WT<-wilcox.test(as.formula(paste0(X, "~",Y)), paired=T,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
      }
      
      if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps")) ){
        try(WRS::yuend(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=.2),silent=T)->moy.tr
        if(class(moy.tr)!="try-error"){
          round(unlist(moy.tr),3)->moy.tr
          names(moy.tr)<-c("IC Inf","IC Sup", "valeur.p", "Moyenne1", "Moyenne2", "Difference","se", "Stat", "n", "ddl") 
          if(n.boot>99){
            WRS::ydbt(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=0.2, nboot=n.boot)->moy.tr.bt
            moy.tr->Resultats$Robustes$"Comparaison basee sur les moyennes tronquees"
            round(unlist(moy.tr.bt),4)->Resultats$Robustes$"bootstrap studentise sur les moyennes tronquees"
            if(length(data[,1])>20) {
              try({WRS::bootdpci(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], 
                                                   nboot=n.boot, BA=T)$output[,2:6]->Mest
                names(Mest)<-c("statistique", "valeur.p", "p.crit", "CI inf", "CI sup")
              Mest->Resultats$Robustes$"Bootstrap de type BCa sur le M-estimator"}
                , silent=T)
              }}} else Resultats$Robustes<-"Les statistiques robustes n'ont pas pu etre realisees"
      }
      
      
      
      
      
      return(Resultats)                                                                               
    }  
    indpdts<-function(X, Y, data, param=c("param", "non param","robustes"),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Resultats<-list()
      .e <- environment()
      Resultats$"statistiques descriptives"<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      as.formula(paste0(X," ~ ",Y))->modele
      if(any(param=="param") | any(param=="Test parametrique")){
        Resultats$"Tests de normalite"<-.normalite(data=data, X=X, Y=Y)
        car::leveneTest(data[ ,X], data[ ,Y])->Levene # test de Levene pour homogeneite des variances
        round(unlist(Levene)[c(1,2,3,5)],3)->Levene
        names(Levene)<-c("ddl1","ddl2","F","valeur.p")
        Levene->Resultats$"Test de Levene verifiant l'homogeneite des variances"
        t.test(modele, data=data, alternative=alternative,  var.equal=TRUE, conf.level=0.95)->student
        round(student$statistic^2/(student$statistic^2+student$parameter),3)->R.deux
        d_cohen<-round(cohensD(modele , data=data, method = "pooled"),3)
        data.frame(student[9], round(student$statistic,3), student$parameter, round(student$p.value,3), round(student$conf.int[1],4),
                   round(student$conf.int[2],4),  R.deux, d_cohen)->student
        t.test(modele, data=data, alternative=alternative,  var.equal=FALSE, conf.level=0.95)->corrige
        corrige$statistic^2/(corrige$statistic^2+corrige$parameter)->R.deux.corr
        d_cohen.corr<-cohensD(modele , data=data, method = "unequal")
        data.frame(corrige[9], round(corrige$statistic,3), round(corrige$parameter,3), round(corrige$p.value,3), round(corrige$conf.int[1],4),
                   round(corrige$conf.int[2],4),  R.deux, d_cohen)->corrige
        names(student)<-c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
        names(corrige)<- c("modele", "test t", "ddl", "valeur.p", "lim.inf.IC", "lim.sup.IC","R.carre","d de Cohen")
        student<-rbind(student, corrige)
        dimnames(student)[[1]]<-c("sans correction de Welch","avec correction de Welch")
        student->Resultats$"t de student pour echantillons independants"
        p<-ggplot(data)
        p<-p+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        p<-p+  stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        Resultats$"Representation graphique - Moyenne et ecart-type"<-p
        
      }
      if(any(param=="Bayes") | any(param=="Facteurs bayesiens") ){
        if(all(param!="param") & all(param!="Test parametrique")) Resultats$"Tests de normalite"<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame("Facteur bayesien"=c(round(BF$bf,5), round((1/BF$bf),5)), "Erreur"=round(c( BF$error, BF$error),5))
        dimnames(BF)[[1]]<-c("En faveur de l'hypothese alternative", "En faveur de l'hypothese nulle")
        Resultats$"Facteurs Bayesiens"<-BF
        
        samples<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])
        
        
        bfs<-c()
        tab<-table(data[,Y])
        data1<-data.frame(X=c(data[which(data[,Y]==levels(data[,Y])[1] ),X], data[which(data[,Y]==levels(data[,Y])[2] ),X]), id=c(1:tab[1],1:tab[2]), 
                          Y=c(rep(levels(data[,Y])[1], tab[1]), rep(levels(data[,Y])[2], tab[2])))
        data1<-data1[order(data1$id),]
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(formula=X~Y,data=data1[1:i,], paired=FALSE, rscale=0.707)
          bfl <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1)
          bful <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }
        
        SBF<-data.frame("n"=rep(5:(length(data[,X])), each=3 ),"BF"= bfs, 
                        "rscale"=rep(c("moyen", "large", "ultra large"), length.out= 3*(length(data[,X])-4) ))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats$"Facteurs bayesiens sequentiels"<-.plotSBF(SBF)
        
        ##### Debut du graphique  Bayes Factor Robustness Check     
        
        # what is the t-value for the data?
        tVal <-  t.test(formula=modele, data=data, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)
        
        # get the Bayes factor for each prior value
        
        bayesFactors <- sapply(cauchyRates, function(x) 
          exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = x)[['bf']]))
        
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", 
             ylim = c(0, max(bayesFactors)), xaxt = "n", 
             xlab = "Cauchy Prior Width (r)", ylab = "Bayes Factor (10)")
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")
        
      }
      if(any(param=="non param")| any(param=="Test non parametrique")) {
        WT<-wilcox.test(modele, paired=F,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats$"test de Mann-Whitney - Wilcoxon"<- data.frame("Wilcoxon W"=WT$statistic, "valeur.p"=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                                                 "lim.inf.IC"=WT$conf.int[1],"lim.sup.IC"=WT$conf.int[2])
      }
      
      if(any(param=="robustes"| any(param=="Test robustes - impliquant des bootstraps")) ){
        data[which(data[,Y]==levels(data[,Y])[1]),]->g1 # on cree une base de Donnees avec le groupe 1 uniquement (sans valeur aberrantes)
        data[which(data[,Y]==levels(data[,Y])[2]),]->g2 # on cree une base de Donnees avec le groupe 2 uniquement (sans valeur aberrantes)
        try(WRS::yuen(g1[,X],g2[,X]), silent=T)->yuen.modele### fournit la probabilite associee a des moyennes tronquees.Par defaut, la troncature est de 0.20
        if(class(yuen.modele)!="try-error"){
          round(unlist(yuen.modele),4)->yuen.modele
          cbind(yuen.modele[1:2], yuen.modele[3:4])->yuen.desc
          dimnames(yuen.desc)[[1]]<-levels(data[,Y])
          dimnames(yuen.desc)[[2]]<-c("n", "moyennes tronquees")
          yuen.desc->Resultats$Robustes$"statistiques descriptives"
          
          yuen.modele[c(5,6,8,9,10,11,12,7)]->yuen.modele
          names(yuen.modele)<-c("lim.inf.IC", "lim.sup.IC", 
                                "Difference","Err-type","Stat", "Seuil", "ddl","valeur.p")
          yuen.modele->Resultats$Robustes$"Analyse sur les moyennes tronquees"
          if(n.boot>99){
            WRS::yuenbt(g1[,X],g2[,X], nboot=n.boot, side=T)->yuen.bt.modele ### fournit la probabilite associee a des moyennes tronquees apres un bootstrap.
            round(unlist(yuen.bt.modele)[1:4],4)->yuen.bt.modele
            names(yuen.bt.modele)<-c("lim.inf.IC", "lim.sup.IC", "Stat", "valeur.p")
            yuen.bt.modele->Resultats$Robustes$"Bootstrap utilisant la methode t sur les moyennes tronquees"
            WRS::pb2gen(g1[,X],g2[,X], nboot=n.boot)->pb2gen.modele### calcule le bootstrap sur le M-estimateur et fournit l intervalle de confiance. 
            round(unlist(pb2gen.modele)[1:6],4)->pb2gen.modele
            names(pb2gen.modele)<-c("M.estimaror.G1", "M.estimator.G2", "diff", "lim.inf.IC", "lim.sup.IC", "valeur.p")
            pb2gen.modele->Resultats$Robustes$"Percentile bootstrap sur les M-estimator"
            Resultats$Robustes$Informations<-c("la methode du percentile bootstrap doit etre preferee pour les petits echantillons",
                                               "Pour des echantillons plus importants, les boostrap utilisant la methode t doit etre preferee.") 
          }
          
          WRS::ks(g1[,X],g2[,X],w=F,sig=T)->KS
          round(unlist(KS),4)->KS
          names(KS)<-c("KS", "Seuil.critique","valeur.p")
          KS->Resultats$Robustes$"Test de Kolmogorov-Smirnov comparant deux distributions"
        }else Resultats$"Statistiques robustes"<-"Les statistiques robustes n'ont pas pu etre realisees. Verifiez l'installation du package WRS"
        
        
      }
      
      return(Resultats)
    }
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))
    }
    #### 5 fonctions qui seront appelees pour realiser l'analyse
    options (warn=-1) 
    # chargement des packages
    packages<-c("BayesFactor", "svDialogs", "outliers", "nortest","psych", "lsr","ggplot2", "reshape2", "car", "plyr")
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    try(library("WRS"),silent=T)
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=="try-error") quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data 
    test.t.options<-test.t.in(X=X, Y=Y, data=data, choix=choix, param=param, outlier=outlier, sauvegarde=sauvegarde, info=info, group=group,alternative=alternative, 
                              formula=formula,n.boot=n.boot, rscale=rscale, mu=mu) 
    if(is.null(test.t.options)) return(analyse())
    
    choix<-test.t.options$choix
    X<-test.t.options$X
    Y<-test.t.options$Y
    mu<-test.t.options$mu
    group<-test.t.options$group
    data<-test.t.options$data
    alternative<-test.t.options$alternative
    group<-test.t.options$group
    param<-test.t.options$options$choix
    rscale<-test.t.options$options$rscale
    n.boot<-test.t.options$options$n.boot
    sauvegarde<-test.t.options$options$sauvegarde
    outlier<-test.t.options$options$desires
    
    for(i in 1 : length(X)) {
      
      
      if(choix=="Deux echantillons apparies"){
        diffs<-data[which(is.na(data[,X])), "IDeasy"]
        if(length(diffs)==0) data->data1 else data[which(data$IDeasy!=diffs), ]->data1 
      } else  {
        data1<-data[complete.cases(data[,c(Y,X[i])]),]
      }
      
      
      
      
      X1<-X[i]
      R1<-list()
      if(any(outlier==  "Donnees completes")){
        switch(choix,  "Comparaison a une norme"=  R1$"Donnees completes"<-norme(X=X1, mu=mu, data=data1, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
               "Deux echantillons apparies"=R1$"Donnees completes"<-apparies(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
               "Deux echantillons independants"= R1$"Donnees completes"<-indpdts(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
      }

      if(any(outlier=="Identification des valeurs influentes")|any(outlier=="Donnees sans valeur influente")){
        if(choix=="Comparaison a une norme") {
          if(class(data1)!="data.frame"){data1<-data.frame(data1)
                                       names(data1)[1]<-X1}
          data1$residu<-data1[,X1]
                                             }else data1$residu<-unlist(tapply(data1[,X1], data1[,Y], scale, center=T, scale=F))
        critere<-ifelse(is.null(z), "Grubbs", "z")
        valeurs.influentes(X="residu", critere=critere,z=z, data=data1)->influentes
      }
      if(any(outlier== "Identification des valeurs influentes")){influentes->R1$"Valeurs influentes"}
      if(any(outlier== "Donnees sans valeur influente")) {
        if(length(influentes$"observations influentes")!=0 | all(outlier!="Donnees completes")){
          
          if(choix=="Deux echantillons apparies"){
            setdiff(data$IDeasy,influentes$"observations influentes"$IDeasy)->diffs
            data[which(data$IDeasy%in%diffs), ]->nettoyees
          } else  get("nettoyees", envir=.GlobalEnv)->nettoyees
          
          ### Regler le souci pour les echantillons apparies
          switch(choix,  "Comparaison a une norme"=  R1$"Donnees sans valeur influente"<-norme(X=X1, mu=mu, data=nettoyees, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale), 
                 "Deux echantillons apparies"=R1$"Donnees sans valeur influente"<-apparies(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale),
                 "Deux echantillons independants"= R1$"Donnees sans valeur influente"<-indpdts(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale))
        }
      }
      Resultats[[i]]<-R1
    }
    
    names(Resultats)<-paste("Analyse sur la variable", X)
    
    paste(unique(X), collapse="','", sep="")->X
    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("test.t(X=c('", X,
                           "'), Y=", ifelse(!is.null(Y),paste0("'",Y,"'"), "NULL"), 
                           ",group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), 
                           ", choix='", choix, 
                           "', sauvegarde = ", sauvegarde, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                           ", data=", test.t.options$nom, ",alternative='", alternative, "', mu=", ifelse(!is.null(mu),mu, "NULL"),
                           ",formula =NULL, n.boot=", ifelse(is.null(n.boot), "NULL", n.boot), ",param=c('", param, "'),info=T, rscale=", rscale, ")"
    )
    .add.history(data=data, command=Resultats$Call, nom=test.t.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))
    
    if(sauvegarde){save(Resultats=Resultats ,choix =choix, env=.e)}
    
    ref1(packages)->Resultats$"References"
    if(html) ez.html(Resultats)
    ### Obtenir les Resultats
    return(Resultats) 
    
    }
