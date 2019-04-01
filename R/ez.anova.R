ez.anova<-function(data=NULL, DV=NULL, between=NULL, within=NULL,id=NULL, cov=NULL, RML=NULL, 
                   RML.factor=NULL, param=c("param","bayes"),outlier=c("complete","id", "removed"), 
                   ES="ges", SumS="3", save=F , html=T, contrasts="none",p.adjust="none", n.boot=1000, rscaleFixed = 0.5, rscaleRandom = 1 ){
  # data = a data frame
  # between = character. Names of the between-participant variables 
  # within = character.  Names of the within-participant variables. Must be factor. Use for data in long format. 
  # id = character. Name of the variable that identify multiple records from the same individual. Required only if within is not NULL
  # cov = character. names of the covarables. 
  # RML = character with length >= 2. Repeated measure levels. All the columns that corresponds to repeated measures in the wide format
  # RML.factor = list. The names in the list corresponds to the names of the factors and the values 
  #              at each level correspond to the levels of each factor. The product of the number of levels must equal the length of RML. 
  #              If RML is not NULL and RML.factor is NULL, it is assumed that there is only one factor and the name of the factor is "variable.1"
  # param = character. One or several among "param", "bayes", non param", and "robust"
  # outlier = character. One or several among "complete", "id", or "removed"
  # ges = one among "ges" or "pes"
  # SumS = Type of sum of squares, one among "2" or "3". 
  # save = logical. Do you want to save the results
  # html = logical. Do you want easieR to output the results in nice html document ? 
  # contrast = list. The names in the list corresponds to the names of the factors and the values is a matrix of coefficients for the contrasts. "pairs" or "none" are also possible
  # p.adjust = adjust p values for multiples comparisons. see <code>p.adjust</code>
  packages<-c("BayesFactor", "car","afex", "DescTools","emmeans","ggplot2","nortest", "outliers", "PMCMRplus",
              "pgirmess",  "psych", "reshape2", "sjstats", "svDialogs", "WRS2" )
  test2<-try(lapply(packages, library, character.only=T), silent=T)
  if(class(test2)== "try-error") return(ez.install())
  try(require(WRS), silent=T)
  .e <- environment()
  Resultats<-list()
  if(!is.null(data) & class(data)!="character") data<-deparse(substitute(data))
   try( windows(record=T), silent=T)->win
   if(class(win)=="try-error") quartz()
  ez.aov.out<-.ez.anova.in(data=data, DV=DV, between=between, within=within,id=id, cov=cov, RML=RML, 
                           RML.factor= RML.factor, param=param,outlier=outlier, 
                           ES=ES, SumS=SumS, save=save, contrasts=contrasts,p.adjust=p.adjust)
  data<-ez.aov.out$data
  DV<-ez.aov.out$DV
  between<-ez.aov.out$between
  within<-ez.aov.out$within
  cov<-ez.aov.out$cov
  id<-ez.aov.out$id
  param<-ez.aov.out$param
  outlier<-ez.aov.out$outlier
  ES<-ez.aov.out$ES
  SumS<-ez.aov.out$SumS
  nom<-ez.aov.out$nom
  save<-ez.aov.out$save
  contrasts<-ez.aov.out$contrastes$contrastes
  p.adjust<-ez.aov.out$contrastes$p.adjust
  reshape.data<-ez.aov.out$reshape.data
  list(ez.aov.out)->aov.plus.list
  
  
  complet<-.ez.anova.out(data=data, DV=DV, between=between, within=within,id=id, cov=cov,  
                         ES=ES, SumS=SumS, contrasts=contrasts,p.adjust=p.adjust, rscaleFixed=rscaleFixed , rscaleRandom= rscaleRandom, n.boot=n.boot, param=param) 
  data<-complet[["data"]]
  aov.plus.in<-complet[["aov.plus.in"]]
  complet[["data"]]<-NULL
  complet[["aov.plus.in"]]<-NULL
  
  
  if(any(outlier %in% c("complete", "Donnees completes","Complete dataset"))){
    Resultats[[.ez.anova.msg("title", 12)]]<-complet
    aov.plus.in->aov.plus.list$"Donnees completes"}
  
  if(any(outlier %in% c("id", "removed" , "Identification of outliers", "Dataset with outliers removed",
                        "Identification des valeurs influentes", "Donnees sans valeur influente"))) { 
    if(is.null(data$residu)) {
      Resultats[[.ez.anova.msg("title", 55)]]<-.ez.anova.msg("msg", 34)
      return(Resultats)}
    valeurs.influentes(X="residu", critere="Grubbs",z=3.26, data=data)->influentes
    
    if(any(outlier %in% c("Identification des valeurs influentes","id","Identification of outliers" ))) Resultats[[.ez.anova.msg("title", 13)]]<-influentes
    if(any(outlier %in% c( "Donnees sans valeur influente",  "Dataset with outliers removed","removed" ))){
      
      if(!is.null(influentes$"observations influentes"[,id])){
        setdiff(data[,as.character(id)],influentes$"observations influentes"[,as.character(id)])->diffs
        data[which(data[,id] %in% diffs), ]->nettoyees
        factor(nettoyees[,id])->nettoyees[,id]
        nett<-.ez.anova.out(data=nettoyees, DV=DV, between=between, within=within,id=id, cov=cov,  
                            ES=ES, SumS=SumS, contrasts=contrasts,p.adjust=p.adjust, rscaleFixed=rscaleFixed , rscaleRandom= rscaleRandom, n.boot=n.boot, param=param) 
        aov.plus.in<-nett[["aov.plus.in"]]
        nett[["data"]]<-NULL
        nett[["aov.plus.in"]]<-NULL
        Resultats[[.ez.anova.msg("title", 14)]]<-nett
        aov.plus.in->aov.plus.list$"Donnees sans valeur influente"
      }
      print(!all(outlier %in% c("complete", "Donnees completes","Complete dataset")))
    if(!any(outlier %in% c("complete", "Donnees completes","Complete dataset")))   Resultats[[.ez.anova.msg("title", 14)]]<-complet
      
    }
    
  }
  
  class(aov.plus.list)<-"aovplus"
  assign("aov.plus.in", aov.plus.list,envir=.GlobalEnv) 
  
  
if(reshape.data) Resultats$call.reshape<-ez.history[[length(ez.history)]][[2]]
  
  if(!is.null(between)) between<-paste(unique(between), collapse="','", sep="") 
  if(!is.null(within)) within<-paste(unique(within), collapse="','", sep="") 
  if(!is.null(cov)) cov<-paste(unique(cov), collapse="','", sep="") 
  param<-paste(unique(param), collapse="','", sep="") 
  outlier<-paste(unique(outlier), collapse="','", sep="")
  if(class(contrasts)=="list"){
    cont.call<-"list("
    for(i in 1:length(contrasts)){
      if(i>1) cont.call<-paste0(cont.call, ",")
      cont.call<- paste0(cont.call, names(contrasts)[i], "=matrix(c(", paste0(contrasts[[i]], collapse=","), "), ncol=", ncol(contrasts[[i]]),")" )
    }
    cont.call<-paste0(cont.call, ")")
  }else cont.call<-paste0("'", contrasts, "'")
  
  call<-paste0("ez.anova(data=", nom, ", DV='", DV,"', between =", ifelse(is.null(between), "NULL", paste0("c('", between,"')" )),
               ", within =", ifelse(is.null(within), "NULL", paste0("c('", within,"')" )), 
               ", cov=", ifelse(is.null(cov), "NULL", paste0("c('", cov,"')" )), ",id ='", id, "', param =c('", param, "'), outlier= c('",outlier ,"')",
               ", ES ='", ES, "', SumS= '", SumS, "', save =", save, ", html =", html, 
               ", contrasts =" , cont.call,
               ", p.adjust = '", p.adjust, "', n.boot = ", n.boot, ",rscaleFixed = ", rscaleFixed, ", rscaleRandom = ", rscaleRandom, ")")
  Resultats$call<-call
  
  .add.history(data=data, command=Resultats$Call, nom="Anova")
  .add.result(Resultats=Resultats, name =paste("Anova", Sys.time() ))
  if(save==T) save(Resultats=Resultats ,choix =paste("anova sur", nom), env=.e)
  ref1(packages)->Resultats[[.ez.anova.msg("title", 56)]]
  if(html) try(ez.html(Resultats), silent=T) 
  return(Resultats)
  
  
  
  
}


.ez.anova.msg<-function(type, number){
  # type : either "msg" or "title"
  # number : number of message 
  if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
    msg<-c("Veuillez preciser le(s) type(s) de variable(s) que vous souhaitez inclure dans l'analyse.\nVous pouvez en choisir plusieurs (e.g., pour anova mixte ou des ancova",
           "Il est indispensable d'avoir au minimum des variables a groupes independants ou en mesures repetees",
           "Veuillez selectionner les variables OU les modalites de la (des) variables a mesure(s) repetee(s).",
           "Quelle est la variable identifiant les participants ?",
           "Chaque participant doit apparaître une et une seule fois pour chaque combinaison des modalites",
           "Pour un facteur en mesures repetees en format large, il faut au moins deux colonnes",
           "Si vos donnees sont en format large, les mesures doivent toutes etre numeriques ou des entiers (integer)",
           "Veuillez choisir les variable-s a groupes independants",
           "Vous n avez pas choisi de variable. Voulez-vous continuer  (ok) ou abandonner (annuler) cette analyse ?",
           "Veuillez choisir la variable dependante.", 
           "Certains participants ont des valeurs manquantes sur les facteurs en mesures repetees. Ils vont etre supprimes des analyses",
           "Veuillez choisir la ou les covariables",
           "Il n'y a pas assez d'observations pour realiser l'analyse. Veuillez verifier vos donnees \net vous assurer qu'il y a au moins trois observations par modalite de chaque facteur",
           "le modele parametrique renvoie l'anova classique,le non parametrique calcule le test de Kruskal Wallis \nsi c'est un modele a groupes independants, ou une anova de Friedman pour un modele en Mesures repetees.\nLe modele bayesien est l'equivalent du modele teste dans l'anova en adoptant une approche bayesienne,\nles statistiques robustes sont des anovas sur des medianes ou les moyennes tronquees avec ou sans bootstrap.",
           "Les donnees completes representent l'analyse realisee sur l'ensemble des observations. L'analyse sans les valeurs influentes
           est une analyse pour laquelle les valeurs influentes ont ete supprimees.\nL'identification des valeurs influentes est realisee sur la base du test de Grubbs",
           "Vous ne pouvez pas avoir a la fois des arguments dans within et RML",
           "la taille d'effet la plus frequente est le eta carre partiel - pes.\nLa taille d'effet la plus precise est le eta carre generalise - ges",
           "Il existe plusieurs maniere de calculer la somme des carres. Le choix par defaut des logiciels commerciaux est une somme des carres\nde type 3, mettant la priorite sur les interactions plutot que sur les effets principaux.",
           "Voulez-vous sauvegarder les resultats de l'analyse ?",
           "La variable dependante a moins de trois valeurs differentes. Verifiez vos donnees ou l'analyse que vous tentez de realiser n'est pas pertinente.",
           "Les contrastes a priori correspondent aux contrastes qui permettent de tester des hypotheses a priori.\nLes contrastes 2 a 2 permettent de faire toutes les comparaisons 2 a 2 en appliquant ou non une correction a la probabilite",
           "Vous pouvez choisir les contrastes predefinis ou les specifier manuellement. Dans ce dernier cas, veuillez choisir specifier les contrastes",
           "Les contrastes doivent respecter l orthogonalite. Voulez-vous continuer ?",
           "Les contrates doivent etre des matrices de coefficients placees dans une list dont le nom de chaque niveau correspond a un facteur",
           "Si vous entrez des contrastes manuellement, toutes les variables de l'analyse doivent avoir leur matrice de coefficients",
           "Quelle correction de la probabilite voulez-vous appliquer ? Pour ne pas appliquer de correction, choisir +none+",
           "Les valeurs autorisees pour les contrastes sont +none+ pour aucun contraste, +pairwise+ pour les comparaisons 2 a 2 ou une liste de coefficients de contrastes",
           "Au moins une de vos matrices de contrastes n'est pas correcte.",
           "il y a moins de 3 observations pour un des groupes ou \nla variance d'au moins un groupe vaut 0. Les resultats risquent d'etre considerablement biaises" ,
           "Les facteurs bayesiens n'ont pas pu etre calcules.",
           "Desole, nous n'avons pas pu calcule l'anova sur les medianes, possiblement en raison d'un nombre important d'ex aequo.",
           "Les probabilites et les IC sont estimes sur la base d'un bootsrap. L'IC est corrige pour comparaison multiple, contrairement a la probabilite reportee.",
           "Desole, nous n'avons pas pu calcule l'anova robuste.", "L'analyse n'a pas pu aboutir"
    )
    
    
    title<-c("Quel-s type-s de variables ?", "Mesures repetees","Identifiant participant","Variables a groupes independants",
             "Variable dependante", "Covariable-s ?",
             "Modele parametrique", "Modele non parametrique","Facteurs bayesiens", "Statistiques robustes - peut prendre du temps",
             "Quelle(s) analyses voulez-vous  ?","Donnees completes","Identification des valeurs influentes", "Donnees sans valeur influente",
             "Quels resultats voulez-vous obtenir ?", "Quelle taille d effet voulez-vous  ?","Quelle somme des carres voulez-vous utiliser ?",
             "Voulez-vous sauvegarder ?", "a priori",  "Comparaison 2 a 2", "aucun","Quel types de contraste voulez-vous ?",
             "Contrastes pour", "specifier vos contrastes","Quelle est la ligne de base ?", "Statistiques descriptives","Modele teste",
             "Statistiques descriptives de la variable","Statistiques descriptives de l'interaction entre","Avertissement",
             "Tests de normalite","Conditions d'application de l'ancova","Test de l'absence de difference entre les groupes sur ",
             "Test de l'homogeneite des pentes entre les groupes sur la variable dependante",
             "Test de Levene verifiant l'homogeneite des variances","Test de Mauchly testant la sphericite de la matrice de covariance",
             "Analyse principale","Anova avec correction de Welch pour variances heterogenes", "Comparaisons 2 a 2","contrastes",
             "Matrice de coefficients variables","Table des contrastes","Table des contrastes imitant les logiciels commerciaux","Facteurs bayesiens",
             "Analyse non parametrique","Test de Kruskal-Wallis","Anova de Friedman","Comparaison 2 a 2 pour ANOVA de Friedman",
             "Test de Kruskal-Wallis - Comparaison deux a deux"  ,"Anova sur les medianes","Analyse principale","Anova basee sur les moyennes tronquees",
             "ANOVA sur M estimator", "Anova sur l'estimateur modifie de localisation de Huber","Arret premature de l'analyse",
             "References des packages utilises pour cette analyse"
             
    )
    
  } else {
    msg<-c("Which kind of variable do you want to include in the analysis ?\nYou are allowed to choose several (e.g., for mixed anova or for ancova).",
           "It is required that at least one variable is either an independant group variable or a repeated measure variable",
           "Please select the repeated measures variable-s or the levels of the repeated measures variables",
           "Which variable identify individual ?",
           "Each participant must occur once and only once for each combinations of levels",
           "If your data have a wide format, you must select at least 2 columns for repeated measure factors",
           "If your data a wide format, the format in each column must be either numeric or integer",
           "Please choose between participant variables.",
           "You have not chosen any variable. Do you want to continue (ok) or to leave (cancel) this analysis ?",
           "Please choose the dependant variable",
           "Some participants have missing data on repeated measure factors. They are removed from the analyses.",
           "Please choose the covariables",
           "The number of observations is not enough given the number of levels for each variable. \nPlease ensure that there are at least 3 observations for each combination of levels",
           "The parametruc model is the usual anova presented in statistical packages. The non parametric test is \nthe Kruskal Wallis test for one way anova and Friedman test for repeated measure anova.\n
           The bayesian approach assesses the same model as the classical anova by computing the Bayes Factor.\nRobust statistics are anovas on trimmed means or median with or without bootstrap",
           "Complete data are the analyses performed on the entire dataset. The analysis without outliers means that outliers have\nbeen removed before performing the analysis. The criteria for detecting outliers is the Grubbs test.",
           "If within is not null, RML must be null and conversely",
           "The most often used effect size is the partial eta squarred (pes). The most accurate is the generalized eta squared (ges)",
           "There are several ways to estimate the Sum of Squares. Default value for comercial softwares is Type 3,\nwhich prioritizes interaction instead of main effects",
           "Do you want to save the results of the analysis?",
           "The dependant variable has less than 3 unique different values. Check your data or the analysis that you try to make is not relevant.",
           "A priori contrast allow to make comparisons for a priori hypotheses. Pairwise contrasts make all possible pairwise comparisons, with or without p. adjust.",
           "You may use one of predefined contrast matrix or state the contrast by yourself. In the latter case, you must choose state the contrasts.",
           "The contrasts must be orthogonal. Do you want to continue ?",
           "Les contrates must be matrix of coefficients stored in list for which the name of each levels is the name of one variable.",
           "If you choose the coefficients yourself, all the variables in the analysis must have their coefficient matrix",
           "Which p adjustment do you want ? If you do not want any p adjust, choose +none+",
           "Allowed values for contrasts are +none+, +pairwise+ or a list with the coefficients of contrasts.",
           "At least one of your contrast matrix is not correct.",
           "There are less than 3 observations for at least one group or the variance for one groupe is 0. Results are probably biased",
           "Bayes Factors estimation failed",
           "We are sorry but it was not possible to perform the ANOVA on the medians, probably du to ex aequo.",
           "Probability and CI are estimated by bootstrap. CI is corrected for multiple comparisons, contrary to reported p values",
           "We are sorry but robust ANOVA failed", "The analysis has failed"
           
    )
    
    
    
    title<-c("Which kind of variable ?", "Repeated measure", "Id of individuals", "Between participant variables",
             "Dependant variable","Covariable-s ?",
             "Parametric", "Non parametric","Bayes Factors", "Robust statistics - might take some time",
             "Which analysis do you want ?", "Complete dataset", "Identification of outliers", "Dataset with outliers removed",
             "Which results do you want ?", "Which effect size do you want?", "Which sum of squares do you want ?","Do you want to save?",
             "a priori", "Pairwise", "none", "Please choose the type of contrast", "Contrasts for", "Choose your own contrasts",
             "Which level is the baseline?","Descriptive statistics", "Model", "Descriptive for",
             "Descriptive statistics for the interaction between","Warning","Normal distribution test",
             "Assumptions of ancova","Test of absence of difference between groups on ",
             "Homogeneity of slopes between groups on the dependant variable",
             "Levene's test testing homogeneity of variances","Mauchly's sphericity test","Main analysis",
             "Welch's ANOVA for heterogeneous variances","Pairwise comparisons","contrasts","Matrix of coefficients",
             "Table of contrasts","Contrasts that mimics commercial software","Bayes factors", "Non parametrique test", "Kruskal-Wallis test", "Friedman Anova",
             "Friedman pairwise comparison","Kruskal-Wallis test- pairwise comparisons", "Anova on medians","Main analysis",
             "Anova on trimmed mean", "M estimator ANOVA","Anova on modified Huber estimator", "A problem occurred. The analysis has stopped",
             "References of packages used for this analysis")
    
  }
  
  ifelse(type=="msg", r<-msg, r<-title)
  r<-r[number]   
  return(r)}

.contrastes.ez<-function(data, between=NULL, within=NULL, contrasts="none", p.adjust="none", dial=T){
  options (warn=1)
  c(between, unlist(within))->betweenwithin
  if(contrasts!="none" & contrasts!="pairwise" & class(contrasts)!="list") {
    okCancelBox( .ez.anova.msg("msg", 27))
    return(.contrastes.ez(data=data, between=between, within=within, contrasts="none", p.adjust="none", dial=T))
  }
  if(class(contrasts)=="list"){
    if(length(betweenwithin) != length(contrasts) | !any(betweenwithin %in% names(contrasts)) ){
      okCancelBox( .ez.anova.msg("msg", 25))
      return(.contrastes.ez(data=data, between=between, within=within, contrasts="none", p.adjust="none", dial=T))
    }
    
    for(i in 1:length(betweenwithin)){
      
      j<-which(betweenwithin[[i]]==names(contrasts))
      if(!all(class(contrasts[[j]]) %in% c("matrix", "data.frame")) || !is.numeric(as.matrix(contrasts[[j]]))){
        okCancelBox( .ez.anova.msg("msg", 24))
        return(.contrastes.ez(data=data, between=between, within=within, contrasts="none", p.adjust="none", dial=T)) 
      }
      
      if(nrow(contrasts[[j]])!=nlevels(data[, betweenwithin[i]])| any(is.na(contrasts[[j]]))){
        okCancelBox( .ez.anova.msg("msg", 28))
        return(.contrastes.ez(data=data, between=between, within=within, contrasts="none", p.adjust="none", dial=T)) 
      }   
    }
  }
  
  
  
  contrastes<-list()
  Resultats<-list() 
  
  if(dial){
    writeLines(.ez.anova.msg("msg", 21))
    cont.choix<-c(.ez.anova.msg("title", 19),.ez.anova.msg("title", 20), .ez.anova.msg("title", 21))
    type.cont<- dlgList(cont.choix, preselect=.ez.anova.msg("title", 19),multiple = FALSE, 
                        title=.ez.anova.msg("title", 22))$res
    if(length(type.cont)==0) return(NULL)
    Resultats$type.cont<-type.cont
    
    if(type.cont=="a priori") {
      
      writeLines(.ez.anova.msg("msg", 21))
      cont.exemple<-list()
      cont.exemple$Poly<-emmeans:::poly.emmc(1:3)
      cont.exemple$Trait.vs.contr<-emmeans:::trt.vs.ctrl1.emmc(1:3)
      cont.exemple$Eff<-emmeans:::del.eff.emmc(1:3)
      cont.exemple$Consec<-emmeans:::consec.emmc(1:3)
      cont.exemple$mean.change<-emmeans:::mean_chg.emmc(1:3)
      cont.exemple$Helmert<-contr.helmert(3)
      cont.exemple$Helmert.rev<-apply(contr.helmert(3), 2, rev)
      
      print(cont.exemple)
      
      for (i in 1:length(betweenwithin)){
        
        type.cont2<- dlgList(c("Helmert", "Helmert reversed", "poly","Trait.vs.contr","Eff", "consec", "mean.change", .ez.anova.msg("title", 24)),
                             preselect=c("Helmert"), multiple = FALSE, title=paste(.ez.anova.msg("title", 23), betweenwithin[i],"?"))$res
        
        if(length(type.cont2)==0) return(contrastes.ez())
        contrastes[[i]]<-switch(EXPR =type.cont2,
                                "Helmert"= contr.helmert(nlevels(data[,betweenwithin[i]])), 
                                "Helmert reversed"= apply(contr.helmert(nlevels(data[,betweenwithin[i]])), 2, rev), 
                                "poly"= emmeans:::poly.emmc(1:nlevels(data[,betweenwithin[i]])),
                                "Trait.vs.contr"= {
                                  base<- dlgList(levels(data[, betweenwithin[i]]), preselect=levels(data[,betweenwithin[i]])[1],
                                                 multiple = FALSE, title=.ez.anova.msg("title", 25))$res
                                  which(levels(data[, betweenwithin[i]])==base)->base
                                  emmeans:::trt.vs.ctrl1.emmc(1:nlevels(data[,betweenwithin[i]]), ref=base)
                                }, 
                                "Eff"=emmeans:::del.eff.emmc(1:nlevels(data[,betweenwithin[i]])), 
                                "consec"= emmeans:::consec.emmc(1:nlevels(data[,betweenwithin[i]])), 
                                "mean.change"= emmeans:::mean_chg.emmc(1:nlevels(data[,betweenwithin[i]]))) 
        
        if(type.cont2 %in% c("specifier vos contrastes", "Choose your own contrasts")){
          ortho<-FALSE
          while(ortho!=TRUE){
            own.cont<-matrix(rep(0,times=(nlevels(data[,betweenwithin[i]])*(nlevels(data[,betweenwithin[i]])-1))), 
                             nrow=nlevels(data[,betweenwithin[i]]))
            dimnames( own.cont)[[1]]<-levels(data[,betweenwithin[i]])
            dimnames( own.cont)[[2]]<-paste("contraste", 1:(nlevels(data[,betweenwithin[i]])-1), sep=".")
            own.cont<-fix( own.cont)
            if(any(colSums( own.cont)!=0)|(nlevels(data[,betweenwithin[i]])>2 & 
                                           max(rle(c( own.cont))$lengths)>2*(nlevels(data[,betweenwithin[i]])-2))) ortho<-FALSE else {
                                             test.out<-rep(1, length( own.cont[,1]))
                                             for(j in 1:length( own.cont[1,])) { 
                                               test.out<-own.cont[,j]*test.out
                                             }
                                             if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE
                                           }
            if(ortho==FALSE) {
              cont<-dlgMessage(.ez.anova.msg("msg", 23), "yesno")$res
              if(cont=="no") return(.contrastes.ez(data=data, between=between, within=within ))  }
            contrastes[[i]]<-own.cont
            
          }
          
        }
        
        dimnames(contrastes[[i]])[[2]]<-paste("contraste", 1:(ncol(contrastes[[i]])), sep=".")
        dimnames(contrastes[[i]])[[1]]<-levels(data[,betweenwithin[i]])
      }
      names(contrastes)<-betweenwithin
      Resultats$contrastes<-contrastes     
      
    }else{
      if(type.cont %in% c("Comparaison 2 a 2","Pairwise", "pairwise", "none", "aucun"))  { Resultats$contrastes<-type.cont
      contrastes<-type.cont}
      
    }
  }else{
    contrastes<-list()
    for(i in 1:length(contrasts)){
      cont2<-contrasts[[i]]
      cont2<-as.matrix(cont2)
      j<-which(names(data)==names(contrasts)[[i]])
      noms<-list()
      noms[[1]]<-levels(data[,j])
      noms[[2]]<-paste("contraste", 1:(ncol(cont2)), sep=".")
      dimnames(cont2)<-noms
      contrastes[[i]]<-cont2 
    }
    names(contrastes)<-names(contrasts)
    Resultats$contrastes<-contrastes
    
  }
  if((dial & contrastes %in% c("Comparaison 2 a 2","Pairwise", "pairwise")) || 
     (!p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
                       "sidak","dunnettx","mvt" ,"none" ))){
    list()->p.adjust
    writeLines(.ez.anova.msg("msg", 26) )
    dlgList(c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
              "sidak","dunnettx","mvt" ,"none"), preselect="holm", multiple = FALSE, title="Correction ?")$res->p.adjust
    
    if(length(p.adjust)==0) return(contrastes.ez())
    
  } 
  Resultats$p.adjust<-p.adjust
  return(Resultats)
}



.options.aov<-function(between=NULL, within=NULL, cov=NULL, dial=T, outlier=NULL, param=NULL, ES=NULL, SumS=NULL, save=F){
  list()->Resultats
  
  if(dial || !any(param %in% c("param", "non param", "bayes", "robust",
                               "Modele parametrique", "Modele non parametrique","Facteurs bayesiens", "Statistiques robustes - peut prendre du temps",
                               "Parametric", "Non parametric","Bayes Factors", "Robust statistics - might take some time"))){
    writeLines(.ez.anova.msg("msg",14))
    msg<-c(.ez.anova.msg("title",7),.ez.anova.msg("title",9))
    if(is.null(cov)) {    
      if((length(between)==1 & is.null(within)) | (length(within)==1 & is.null(between))) msg<-c(msg, .ez.anova.msg("title",8))
      if((length(between)==1 & length(within)==1) || (length(between)<4 & is.null(within)) ||
         (is.null(between) & length(within)==1) ) msg<-c(msg, .ez.anova.msg("title",10))
    }
    param<- dlgList(msg,preselect=msg, multiple = TRUE, title=.ez.anova.msg("title",11))$res
    if(length(param)==0) return(NULL)
    
  }
  
  
  
  if(dial | !any(outlier%in% c("complete","id", "removed","Complete dataset", "Identification of outliers", "Dataset with outliers removed",
                               "Donnees completes","Identification des valeurs influentes", "Donnees sans valeur influente")) ){
    outlier<-c(.ez.anova.msg("title",12),.ez.anova.msg("title",13),.ez.anova.msg("title",14))
    outlier<- dlgList(outlier, preselect=outlier,multiple = TRUE, title=.ez.anova.msg("title",15))$res
    if(length(outlier)==0) return(.options.aov(between=between, within=within, cov=cov))
  }
  
  
  if(any(param %in% c("Modele parametrique", "Parametric", "param"))){
    if(!ES %in% c("ges", "pes") | dial){
    writeLines(.ez.anova.msg("msg",17))
    ES<- dlgList(c("ges", "pes"), preselect=c("ges"),multiple = FALSE, title=.ez.anova.msg("title",16))$res
    if(length(ES)==0) return(.options.aov(between=between, within=within, cov=cov))
    }
    if(dial | !SumS %in% c("2", "3")){
      writeLines(.ez.anova.msg("msg",18))
      SumS<- dlgList(c(2,3), preselect=3,multiple = FALSE, title=.ez.anova.msg("title",16))$res
      if(length(SumS)==0) return(.options.aov(between=between, within=within, cov=cov))
    }
     
  }
  if(dial | class(save)!="logical"){
    writeLines(.ez.anova.msg("msg",19))
    save<-dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title=.ez.anova.msg("title",18))$res
    if(length(save)==0) return(.options.aov(between=between, within=within, cov=cov))
  }
  Resultats$param<-param
  Resultats$outlier<-outlier
  Resultats$ES<-ES
  Resultats$SumS<-SumS
  Resultats$save<-save
  
  return(Resultats)
}

.ez.anova.in<-function(data=NULL, DV=NULL, between=NULL, within=NULL,id=NULL, cov=NULL, RML=NULL, 
                       RML.factor=NULL, param=c("param","bayes"),outlier=c("complete","id", "removed"), 
                       ES="ges", SumS="3", save=F, contrasts="none",p.adjust="none"){
  .e <- environment()
  Resultats<-list()
  reshape.data<-FALSE  
  
  choix.data(data=data, info=TRUE, nom=TRUE)->data
  if(length(data)==0) return(NULL)
  nom<-data[[1]]
  data<-data[[2]]
  
  type.v<-c()
  if((!is.null(between) | !is.null(within) | !is.null(RML)) && all(c(between, within, RML) %in% names(data))) dial<-F else dial<-T
  if(!is.null(within) & !is.null(RML))  {okCancelBox(.ez.anova.msg("msg",14))
    return(.ez.anova.in())}
  
  if(is.null(c(between,within, RML))) {
    if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) index<-1 else index<-2
    type.v<-matrix(c("Groupes independants", "Mesures repetees", "Covariables",
                     "Independant groups", "Repeated measures", "Covariables"), ncol=2) 
    writeLines(.ez.anova.msg("msg", 1))
    type.v2<-dlgList(type.v[,index], multiple = TRUE, title=.ez.anova.msg("title", 1))$res
    if(length(type.v2)==0) return(.ez.anova.in())
    type.v<-type.v[which(type.v[, index]%in%type.v2),1]
    if(!any(type.v %in% c("Groupes independants", "Mesures repetees"))) {
      writeLines(.ez.anova.msg("msg",2))
      return(.ez.anova.in())
    }
  } 
  
  if(any(type.v=="Mesures repetees") | !is.null(within) | !is.null(RML)) {
    if(!is.null(RML)) within<-RML
    within<-.var.type(X=within, info=T, data=data, type=NULL, check.prod=F, message=.ez.anova.msg("msg",3),  multiple=TRUE, 
                      title=.ez.anova.msg("title",2), out=NULL)
    if(is.null(within)) return(.ez.anova.in())
    within<-within$X
  }
  
  if(!is.null(within)){
    if(all(sapply(data[,within], class)=="factor")) {
      id<-.var.type(X=id, info=T, data=data, type=NULL, check.prod=F, message=.ez.anova.msg("msg",4),  multiple=FALSE, 
                    title=.ez.anova.msg("title",3), out=within)
      if(is.null(id)) return(.ez.anova.in())
      id<-id$X
      data[, id]<-factor(data[,id])
      if(length(within)==1) {
        N.modalites2<-nlevels(data[,unlist(within)])
      } else {
        N.modalites2<-sapply(data[,unlist(within)],nlevels)
      }
      if(nlevels(data[,id])*prod(N.modalites2)!=length(data[,1])) {
        okCancelBox(.ez.anova.msg("msg",5))
        return(.ez.anova.in())}
    }else {
      if(length(within)==1) {
        writeLines(.ez.anova.msg("msg",5))
        return(.ez.anova.in(data=NULL))}
      if(!any(sapply(data[,within], class) %in% c("numeric","integer"))){ 
        writeLines(.ez.anova.msg("msg",6))
        return(.ez.anova.in(data=NULL))
      }
      data<-data[complete.cases(data[,within]),]
      RML<-within
    }
  } 
  
  
  
  if(!is.null(RML)) {
    idvar<-setdiff(names(data), RML)
    if(!is.null(RML.factor)) {
      IV.names<-as.list(names(RML.factor))
    }else{
      IV.names<-NULL
    }
    data<-ez.reshape(data=nom, varying= list(RML), v.names =c('value'),idvar =idvar,
                     IV.names=IV.names, IV.levels=RML.factor) 
    nom<-paste0(nom, ".long")
    reshape.data<-TRUE
    DV<-"value"
    within<-setdiff(names(data), c(idvar, "value","IDeasy"))
    if(length(within)>1) {
      data[,within]<-lapply(data[, within], factor)
      within<-within[-which(within=="time")]
    } else {
      data[,within]<-factor(data[,within])
    }
  }
  
  diffs<-c(id,  within, DV)
  if(is.null(id) || !id %in%names(data)) {
    
    if(length(within)==1) {
      N.modalites2<-nlevels(data[,unlist(within)])
    } else {
      if(length(within)>1) N.modalites2<-sapply(data[,unlist(within)],nlevels) else N.modalites2<-1
    }
    
    
    data$IDeasy<-paste0("p", 1:(nrow(data)/prod(N.modalites2)))
    data$IDeasy<-factor( data$IDeasy)
    id<-"IDeasy"
  }     
  if(any(type.v=="Groupes independants") | !is.null(between)){
    between<-.var.type(X=between, info=T, data=data, type="factor", check.prod=F, message=.ez.anova.msg("msg",8),  multiple=TRUE, 
                       title=.ez.anova.msg("title",4), out=diffs)
    if(is.null(between)) {
      if(okCancelBox(.ez.anova.msg("msg",9))) .ez.anova.in(data=data, within= within, id=id) else return(NULL)
    }
    data<-between$data
    between<-between$X
    diffs<-c(diffs, between)
  }
  
  if(is.null(DV)){
    DV<-.var.type(X=DV, info=T, data=data, type="numeric", check.prod=F, message=.ez.anova.msg("msg",10),  multiple=TRUE, 
                  title=.ez.anova.msg("title",5), out=diffs)
    if(is.null(DV)) {
      if(okCancelBox(.ez.anova.msg("msg",9))) .ez.anova.in(data=data, within= within, id=id, between=between) else return(NULL)
    }
    DV<-DV$X
    diffs<-c(diffs, DV)
  }
  
  
  if(!is.null(within)) {
    if( min(table(data[,id]))!=  max(table(data[,id])))  msgBox(.ez.anova.msg("msg",11))
    
    while(min(table(data[,id]))!=  max(table(data[,id]))){
      mid<-names(table(data[,id]))[which.min(table(data[,id]))]
      data<-data[-which(data[,id]==mid) , ]
      data[,id]<-factor(data[,id])
    }       
  }
  
  
  
  if(any(type.v=="Covariables")) {
    
    cov<-.var.type(X=cov, info=T, data=data, type="numeric", check.prod=F, message=.ez.anova.msg("msg",12),  multiple=TRUE, 
                   title=.ez.anova.msg("title",6), out=diffs)
    if(is.null(cov)) {
      if(okCancelBox(.ez.anova.msg("msg",9))) .ez.anova.in(data=data, within= within, id=id, between=between, DV=DV) else return(NULL)
    }
    cov<-cov$X
  }
  
  
  
  if(length(within)==1 & is.null(between)) nlevels(data[,unlist(within)])->N.modalites2 else {
    if(length(between)==1 & is.null(within)) nlevels(data[,unlist(between)])->N.modalites2 else sapply(data[,c(between, unlist(within))],nlevels)->N.modalites2 }
  
  options.out<-.options.aov(between=between, within=within, cov=cov, dial=dial, outlier=outlier, param=param, ES=ES, SumS=SumS, save=save)
#print(options.out)  
  if(is.null(options.out)) return(.ez.anova.in())
  
  
  
  data<-data[complete.cases(data[,c(between,unlist(within), DV, cov)]),]
  if(min(table(data[,id])) !=max(table(data[,id]))){
     id.out<-which(table(data[,id])!=max(table(data[,id])))
     data<-data[which(!data[,id]%in% names(id.out)), ]
     }

  ftable(data[,c(between,unlist(within))])->aov.check
  if(any(is.na(aov.check)) || min(aov.check)<3) {
    msgBox(.ez.anova.msg("msg",13))
    return(NULL)
  }
  if(length(unique(data[,DV]))<3) {
    msgBox(.ez.anova.msg("msg",13))
    return(NULL)
  }
#if(any(param %in% c("Modele parametrique", "Parametric", "param")))  
  if(any(options.out$param %in% c("param", "Modele parametrique", "Parametric", "param"))){
    contrasts<-.contrastes.ez(data=data, between=between, within=within, contrasts=contrasts, dial=dial, p.adjust=p.adjust)
    if(is.null(contrasts)) return(.ez.anova.in()) 
  } else contrasts<-NULL
  
  
  Resultats$between<-between
  Resultats$id<-id
  Resultats$within<-within
  Resultats$cov<-cov
  Resultats$DV<-DV
  Resultats$data<-data
  Resultats$nom<-nom
  Resultats$outlier<-options.out$outlier
  Resultats$param<-options.out$param
  Resultats$ES<-options.out$ES
  Resultats$SumS<-options.out$SumS
  Resultats$save<-options.out$save
  Resultats$p.adjust<-p.adjust
  Resultats$contrastes<-contrasts
  Resultats$reshape.data<-reshape.data
  return(Resultats)
}

.ez.anova.out<-function(data=NULL, DV=NULL, between=NULL, within=NULL,id=NULL, cov=NULL,  
                        ES="ges", SumS="3", contrasts="none",p.adjust="none", rscaleFixed=0.5 , rscaleRandom= 1, n.boot=1000, param=c("param", "bayes")){
  data<-data[,c(DV,between, within, id, cov)]
  list()->Resultats
  cov1<-NULL
  if(!is.null(cov)) { 
    for(i in 1:length(cov)) {paste0(cov1,cov[i],"+")->cov1}}
  
  if(!is.null(between))  {pred.ind<-between[1]  
  if(length(between)>1) {
    for(i in 1:(length(between)-1)){ paste(pred.ind, "*",between[1+i])->pred.ind}}
  }
  
  if(!is.null(within))  {
    ez.principal<-within[[1]]
    erreur<-paste0("+Error(", within[[1]])
    if(length(within)>1) {for(i in 1:(length(within)-1)){
      ez.principal<- paste(ez.principal, "*",within[[i+1]])
      erreur<-paste(erreur, "*", within[[i+1]])
    }
    }
    paste(ez.principal, erreur,"|", id, ")")->pred.rep
  }
  
  if(!is.null(between) & !is.null(within)) paste(pred.ind, "*",pred.rep)->predicteurs else {
    if(!is.null(between) & is.null(within)) paste0(pred.ind,"+Error(1|",id,")")->predicteurs else pred.rep->predicteurs
  }
  paste0(DV, "~",cov1, predicteurs)->modele  
  Resultats[[.ez.anova.msg("title",27)]]<-modele
  as.formula(modele)->modele  
  
  Resultats[[.ez.anova.msg("title",26)]]<-.stat.desc.out(X=DV, groupes=c(between, within), data=data, tr=.1, type=3, plot=T)
  
  
  list()->aov.plus.in
  for(i in 1:length(c(between, unlist(within)))){
    combn(c(between, unlist(within)), i)->facteurs
    for(j in 1:ncol(facteurs)){
      psych::describeBy(data[,DV], data[ ,facteurs[,j]] ,mat=TRUE,type=3)->sd.aov
      
      if(nrow( facteurs) ==1) paste(.ez.anova.msg("title",28), facteurs[,j])->nsd else {
        paste(.ez.anova.msg("title",29), facteurs[1,j])->nsd
        for(k in 2: nrow( facteurs)){paste(nsd, ":",  facteurs[k,j])->nsd
        }
        
      }
      sd.aov->aov.plus.in[[nsd]]
    }
  }
  
  if(any(Resultats[[2]][[1]]$n<3)) {
    Resultats$"information"<-.ez.anova.msg("msg",29)
    return(Resultats)
  }  
  
  if(any(param %in% c("Modele parametrique","param", "Parametric"))){
    if(any(Resultats[[2]][[1]]$sd==0)) Resultats[[.ez.anova.msg("title",30)]]<-.ez.anova.msg("msg",29)
    options(contrasts=c("contr.sum","contr.poly"))
    if(!is.null(cov)) factorize<-FALSE else factorize<-TRUE
    aov.out<-aov_4(as.formula(modele),data=data, es_aov=ES, type=SumS,factorize=factorize)
    residus<-data.frame(aov.out$lm$residuals)
    residus[,"match"] <-aov.out$data$wide[,IDeasy]
    if(!is.null(within)){ residus<-melt(residus, id.vars="match") 
                         names(residus)[3]<-"residu"
                          residus$match<-paste0(residus[,1], residus[,2])
                          data$match<-paste0(data[,IDeasy], data[,within[1]])
                          if(length(within)>1){
                             for(i in 2:length(within)){
                                 data$match<-paste0(data$match, "_", within[i])
                                                        }
                                               }
                          }else{
      names(residus)<-"residu"
      }
    data<-merge(x=data, y=residus, by="match")
    print(names(data))
    Resultats[[.ez.anova.msg("title",31)]]<-.normalite(data=data, X="residu", Y=NULL)
    print(Resultats)
    
    if(!is.null(cov) & !is.null(between)){
      options(contrasts = c("contr.helmert", "contr.poly"))
      for(i in 1:length(cov)){
        aov(as.formula(paste0(cov[i], "~",pred.ind)), data=data)->aov.cov
        Anova(aov.cov, type="III")->aov.cov
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          names(aov.cov)<-c("SC", "ddl", "F", "valeur.p") 
        }
        
        Resultats[[.ez.anova.msg("title",32)]][[paste0(.ez.anova.msg("title",33), cov[i])]]<-aov.cov
        if(i==1) {paste(cov[1],"*")->cov2} else {paste0(cov2, cov[i],"*")->cov2}
      }
      aov(as.formula(paste0(DV, "~", cov2,pred.ind)), data=data)->aov.cov
      Anova(aov.cov, type="III")->aov.cov
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(aov.cov)<-c("SC", "ddl", "F", "valeur.p") 
      }
      Resultats[[.ez.anova.msg("title",32)]][[.ez.anova.msg("title",34)]]<-aov.cov
      
    }
    
    if(!is.null(between)){
      paste0(DV, "~",pred.ind)->modele2
      Levene<-leveneTest(as.formula(modele2),data=data) # test de Levene pour homogeneite des variances
      Levene<-round(unlist(Levene)[c(1,2,3,5)],3)
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(Levene)<-c("ddl1","ddl2","F","valeur.p")}
      Resultats[[.ez.anova.msg("title",35)]]<- Levene
    }
    
    c(unlist(within), between)->withinbetween
    if(length(withinbetween)>1) {
      
      graph.modele<-paste0(withinbetween[1],"~",withinbetween[2])
      if(length(withinbetween)>2){paste0(graph.modele, "|",withinbetween[3] )->graph.modele
        if(length(withinbetween)>3){ for(i in 4:length(withinbetween)){paste0(graph.modele, "*",withinbetween[i] )->graph.modele} 
          
        }} 
      
      Resultats$Figure<-emmip(aov.out,as.formula(graph.modele),CIs=T)
    }
    
    
    aov.out2<-summary(aov.out)
    if(!is.null(within) && any( sapply(data[,c(unlist(within))],nlevels)>2)) {
      aov.out2b<-round(aov.out2$sphericity.test,5)
      aov.out2b<-matrix(aov.out2b, ncol=2)
      dimnames(aov.out2b)<-dimnames(aov.out2$sphericity.test)
      Resultats[[.ez.anova.msg("title",36)]]<-aov.out2b
    }

    aov.out3<-aov.out[[1]]
    aov.out3<-data.frame(aov.out3)
    aov.out3[,6]<-round.ps(aov.out3[,6])
    if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
      names(aov.out3)<-c("ddl.num", "ddl.denom", "CME", "F", ES, "valeur.p" )
    }
    omega.out<-omega_sq(aov.out$aov)
    aov.out3<-cbind(aov.out3, omega.2=omega.out[match(rownames(aov.out3), omega.out$term),2])
    
    Resultats[[.ez.anova.msg("title",37)]]<- aov.out3
    if(!is.null(within) && any( sapply(data[,c(unlist(within))],nlevels)>2)) {
      GG.HF<-data.frame(round(aov.out2$pval.adjustments,5))
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(GG.HF)<-c("GG.eps", "GG.valeur.p","HF.eps", "HF.valeur.p")
      }
      Resultats$"Correction : Greenhouse-Geisser &  Hyunh-Feldt"<-GG.HF}
    if(length(between)==1 & is.null(within) & is.null(cov)) {
      Welch<-oneway.test(as.formula(paste(DV,"~", between)),data=data)
      Welch<-round(data.frame("F"=Welch$statistic,"num"=Welch$parameter[1],"denom"=Welch$parameter[2],"p"=Welch$p.value),4)
      
      Resultats[[.ez.anova.msg("title",38)]]<-Welch
    }  
    
    em.out<-emmeans(aov.out, withinbetween)
    aov.plus.in$em.out<-em.out
    
    
    if(!is.list(contrasts) && contrasts %in% c("pairwise",  "Comparaison 2 a 2" )){
      pair<-pairs(em.out, adjust=p.adjust)
      Resultats[[.ez.anova.msg("title",39)]]<-summary(pair)
    }
    
    
    if(class(contrasts)=="list"){
      if(length(withinbetween)==1) {
        mod<-data.frame(withinbetween = levels(data[,withinbetween])) 
        names(mod)<-withinbetween
      }else {
        mod<-lapply(data[, withinbetween], levels)
        mod<-expand.grid(mod)
      }
      
      j<-length(mod)
      for(i in 1:length(withinbetween)){
        var1<-which(names(mod)==names(contrasts)[i])
        mod<-cbind(mod, contrasts[[i]][match(mod[,var1], rownames(contrasts[[i]])),])
        colnames(mod)[(j+1):length(mod)]<-paste0(names(contrasts)[i], "_cont",1:ncol(contrasts[[i]]))
        j<-length(mod)
      }
      
      noms<-list()
      for(i in 1:length(withinbetween)){
        noms[[i]]<-names(mod)[which(grepl(paste0(withinbetween[i], "_cont"), names(mod))) ]
      }
      if(length(withinbetween)>1){   
        Grid0<-list()
        for(i in 2: (length(noms))){
          comb<-combn(1:length(noms), i)
          
          for(j in 1:ncol(comb)){
            Grid<-expand.grid(noms[c(comb[,j])])
            Grid0[[length(Grid0)+1]]<-Grid
          }
        }
        for(i in 1:length(Grid0)){
          
          for(j in 1:nrow(Grid0[[i]])){
            new.cont<-rep(1, nrow(mod))
            for(k in 1:ncol(Grid0[[i]])){
              n.cont<-Grid0[[i]][j,k]
              new.cont<-new.cont*mod[,as.character(n.cont)]
              if(k==1) nom.cont<-n.cont else nom.cont<-paste0(nom.cont,":", n.cont)
            }
            mod[,(length(mod)+1)]<-new.cont
            names(mod)[length(mod)]<-nom.cont
          }
        }
      }
      emmean.out<-emmeans::contrast(em.out, mod[,(length(withinbetween)+1):length(mod)])
      table.cont<-summary(emmean.out)
      Resultats[[.ez.anova.msg("title",40)]][[.ez.anova.msg("title",41)]]<-contrasts
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(table.cont)<-c("contraste","estimateur", "erreur.st", "ddl","t", "p")}
      table.cont$R.2<-round(table.cont$t^2/(table.cont$t^2+table.cont$ddl),4)
      if(!is.null(between)) {
        grepl(paste(between,collapse = "|"),  table.cont[,1])->table.cont$D.Cohen
        round( ifelse(table.cont$D.Cohen==T, (2*table.cont$t)/(nlevels(data[,id])^0.5), table.cont$t/(nlevels(data[,id])^0.5)),4)->table.cont$D.Cohen
      }else round(table.cont$t/((nlevels(data[,id]))^0.5),4)->table.cont$D.Cohen
      
      
      Resultats[[.ez.anova.msg("title",40)]][[.ez.anova.msg("title",42)]]<-table.cont
      
      
      
      
      if(!is.null(within) & is.null(between) & is.null(cov)) {
        if(length(within)==1) nlevels(data[,unlist(within)])->N.modalites2 else {
          sapply(data[,within],nlevels)->N.modalites2 
        }
        
        data[do.call("order", data[unlist(within)]), ]->data
        list()->combinaison
        for(i in 1:length(contrasts)){ combn(1:length(contrasts), i)->combinaison[[i]]        }
        Table.contrasts<-c()
        for(i in 1:length(combinaison) ){
          
          for(j in 1:ncol(combinaison[[i]])){
            M1<-matrix(rep(1, length(data[,DV])), ncol=1)
            for(k in 1:nrow(combinaison[[i]])){
              M2<-c()
              for(l in 1:ncol(contrasts[[combinaison[[i]][k,j]]])){
                rep(contrasts[[combinaison[[i]][k,j]]][,l], each=length(data[,DV])/prod(N.modalites2[1:combinaison[[i]][k,j]]), len =length(data[,DV]))->coef1
                cbind(M2,coef1)->M2
                
              }
              M4<-c()
              for(m in 1:ncol(M1))  {
                for(n in 1 : ncol(M2)){
                  M1[,m]*M2[,n]->M3
                  cbind(M4, M3)->M4
                }
                
              }
              M4->M1
            }
            for(o in 1:ncol(M1)){
              data[,DV]*M1[,o]->coef1
              t.test(rowSums( matrix(coef1, ncol=prod(N.modalites2))), mu = 0, paired = FALSE, conf.level = 0.95)->C1
              rbind(Table.contrasts,c(C1$estimate, C1$parameter, C1$statistic, C1$p.value))->Table.contrasts
              
            }
          }
          
        }
        
        round(Table.contrasts,4)->Table.contrasts
        data.frame(Table.contrasts)->Table.contrasts  
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          names(Table.contrasts)<-c("estimateur", "ddl","t", "p")}else names(Table.contrasts)<-c("estimate", "df","t", "p.value")
        
        
        dimnames(Table.contrasts)[[1]]<-table.cont[,1]
        Table.contrasts$t^2/(Table.contrasts$t^2+Table.contrasts$ddl)->Table.contrasts$R.2
        round(Table.contrasts$t/(nlevels(data[,id]))^0.5,4)->Table.contrasts$D.Cohen
        Resultats[[.ez.anova.msg("title",40)]][[.ez.anova.msg("title",43)]]<-Table.contrasts
        
        
      } 
      
    }
  }
  ##### Bayes 
  if(any(param %in% c("bayes","Facteurs bayesiens", "Bayes Factors")) )  {
    modeleBF<-paste0(DV,"~")
    if(!is.null(cov)){
      for(i in 1 : length(cov)){
        modeleBF<-paste0( modeleBF, cov[i],"+")
      }
    }
    for(i in 1:length(withinbetween)){
      if(i !=length(withinbetween)) {
        modeleBF<-paste0( modeleBF, withinbetween[i],"*")
      } else {modeleBF<-paste0( modeleBF, withinbetween[i],"+", id)}
      
    }
    BF.out<-try(generalTestBF(as.formula(modeleBF), whichRandom=id,data=data, rscaleFixed=rscaleFixed , 
                              rscaleRandom= rscaleRandom, iterations=1000), silent=T)
    
    
    if(class(BF.out)=="try-error") Resultats[[.ez.anova.msg("title",43)]]<-.ez.anova.msg("msg",30) else{
      BF.out<-extractBF(BF.out)
      BF.out<-BF.out[,1:2]
      BF.out<-BF.out[which(grepl(id, dimnames(BF.out)[[1]])==F),]
      options("scipen"=7, "digits"=4)
      Resultats[[.ez.anova.msg("title",44)]]<-BF.out
      
    }
  } 
  
  
  
  if(any(param %in% c( "non param", "Modele non parametrique", "Non parametric"))){
    if(!is.null(between)){
      kruskal.test(as.formula( paste0(DV, "~",between[1])), data = data)->KW
      round(data.frame(KW$statistic,KW$parameter,KW$p.value),4)->KW
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(KW)<-c("H","ddl","valeur.p")
      }
      round((KW$H-nlevels(data[,between])+1)/(length(data[,1])-nlevels(data[,between])),4)->eta
      if(eta<0.0001) "<0.001"->KW$eta.2.H else KW$eta.2.H
      KW$espilon.2<-round(KW$H/((length(data[,1])^2-1)/(length(data[,1])+1)),4)
      Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",46)]]<-KW
       ans <- kwAllPairsConoverTest(as.formula( paste0(DV, "~",between[1])), data = data,p.adjust.method = p.adjust)
       comp<-expand.grid(dimnames(ans$p.value))
       comp<- paste0(comp[,1],"-", comp[,2])
       KW.MC<-data.frame(stat=c(ans$statistic), p=c(ans$p.value))
       dimnames(KW.MC)[[1]]<-comp
       KW.MC<-KW.MC[complete.cases(KW.MC),]
      KW.MC$p<-round.ps(KW.MC$p)
       Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",49)]]<- KW.MC
    }else{
      friedman<-friedman.test(as.formula(paste0(DV,"~", within[[1]], "|", id )),data=data)
      friedman<-round(data.frame(friedman$statistic,friedman$parameter,friedman$p.value),4)
      friedman$W.de.Kendall<-round(friedman[,1]/(nrow(data)*(nlevels(data[,unlist(within)])-1)),4)
      if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
        names(friedman)<-c("chi.2","ddl","valeur.p", "W.de.Kendall")
      }else { names(friedman)<-c("chi.2","df","p.value","Kendall's W") }
      Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",47)]]<-friedman
      ans<-frdAllPairsExactTest(y=data[,DV],groups=data[,within], blocks=data[,id], p.adjust = p.adjust)
      comp<-expand.grid(dimnames(ans$p.value))
      comp<- paste0(comp[,1],"-", comp[,2])
      F.MC<-data.frame(D.exact.test=c(ans$statistic), p=c(ans$p.value))
       dimnames(F.MC)[[1]]<-comp
       F.MC<-F.MC[complete.cases(F.MC),]
      F.MC$p<-round.ps(F.MC$p)
      Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",48)]]<-F.MC
      
      
    }
  }
  
  
  if(any(param %in% c("Statistiques robustes - peut prendre du temps", "robust", "Robust statistics - might take some time"))){
    if(length(between)==1 & is.null(within)){
      split(data[,DV], data[,between])->robuste
      try(unlist(WRS::med1way(robuste,iter = n.boot)), silent=T)->mediane
      if(class(mediane)!="try-error"){
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          names(mediane)<-c("Test", "Valeur.critique","valeur.p")
        }
        Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",51)]]<-round(mediane,4)
        # revoir
        if(is.list(contrasts)){
          contrasts<-contrasts[[1]]
          cont<-WRS::medpb(robuste,alpha=.05,nboot=n.boot,con=contrasts,bhop=FALSE)
          if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
            dimnames(cont$output)[[2]]<-c("Numero.contraste","Valeur.contraste",
                                          "valeur.p","p.critique.corrigee","lim.inf.IC","lim.sup.IC")
          }
          
          Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",42)]]<-cont$output
        }
        
        
      }else {
        Resultats[[.ez.anova.msg("title",49)]]<-.ez.anova.msg("msg",31)
        
      }
      
      
      try( WRS2::t1way(as.formula(paste0(DV, "~",between)), tr=.2,data=data),silent=T)->AR1
      if(class(AR1)!="try-error"){
        WRS2::t1way(as.formula(paste0(DV, "~",between)), tr=.2,data=data)->AR1
        WRS2::t1waybt(as.formula(paste0(DV, "~",between)), tr=.2, nboot=n.boot,data=data)->AR2
        data.frame(AR1[[2]],AR1[[3]],AR1[[1]],AR2[[2]],AR2[[3]],AR2[[4]], AR2[[5]])->AR1
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          names(AR1)<-c("ddl.num","ddl.denom","Stat","valeur.p","Var.expliquee","Taille.effet","Nombre.bootstrap" )
        }else {
          names(AR1)<-c("df.num","df.denom","Stat","p.value","Var.explained","effect size","N.bootstrap" )
        }
        
        
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-AR1
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",30)]]<-.ez.anova.msg("msg",32)
        
        try(WRS::lincon(robuste, tr=.2, con=contrasts),silent=T)->cont
        try(WRS::mcppb20(robuste, tr=.2, nboot=n.boot, con=contrasts),silent=T)->cont2
        if(class(cont)!= "try-error") {
          cont<-data.frame(cont$psihat[,2],cont$test[,4],cont$test[,5],cont$test[,2],cont$test[,3],cont2$psihat[,4],cont2$psihat[,5],cont2$psihat[,6])
          if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
            names(cont)<-c("Valeur.contraste","erreur.standard","ddl","test","seuil.critique","lim.inf.IC","lim.sup.IC","valeur.p")
          }else {
            names(cont)<-c("estimate","se","df","test","critic","lower.lim.CI","upper.lim.CI","p.value")
          }
          Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",42)]] <-cont
        }
        
      }else{
        Resultats[[.ez.anova.msg("title",52)]]<-.ez.anova.msg("title",33)
      }
      
    }
    
    
    if(length(between)==2 & is.null(within)) { 
      
      try( WRS2::t2way(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, tr = 0.2), silent=T)->T2
      if(class(T2)!="try-error"){
        T2<-matrix(unlist(T2[c(1:6)]), ncol=2, byrow=T)
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          dimnames(T2)[[2]]<-c("valeur", "valeur.p")
        }
        c(names(data[,between]), paste(names(data[,between])[1],":",names(data[,between])[2]))->dimnames(T2)[[1]]
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-T2
      }
      try(
        Resultats[[.ez.anova.msg("title",53)]][[.ez.anova.msg("title",51)]]<-WRS2::pbad2way(as.formula(paste0(DV, "~",between[1],"*",between[2])),
                                                                                            data=data, est = "mom", nboot = n.boot),silent=T)
      try( Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",51)]]<-WRS2::pbad2way(as.formula(paste0(DV, "~",between[1],"*",between[2])), 
                                                                                               data=data, est = "median", nboot = n.boot),silent=T)
      
      try(WRS2::mcp2a(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, est = "mom", nboot = n.boot), silent=T)->mediane
      if(class(mediane)!="try-error") {
        mediane$call<-NULL
        Resultats[[.ez.anova.msg("title",53)]][[.ez.anova.msg("title",42)]]<-mediane
      }
      
      try(mediane<-WRS2::mcp2a(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, est = "median", nboot = n.boot), silent=T)
      if(class(mediane)!="try-error") {
        mediane$call<-NULL
        Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",42)]]<-mediane
      }
    }
    
    if(length(between)==3 & is.null(within)){
      tronquees<-try( WRS2::t3way(as.formula(paste0(DV, "~",between[1],"*",between[2],"*",between[3])), data=data, tr = 0.2), silent=T)
      if(class(tronquees)!="try-error") {
        tronquees<-round(matrix(unlist(tronquees[c(1:14)]), ncol=2, byrow=T), 4)
        colnames(tronquees)<-c("F", "p")
        rownames(tronquees)<- nice(aov.out)$Effect
        Resultats[[.ez.anova.msg("title",52)]]<-tronquees  
      }
    }
    if(length(within)==1 & is.null(between)){
      ANOVA.tr<-try( WRS2::rmanova(data$value,data[,within[[1]]] ,data[,id]), silent=T)
      if(class(ANOVA.tr)!="try-error"){
        
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          ANOVA.tr<-round(data.frame("Valeur.test"= ANOVA.tr$test,"ddl1"=ANOVA.tr$df1, "ddl2"=ANOVA.tr$df2,"valeur.p"=ANOVA.tr$p.value),4)
          
        }else {
          ANOVA.tr<-round(data.frame("Test"= ANOVA.tr$test,"df1"=ANOVA.tr$df1, "df2"=ANOVA.tr$df2,"p.value"=ANOVA.tr$p.value),4)
        }
        
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-ANOVA.tr
        if((nlevels(data[,within[[1]]]))>2) {
          WRS2::rmmcp(data[,DV],data[, within[[1]]],data[,id])->comp
          comp$call<-NULL
          Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",39)]]<-comp
        }else Resultats[[.ez.anova.msg("title",10)]]<-.ez.anova.msg("msg",33)
      }
      
    } 
    
    if(length(between)==1 & length(within)==1){
      modeleR<-as.formula(paste0(DV, "~", within,"*", between))
      try(WRS2::tsplit( as.formula(modeleR), data[,id], data=data, tr = 0.2), silent=T)->tronquees
      if(class(tronquees)!="try-error"){
        tronquees<-matrix(unlist(tronquees)[c(1:12)],ncol=4, byrow=T)
        rownames(tronquees)<-c(within, between, paste0(within,":",between))
        colnames(tronquees)<-c("F", "p", "df1", "df2")
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]] <-tronquees
        WRS2::sppba(modeleR, data[,id], data=data, est = "mom", avg = TRUE, nboot = n.boot, MDIS = FALSE)->MoMa 
        WRS2::sppbb(modeleR, data[,id], data=data, est = "mom", nboot = n.boot)->MoMb
        WRS2::sppbi(modeleR, data[,id], data=data, est = "mom", nboot = n.boot)->MoMi 
        if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())){
          MoM<-data.frame("effet"= c(between,within[[1]],"interaction"), "valeur.p"=c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )
          
        }else {
          MoM<-data.frame("effect"= c(between,within[[1]],"interaction"), "p.value"=c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )
        }
        
        Resultats[[.ez.anova.msg("title",54)]][[.ez.anova.msg("title",51)]]  <-MoM
      }else Resultats[[.ez.anova.msg("title",10)]]<-.ez.anova.msg("msg",33)
      
    }
    
  }
  Resultats[["data"]]<-data
  Resultats[["aov.plus.in"]]<-aov.plus.in
  return(Resultats)
}  

round.ps<-function (x) 
{
    substr(as.character(ifelse(x < 0.0001, " <.0001", ifelse(round(x, 
        2) == 1, " >.99", formatC(x, digits = 4, format = "f")))), 
        2, 7)
}
