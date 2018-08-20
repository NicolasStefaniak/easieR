
easieR <-
function(info=TRUE){
  # 1. l'argument info permettra a terme de choisir les informations qui s'affichent dans la console ou non 
  options (warn=1)
  options(scipen=999)
  test<-try(library(svDialogs), silent=T)
  if(class(test)== "try-error") return(ez.install())
  # require(tcltk)
  # 
  # # 2. installer les packages nécessaires et MAJ des packages installés
  # # 2a. packages à installer, par ordre alphabétique
  # pack.to.inst <- c("afex", "akima",  "Amelia", "asbio","BayesFactor", "bibtex","car", "cobs", "corpcor", "DAAG","deldir", "DescTools","devtools", "doBy","dplyr", "epitools","emmeans",  
  #                   "foreign","ggplot2", "ggplotgui", "gmodels", "GPArotation", "gsl", "knitr","lars", "lsr", "MBESS", "mc2d", "mgcv", "mlogit", "nFactors", "nortest", 
  #                   "outliers", "pander","pgirmess", "phia", "pkgmaker", "plyr", "ppcor", "psych", "pwr", "QuantPsyc", "quantreg", "Rcpp", "readxl", "Rfit", 
  #                   "reshape2", "rmarkdown","rms", "robust", "robustbase","rpivotTable", "rtf", "rrcov", "scatterplot3d","semPlot", "sos", "sp", "stringi", "stringr", "svDialogs", "TeachingDemos",
  #                   "trimcluster", "wle", "WRS2")
  # 
  # # 2b. packages manquants
  # pack.uninst <- pack.to.inst[!(pack.to.inst %in% rownames(installed.packages()))]
  # 
  # # 2c. installer packages manquants si nécessaires et si utilisateur le souhaite
  # if(length(pack.uninst)>0){
  #   inst <- menu(choices=c("oui","non"), graphics=TRUE, title="Voulez-vous installer les packages manquants ?")
  #   if(length(inst)==0 || inst==2){
  #     tk_messageBox(type="ok", caption="Attention", message="Vous avez choisi de ne pas installer les packages manquants, cela peut gêner l'exécution de certaines fonctions. Relancez easieR() si vous souhaitez installer les packages.")
  #   } else {
  #     writeLines("Installation des packages")
  #     print(pack.uninst)
  #     flush.console()
  #     ## install devtools if necessary
  #     install.packages('devtools')
  #     ## Load devtools package for install_github()
  #     library(devtools)
  #     ## get BayesFactorExtras from github
  #     try(install_github("richarddmorey/BayesFactorExtras", subdir="BayesFactorExtras"), silent=T)
  #     install.packages(pack.uninst, quiet=TRUE)
  #     #WRS is a special case because it is not on CRAN
  #     if (!("WRS" %in% rownames(installed.packages()))) {
  #       # third: install an additional package which provides some C functions
  #       library("devtools")
  #       try(install_github("nicebread/WRS", subdir="pkg"),silent=T)
  #     }
  #   }
  # } 
  # flush.console()

  
  # 3. choix du groupe de fonctions
#  require(svDialogs)
  
  library(rmarkdown)
  if(is.null(pandoc_version())){
     if(grepl("mac",  .Platform$pkgType)){
     return(easieR.msg(msg=1))
  }else{
  
    install.packages("installr")
    library(installr)
    install.pandoc()
  }
  }
  
  
  choix <- dlgList(c("Données - (Importation, exportation, sauvegarde)", "Prétraitements (tri, sélection, opérations mathématiques, valeurs manquantes", 
                     "Analyses - Tests d'hypothèse", "Graphiques",
                     "Interface - objets en mémoire, nettoyer la mémoire, répertoire de travail", 
                     "Matériel pédagogique"), preselect=NULL, multiple = FALSE, title="Que voulez-vous ?")$res
  if(length(choix)==0) writeLines("Vous avez quitté easieR") else {
    if(choix=="Données - (Importation, exportation, sauvegarde)") Resultats <- donnees()
    if(choix=="Analyses - Tests d'hypothèse") Resultats <-analyse()
    if(choix=="Interface - objets en mémoire, nettoyer la mémoire, répertoire de travail") Resultats <- interfaceR()
    if(choix=="Prétraitements (tri, sélection, opérations mathématiques, valeurs manquantes") Resultats<-preprocess()
    if(choix== "Matériel pédagogique") return(teaching())
    if(choix=="Graphiques") return(graphiques())
    return(Resultats)

  }
}

easieR.msg<-function(msg=1){
  if(msg==1){
if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {msg<-"Pour que easieR fonctionne correctement, 
il faut installer Pandoc disponible à l'url suivant : https://github.com/jgm/pandoc/releases/tag/2.2.3.2" } else {
    msg<-"In order to ensure that easieR is properly installed, please install Pandoc at the following url :
https://github.com/jgm/pandoc/releases/tag/2.2.3.2" }}

return(msg)
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
    if(info)   writeLines("Voulez-vous centrer les variables numériques ? Centrer est généralement conseillé (e.g., Schielzeth, 2010).")
    scale<-dlgList(c("Centré", "Non centré"), multiple = FALSE, title="Centrer?")$res
    if(length(scale)==0) return(NULL)
    scale<-ifelse(scale=="Centré",T,F) 
  }
  Resultats$scale<-scale
  if(dial || !is.logical(inf) || !is.logical(CV)) {
    writeLines("Voulez-vous préciser d'autres options ? Vous pouvez en sélectionner plusieurs.
               Les méthodes de sélection permettent de sélectionner le meilleur modèle sur la base de critères statistiques.
               Les modèles hiérarchiques permettent de comparer plusieurs modèles. 
               Les validations croisées permettent de vérifier si un modèle n'est pas dépendant des données. Cette option est à utiliser notamment 
               avec les méthodes de sélection. L'analyse par groupe permet de réaliser la même régression pour des sous-groupes.
               Les mesures d'influences sont les autres mesures habituellement utilisées pour identifier les valeurs influentes.")
    autres.options<-c("Méthodes de sélection", "Modèles hiérarchiques", "Validation croisée","Mesure d influence",  "aucune")
    if(length(step2)<length(data))  autres.options<-c("analyse par groupes",autres.options)
    
    autres.options<- dlgList( autres.options, preselect=c("aucune"), multiple = TRUE, title="Autres options?")$res 
    if(length(autres.options)==0) return(.regressions.options(data=data, modele=modele))
    # if(any(autres.options=="aucune")) return(Resultats)   
    if(any(autres.options=="Mesure d influence") ) Resultats$inf<-T else  Resultats$inf<-F
    if(any(autres.options=="Validation croisée") ) Resultats$CV<-T else Resultats$CV<-F
  }else{Resultats$inf<-inf
  Resultats$CV<-CV 
  autres.options<-"aucune"
  }
  
  
  if(any(autres.options=="analyse par groupes") || !is.null(group)) {
    
    msg5<-"Veuillez choisissez le facteur de classement catégoriel."
    group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=T, message=msg5,  multiple=FALSE, title="Variable-s groupes", out=step2)
    if(length(group)==0) { return(.regressions.options(data=data, modele=modele))}
    data<-group$data
    group<-group$X 
    ftable(data[,group])->groupe.check
    if(any(is.na(groupe.check)) || min(groupe.check)<(length(dimnames(model.matrix(as.formula(modele), data))[[2]])+10)) {
      msgBox("Il faut au moins 10 observations plus le nombre de variables pour réaliser l'analyse. Vérifiez vos données.")
      return(groupe.check)
    }
  }
  
  if(any(autres.options=="Méthodes de sélection") || select.m!="none" & length(select.m)!=1 | !select.m%in%c("none","forward", "backward", "bidirectional","Forward - pas-à-pas ascendant",
                                                                                                             "Backward- pas-à-pas descendant", "Bidirectionnel")){
    if(info) writeLines("Veuillez choisir la méthode de sélection que vous souhaitez utiliser")
    select.m<- dlgList(c("Forward - pas-à-pas ascendant","Backward- pas-à-pas descendant", "Bidirectionnel"), 
                       preselect=NULL, multiple = FALSE, title="Choix de la méthode")$res
    if(length(select.m)==0) return(.regressions.options(data=data, modele=modele))
  } 
  if(!is.null(method)){
    if(any(autres.options=="Méthodes de sélection")   || (select.m!="none" && !method%in%c("AIC", "p", "F", "valeur du F","valeur de la probabilité", "AIC - Akaike Information criterion")) ){
      if(info) writeLines("Quel méthode faut-il appliquer pour la méthode de sélection ?")
      method<- dlgList(c("valeur du F","valeur de la probabilité", "AIC - Akaike Information criterion"), 
                       preselect=c("valeur du F"), multiple = FALSE, title="Choix de la méthode")$res
      if(length(method)==0) return(.regressions.options(data=data, modele=modele)) 
    }
    
    if(select.m!="none" & (method=="valeur du F" | method=="F")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<1)) {msgBox("Vous devez spécifier la valeur du F. Cette valeur doit être supérieure à 1")
        criteria<-NULL}
      
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur du F voulez-vous utiliser ?", 4)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria<1) {criteria<-NULL
          msgBox("Vous devez spécifier la valeur du F. Cette valeur doit être supérieure à 1")
          }
        }
      }
    }
    
    if(select.m!="none" & (method=="valeur de la probabilité" | method=="p")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<0 || criteria>1)) {msgBox("Vous devez spécifier la valeur de la probabilité. Cette valeur doit être entre 0 et 1")
        criteria<-NULL}
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput("Quelle valeur de la probabilité voulez-vous utiliser ?", 0.15)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria>1 || criteria<0 ) {criteria<-NULL
          msgBox("Vous devez spécifier la valeur de la probabilité. Cette valeur doit être entre 0 et 1")}
        }
      }
      qf(criteria, 1, (length(data[,1])-1-length(step1)), lower.tail = F, log.p = FALSE)->criteria
    }
  }
  if(any(autres.options=="Modèles hiérarchiques")| !is.null(step)) {
    
    if(!is.null(step) ){
      st1<-unlist(step)
      if(any(table(st1>1))) st1<-"erreur"
      if(any(!st1%in%step1 ))st1<-"erreur"
      if(st1=="erreur"){
        msgBox("Un problème a été identifié dans les étapes de votre régression hiérarchique")
        step<-NULL
      }
    }         
    if(is.null(step)){
      if(info) writeLines("Veuillez choisir les variables à utiliser pour chaque étape")      
      step<-list()
      step[[1]]<- dlgList(step1, preselect=NULL, multiple = TRUE, title="Variable(s) de cette étape")$res
      if(length(step[[1]])==0) return(.regressions.options(data=data, modele=modele))
      setdiff(step1,step[[1]])->step1
      
      while(length(step1!=0)){
        step[[length(step)+1]]<-dlgList(step1, multiple = TRUE,title="Variable(s) de cette étape")$res
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




#### RÃÂ©gressions logistiques ####
# ajouter les modÃÂ¨les multinomiques
# laisser la possibilitÃÂ© de faire d'autres distributions 




#### statistiques descriptives ####


#### permet d'identifier et enlever les valeurs influentes ####
# pas encore intÃÂ©grÃÂ© ÃÂ  l'interface graphique mais dans les fonctions. Il faut rajouter l'interace graphique pour la faire fonctionnerdirectement de easier



VI.multiples<-function(data){ require("pych") 
  Resultats<-list()
  nvar<-length(data)
  try(psych::outlier(data, bad=T, na.rm=T,plot=T),silent=T)->essai
  if(class(essai)=="try-error"){
    msgBox("Votre matrice est singulière, ce qui pose souci. Nous tentons de  de résoudre le souci. Si possible, la distance de Mahalanobis sera alors calculée sur le maximum d'information tout en évitant la singularité.")
    data->data2
    rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
    which(rankifremoved == max(rankifremoved))->rangs
    if(length(rangs)==length(data2)){ 
      sample(rangs,1)->rang2
      data2[,-rang2]->data2
    } else {
      while(length(rangs)!=length(data2)){
        sample(rangs,1)->rang2
        data2[,-rang2]->data2
        rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
        which(rankifremoved == max(rankifremoved))->rangs
      }
    }
    try(psych::outlier(data2), silent=T)->essai
    if(class(essai)=="try-error") {
      corr.test(data2)$r->matrice
      if(any(abs(matrice)==1)) {
        msgBox("vous tenter de faire une matrice de corrélations avec des variables parfaitement corrélées. Cela pose souci pour le calcul de la distance de Mahalanobis. Nous tentons de résoudre le souci")
        which(abs(matrice)==1, arr.ind=TRUE)->un
        un<-un[-which(un[,1]==un[,2]),]
        data2[,-un[,2]]->data2
        try(psych::outlier(data2), silent=T)->essai
        if(class(essai)=="try-error") {
          writeLines("Désolé, nous ne pouvons pas calculer la distance de Mahalanobis sur vos données. Les analyses seront résalisées sur les données complètes")
          0->data$D.Mahalanobis  }
      }else{essai-> data$D.Mahalanobis}
    } else{ essai-> data$D.Mahalanobis
    }
  }else{
    essai-> data$D.Mahalanobis  
  }
  
  qchisq(p=0.001, df=nvar, ncp = 0, lower.tail = FALSE, log.p = FALSE)->seuil
  data[which(data$D.Mahalanobis>seuil),]->outliers
  length(outliers[,1])/length(data[,1])*100->pourcent
  
  msgBox(paste(round(pourcent,2), "% des observations sont considérées comme outliers."))
  
  
  if(pourcent!=0){
    writeLines("Supprimer l'ensemble des outliers supprime l'ensemble des valeurs au-delà p(chi.deux)< 0.001.   
               Supprimer une observation à la fois permet de faire une analyse détaillée de chaque observation  
               considerée comme influente en partant de la valeur la plus extrême. La procédure s'arrête  
               quand plus aucune observation n'est considérée comme influente")  
    
    suppr<- dlgList(c("Suppression de l'ensemble des outliers", "Suppression manuelle"), 
                    preselect=c("Suppression de l'ensemble des outliers"), multiple = FALSE, title="Comment voulez-vous les supprimer?")$res
    if(length(suppr)==0) return(NULL)
    if(suppr=="Suppression de l'ensemble des outliers") {data[which(data$D.Mahalanobis<seuil),]->data 
      outliers->Resultats$"Valeurs considérées comme influentes"}else{
        suppression<-"yes"
        outliers<-data.frame()
        while(suppression=="yes"){
          print(data[which.max(data$D.Mahalanobis),])
          cat ("Appuyez [entrée] pour continuer")
          line <- readline()
          dlgMessage("Voulez-vous supprimer cette observation ?", "yesno")$res->suppression
          if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
            data[-which.max(data$D.Mahalanobis),]->data
            
          }
        }
        Resultats$"Valeurs considérées comme influentes"<-outliers
      }
  }
  Resultats$data<-data
  return(Resultats)
}




#############################################
####                                     ####
####     Fonctions non commentées        ####
####                                     ####
#############################################










.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
} 

.plotSBF<-function(SBF){
  min.y<-min(log(SBF$BF))
  max.y<-max(log(SBF$BF))
  etend.y<-max.y-min.y
  y_breaks<-c(min.y, min.y+1/4*etend.y ,min.y+1/2*etend.y ,min.y+3/4*etend.y , max.y )
  y_labs<-as.character(round(exp(y_breaks),2))
  reorder( c("moyen", "large", "ultra large"),levels(SBF$rscale))->levels(SBF$rscale)
  p1 <- ggplot(SBF, aes(x = n, y = log(BF), group=rscale)) + ylab("Facteur bayesiens 10") + 
    # p1 <- ggplot(SBF, aes(x = as.factor(n), y = log(BF), group=rscale)) + ylab("Facteur bayesiens 10") + 
    xlab("n")+ geom_line(aes(linetype=rscale))+ geom_point()
  p1<-p1+theme(plot.title = element_text(size = 12))+ggtitle("Facteurs bayesiens sequentiels - Analyse de robustesse")
  p1<-p1+scale_y_continuous(breaks = y_breaks, labels =y_labs )
  return(p1) 
}




########################################
####                                ####
####     Manipulation dataframe     ####
####                                ####
########################################


########################################
####                                ####
####         En construction        ####
####                                ####
########################################







.var.type<-function(X=NULL, info=T, data=NULL, type=NULL, check.prod=T, message=NULL, multiple=F, title="Variable", out=NULL){
  # permet de sélectionner des variables
  # vérifie les conditions pour les variables qui doivent respecter certaines conditions 
  # data : data.frame name which allow to check whether the variable is the data.frame
  # X : character. Name of the variable X (or vector allow to determine whether the selected variable belongs to data.frame)
  # info : logical. Should information be printed in the console ? 
  # liste
  # out : character or vector of names for variables of the data.frame which cannot be choosen (e.g. has already be choosen earlier). 
  # type : character. Class of variables which can be selected. One or several among "factor", "integer", "numeric". (see details). NULL means that all types are allowed
  # check.prod : logical. Should the product of the levels of factor variables be inferior to the number of rows?
  # message : message which should be printed if info is true
  # multiple : logical. Does the selection of several variables be allowed ? 
  # title : character. Title of the dialog box
  
  setdiff(names(data), out)->diff
  listes<-data.frame(paste(diff, "(format :", sapply(data[diff], class), ")", sep=" "), diff)
  
  if(is.null(X) | any(X %in% diff==F)) {
    if(info==T) writeLines(message)
    if(length(diff)>1){
      X<-dlgList(paste(diff, "(format :", sapply(data[,diff], class), ")", sep=" "), multiple = multiple, 
                 title=title)$res 
    } else {X<-dlgList(paste(diff, "(format :", class(data[,diff]), ")", sep=" "), multiple = multiple, 
                       title=title)$res}
    
    if(length(X)==0) return(NULL)
    subset(listes, listes[,1] %in% X)[,2]->X 
    as.character(X)->X}
  
  if(!is.null(type) && type=="factor"){
    if(all(sapply(data[,X], class)%in% c("factor", "character"))!=T ) {
      res<-okCancelBox("Vous devez utiliser des variables catégorielles. Voulez-vous transformer les variables numériques en variables catégorielles ?")
      if(res==F) {X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)}
    }
    if(length(X)==1) factor(data[,X])->data[,X] else lapply(data[, X], factor)->data[, X] 
    if((length(X)==1 && nlevels(data[,X])<2) | (length(X)>1 && any(sapply(data[, X], nlevels)<2))) {
      okCancelBox("Une variable catégorielle doit avoir au moins 2 modalités différentes. Veuillez choisir une variable avec au moins deux modalités")  
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
      return(Resultats)
    }
    if(check.prod){
      if(length(X)>1 && sapply(data[,X],nlevels)>length(data[,1])) {
        msgBox("Le produit des modalités des variables définissant les groupes est supérieur au nombre de vos observations. Il faut au moins une observation par combinaison de modalités de vos variables. Veuillez redéfinir votre analyse") 
        .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
        return(Resultats)
      }
      
    }
    
    
  }
  if(!is.null(type) && type=="integer"){
    if((any(data[,X]%%1==0) %in% c(FALSE, NA)) || min(data[,X])<0) {
      okCancelBox("la variable doit être un entier *integer* positif")
      X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)
    }
  }
  if(!is.null(type) && type=="numeric"){
    if(length(X)==1) moy<-is.na(mean(data[,X],na.rm=T)) else moy<-any(is.na(sapply(data[,X], mean, na.rm=T)))
    if(moy || var(data[,X],na.rm=T)==0){
      okCancelBox("la variable doit être numérique et avoir une variance non nulle.")
      X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)
    }
  }
  Resultats<-list()
  Resultats$X<-X
  Resultats$data<-data 
  
  
  return(Resultats)
}

# save : logical. Should the output be saved in rtf and R file ? 
.ez.options<-function(options="choix", n.boot=NULL,param=T, non.param=T, robust=T, Bayes=T, msg.options1=NULL, msg.options2=NULL, info=T, dial=T, 
                      choix=NULL,sauvegarde=F, outlier=NULL, rscale=NULL){
  # options : character or vector. List of options that must be used ("choix", "outlier") 
  # n.boot : Positive integer. Number of bootstrap that must be performed. 1 for no bootstrap
  # param : Logical. Is the parametric analysis  an option ? 
  # non.param : Logical. Is the non.parametric analysis  an option ? 
  # robust : Logical. Are robuste statistics  an option ? 
  # Bayes : Logical. are Bayes factors  an option ? 
  # msg.options1 : message that must be printed for the parametric analysis if info is true
  # msg.options2 : message that must be printed for the non-parametric analysis if info is true
  # info : logical. Must information be printed in the console ? 
  # dial = logical. Should dialog box be used ? 
  # choix = character or list of analyses that must be done c("parametric", "non parametric", "robust" or/and "bayesian")
  # sauvegarde = Logical. Must the results be saved ? 
  Resultats<-list()
  if(any(options=="choix") & dial==T){
    choix<-c()
    if(param==T){
      if(info) writeLines(msg.options1)
      choix<-c(choix, "Test paramétrique")
    } 
    if(non.param==T) {
      if(info) writeLines(msg.options2)
      choix<-c(choix, "Test non paramétrique")
    }
    if(robust==T) {
      if(info) writeLines("Les statistiques robustes sont des analyses alternatives à l'analyse principale, impliquant le plus souvent des bootstraps. Ces analyses sont souvent plus lentes")
      choix<-c(choix, "Test robustes - impliquant des bootstraps")
    }
    if(Bayes==T) {
      if(info) writeLines("Facteurs bayesiens : calcule l'équivalent du test d'hypothèse nulle en adoptant une approche bayesienne.")
      choix<-c(choix, "Facteurs bayesiens")
    }
    
    choix<- dlgList(choix, preselect=choix, multiple = TRUE, title="Quelle(s) analyses voulez-vous  ?")$res 
    if(length(choix)==0) return(NULL)
  } 
  Resultats$choix<-choix 
  
  
  if(exists("choix") && any(choix== "Test robustes - impliquant des bootstraps") || !is.null(n.boot)){{
    if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
      msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
      n.boot<-NULL
    }
    while(is.null(n.boot)){
      writeLines("Veuillez préciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
      
      n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
      if(length(n.boot)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                         Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                         choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
        return(Resultats)}
      strsplit(n.boot, ":")->n.boot
      tail(n.boot[[1]],n=1)->n.boot
      as.numeric(n.boot)->n.boot
      if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
        msgBox("Le nombre de bootstrap doit être un nombre entier positif") 
        n.boot<-NULL
      }
    }
  }
    Resultats$n.boot<-n.boot
  }
  if(!is.null(rscale)){
    if(dial & any(choix=="Facteurs bayesiens")|| (is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("moyen", "large", "ultralarge")==F)) {
      if(info) writeLines("Veuillez préciser la distribution a priori de Cauchy")
      rscale<-dlgList(c("moyen", "large", "ultralarge"), preselect="moyen", multiple = F, title="Quelle distribution voulez-vous  ?")$res 
      if(length(rscale)==0) {
        .ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                    Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                    choix=choix,sauvegarde=F, outlier=NULL, rscale=rscale)->Resultats
      }
    }
    if(is.character(rscale)) {
      ifelse(rscale=="moyen", rscale<-2^0.5/2, ifelse(rscale=="large", rscale<-1, ifelse(rscale=="ultralarge", rscale<-2^0.5, rscale<-rscale)))
      Resultats$rscalei<-T
    } else Resultats$rscalei<-F
    
    Resultats$rscale<-rscale
  }
  
  
  if(any(options=="outlier")){
    if(dial || is.null(outlier)|| (dial==F & any(outlier %in%c("Données complètes", "Identification des valeurs influentes","Données sans valeur influente"))==F)) {
      if(info==TRUE) writeLines("les données complètes représentent l'analyse classique sur toutes les données utilisables, l'identification des valeurs influentes
                                permet d'identifier les observations qui sont considérees statistiquement comme influençant les résultats.
                                les analyses sur les données sans les valeurs influentes réalise l'analyse après suppression des valeurs influentes. 
                                Cette option stocke dans la mémoire de R une nouvelle base de données sans valeur influente dans un objet portant le nom *nettoyees*")
      Resultats$desires<- dlgList(c("Données complètes", "Identification des valeurs influentes","Données sans valeur influente"), 
                                  preselect=c("Données complètes","Identification des valeurs influentes", "Données sans valeur influente"),
                                  multiple = TRUE, title="Quelles analyse voulez-vous ?")$res
      if(length(Resultats$desires)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                                    Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                                    choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
        return(Resultats)}
    } else Resultats$desires<-outlier
  }
  
  if( dial==T) {Resultats$sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Enregistrer les résultats ?")$res 
  if(length(Resultats$sauvegarde)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust, 
                                                   Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T, 
                                                   choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
    return(Resultats)}
  }else Resultats$sauvegarde<-sauvegarde
  
  return(Resultats)
  
}

# crée l'historique des commande (pour knitr)
.add.history<-function(data, command, nom){
  require(dplyr)
  try(get("ez.history", envir=.GlobalEnv),silent=T)->ez.history
  if(class(ez.history)=="try-error") {ez.history<-list()
  ez.history$Analyse[[1]]<-data
  names(ez.history)[length(ez.history)]<-paste("analyse sur",nom)
  names(ez.history[[length(ez.history)]])[1]<-nom  
  ez.history[[length(ez.history)]]$historique<-command 
  }else{
    if(nom==names(ez.history[[length(ez.history)]])[1] && all.equal(target=ez.history[[length(ez.history)]][[1]], current=data, ignore_col_order=T, ignore_row_order=T )!=TRUE){
      ez.history[[length(ez.history)]]$historique<-rbind(ez.history[[length(ez.history)]]$historique,command)
    }else {
      ez.history$Analyse[[1]]<-data
      names(ez.history)[length(ez.history)]<-paste("analyse sur",nom)
      names(ez.history[[length(ez.history)]])[1]<-nom  
      ez.history[[length(ez.history)]]$historique<-command 
    }
    
  }
  
  assign("ez.history",ez.history, envir=.GlobalEnv)  
}





# crée la liste avec tous les résultats
.add.result<-function(Resultats, name){
  
  try(get("ez.results", envir=.GlobalEnv),silent=T)->ez.results
  if(class(ez.results)=="try-error") {ez.results<-list()
  ez.results[[1]]<-Resultats
  }else{
    ez.results[[length(ez.results)+1]]<-Resultats
  }
  names(ez.results)[length(ez.results)]<-name
  assign("ez.results",ez.results, envir=.GlobalEnv)  
}


### test de normalité
.normalite<-function(data=NULL, X=NULL, Y=NULL){
  # data : dataframe in which data are stored
  # X : character. Name or list of the variables for the numerical values.Multinormality is prefered if X>1
  # Y : character. Name or list of the variabes which are used as groups. 
  packages<-c("outliers", "nortest","psych","ggplot2")
  if(length(X)==1){
    if(is.null(Y)){
      scale(data[,X], center=T, scale=F)->res
      res[1:length(res),]->data$res
    } else {
      tapply(data[,X], data[,Y], scale, center=T, scale=F)->res
      data$res<-unlist(res)
    }
    n2<-list()
    if(length(data[,"res"])<5000){
      shapiro.test(data[,"res"])->Shapiro_Wilk # realise le Shapiro-Wilk
      lillie.test(data[,"res"])->Lilliefors  # realise le Lilliefors
      round(data.frame(Shapiro_Wilk$statistic,Shapiro_Wilk$p.value, Lilliefors$statistic, Lilliefors$p.value),4)->normalite
      names(normalite)<-c("W de Shapiro-Wilk", "valeur.p SW", "D de Lilliefors", "valeur.p Llfrs")
      dimnames(normalite)[1]<-" "
      format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->normalite
      n2$"Test de normalité"<-normalite}
    
    
    p1<-ggplot(data, aes(x=res))+geom_histogram(aes(y=..density..))
    p1<-p1+ stat_function(fun = dnorm, colour = "red",
                          args = list(mean = mean(data[,"res"], na.rm = TRUE),
                                      sd = sd(data[,"res"], na.rm = TRUE)))
    p1<-p1+theme(plot.title = element_text(size = 12))+labs(x = "Distribution du résidu")
    #print(p1)
    n2$"Distribution des résidus"<-p1
    p2<-ggplot(data, aes(sample=res))+stat_qq() 
    p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("QQplot")
    n2$"QQplot"<-p2
    p3<-.multiplot(p1,p2,cols=2)
    print(p3)
  } else {
    try(mardia(data[,X],na.rm = TRUE, plot=TRUE), silent=TRUE)->mardia.results 
    
    if(any(class(mardia.results)=="mardia")) {
      data.frame("n"=mardia.results$n.obs, "N.var"=mardia.results$n.obs, "b1p"=mardia.results$b1p,"b2p"=mardia.results$b2p,
                 "skew"=mardia.results$skew,"p.skew"=mardia.results$p.skew,"small.skew"= mardia.results$small.skew,"p.small"= mardia.results$p.small,
                 "kurtosis"=mardia.results$kurtosis,"p.kurtosis"=mardia.results$p.kurt )->n2
    } else {
      msgBox("La matrice est singulière et le test de Mardia ne peut être réalisé. Seules les analyses univariées peuvent être réalisées")
      n2<-data.frame("W de Shapiro-Wilk"=NULL, "valeur.p SW"=NULL, "D de Lilliefors"=NULL, "valeur.p Llfrs"=NULL)
      for(i in 1:length(X)){
        X[i]->Z
        .normalite(data=data, X=Z,Y=Y)->nor1
        normalite<-rbind(n2, nor1)
      }
      dimnames(n2)[[1]]<-X
    }
  }
  return(n2)
  
}


# crée la liste avec tous les résultats
.stat.desc.out<-function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3, plot=T){
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  Resultats<-list()
  if(length(X)==1 && class(data[, X])=="factor"){X->categ 
    X<-NULL} else if(any(sapply(data[,X], class)=="factor")) {
      X[which(sapply(data[,X], class)=="factor")]->categ
      setdiff(X, categ)->X
    }else categ<-NULL
  
  if(length(X)!=0){
    if(is.null(groupes)) NULL->groupes2 else data.frame(data[,groupes])->groupes2
    try(  psych::describeBy(data[,X], group=groupes2,mat=(!is.null(groupes)),type=type,digits=4, check=FALSE,skew = TRUE, 
                            ranges = TRUE,trim=tr, fast=FALSE), silent=T)->psych.desc
    if(class(psych.desc)=="try-error") {
      psych::describeBy(data[,X], group=groupes2,mat=F,type=type,digits=15, check=FALSE,skew = TRUE, 
                        ranges = TRUE,trim=tr)->psych.desc
      expand.grid(sapply(groupes2, levels))->modalites
      for(i in 1:length(modalites[,1])) {
        if(is.null(psych.desc[[i]])) paste("pas d'observations pour la combinaison", paste(unlist(modalites[i,]), collapse=" & "))->Resultats[[i]] else   psych.desc[[i]]->Resultats[[i]]
        paste(unlist(modalites[i,]), collapse=" & ")->names(Resultats)[i]}
    } else psych.desc-> Resultats$'Variables numériques'
    
    
    
    if(plot){
      graphiques<-list()
      for(j in 1:length(X)){
        local({
          j<-j
          p <- ggplot(data, aes(x=factor(0), y=data[, X[j]])) + geom_violin()
          p<-p+ labs( y=X[j])
          p<-p + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
          
          if(!is.null(groupes)){
            if(length(groupes)==1) p<-p+ eval(parse(text=paste0("aes(x=", groupes, ")")))
            if(length(groupes)>=2){
              pr<-which.max(sapply(data[,groupes], nlevels))
              sec<-setdiff(groupes, names(pr))
              sec<-which(names(data)==sec[1])
              pr<-which(names(data)==names(pr)) 
              
              
              p<-p+ eval(parse(text=paste0("aes(x=", names(data)[pr], ", fill=" , names(data)[sec], ")")))
              p<-p+scale_fill_brewer(palette="PRGn")
              p<-p + theme(legend.position="right")
              p<- p+ geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize=1/4)
              if(length(groupes)>2){
                diff<-setdiff(groupes, names(data)[c(pr,sec)])
                for(i in 1:length(diff)){
                  if(i==1) paste0(".~", diff[1])->panneau
                  if(i==2) paste0(diff[2],"~", diff[1])->panneau
                  if(i>2 & i%%2!=0) paste0(panneau, "+", diff[i])->panneau 
                  if(i>2 & i%%2==0) paste0(diff[i], "+", panneau)->panneau
                } 
                p<-p+ facet_grid(as.formula(panneau), labeller=label_both)
              }  
            }
          }else{
            p<-p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)
            p<-p+scale_fill_brewer(palette="Dark2")
            p<-p + theme(legend.position="none") 
          }
          graphiques[[j]]<<-p
        })
        
        Resultats$Graphiques<-graphiques
        Resultats$"Informations sur les graphiques"[[1]]<-"L'épaisseur du graphique donne la densité, permettant de mieux cerner la distribution."
        Resultats$"Informations sur les graphiques"[[2]]<-"Le point rouge est la moyenne. La barre d'erreur est l'écart-type"
      }
    }
    
  }
  if(!is.null(categ)) {
    for(i in 1:length(categ)) {
      Resultats$'Variables catégorielles'[[categ[i]]] <-ftable(data[, c(categ[i], groupes)]) 
    }
  }
  
  return(Resultats)
}



ref1 <-
function(packages){
  require("bibtex")
  c("base", packages, "bibtex")->packages
  write.bib(packages, file='references')
  bibtex::read.bib('references.bib')->Resultats
  file.remove('references.bib')
  return(Resultats)
}



.onAttach <- function(libname, pkgname) {

  packageStartupMessage("##############\n Welcome in easieR -  For more information, please visit :https://theeasierproject.wordpress.com/")
  packageStartupMessage(" If you are using easieR for the first time, please use the function ez.install in order to ensure that easieR will work properly.\n Si vous utilisez easieR pour la 1e fois, veuillez utiliser la fonction ez.install pour vous assurer de bon fonctionnement de easieR.")
  packageStartupMessage("##############")
 # options(encoding="UTF-8")
}
