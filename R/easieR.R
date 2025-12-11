.onAttach <- function(libname, pkgname) {
  load_language(lang='auto')
  textVersion =
    paste(.dico[["desc_how_to_cite_easier"]],
          .dico[["desc_easier_metapackage"]],
          sep = "")
  
  packageStartupMessage("##############")
  packageStartupMessage(.dico[["desc_welcome_in_easieR"]])
  packageStartupMessage(.dico[["desc_first_time_easier"]])
  packageStartupMessage(.dico[["desc_special_characters_have_been_removed"]])
  packageStartupMessage(textVersion)
  packageStartupMessage("Last update 12/11/2025 - 11.30")
  packageStartupMessage("##############")
  
}
easieR <-
  function(info=TRUE, html=T, lang=NULL){
    
    # 1. l'argument info permettra a terme de choisir les informations qui s'affichent dans la console ou non
    options (warn=1)
    options(scipen=999)
    test<-try(library(svDialogs), silent=T)
    if(class(test)== 'try-error') return(ez.install())
    # 2. choisir la langue
    if(!is.null(lang) ) load_language(lang=lang)
    
    library(rmarkdown)
    if(is.null(pandoc_version())){
      return(easieR.msg(msg=1))
    }
    
    choix <- dlgList(easieR.msg("2"), preselect=NULL, multiple = FALSE, title=easieR.msg("3"))$res
    if(length(choix)==0) return(writeLines(easieR.msg("4"))) else {
      if(choix %in%c(.dico[["txt_data_import_export_save"]], .dico[["txt_data_import_export_save"]])) Resultats <- donnees()
      if(choix %in% c(.dico[["txt_hypothesis_analysis"]], .dico[["txt_hypothesis_analysis"]])) Resultats <-analyse(html=html)
      if(choix%in%c(.dico[["txt_interface_objects_in_memory"]], .dico[["txt_interface_objects_in_memory"]])) Resultats <- interfaceR()
      if(choix%in% c(.dico[["txt_preprocess_sort_select_operations"]], .dico[["txt_preprocess_sort_select_operations"]])) Resultats<-preprocess()
      if(choix%in% c(.dico[["txt_teaching_material"]], .dico[["txt_teaching_material"]])) return(teaching())
      if(choix%in%c(.dico[["txt_graphics"]], .dico[["txt_graphics"]])) return(graphiques())
      return(Resultats)
    }
  }

easieR.msg<-function(msg="1"){
  #if(grepl("French",Sys.setlocale()) | grepl("fr",Sys.setlocale())) {
  #  msg<-switch(msg,
  #              "1"=c(.dico[["desc_for_easier_to_work"]]) ,
  #              "2"=c(.dico[["txt_data_import_export_save"]],
  #                    .dico[["txt_preprocess_sort_select_operations"]],
  #                    .dico[["txt_hypothesis_analysis"]], .dico[["txt_graphics"]],
  #                  .dico[["txt_interface_objects_in_memory"]],
  #                  .dico[["txt_teaching_material"]]),
  #              "3"=.dico[["ask_what_do_you_want"]],
  #              "4"=.dico[["txt_user_exited_easieR"]])
  #  }else {
  #    msg<-switch(msg, "1"="In order to ensure that easieR is properly installed, please install Pandoc at the following url :
  #   https://github.com/jgm/pandoc/releases",
  #                "2"=c("Data - (Import, export, save)", .dico[["txt_preprocess_sort_select_operations"]],
  #                      "Analyses - Hypothesis tests", "Graphics",
  #                    .dico[["txt_interface_objects_in_memory"]],
  #                    "Teaching material"),
  #                "3"="What do you want to do?",
  #                "4"="User has terminated easieR")
  #
  # }
  
  if      (msg=="1") c(.dico[["desc_for_easier_to_work"]]) -> msg
  else if (msg=="2") c(.dico[["txt_data_import_export_save"]],
                       .dico[["txt_preprocess_sort_select_operations"]],
                       .dico[["txt_hypothesis_analysis"]],
                       .dico[["txt_graphics"]],
                       .dico[["txt_interface_objects_in_memory"]],
                       .dico[["txt_teaching_material"]]) -> msg
  else if (msg=="3") .dico[["ask_what_do_you_want"]] -> msg
  else if (msg=="4") .dico[["txt_user_exited_easieR"]] -> msg
  return(msg)
}






#### RÃÂ©gressions logistiques ####
# ajouter les modÃÂ¨les multinomiques
# laisser la possibilitÃÂ© de faire d'autres distributions




#### statistiques descriptives ####


#### permet d'identifier et enlever les valeurs influentes ####
# pas encore intÃÂ©grÃÂ© ÃÂ  l'interface graphique mais dans les fonctions. Il faut rajouter l'interace graphique pour la faire fonctionnerdirectement de easier



VI.multiples<-function(data, X){
  # data =data.frame
  # X = variable names to include in the analysis
  require('pych')
  Resultats<-list()
  data$ideasy<- 1:NROW(data)
  nvar<-length(X)
  try(psych::outlier(data[,X], bad=T, na.rm=T,plot=T),silent=T)->essai
  if(class(essai)=='try-error'){
    msgBox(.dico[["desc_singular_matrix_mahalanobis_on_max_info"]])
    data2<-data
    rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
    which(rankifremoved == max(rankifremoved))->rangs
    if(length(rangs)==length(data2)){
      sample(rangs,1)->rang2
      data2[,-rang2]->data2
    } else {
      while(length(rangs)!=length(X)){
        sample(rangs,1)->rang2
        data2[,-rang2]->data2
        rankifremoved <- sapply(1:ncol(data2), function (x) qr(data2[,-x])$rank)
        which(rankifremoved == max(rankifremoved))->rangs
      }
    }
    try(psych::outlier(data2[,X]), silent=T)->essai
    if(class(essai)=='try-error') {
      corr.test(data2[,X])$r->matrice
      if(any(abs(matrice)==1)) {
        msgBox(.dico[["desc_perfectly_correlated_variables_in_matrix_trying_to_solve"]])
        which(abs(matrice)==1, arr.ind=TRUE)->un
        un<-un[-which(un[,1]==un[,2]),]
        data2[,-un[,2]]->data2
        try(psych::outlier(data2), silent=T)->essai
        if(class(essai)=='try-error') {
          writeLines(.dico[["desc_cannot_compute_mahalanobis"]])
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
  
  
  msgBox(paste(round(pourcent,2), .dico[["desc_percentage_outliers"]]))
  
  
  if(pourcent!=0){
    writeLines(.dico[["desc_outliers_removal_implications"]])
    
    suppr<- dlgList(c(.dico[["txt_suppress_all_outliers"]], .dico[["txt_suppress_outliers_manually"]]),
                    preselect=c(.dico[["txt_suppress_all_outliers"]]), multiple = FALSE, title=.dico[["ask_how_to_remove"]])$res
    if(length(suppr)==0) return(NULL)
    if(suppr==.dico[["txt_suppress_all_outliers"]]) {data[which(data$D.Mahalanobis<seuil),]->data
      outliers->Resultats[[.dico[["txt_labeled_outliers"]]]]}else{
        suppression<-"yes"
        outliers<-data.frame()
        while(suppression=="yes"){
          print(data[which.max(data$D.Mahalanobis),])
          cat (.dico[["ask_press_enter_to_continue"]])
          line <- readline()
          dlgMessage(.dico[["ask_suppress_this_obs"]], "yesno")$res->suppression
          if(suppression=="yes") {rbind(outliers, data[which.max(data$D.Mahalanobis),])->outliers
            data[-which.max(data$D.Mahalanobis),]->data
            
          }
        }
        Resultats[[.dico[["txt_labeled_outliers"]]]]<-outliers
      }
  }
  Resultats$data<-data
  return(Resultats)
}



#############################################
####                                     ####
####     Fonctions non commentees        ####
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
  reorder( c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]),levels(SBF$rscale))->levels(SBF$rscale)
  p1 <- ggplot(SBF, aes(x = n, y = log(BF), group=rscale)) + ylab(.dico[["txt_bayesian_factors_10"]]) +
    # p1 <- ggplot(SBF, aes(x = as.factor(n), y = log(BF), group=rscale)) + ylab(.dico[["txt_bayesian_factors_10"]]) +
    xlab("n")+ geom_line(aes(linetype=rscale))+ geom_point()
  p1<-p1+theme(plot.title = element_text(size = 12))+ggtitle(.dico[["txt_sequential_bayesian_factors_robustness_analysis"]])
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







.var.type<-function(X=NULL, info=T, data=NULL, type=NULL, check.prod=T, message=NULL, multiple=F, title=.dico[["txt_variable"]], out=NULL){
  # permet de selectionner des variables
  # verifie les conditions pour les variables qui doivent respecter certaines conditions
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
  
  if(!is.null(type) && all(type=="factor")){
    if(all(sapply(data[,X], class)%in% c("factor", "character"))!=T ) {
      res<-okCancelBox(.dico[["ask_transform_numerical_to_categorial_variables"]])
      if(res==F) {X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)}
    }
    if(length(X)==1) factor(data[,X])->data[,X] else lapply(data[, X], factor)->data[, X]
    if((length(X)==1 && nlevels(data[,X])<2) | (length(X)>1 && any(sapply(data[, X], nlevels)<2))) {
      okCancelBox(.dico[["ask_choose_a_variable_with_at_least_two_modalities"]])
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
      return(Resultats)
    }
    if(check.prod){
      if(length(X)>1 && prod(sapply(data[,X],nlevels))>length(data[,1])) {
        msgBox(.dico[["ask_redefine_analysis_because_modalities_product_is_superior_to_obs"]])
        .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title,out=out)->Resultats
        return(Resultats)
      }
      
    }
    
    
  }
  if(!is.null(type) && all(type%in%c("integer"))){
    if((any(data[,X]%%1==0) %in% c(FALSE, NA)) || min(data[,X])<0) {
      okCancelBox(.dico[["desc_variable_must_be_positive_int"]])
      X<-NULL
      .var.type(X=NULL, info=info, data=data, type=type,message=message, multiple=multiple, title=title, out=out)->Resultats
      return(Resultats)
    }
  }
  if(!is.null(type) && all(type %in%c("numeric", "integer"))){
    if(length(X)==1) moy<-is.na(mean(data[,X],na.rm=T)) else moy<-any(is.na(sapply(data[,X], mean, na.rm=T)))
    if(any(moy!=0) || any(var(data[,X],na.rm=T)==0)){
      okCancelBox(.dico[["desc_variable_must_be_numeric_and_of_non_null_variance"]])
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
.ez.options<-function(options='choix', n.boot=NULL,param=T, non.param=T, robust=T, Bayes=T, msg.options1=NULL, msg.options2=NULL, info=T, dial=T,
                      choix=NULL,sauvegarde=F, outlier=NULL, rscale=NULL){
  # options : character or vector. List of options that must be used ('choix', "outlier")
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
  if(any(options=='choix') & dial==T){
    choix<-c()
    if(param==T){
      if(info) writeLines(msg.options1)
      choix<-c(choix, .dico[["txt_param_tests"]])
    }
    if(non.param==T) {
      if(info) writeLines(msg.options2)
      choix<-c(choix, .dico[["txt_non_parametric_test"]])
    }
    if(robust==T) {
      if(info) writeLines(.dico[["desc_robust_statistics_are_alternative_to_the_principal_but_slower"]])
      choix<-c(choix, .dico[["txt_robusts_tests_with_bootstraps"]])
    }
    if(Bayes==T) {
      if(info) writeLines(.dico[["txt_bayesian_factors_compute_null_with_bayesian_approach"]])
      choix<-c(choix, .dico[["txt_bayesian_factors"]])
    }
    
    choix<- dlgList(choix, preselect=choix, multiple = TRUE, title=.dico[["ask_which_analysis"]])$res
    if(length(choix)==0) return(NULL)
  }
  Resultats$choix<-choix
  
  if(exists('choix') && any(choix== .dico[["txt_robusts_tests_with_bootstraps"]]) || !is.null(n.boot)){{
    if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
      msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
      n.boot<-NULL
    }
    while(is.null(n.boot)){
      writeLines(.dico[["ask_bootstrap_numbers_1_for_none"]])
      
      n.boot<-dlgInput(.dico[["ask_bootstraps_number"]], 1000)$res
      if(length(n.boot)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust,
                                         Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T,
                                         choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
        return(Resultats)}
      strsplit(n.boot, ":")->n.boot
      tail(n.boot[[1]],n=1)->n.boot
      as.numeric(n.boot)->n.boot
      if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
        msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
        n.boot<-NULL
      }
    }
  }
    Resultats$n.boot<-n.boot
  }
  if(!is.null(rscale)){
    if(dial & any(choix==.dico[["txt_bayesian_factors"]])|| (is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]])==F)) {
      if(info) writeLines(.dico[["ask_cauchy_apriori_distribution"]])
      rscale<-dlgList(c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]), preselect="moyen", multiple = F, title=.dico[["ask_distribution_type"]])$res
      if(length(rscale)==0) {
        .ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust,
                    Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T,
                    choix=choix,sauvegarde=F, outlier=NULL, rscale=rscale)->Resultats
      }
    }
    if(is.character(rscale)) {
      ifelse(rscale=="moyen", rscale<-2^0.5/2, ifelse(rscale==.dico[["txt_large"]], rscale<-1, ifelse(rscale==.dico[["txt_ultrawide"]], rscale<-2^0.5, rscale<-rscale)))
      Resultats$rscalei<-T
    } else Resultats$rscalei<-F
    
    Resultats$rscale<-rscale
  }
  
  
  if(any(options=="outlier")){
    if(dial || is.null(outlier)||
       (dial==F & any(outlier %in%c(.dico[["txt_complete_dataset"]], .dico[["txt_identifying_outliers"]],.dico[["txt_without_outliers"]],
                                    "complete", "id", "removed"))==F)) {
      if(info==TRUE) writeLines(.dico[["desc_complete_dataset_vs_identification_outliers_vs_without_outliers"]])
      Resultats$desires<- dlgList(c(.dico[["txt_complete_dataset"]], .dico[["txt_identifying_outliers"]],.dico[["txt_without_outliers"]]),
                                  preselect=c(.dico[["txt_complete_dataset"]],.dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]]),
                                  multiple = TRUE, title=.dico[["ask_which_analysis"]])$res
      if(length(Resultats$desires)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust,
                                                    Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T,
                                                    choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
        return(Resultats)}
    } else Resultats$desires<-outlier
  }
  
  if( dial==T) {Resultats$sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title=.dico[["ask_save_results"]])$res
  if(length(Resultats$sauvegarde)==0) {.ez.options(options=options, n.boot=NULL,param=param, non.param=non.param, robust=robust,
                                                   Bayes=Bayes, msg.options1=msg.options1, msg.options2=msg.options2, info=T, dial=T,
                                                   choix=choix,sauvegarde=F, outlier=NULL,rscale=rscale)->Resultats
    return(Resultats)}
  }else Resultats$sauvegarde<-sauvegarde
  
  return(Resultats)
  
}

# cree l'historique des commande (pour knitr)
.add.history<-function(data, command, nom){
  require(dplyr)
  try(get("ez.history", envir=.GlobalEnv),silent=T)->ez.history
  if(class(ez.history)=='try-error') {ez.history<-list()
  ez.history$Analyse[[1]]<-data
  names(ez.history)[length(ez.history)]<-paste(.dico[["txt_analysis_on"]],nom)
  names(ez.history[[length(ez.history)]])[1]<-nom
  ez.history[[length(ez.history)]]$historique<-command
  }else{
    if(nom==names(ez.history[[length(ez.history)]])[1] && any(all.equal(target=ez.history[[length(ez.history)]][[1]], current=data,
                                                                        ignore_col_order=T, ignore_row_order=T )!=TRUE)){
      ez.history[[length(ez.history)]]$historique<-rbind(ez.history[[length(ez.history)]]$historique,command)
    }else {
      ez.history$Analyse[[1]]<-data
      names(ez.history)[length(ez.history)]<-paste(.dico[["txt_analysis_on"]],nom)
      names(ez.history[[length(ez.history)]])[1]<-nom
      ez.history[[length(ez.history)]]$historique<-command
    }
    
  }
  
  assign("ez.history",ez.history, envir=.GlobalEnv)
}





# cree la liste avec tous les resultats
.add.result<-function(Resultats, name){
  
  try(get("ez.results", envir=.GlobalEnv),silent=T)->ez.results
  if(class(ez.results)=='try-error') {ez.results<-list()
  ez.results[[1]]<-Resultats
  }else{
    ez.results[[length(ez.results)+1]]<-Resultats
  }
  names(ez.results)[length(ez.results)]<-name
  assign("ez.results",ez.results, envir=.GlobalEnv)
}


### test de normalite
.normalite<-function(data=NULL, X=NULL, Y=NULL){
  # data : dataframe in which data are stored
  # X : character. Name or list of the variables for the numerical values.Multinormality is prefered if X>1
  # Y : character. Name or list of the variabes which are used as groups.
  packages<-c('outliers', 'nortest','psych','ggplot2')
  try(lapply(packages, library, character.only=T), silent=T)->test2
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
      names(normalite)<-c(.dico[["txt_shapiro_wilk"]], .dico[["txt_p_dot_val_sw"]], .dico[["txt_lilliefors_d"]], .dico[["txt_p_dot_val_lilliefors"]])
      dimnames(normalite)[1]<-" "
      #      format(normalite, width = max(sapply(names(normalite), nchar)), justify = "centre")->normalite
      n2[[.dico[["txt_normality_tests"]]]]<-normalite}
    
    
    #p1<-ggplot(data, aes(x=res))+geom_histogram(aes(y=..density..))
    p1<-ggplot(data, aes(x=res))+geom_histogram(aes(y=after_stat(density))) # The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    p1<-p1+ stat_function(fun = dnorm, colour = "red",
                          args = list(mean = mean(data[,"res"], na.rm = TRUE),
                                      sd = sd(data[,"res"], na.rm = TRUE)))
    p1<-p1+theme(plot.title = element_text(size = 12))+labs(x = .dico[["txt_residual_distribution"]])
    #print(p1)
    n2[[.dico[["txt_residuals_distribution"]]]]<-p1
    p2<-ggplot(data, aes(sample=res))+stat_qq()
    p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle("QQplot")
    n2$"QQplot"<-p2
    p3<-.multiplot(p1,p2,cols=2)
    print(p3)
  } else {
    try(mardia(data[,X],na.rm = TRUE, plot=TRUE), silent=TRUE)->mardia.results
    
    if(any(class(mardia.results)=="mardia")) {
      n2<-data.frame("n"=rep(mardia.results$n.obs),
                 Test =c(.dico[["txt_bp1"]],.dico[["txt_bp2"]],.dico[["txt_small"]] ),
                 stat = c( mardia.results$b1p,mardia.results$b2p,mardia.results$small.skew),
                  p = round(c(mardia.results$p.skew, mardia.results$p.kurt,mardia.results$p.small ),3))
      names(n2)=c("n", "test", .dico[["txt_Mardia_statistic"]], .dico[["txt_p_dot_val"]])
    } else {
      msgBox(.dico[["desc_matrix_is_singular_mardia_cannot_be_performed"]])
      n2<-data.frame(txt_shapiro_wilk=NULL, txt_p_dot_val_sw=NULL, txt_lilliefors_d=NULL, txt_p_dot_val_lilliefors=NULL)
      for(i in 1:length(X)){
        X[i]->Z
        .normalite(data=data, X=Z,Y=Y)->nor1
        n2<-rbind(n2, nor1[[1]])
      }
      dimnames(n2)[[1]]<-X
    }
  }
  return(n2)
  
}


# cree la liste avec tous les resultats
.stat.desc.out<-function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3, plot=T){
  packages<-c('psych', 'ggplot2')
  test2<-try(lapply(packages, library, character.only=T), silent=T)
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
    if(any(class(psych.desc)=='try-error')) {
      psych::describeBy(data[,X], group=groupes2,mat=F,type=type,digits=15, check=FALSE,skew = TRUE,
                        ranges = TRUE,trim=tr)->psych.desc
      expand.grid(sapply(groupes2, levels))->modalites
      for(i in 1:length(modalites[,1])) {
        if(is.null(psych.desc[[i]])) paste(.dico[["desc_no_obs_for_combination"]], paste(unlist(modalites[i,]), collapse=" & "))->Resultats[[i]] else   psych.desc[[i]]->Resultats[[i]]
        paste(unlist(modalites[i,]), collapse=" & ")->names(Resultats)[i]}
    } else psych.desc-> Resultats[[.dico[["txt_numeric_variables"]]]]
    
    
    
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
        Resultats[[.dico[["txt_graphics_informations"]]]][[1]]<-.dico[["desc_graph_thickness_gives_density"]]
        Resultats[[.dico[["txt_graphics_informations"]]]][[2]]<-.dico[["desc_red_dot_is_mean_error_is_sd"]]
      }
    }
    
  }
  if(!is.null(categ)) {
    for(i in 1:length(categ)) {
      Resultats$'Variables categorielles'[[categ[i]]] <-ftable(data[, c(categ[i], groupes)])
    }
  }
  
  return(Resultats)
}



ref1 <-
  function(packages){
    require('bibtex')
    c('base', packages, 'bibtex')->packages
    if(Sys.info()[[1]]=="Windows"){
      file.nametxt<-paste0(tempdir(), "\\references.bib")
    } else {
      file.nametxt<-paste0(tempdir(), "/references.bib")
    }
    
    write.bib(packages, file=file.nametxt)
    bibtex::read.bib(file.nametxt)->Resultats
    file.remove(file.nametxt)
    return(Resultats)
  }




