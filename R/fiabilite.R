.fiabilite.msg<-


fiabilite <-
  function(X=NULL,Y=NULL, data=NULL, choix=NULL, ord=NULL,outlier=.dico[["txt_complete_dataset"]], keys=NULL, n.boot=NULL, sauvegarde=F,
           imp=NULL, html=TRUE){
    # choix

    options (warn=-1)
    packages<-c('svDialogs', 'psych', 'lavaan')
    try(lapply(packages, library, character.only=T), silent=T)->test2
    rev<-FALSE
    if(class(test2)== 'try-error') return(ez.install())

    .e<- environment()
    Resultats<-list()
    if(is.null(data) | is.null(X))  {dial<-TRUE}else dial<-F
    if(dial || is.null(choix) || length(choix)!=1 ||choix %in% c(.dico[["txt_cronbach_alpha"]],"alpha","ICC","CCK",.dico[["txt_intraclass_correlation"]],.dico[["txt_kendall_coeff"]])==FALSE){
      dial<-T
      writeLines(.dico[["ask_chose_analysis"]])
      dlgList(c(.dico[["txt_cronbach_alpha"]], .dico[["txt_intraclass_correlation"]],.dico[["txt_kendall_coeff"]]), preselect=NULL, multiple = FALSE, title=.dico[["ask_analysis_type"]])$res->choix
      if(length(choix)==0) return(analyse())
    }


    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, nom=T)
      if(length(data)==0) return(analyse())
      nom<-data[[1]]
      data<-data[[2]]
    }else{
      deparse(substitute(data))->nom
    }

    if(choix=="CCK" | choix==.dico[["txt_kendall_coeff"]]){
      msg3<-.dico[["ask_chose_first_judge"]]
      type<-"factor"
      title<-.dico[["txt_judge1"]]
      multiple<-T
    } else{
      multiple<-T
      msg3<-.dico[["ask_chose_variable"]]
      type<-"numeric"
      title<-.dico[["txt_variables"]]
    }

    X<-.var.type(X=X, data=data, type=type, check.prod=F, message=msg3,  multiple=multiple, title=title, out=NULL)
    if(is.null(X)) {
      Resultats<-fiabilite()
      return(Resultats)}
    data<-X$data
    X<-X$X

    if(choix %in% c(.dico[["txt_cronbach_alpha"]],.dico[["txt_intraclass_correlation"]],"ICC","alpha") ){
      if(dial || length(outlier)>1 || outlier %in% c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]) ==FALSE){
     writeLines(.dico[["ask_analysis_on_complete_data_or_remove_outliers"]])
     writeLines(.dico[["desc_outliers_identified_on_mahalanobis"]])
        outlier<- dlgList(c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]), preselect=.dico[["txt_complete_dataset"]],multiple = FALSE, title=.dico[["ask_results_desired"]])$res
        if(length(outlier)==0) { Resultats<-fiabilite()
        return(Resultats)}
      }

      if(outlier==.dico[["txt_without_outliers"]]){
        inf<-VI.multiples(data[,X])
        Resultats[[.dico[["txt_labeled_outliers"]]]]<-inf[[.dico[["txt_labeled_outliers"]]]]
        data<-inf$data
      }


      if(choix %in% c(.dico[["txt_cronbach_alpha"]],"alpha"))  {
        if(dial){
         writeLines(.dico[["ask_variables_types_correlations"]])
          type<-dlgList(c(.dico[["txt_dichotomic_ordinal"]], .dico[["txt_continuous"]], "mixte"), preselect=NULL, multiple = FALSE, title=.dico[["ask_variables_type"]])$res
          if(length(type)==0) {Resultats<-fiabilite()
          return(Resultats)
          }} else{if(is.null(ord)) type<-.dico[["txt_continuous"]] else type<-.dico[["txt_dichotomic_ordinal"]]}

        if(dial){
          writeLines(.dico[["ask_are_there_inversed_items"]])
          rev<-dlgList(c(TRUE,FALSE), multiple = FALSE, title=.dico[["ask_inversed_items"]])$res
          if(length(rev)==0) {
            Resultats<-fiabilite()
            return(Resultats)
          }  }

          if(rev=="TRUE" || !is.null(keys) && any(keys %in% X==FALSE)){
            writeLines(.dico[["ask_specify_inverted_item"]])
            keys<-dlgList(X, multiple = TRUE, title=.dico[["ask_inversed_items"]])$res
            if(length(keys)==0) {
              Resultats<-fiabilite()
            return(Resultats)
            }
          }else keys<-NULL



          if(type==.dico[["txt_continuous"]]){
            if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
              msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
              n.boot<-NULL
            }
            while(is.null(n.boot)){
              writeLines(.dico[["ask_bootstrap_numbers_1_for_none"]])
              n.boot<-dlgInput(.dico[["ask_bootstraps_number"]], 1)$res
              if(length(n.boot)==0) {Resultats<-fiabilite()
              return(Resultats)}
              strsplit(n.boot, ":")->n.boot
              tail(n.boot[[1]],n=1)->n.boot
              as.numeric(n.boot)->n.boot
              if(is.na(n.boot) ||  n.boot%%1!=0 || n.boot<1){
                msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
                n.boot<-NULL
              }
            }
            psych::alpha(data[,X], keys=keys, n.iter=n.boot)->cron
          }else{
            n.boot<-0
            if(type=="mixte") {
              writeLines(.dico[["ask_ordinal_variables"]])
              ord<-dlgList(X, multiple = TRUE, title=.dico[["ask_ordinal_variables"]])$res
              if(length(ord)==0){
                Resultats<-fiabilite()
                return(Resultats)
              }
            }else ord<-X
            Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp=imp,html=F)[[1]]
            if(all(class(Matrice)!="matrix")) {
              sortie<-dlgMessage(.dico[["ask_exit_because_of_alpha_on_non_matrix"]], type="yesno")$res
              if(sortie=="yes") return(analyse()) else Matrice<-tetrapoly(data=data[,X],X=X,info=T, ord=ord,group=NULL,estimator='two.step',output='cor', imp="rm")[[1]]
            }

            psych::alpha(Matrice, keys=keys,n.obs=length(data[,1]))->cron
          }

          round(cron$total,3)->Resultats[[.dico[["txt_cronbach_alpha_on_whole_scale"]]]]
          if(n.boot>1) cron$boot.ci->Resultats[[.dico[["txt_confidence_interval_on_bootstrap"]]]]
          cron$total[,1]->a1
          cron$total[,6]->ase
          data.frame(Lim.inf.IC.95=a1-1.96*ase, alpha=a1, Lim.sup.IC.95=a1+1.96*ase)->Resultats[[.dico[["txt_confidence_interval_on_standard_error"]]]]
          round(data.frame(cron$alpha.drop, cron$item.stats ),3)->Resultats[[.dico[["txt_fiability_by_removed_item"]]]]

      }

      if(choix==.dico[["txt_intraclass_correlation"]]| choix=="ICC"){psych::ICC(data[,X], missing=FALSE)->ICC.out
        ICC.out[[1]]->Resultats[[.dico[["txt_intraclass_correlation"]]]]
        names(Resultats[[.dico[["txt_intraclass_correlation"]]]])<-c("type", "ICC", "F", .dico[["txt_df1"]], .dico[["txt_df2"]], .dico[["txt_p_dot_val"]], .dico[["txt_inferior_limit"]],.dico[["txt_ci_superior_limit"]])
        Resultats$"informations"<-paste(.dico[["desc_number_of_judge_is"]], length(X), .dico[["txt_and_the_number_of_obs"]], ICC.out$n.obs) }
    }


    if(choix==.dico[["txt_kendall_coeff"]]){
      msg4<-.dico[["ask_chose_second_judge"]]
      Y<-.var.type(X=Y, data=data, type=type, check.prod=F, message=msg4,  multiple=F, title=.dico[["txt_judge2"]], out=X)
      if(is.null(Y)) {
        Resultats<-fiabilite()
        return(Resultats)}
      data<-Y$data
      Y<-Y$X
      cohen.kappa(data[,c(X,Y)], w=NULL,n.obs=NULL,alpha=.05)->CK.out
      dimnames(CK.out$confid)<-list(c(.dico[["txt_non_pondered_coeff"]],.dico[["txt_pondered_kappa"]]),c(.dico[["txt_inferior_limit"]],.dico[["txt_estimation"]],.dico[["txt_ci_superior_limit"]]))
      round(CK.out$confid,3)->Resultats[[.dico[["txt_kendall_coeff"]]]]
      CK.out$agree->Resultats[[.dico[["txt_agreement"]]]]
      Resultats$information<-paste(.dico[["desc_number_of_observations_is"]], CK.out$n.obs)
    }

    if(dial) dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title=.dico[["ask_save_results"]])$res->sauvegarde
    if(length(sauvegarde)==0) {
      Resultats<-fiabilite()
      return(Resultats)
    }

    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    if(!is.null(keys)) paste(ord, collapse="','", sep="")->keys

    Resultats$Call<-paste0("fiabilite(X=c('", X,"'),Y=", ifelse(is.null(Y), "NULL", paste0("'",Y,"'")), ",data=", nom, ",choix='", choix,"',ord=",
                           ifelse(!is.null(ord),paste0("c('", ord, "')"), "NULL" ), ",outlier='", outlier, "', keys=", ifelse(!is.null(keys), paste0("c('",keys,"')"), "NULL"),
                           ",n.boot=", ifelse(!is.null(n.boot), n.boot, "NULL"), ", sauvegarde=", sauvegarde, ")")

    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))


    if(sauvegarde)save(Resultats=Resultats, choix=choix, env=.e)
    ref1(packages)->Resultats[[.dico[["txt_references"]]]]
   if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
  }
