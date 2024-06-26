ez.cfa <-
  function(modele=NULL, X=NULL, data=NULL,ord=NULL, outlier=.dico[["txt_complete_dataset"]],imp="rm", output="default", info=T, sauvegarde=F, mimic=NULL, fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
           estimator="ML",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
           link="probit",int.ov.free=FALSE, int.lv.free=FALSE, std.lv=FALSE, n.boot=1000, group.w.free=F,
           group.equal=c("loadings", "intercepts", "means", "thresholds", .dico[["txt_regressions"]], "residuals", "residual.covariances", "lv.variances" , "lv.covariances")){
    # modele : lavaan modele if X is null
    # data : dataframe
    # X : character. names of the variables if modele is null
    # LV : Vector. names of LV=atent Variables
    # ord: Character. Vector of ordered variables among X
    # outlier : should outliers be detected and removed on Mahalanobis distance ? (.dico[["txt_without_outliers"]]) or not (.dico[["txt_complete_dataset"]])
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
        if(class(semPlot.modele)=='try-error'){
          msgBox(.dico[["desc_model_seems_incorrect_could_not_be_created"]])
          return(NULL)
        }
        semPaths(semPlot.modele, edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T, cex=0.5)
        cat (.dico[["ask_press_enter_to_continue"]])
        line <- readline()
        dlgMessage(.dico[["ask_is_model_correct"]], "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T))
        return(modele)
      }

      if(is.null(LV) && length(X)>3) {
        if(info)   writeLines(.dico[["ask_latent_variables_number"]])
        nF<-NA
        while(!is.numeric(nF)) {
          if(info) writeLines(.dico[["ask_latent_variables_number"]])
          nF <- dlgInput(.dico[["ask_factors_number"]], 2)$res
          if(length(nF)==0) return(NULL)
          strsplit(nF, ":")->nF
          tail(nF[[1]],n=1)->nF
          as.numeric(nF)->nF
          if(any((nF%%1==0)%in% c(FALSE, NA))|| nF<0 || nF>length(X) ){
            msgBox(.dico[["desc_facotrs_must_be_positive_int_inferior_to_variables_num"]])
            nF<-NA }
        }} else if(!is.null(LV)) nF<-length(LV) else nF<-1

        O2<-c()
        X->reste
        list()->modele2
        for(i in 1:nF){
          if(is.null(LV[i]))  {dlgInput(paste(.dico[["txt_latent_variable_name"]],i,  "?"), paste(.dico[["txt_factor"]],i, sep="."))$res->noms
            if(length(noms)==0) return(Lav.modele(X=X, LV=NULL))
            strsplit(noms, ":")->noms
            tail(noms[[1]],n=1)->noms} else noms<-LV[i]
            title<-paste(.dico[["desc_manifest_variables_of"]], noms)
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

        cat (.dico[["ask_press_enter_to_continue"]])
        line <- readline()
        dlgMessage(.dico[["ask_is_model_correct"]], "yesno")$res->suppression
        if(suppression=="no") return( Lav.modele(X=X, modele=NULL, LV=NULL, info=T))
        return(modele)
    }
    .ez.lavaan.options<-function(modele=NULL, data=NULL, X=NULL, info=TRUE, opt.list=NULL, dial=T, imp=NULL, outlier=NULL,output=NULL){
      if(dial || is.null(opt.list$mimic) || !opt.list$mimic%in% c("default", "Mplus", "EQS")){dial<-T
      if(info) writeLines(.dico[["ask_specify_all_parameters_or_imitate_specific_software"]])
      opt.list$mimic<-dlgList(c("default", "Mplus", "EQS"), preselect="default", multiple = FALSE, title=.dico[["ask_imitate"]])$res
      if(length(opt.list$mimic)==0) return(NULL)
      }

      if(dial){
        if(opt.list$mimic=="default"){
          options2<-c(.dico[["txt_exogenous_fixed_variables"]], .dico[["txt_cfa_information_default"]], .dico[["txt_cfa_continuity_correction_zero_keep_margins_default"]],
                      .dico[["txt_likelihood_only_for_estimator"]])
        } else options2<-c()
        options<-c(.dico[["txt_cfa_estimator_ml_default"]], .dico[["txt_cfa_groups_null_default"]], .dico[["txt_cfa_test_standard_default"]], .dico[["txt_cfa_standard_error_default"]], .dico[["txt_cfa_observed_variabes_standardization_true_default"]],
                   .dico[["txt_factors_ortho"]], .dico[["txt_link_only_for_estimator"]],
                   .dico[["txt_observed_variables_intercept"]], .dico[["txt_latent_variables_intercept"]], .dico[["txt_exogenous_fixed_variables"]],
                   .dico[["txt_cfa_latent_variables_indicators_estimates_true_default"]], options2)

        if(info) writeLines(.dico[["ask_which_options_to_specify"]])
        options<-dlgList(c(.dico[["txt_keep_default_values"]], options), preselect=c(.dico[["txt_cfa_estimator_ml_default"]],.dico[["txt_cfa_test_standard_default"]], .dico[["txt_cfa_standard_error_default"]]), multiple = TRUE, title=.dico[["ask_which_options"]])$res
        if(length(options)==0) return(NULL)
        if(options==.dico[["txt_keep_default_values"]]) return(list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                                                                 estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                                                                 link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                                                                 group.equal=c("loadings", "intercepts", "means", "thresholds", .dico[["txt_regressions"]], "residuals", "residual.covariances",
                                                                               "lv.variances" , "lv.covariances")))
      } else options<-NULL



      if(any(options==.dico[["txt_cfa_estimator_ml_default"]])|is.null(opt.list$estimator) || length(opt.list$estimator)!=1||
         try(opt.list$estimator %in%c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ),silent=T)!=T){
        if(info){  writeLines(.dico[["desc_wls_corresponds_to_adf_plus_explaination_other_estimators"]])
          abb<-data.frame(abb=c("ML","GLS", "WLS", "ULS", "DWLS"), nom=c(.dico[["txt_max_likelihood"]],.dico[["txt_less_square_generalized"]],.dico[["txt_less_square_pondered"]],.dico[["txt_less_square_not_pondered"]],.dico[["txt_less_square_diagonally_pondered"]]))
          print(abb)    }
        opt.list$estimator<-dlgList(c("ML","GLS", "WLS", "ULS", "DWLS", "MLM","MLMV","MLMVS","MLF", "MLR", "WLSM","WLSMV", "ULSM", "ULSMV" ), multiple = FALSE, title=.dico[["ask_which_estimator"]])$res
        if(length(opt.list$estimator)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }


      if(any(options==.dico[["txt_cfa_test_standard_default"]]) || length(opt.list$test)!=1 || !opt.list$test%in% c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted",
                                                                                                  "scaled.shifted", "bootstrap","Bollen.Stine")){
        if(info) writeLines(.dico[["ask_which_test"]])
        opt.list$test<-dlgList(c("standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted","scaled.shifted", "bootstrap","Bollen.Stine"), multiple = FALSE, title=.dico[["ask_which_estimator"]])$res
        if(length(opt.list$test)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }


      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine") &&!is.null(opt.list$n.boot) && ((class(opt.list$n.boot)!="numeric" & class(opt.list$n.boot)!="integer") ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1)){
        msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
        opt.list$n.boot<-NULL
      }
      if(dial & opt.list$test%in%c("boot","bootstrap","Bollen.Stine") || is.null(opt.list$n.boot) & opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) {
        while(is.null(opt.list$n.boot)){
          writeLines(.dico[["ask_bootstrap_numbers_1_for_none"]])
          n.boot<-dlgInput(.dico[["ask_bootstraps_number"]], 1)$res
          if(length(n.boot)==0) {Resultats<-Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(n.boot, ":")->n.boot
          tail(n.boot[[1]],n=1)->n.boot
          as.numeric(n.boot)->opt.list$n.boot
          if(is.na(opt.list$n.boot) ||  opt.list$n.boot%%1!=0 || opt.list$n.boot<1){
            msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
            opt.list$n.boot<-NULL
          }
        }
      }

      if( any(is.na(data[,X])) & opt.list$estimator=="ml" & opt.list$mimic=="default") opt.list$missing<-"fiml" else opt.list$missing<-"default"

      if(opt.list$test%in%c("boot","bootstrap","Bollen.Stine")) se1<-c("standard","first.order", "robust", "bootstrap","none" ) else se1<-c("standard","first.order", "robust", "none" )
      #if(any(options=="erreur standard [se]") || is.null(opt.list$se) || !opt.list$se%in%se1)  { # problem here with original version? erreur standard [se] is not defined elsewhere
      if(any(options==.dico[["txt_cfa_standard_error_default"]]) || is.null(opt.list$se) || !opt.list$se%in%se1)  {
        if(info) writeLines(.dico[["ask_how_standard_error_must_be_estimated"]])
        opt.list$se<-dlgList(se1, multiple = FALSE, title=.dico[["ask_standard_error"]])$res
        if(length(opt.list$se)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      if(any(options==.dico[["txt_cfa_groups_null_default"]]) || !is.null(opt.list$group)){
        msg2<-.dico[["ask_chose_defining_groups"]]
        .var.type(X=opt.list$group, info=T, data=data, type="factor", message=msg2,multiple=T, title=.dico[["ask_group_variable"]], out=X)->group
        if(is.null(group)){
          Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
        group$data->data
        group$X->opt.list$group
        if(dial|| any(opt.list$group.equal %in% c("loadings", "intercepts","means","thresholds",.dico[["txt_regressions"]],"residuals","residual.covariances","lv.variances", "lv.covariances"))==FALSE){
          if(info) writeLines(.dico[["ask_which_constant_parameters"]])
          opt.list$group.equal<-dlgList(c("loadings", "intercepts","means","thresholds",.dico[["txt_regressions"]],"residuals","residual.covariances","lv.variances", "lv.covariances"), multiple = T,
                                        preselect=c("loadings", "intercepts","means","thresholds",.dico[["txt_regressions"]],"residuals","residual.covariances","lv.variances", "lv.covariances"), title=.dico[["ask_constant_parameters"]])$res
          if(length(opt.list$group.equal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
          }}
        # ecrase group equal puisque aa libere les group sur cette contraintes ==> utilite ?
        #group.partial<-dlgList(c("loadings", "intercepts","means","thresholds",.dico[["txt_regressions"]],"residuals","residual.covariances","lv.variances", "lv.covariances"))
        if(info) writeLines(.dico[["ask_are_frequences_free_parameters"]])
        opt.list$group.w.free<-dlgList(c(TRUE, FALSE), multiple=F, preselect=FALSE, title=.dico[["ask_freq_constance"]])$res
        if(length(opt.list$group.w.free)==0) {Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      }
      ### zero.keep.margins
      if(any(options==.dico[["txt_cfa_continuity_correction_zero_keep_margins_default"]]) || is.null(opt.list$zero.keep.margins)||(!is.logical(opt.list$zero.keep.margins) & opt.list$zero.keep.margins!="default")){
        if(info) writeLines(.dico[["ask_add_a_value_to_empty_cells"]])
        opt.list$zero.keep.margins<-dlgList(c(TRUE, FALSE,"default"), preselect="default", multiple = FALSE, title=.dico[["ask_empty_cells"]])$res
        if(length(opt.list$zero.keep.margins)==0) {
          Resultats<-Resultats<.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)
        }
      }

      if( opt.list$zero.keep.margins==TRUE){
        if(!is.null(opt.list$zero.add) && ((class(opt.list$zero.add)!="numeric" ) || any( opt.list$zero.add<0) || any(opt.list$zero.add>1))){
          msgBox(.dico[["txt_correction_for_polyc_corr_must_be_between_zero_and_one"]])
          opt.list$zero.add<-NULL
        }
        while(is.null(opt.list$zero.add)){
          writeLines(.dico[["ask_2x2_table_value"]])
          zero.add1<-dlgInput(.dico[["ask_2x2_table"]], 0.5)$res
          if(length(zero.add1)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
          return(Resultats)}
          strsplit(zero.add1, ":")->zero.add1
          tail(zero.add1[[1]],n=1)->zero.add1
          as.numeric(zero.add1)->zero.add1
          if(is.na(zero.add1) ||  zero.add1<0 || zero.add1>1){
            msgBox(.dico[["desc_value_must_be_between_zero_and_one"]])
            opt.list$zero.add<-NA} else{
              writeLines(.dico[["ask_bigger_tables_value"]])
              #zero.add2<-dlgInput("tableau > 2x2 ?", 0)$res
              zero.add2<-dlgInput(.dico[["ask_2x2_table"]], 0)$res
              if(length(zero.add2)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
              return(Resultats)}
              strsplit(zero.add2, ":")->zero.add2
              tail(zero.add2[[1]],n=1)->zero.add2
              as.numeric(zero.add2)->zero.add2
              if(is.na(zero.add2) ||  zero.add2<0 || zero.add2>1){
                msgBox(.dico[["desc_value_must_be_between_zero_and_one"]])
                opt.list$zero.add<-NA}
            }
          opt.list$zero.add<-c(zero.add1,zero.add2)

        }
      }


      ### fin zero.keep.margins
      if(any(options==.dico[["txt_likelihood_only_for_estimator"]]) & opt.list$mimic=="default" & opt.list$estimator=="ML" ||is.null(opt.list$likelihood) || length(opt.list$likelihood)!=1 || try(opt.list$likelihood%in%c("wishart","normal", "default" ),silent=T)!=T) {
        if(info) writeLines(.dico[["ask_specify_likelihood"]])
        opt.list$likelihood<-dlgList(c("wishart","normal", "default" ), multiple=F, preselect="default", title=.dico[["ask_likelihood"]])$res # depend de mimic
        if(length(opt.list$likelihood)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      if(any(options==.dico[["txt_link_only_for_estimator"]]) & opt.list$estimator=="MML" ||length(opt.list$link)!=1 || try(opt.list$link%in%c("logit","probit" ),silent=T)!=T ){
        if(info) writeLines(.dico[["ask_family"]])
        opt.list$link<-dlgList(c("logit","probit" ), multiple=F, preselect=FALSE, title=.dico[["ask_distribution"]])$res
        if(length(opt.list$link)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }


      if(any(options==.dico[["txt_cfa_information_default"]]) ||is.null(opt.list$information) || try(opt.list$information%in%c("expected","observed", "default" ),silent=T)!=T ){
        if(info) writeLines(.dico[["ask_which_information_matrix_for_standard_error_estimation"]])
        opt.list$information<-dlgList(c("expected","observed", "default" ), multiple=F, preselect=FALSE, title=.dico[["ask_information_matrix"]])$res
        if(length(opt.list$information)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      if(any(options==.dico[["txt_exogenous_fixed_variables"]]) ||length(opt.list$fixed.x)!=1 || (!is.logical(opt.list$fixed.x) & opt.list$fixed.x!="default") ){
        if(info) writeLines(.dico[["desc_if_true_covariates_as_fixed"]])
        opt.list$fixed.x<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_fixed_covariables"]])$res
        if(length(opt.list$fixed.x)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      if(any(options==.dico[["txt_factors_ortho"]]) ||length(opt.list$orthogonal)!=1 || !is.logical(opt.list$orthogonal) ){
        if(info) writeLines(.dico[["ask_correlated_or_orthogonal_factors"]])
        opt.list$orthogonal<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_factors_ortho"]])$res
        if(length(opt.list$orthogonal)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      if(any(options==.dico[["txt_cfa_observed_variabes_standardization_true_default"]]) ||length(opt.list$std.ov)!=1 || !is.logical(opt.list$std.ov) ){
        if(info) writeLines(.dico[["ask_standardize_obs_variables_before"]])
        opt.list$std.ov<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_standardization"]])$res
        if(length(opt.list$std.ov)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }

      #####
      if(any(options==.dico[["txt_observed_variables_intercept"]]) ||length(opt.list$int.ov.free)!=1 || !is.logical(opt.list$int.ov.free) ){
        if(info) writeLines(.dico[["ask_should_intercept_of_obs_variables_be_fixed_to_zero"]])
        opt.list$int.ov.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_observed_variables_intercept_zero"]])$res
        if(length(opt.list$int.ov.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }


      if(any(options==.dico[["txt_latent_variables_intercept"]]) ||length(opt.list$int.lv.free)!=1 || !is.logical(opt.list$int.lv.free) ){
        if(info) writeLines(.dico[["ask_should_intercept_of_latent_variable_be_fixed_to_zero"]])
        opt.list$int.lv.free<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_latent_variables_intercept_zero"]])$res
        if(length(opt.list$int.lv.free)==0) {Resultats<-.ez.lavaan.options(X=X, data=data, opt.list=opt.list)
        return(Resultats)}
      }



      if(any(options==.dico[["txt_cfa_latent_variables_indicators_estimates_true_default"]]) ||length(opt.list$std.lv)!=1 || !is.logical(opt.list$std.lv) ){
        if(info) writeLines(.dico[["desc_if_true_latent_residuals_one"]])
        opt.list$std.lv<-dlgList(c(TRUE, FALSE ), multiple=F, preselect=FALSE, title=.dico[["ask_standardization_vl"]])$res
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
        msg3<-.dico[["ask_chose_manifest_variables_at_least_three"]]

        X<-.var.type(X=X, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=T, title=.dico[["txt_variables"]], out=NULL)
        data<-X$data
        X<-X$X
        if(is.null(X) || length(X)<3) return(NULL)


        if(dial || length(outlier)>1 || outlier %in% c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]) ==FALSE){
          if(info) writeLines(.dico[["ask_analysis_on_complete_data_or_remove_outliers"]])
          if(info) writeLines(.dico[["desc_outliers_identified_on_mahalanobis"]])
          outlier<- dlgList(c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]), preselect=.dico[["txt_complete_dataset"]],multiple = FALSE, title=.dico[["ask_results_desired"]])$res
          if(length(outlier)==0) { Resultats<-cfa.in()
          return(Resultats)}
        }

        if(outlier==.dico[["txt_without_outliers"]]){
          inf<-VI.multiples(data,X)
          Resultats[[.dico[["txt_labeled_outliers"]]]]<-inf[[.dico[["txt_labeled_outliers"]]]]
          data<-inf$data
        }

        if(dial){
          if(info) writeLines(.dico[["ask_variables_type_correlations"]])
          if(length(unique(unlist(data[,X])))<9) {type<-dlgList(c(.dico[["txt_dichotomic_ordinal"]],.dico[["txt_continuous"]], "mixte"), preselect=NULL, multiple = FALSE, title=.dico[["ask_variables_type"]])$res}else {
            type<-dlgList(c(.dico[["txt_continuous"]], "mixte"), preselect=NULL, multiple = FALSE, title=.dico[["ask_variables_type"]])$res
          }

          if(length(type)==0) {Resultats<-cfa.in()
          return(Resultats)}
        } else{if(is.null(ord)) type<-.dico[["txt_continuous"]] else type<-.dico[["txt_dichotomic_ordinal"]]
        }

        if(type!=.dico[["txt_continuous"]]){
          if(type=="mixte") {
            if(info) writeLines(.dico[["ask_ordinal_variables"]])
            ord<-dlgList(X, multiple = TRUE, title=.dico[["ask_ordinal_variables"]])$res
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
        if(is.null(imp))  {msgBox(.dico[["ask_missing_values_detected_what_to_do"]])
          imp<- dlgList(c(.dico[["txt_do_nothing_keep_all_obs"]], .dico[["txt_delete_observations_with_missing_values"]],.dico[["txt_replace_by_median"]],.dico[["txt_multiple_imputation_amelia"]]),
                        preselect=FALSE, multiple = TRUE, title=.dico[["ask_missing_values_treatment"]])$res}
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


      if(dial || class(output)!="character"|| any(!output%in% c("default", .dico[["txt_default_outputs"]], "parEst", .dico[["txt_estimated_parameters"]], "parSt", .dico[["txt_standardized_parameters"]],.dico[["txt_covariance_matrix_adjusted"]], "fitted.cov",
                                                                .dico[["txt_residue_standardized"]], "res.St","res.Unst",.dico[["txt_non_standardized_residuals"]],"vcov",.dico[["txt_covariance_matrix_estimated"]],
                                                                "AIC", "BIC", .dico[["txt_adequation_measures"]],"fitM", .dico[["txt_inspect_initial_values"]], "start", .dico[["txt_inspect_model_matrices"]],
                                                                "modmat", .dico[["txt_inspect_model_representation"]], "modrep"))==TRUE){
        if(info) writeLines(.dico[["ask_which_results_warning_on_default_output"]])
        output<-c( .dico[["txt_default_outputs"]], .dico[["txt_estimated_parameters"]], .dico[["txt_standardized_parameters"]],.dico[["txt_covariance_matrix_adjusted"]],
                   .dico[["txt_residue_standardized"]], .dico[["txt_non_standardized_residuals"]],.dico[["txt_covariance_matrix_estimated"]],"AIC", "BIC", .dico[["txt_adequation_measures"]],
                   .dico[["txt_inspect_initial_values"]],  .dico[["txt_inspect_model_matrices"]], .dico[["txt_inspect_model_representation"]])
        if(info) writeLines(.dico[["ask_which_output_results"]])
        output<- dlgList(output, preselect=.dico[["txt_default_outputs"]], multiple = TRUE, title=.dico[["ask_results_output"]])$res
        if(is.null( Resultats$opt.list)) {
          Resultats<-cfa.in()
          return(Resultats)
        }
      }


      if(dial || length(sauvegarde)!=1 || !is.logical(sauvegarde)){
        sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title=.dico[["ask_save_results"]])$res
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
                            bootstrap=n.boot,meanstructure="default",zero.cell.warn=F,
                            missing=missing, group=group, #ifelse(!is.null(group), group.equal=group.equal,group.equal="means"),
                            group.w.free= group.w.free,fixed.x=fixed.x,information=information,se=se,std.ov=as.logical(std.ov),
                            orthogonal=as.logical(orthogonal),likelihood=likelihood, link=link, int.ov.free=as.logical(int.ov.free),
                            int.lv.free=as.logical(int.lv.free),std.lv=as.logical(std.lv),zero.add=zero.add, zero.keep.margins=zero.keep.margins), silent=T)
      if(class(fit)=='try-error') {msgBox(.dico[["ask_could_not_finish_analysis_respecify_parameters"]])
        return(ez.cfa())}

      if(any(output== "default") | any(output== .dico[["txt_default_outputs"]]))  {
        print(summary(fit, fit.measures = TRUE, standardized=T))
        Resultats<-.dico[["desc_to_display_results_use_summary"]]
        summary(fit)->>fit
        if(length(output)==1) fit->>modele.cfa
        }
      if(any(output== "parEst") | any(output==.dico[["txt_estimated_parameters"]])) parameterEstimates(fit)->Resultats[[.dico[["txt_estimated_parameters_not_standardized"]]]]
      if(any(output== "parSt") | any(output==.dico[["txt_standardized_parameters"]])) standardizedSolution(fit)->Resultats[[.dico[["txt_estimated_parameters_standardized"]]]]
      if(any(output== .dico[["txt_covariance_matrix_adjusted"]]) | any(output=="fitted.cov")) fitted(fit)->Resultats[[.dico[["txt_covariance_matrix_adjusted"]]]]
      if(any(output== .dico[["txt_residue_standardized"]]) | any(output=="res.St")) resid(fit, type="standardized")->Resultats[[.dico[["txt_residue_standardized"]]]]
      if(any(output== .dico[["txt_non_standardized_residuals"]]) | any(output=="res.Unst")) resid(fit)->Resultats[[.dico[["txt_non_standardized_residuals"]]]]
      if(any(output== "vcov") | any(output==.dico[["txt_covariance_matrix_estimated"]])) vcov(fit)->Resultat[[.dico[["txt_covariance_matrix_estimated"]]]]
      if(any(output== "AIC") ) AIC(fit)->Resultats$AIC
      if(any(output== "BIC") ) BIC(fit)->Resultats$BIC
      if(any(output== .dico[["txt_adequation_measures"]]) | any(output=="fitM")) fitMeasures(fit)->Resultats[[.dico[["txt_adjustement_measure"]]]]
      if(any(output== .dico[["txt_inspect_initial_values"]]) | any(output=="start"))inspect(fit, what=start)->Resultats[[.dico[["txt_init_values"]]]]
      if(any(output== .dico[["txt_inspect_model_matrices"]]) | any(output=="modmat")) inspect(fit)->Resultats[[.dico[["txt_model_matrix"]]]]
      if(any(output== .dico[["txt_inspect_model_representation"]]) | any(output=="modrep"))inspect(fit, what=list)->Resultats[[.dico[["txt_model_representation"]]]]
      semPaths(fit, what="path", whatLabels="std", edge.label.cex = 0.65,edge.color="black", exoVar = FALSE,exoCov =T)


      return(Resultats)



    }

    packages<-c('svDialogs', 'psych','lavaan','semPlot')
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    try( windows(record=T), silent=T)->win
    if(class(win)=='try-error') quartz()

    Resultats<-list()
    opt.list<-list(mimic=mimic,fixed.x=fixed.x,missing=missing,information=information,zero.keep.margins=zero.keep.margins,zero.add=zero.add,
                   estimator=estimator,group=group,test=test,se=se,std.ov=std.ov,orthogonal=orthogonal,likelihood=likelihood,
                   link=link,int.ov.free=int.ov.free,int.lv.free=int.lv.free,fixed.x=fixed.x,std.lv=std.lv,n.boot=n.boot,group.w.free=group.w.free,group.equal=group.equal)
    cfa.options<-cfa.in(modele=modele,X=X, data=data, ord=ord, outlier=outlier,imp=imp,output=output, info=T, opt.list=opt.list, sauvegarde=sauvegarde)
    if(is.null(cfa.options)) return(analyse())
    AFC<-cfa.out(cfa.options)
    if(!is.null(AFC)) Resultats$AFC<-AFC
    if(AFC[[1]]=="Default ouput") print(summary(modele.cfa))

   try({
      def.values<-list(mimic="default", fixed.x="default", missing="default",information="default", zero.keep.margins="default",zero.add=c(0.5,0),
                     estimator="ml",group=NULL, test="standard",se="standard",std.ov=T, orthogonal=F, likelihood="default",
                     link="probit",int.ov.free=FALSE, int.lv.free=FALSE,fixed.x="default", std.lv=FALSE, n.boot=1000, group.w.free=F,
                     group.equal=c("loadings", "intercepts", "means", "thresholds", .dico[["txt_regressions"]], "residuals", "residual.covariances",
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
   }, silent=T)



    if(sauvegarde) save(Resultats=Resultats, choix="AFC", env=.e)
    Resultats$ref<-ref1(packages)
    try(ez.html(Resultats), silent=T)
    return(Resultats)
  }
