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
  # contrast = list. The names in the list corresponds to the names of the factors and the values is a matrix of coefficients for the contrasts. "pairwise" or "none" are also possible
  # p.adjust = adjust p values for multiples comparisons. see <code>p.adjust</code>
  packages<-c('BayesFactor', 'car','afex', 'DescTools','emmeans','ggplot2','nortest', 'outliers', 'PMCMRplus',
              'psych', 'reshape2', 'sjstats', 'svDialogs', 'WRS2' )
  test2<-try(lapply(packages, library, character.only=T), silent=T)
  if(class(test2)== 'try-error') return(ez.install())
  .e <- environment()
  Resultats<-list()
  if(!is.null(data) & class(data)!="character") data<-deparse(substitute(data))
   try( windows(record=T), silent=T)->win
   if(class(win)=='try-error') quartz()
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
  
  
  if(any(outlier %in% c("complete", .dico[["txt_complete_dataset"]],.dico[["txt_complete_dataset"]]))){
    Resultats[[.ez.anova.msg("title", 12)]]<-complet
    aov.plus.in->aov.plus.list[[.dico[["txt_complete_dataset"]]]]}
  
  if(any(outlier %in% c("id", "removed" , .dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]],
                        .dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]]))) { 
    if(is.null(data$'residu')) {
      Resultats[[.ez.anova.msg("title", 55)]]<-.ez.anova.msg("msg", 34)
      return(Resultats)}
    valeurs.influentes(X='residu', critere="Grubbs",z=3.26, data=data)->influentes
    
    if(any(outlier %in% c(.dico[["txt_identifying_outliers"]],"id",.dico[["txt_identifying_outliers"]] ))) Resultats[[.ez.anova.msg("title", 13)]]<-influentes
    if(any(outlier %in% c( .dico[["txt_without_outliers"]],  .dico[["txt_without_outliers"]],"removed" ))){
      
      #if(!is.null(influentes[[.dico[["txt_outliers"]]]][,id])){
      #  setdiff(data[,as.character(id)],influentes[[.dico[["txt_outliers"]]]][,as.character(id)])->diffs
      #if(influentes[[.dico[["txt_outliers_synthesis"]]]]$Synthese[1]!=0){
      if(influentes[[.dico[["txt_outliers_synthesis"]]]][[.dico[["txt_synthesis"]]]][1]!=0){
        setdiff(data[,as.character(id)],influentes[[.dico[["txt_outliers"]]]][,as.character(id)])->diffs
        data[which(data[,id] %in% diffs), ]->nettoyees
        factor(nettoyees[,id])->nettoyees[,id]
        nett<-.ez.anova.out(data=nettoyees, DV=DV, between=between, within=within,id=id, cov=cov,  
                            ES=ES, SumS=SumS, contrasts=contrasts,p.adjust=p.adjust, rscaleFixed=rscaleFixed , rscaleRandom= rscaleRandom, n.boot=n.boot, param=param) 
        aov.plus.in<-nett[["aov.plus.in"]]
        nett[["data"]]<-NULL
        nett[["aov.plus.in"]]<-NULL
        Resultats[[.ez.anova.msg("title", 14)]]<-nett
        aov.plus.in->aov.plus.list[[.dico[["txt_without_outliers"]]]]
      }
      print(!all(outlier %in% c("complete", .dico[["txt_complete_dataset"]],.dico[["txt_complete_dataset"]])))
    if(!any(outlier %in% c("complete", .dico[["txt_complete_dataset"]],.dico[["txt_complete_dataset"]])))   Resultats[[.ez.anova.msg("title", 14)]]<-complet
      
    }
    
  }
  
  class(aov.plus.list)<-"aovplus"
  assign("aov.plus.in", aov.plus.list,envir=.GlobalEnv) 
  
  
if(reshape.data) Resultats$call.reshape<-as.character(ez.history[[length(ez.history)]][[2]])
  
  if(!is.null(between)) between<-paste(unique(between), collapse="','", sep="") 
  if(!is.null(within)) within<-paste(unique(within), collapse="','", sep="") 
  if(!is.null(cov)) cov<-paste(unique(cov), collapse="','", sep="") 
  param<-paste(unique(param), collapse="','", sep="") 
  outlier<-paste(unique(outlier), collapse="','", sep="")
  if(!any(contrasts%in%c("none", .dico[["txt_none"]], .dico[["txt_pairwise"]], .dico[["txt_comparison_two_by_two"]]))){
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
               ", contrasts =" , ifelse(cont.call=="'Comparaison 2 a 2'","'pairwise'", cont.call),
               ", p.adjust = '", p.adjust, "', n.boot = ", n.boot, ",rscaleFixed = ", rscaleFixed, ", rscaleRandom = ", rscaleRandom, ")")
  Resultats$call<-call
  
  .add.history(data=data, command=Resultats$Call, nom=.dico[["txt_anova"]])
  .add.result(Resultats=Resultats, name =paste(.dico[["txt_anova"]], Sys.time() ))
  if(save==T) save(Resultats=Resultats ,choix =paste(.dico[["txt_anova_on"]], nom), env=.e)
  ref1(packages)->Resultats[[.ez.anova.msg("title", 56)]]
  if(html) try(ez.html(Resultats), silent=T) 
  return(Resultats)
  
  
  
  
}


.ez.anova.msg<-function(type, number){
  # type : either "msg" or "title"
  # number : number of message 
    msg<-c(.dico[["ask_variables_type_for_anova"]],
           .dico[["desc_at_least_independant_variables_or_repeated_measures"]],
           .dico[["ask_select_variables_or_modalities_of_repeated_measure_variable"]],
           .dico[["ask_which_variable_identifies_participants"]],
           .dico[["desc_each_participant_must_appear_only_once_"]],
           .dico[["desc_two_cols_are_needed"]],
           .dico[["desc_large_format_must_be_numeric_or_integer"]],
           .dico[["ask_chose_independant_group_variables"]],
	   .dico[["ask_you_did_not_chose_a_variable_continue_or_abort"]],
           .dico[["ask_chose_dependant_variable"]], 
           .dico[["desc_some_participants_have_missing_values_on_repeated_measures"]],
           .dico[["ask_chose_covariables"]],
	   .dico[["ask_not_enough_obs_verify_dataset"]],
	   .dico[["desc_all_tests_description"]],
	   .dico[["desc_complete_dataset_vs_identification_outliers_vs_without_outliers"]],
           .dico[["desc_cannot_have_both_within_RML_arguments"]],
	   .dico[["desc_most_common_effect_size"]],
	   .dico[["desc_multiple_ways_to_compute_squares_sum"]],
           .dico[["ask_save_results"]],
           .dico[["ask_dependant_variable_with_less_than_three_val_verify_dataset"]],
	   .dico[["desc_all_contrasts_description"]],
           .dico[["desc_you_can_chose_predefined_or_manual_contrasts"]],
           .dico[["ask_contrast_must_respect_ortho"]],
           .dico[["desc_contrasts_must_be_coeff_matrices_in_list"]],
           .dico[["desc_manual_contrast_need_coeff_matrice"]],
           .dico[["ask_which_correction"]],
           .dico[["desc_authorized_values_for_contrasts"]],
           .dico[["desc_at_least_on_contrast_matrix_incorrect"]],
	   .dico[["desc_biased_results_risk_because_of_low_number_of_obs_or_zero_variance"]],
           .dico[["desc_bayesian_factors_could_not_be_computed"]],
           .dico[["desc_we_could_not_compute_anova_on_medians"]],
           .dico[["desc_proba_and_IC_estimated_on_bootstrap"]],
           .dico[["desc_we_could_not_compute_robust_anova"]], .dico[["desc_analysis_aborted"]],
           .dico[["RML_and_within_not_allowed"]]
    )
    
    
    title<-c(.dico[["ask_variables_type"]], 
            .dico[["txt_repeated_measures"]],
            .dico[["txt_participants_id"]],
            .dico[["txt_independant_group_variables"]],
             .dico[["txt_dependant_variable"]], 
             .dico[["ask_covariables"]],
             .dico[["txt_param_model"]], 
             .dico[["txt_non_param_model"]],.dico[["txt_bayesian_factors"]], .dico[["txt_robust_statistics"]],
             .dico[["ask_which_analysis"]],.dico[["txt_complete_dataset"]],.dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]],
             .dico[["ask_results_desired"]], .dico[["ask_which_size_effect"]],.dico[["ask_which_squared_sum"]],
             .dico[["ask_save"]], .dico[["txt_apriori"]],  .dico[["txt_comparison_two_by_two"]], .dico[["txt_none"]],.dico[["ask_which_contrasts"]],
             .dico[["txt_contrasts_for"]], .dico[["txt_specify_contrasts"]],.dico[["ask_which_baseline"]], .dico[["txt_descriptive_statistics"]],.dico[["txt_test_model"]],
             .dico[["txt_variable_descriptive_statistics"]],.dico[["txt_descriptive_statistics_of_interaction_between_x"]],.dico[["txt_warning"]],
             .dico[["txt_normality_tests"]],.dico[["txt_ancova_application_conditions"]],.dico[["txt_absence_of_difference_between_groups_test_on"]],
             .dico[["txt_slopes_homogeneity_between_groups_on_dependant_variable"]],
             .dico[["txt_levene_test_verifying_homogeneity_variances"]],.dico[["txt_mauchly_test_sphericity_covariance_matrix"]],
             .dico[["txt_principal_analysis"]],.dico[["txt_anova_with_welch_correction"]], .dico[["txt_pairwise_comparisons"]],.dico[["txt_contrasts"]],
             .dico[["txt_variables_coeff_matrix"]],.dico[["txt_contrasts_table"]],.dico[["txt_contrasts_table_imitating_commercial_softwares"]],.dico[["txt_bayesian_factors"]],
             .dico[["txt_non_param_analysis"]],.dico[["txt_kruskal_wallis_test"]],.dico[["txt_friedman_anova"]],.dico[["txt_friedman_anova_pairwise_comparison"]],
             .dico[["txt_kruskal_wallis_pairwise"]]  ,.dico[["txt_anova_on_medians"]],.dico[["txt_principal_analysis"]],.dico[["txt_anova_on_truncated_means"]],
             .dico[["txt_anova_on_m_estimator"]], .dico[["txt_anova_on_modified_huber_estimator"]],.dico[["txt_analysis_premature_abortion"]],
             .dico[["desc_references"]], .dico[["desc_bootstrap_percentile_anova"]],
             .dico[["txt_effect_size_dot_inf"]], .dico[["txt_effect_size_dot_sup"]]
             
    )
    
  #} else {
  #  msg<-c("Which kind of variable do you want to include in the analysis ?\nYou are allowed to choose several (e.g., for mixed anova or for ancova).",
  #         "It is required that at least one variable is either an independant group variable or a repeated measure variable",
  #         "Please select the repeated measures variable-s or the levels of the repeated measures variables",
  #         "Which variable identify individual ?",
  #         "Each participant must occur once and only once for each combinations of levels",
  #         "If your data have a wide format, you must select at least 2 columns for repeated measure factors",
  #         "If your data a wide format, the format in each column must be either numeric or integer",
  #         "Please choose between participant variables.",
  #         "You have not chosen any variable. Do you want to continue (ok) or to leave (cancel) this analysis ?",
  #         "Please choose the dependant variable",
  #         .dico[["desc_some_participants_have_missing_values_on_repeated_measures"]],
  #         "Please choose the covariables",
  #         "The number of observations is not enough given the number of levels for each variable. \nPlease ensure that there are at least 3 observations for each combination of levels",
  #         "The parametruc model is the usual anova presented in statistical packages. The non parametric test is \nthe Kruskal Wallis test for one way anova and Friedman test for repeated measure anova.\n
  #         The bayesian approach assesses the same model as the classical anova by computing the Bayes Factor.\nRobust statistics are anovas on trimmed means or median with or without bootstrap",
  #         "Complete data are the analyses performed on the entire dataset. The analysis without outliers means that outliers have\nbeen removed before performing the analysis. The criteria for detecting outliers is the Grubbs test.",
  #         "If within is not null, RML must be null and conversely",
  #         "The most often used effect size is the partial eta squarred (pes). The most accurate is the generalized eta squared (ges)",
  #         "There are several ways to estimate the Sum of Squares. Default value for comercial softwares is Type 3,\nwhich prioritizes interaction instead of main effects",
  #         "Do you want to save the results of the analysis?",
  #         "The dependant variable has less than 3 unique different values. Check your data or the analysis that you try to make is not relevant.",
  #         .dico[["desc_all_contrasts_description"]],
  #         "You may use one of predefined contrast matrix or state the contrast by yourself. In the latter case, you must choose state the contrasts.",
  #         .dico[["ask_contrasts_must_be_ortho"]],
  #         .dico[["desc_contrasts_must_be_coeff_matrices_in_list"]],
  #         "If you choose the coefficients yourself, all the variables in the analysis must have their coefficient matrix",
  #         .dico[["ask_probability_correction"]],
  #         "Allowed values for contrasts are +none+, +pairwise+ or a list with the coefficients of contrasts.",
  #         "At least one of your contrast matrix is not correct.",
  #         "There are less than 3 observations for at least one group or the variance for one groupe is 0. Results are probably biased",
  #         "Bayes Factors estimation failed",
  #         "We are sorry but it was not possible to perform the ANOVA on the medians, probably du to ex aequo.",
  #         "Probability and CI are estimated by bootstrap. CI is corrected for multiple comparisons, contrary to reported p values",
  #         "We are sorry but robust ANOVA failed", "The analysis has failed"
  #         
  #  )
  #  
  #  
  #  
  #  title<-c("Which kind of variable ?", .dico[["txt_repeated_measures"]], "Id of individuals", "Between participant variables",
  #           "Dependant variable",.dico[["ask_covariables"]],
  #           "Parametric", "Non parametric",.dico[["txt_bayesian_factors"]], "Robust statistics - might take some time",
  #           "Which analysis do you want ?", "Complete dataset", "Identification of outliers", "Dataset with outliers removed",
  #           "Which results do you want ?", "Which effect size do you want?", "Which sum of squares do you want ?","Do you want to save?",
  #           .dico[["txt_apriori"]], .dico[["txt_pairwise"]], "none", "Please choose the type of contrast", "Contrasts for", "Choose your own contrasts",
  #           "Which level is the baseline?","Descriptive statistics", .dico[["txt_test_model"]], "Descriptive for",
  #           .dico[["txt_descriptive_statistics_of_interaction_between_x"]],"Warning","Normal distribution test",
  #           "Assumptions of ancova",.dico[["txt_absence_of_difference_between_groups_test_on"]],
  #           "Homogeneity of slopes between groups on the dependant variable",
  #           "Levene's test testing homogeneity of variances",.dico[["txt_mauchly_test_sphericity_covariance_matrix"]],"Main analysis",
  #           "Welch's ANOVA for heterogeneous variances",.dico[["txt_pairwise_comparisons"]],"contrasts","Matrix of coefficients",
  #           "Table of contrasts","Contrasts that mimics commercial software",.dico[["txt_bayesian_factors"]], .dico[["txt_non_param_analysis"]], .dico[["txt_kruskal_wallis_test"]], .dico[["txt_friedman_anova"]],
  #           "Friedman pairwise comparison",.dico[["txt_kruskal_wallis_pairwise"]], .dico[["txt_anova_on_medians"]],"Main analysis",
  #           "Anova on trimmed mean", .dico[["txt_anova_on_m_estimator"]],"Anova on modified Huber estimator", "A problem occurred. The analysis has stopped",
  #           "References of packages used for this analysis")
  #  
  #}
  
  ifelse(type=="msg", r<-msg, r<-title)
  r<-r[number]   
  return(r)}

.contrastes.ez<-function(data, between=NULL, within=NULL, contrasts="none", p.adjust="none", dial=T){
  options (warn=1)
  c(between, unlist(within))->betweenwithin
  if(any(!contrasts%in%c("none",.dico[["txt_pairwise"]],.dico[["txt_none"]],.dico[["txt_pairwise_comparison"]])) & class(contrasts)!="list") {
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
      if(all(!class(contrasts[[j]]) %in% c("matrix", "data.frame")) || !is.numeric(as.matrix(contrasts[[j]]))){
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
    
    if(type.cont==.dico[["txt_apriori"]]) {
      
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
	## With translation, switch case can cause trouble when using variables instead of literal strings.
	## That's why I've been converting it here too.
	if (type.cont2=="Helmert") { contr.helmert(nlevels(data[,betweenwithin[i]])) -> contrastes[[i]] }
        if (type.cont2=="Helmert reversed") { apply(contr.helmert(nlevels(data[,betweenwithin[i]])), 2, rev)  -> contrastes[[i]]}
        if (type.cont2=="poly") { emmeans:::poly.emmc(1:nlevels(data[,betweenwithin[i]]))  -> contrastes[[i]]}
        if (type.cont2=="Trait.vs.contr") {
		{ base<- dlgList(levels(data[, betweenwithin[i]]), preselect=levels(data[,betweenwithin[i]])[1],
                        multiple = FALSE, title=.ez.anova.msg("title", 25))$res
                        which(levels(data[, betweenwithin[i]])==base)->base
                        emmeans:::trt.vs.ctrl1.emmc(1:nlevels(data[,betweenwithin[i]]), ref=base)
                      } -> contrastes[[i]]  }
        if (type.cont2=="Eff") { emmeans:::del.eff.emmc(1:nlevels(data[,betweenwithin[i]]))  -> contrastes[[i]]}
        if (type.cont2=="consec") { emmeans:::consec.emmc(1:nlevels(data[,betweenwithin[i]]))  -> contrastes[[i]]}
        if (type.cont2=="mean.change") { emmeans:::mean_chg.emmc(1:nlevels(data[,betweenwithin[i]]))  -> contrastes[[i]]}
        
        if(type.cont2 %in% c(.dico[["txt_specify_contrasts"]], .dico[["txt_specify_contrasts"]])){
          ortho<-FALSE
          while(ortho!=TRUE){
            own.cont<-matrix(rep(0,times=(nlevels(data[,betweenwithin[i]])*(nlevels(data[,betweenwithin[i]])-1))), 
                             nrow=nlevels(data[,betweenwithin[i]]))
            dimnames( own.cont)[[1]]<-levels(data[,betweenwithin[i]])
            dimnames( own.cont)[[2]]<-paste(.dico[["txt_contrast"]], 1:(nlevels(data[,betweenwithin[i]])-1), sep=".")
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
        
        dimnames(contrastes[[i]])[[2]]<-paste(.dico[["txt_contrast"]], 1:(ncol(contrastes[[i]])), sep=".")
        dimnames(contrastes[[i]])[[1]]<-levels(data[,betweenwithin[i]])
      }
      names(contrastes)<-betweenwithin
      Resultats$contrastes<-contrastes     
      
    }else{
      if(type.cont %in% c(.dico[["txt_comparison_two_by_two"]],.dico[["txt_pairwise"]], .dico[["txt_pairwise"]], "none", .dico[["txt_none"]]))  { Resultats$contrastes<-type.cont
      contrastes<-type.cont}
      
    }
  }else{
    contrastes<-list()
    if(any(contrasts %in% c(.dico[["txt_comparison_two_by_two"]],.dico[["txt_pairwise"]], .dico[["txt_pairwise"]], "none", .dico[["txt_none"]]))) {
      Resultats$contrastes<-contrasts
      contrastes<-contrasts
      }else{
    for(i in 1:length(contrasts)){
      cont2<-contrasts[[i]]
      cont2<-as.matrix(cont2)
      j<-which(names(data)==names(contrasts)[[i]])
      noms<-list()
      noms[[1]]<-levels(data[,j])
      noms[[2]]<-paste(.dico[["txt_contrast"]], 1:(ncol(cont2)), sep=".")
      dimnames(cont2)<-noms
      contrastes[[i]]<-cont2 
    }
    names(contrastes)<-names(contrasts)
    Resultats$contrastes<-contrastes
    }
  }
  if((dial & all(contrastes %in% c(.dico[["txt_comparison_two_by_two"]],.dico[["txt_pairwise"]], .dico[["txt_pairwise"]]))) || 
     (!p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
                       "sidak","dunnettx","mvt" ,"none" ))){
    list()->p.adjust
    writeLines(.ez.anova.msg("msg", 26) )
    dlgList(c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
              "sidak","dunnettx","mvt" ,"none"), preselect="holm", multiple = FALSE, title=.dico[["ask_correction_anova_contrasts"]])$res->p.adjust
    
    if(length(p.adjust)==0) return(contrastes.ez())
    
  } 
  Resultats$p.adjust<-p.adjust
  return(Resultats)
}



.options.aov<-function(between=NULL, within=NULL, cov=NULL, dial=T, outlier=NULL, param=NULL, ES=NULL, SumS=NULL, save=F){
  list()->Resultats
  
  if(dial || !any(param %in% c("param", "non param", "bayes", "robust",
                               .dico[["txt_param_model"]], .dico[["txt_non_param_model"]],.dico[["txt_bayesian_factors"]], .dico[["txt_robust_statistics"]],
                               .dico[["txt_param_model"]], .dico[["txt_non_param_model"]],.dico[["txt_bayesian_factors"]], .dico[["txt_robust_statistics"]]))){
                               #"Parametric", "Non parametric",.dico[["txt_bayesian_factors"]], "Robust statistics - might take some time"))){
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
  
  
  
  if(dial | !any(outlier%in% c("complete","id", "removed",.dico[["txt_complete_dataset"]],.dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]],
                               .dico[["txt_complete_dataset"]],.dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]])) ){
    outlier<-c(.ez.anova.msg("title",12),.ez.anova.msg("title",13),.ez.anova.msg("title",14))
    outlier<- dlgList(outlier, preselect=outlier,multiple = TRUE, title=.ez.anova.msg("title",15))$res
    if(length(outlier)==0) return(.options.aov(between=between, within=within, cov=cov))
  }
  
  
  if(any(param %in% c(.dico[["txt_param_model"]], "Parametric", "param"))){
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
  if((!is.null(between) | !is.null(within) | !is.null(RML)) && 
      all(c(between, within, RML) %in% names(data))) dial<-F else dial<-T

  if(!is.null(within) & !is.null(RML))  {
    okCancelBox(.ez.anova.msg("msg",35))
    return(.ez.anova.in())}
  
  if(is.null(c(between,within, RML))) {
    index<-1 # From when language was hard coded in easieR. TODO 
    type.v<-matrix(c(.dico[["txt_independant_groups"]], .dico[["txt_repeated_measures"]], .dico[["txt_covariables"]],
	             .dico[["txt_independant_groups"]], .dico[["txt_repeated_measures"]], .dico[["txt_covariables"]]), ncol=2)
    writeLines(.ez.anova.msg("msg", 1))
    type.v2<-dlgList(type.v[,index], multiple = TRUE, title=.ez.anova.msg("title", 1))$res
    if(length(type.v2)==0) return(.ez.anova.in())
    type.v<-type.v[which(type.v[, index]%in%type.v2),1]
    if(!any(type.v %in% c(.dico[["txt_independant_groups"]], .dico[["txt_repeated_measures"]]))) {
      writeLines(.ez.anova.msg("msg",2))
      return(.ez.anova.in())
    }
  } 
  
  if(any(type.v==.dico[["txt_repeated_measures"]]) | !is.null(within) | !is.null(RML)) {
    if(!is.null(RML)) {
    RML<-.var.type(X=RML, info=T, data=data, type=c("integer", "numeric"), check.prod=F,
                  message=.ez.anova.msg("msg",3), 
                  multiple=TRUE,  title=.ez.anova.msg("title",2), out=NULL)
    if(is.null(RML)) return(.ez.anova.in()) else   RML<-RML$X

    idvar<-setdiff(names(data), RML)
    if(!is.null(RML.factor)) {
      IV.names<-as.list(names(RML.factor))
      }else{
      IV.names<-"variable"
      RML.factor<-list("variable" = paste0("mod", 1:length(RML)))
           }
    data<-ez.reshape(data=nom, varying= list(RML), v.names =c('value'),idvar =idvar,
                     IV.names=IV.names, IV.levels=RML.factor) 
    nom<-paste0(nom, ".long")
    reshape.data<-TRUE
    DV<-.dico[["txt_value"]]
    within<-setdiff(names(data), c(idvar, .dico[["txt_value"]],"IDeasy"))
    if(length(within)>1) {
      data[,within]<-lapply(data[, within], factor)
      within<-within[-which(within=="time")]
    } else {
      data[,within]<-factor(data[,within])
    }
  
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
    }
  
  
  if(is.null(within)) return(ez.anova())




  
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

                  }   	  
  }

  if(any(type.v==.dico[["txt_independant_groups"]]) | !is.null(between)){
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
    if( min(table(data[,id]))!=  max(table(data[,id])) | any(is.na(data[, DV]))) {
        msgBox(.ez.anova.msg("msg",11))
        NA.value<-which(is.na(data[,DV]))
        ID.NA<- data[NA.value, id]
        data<-data[which(!data[,id]%in% ID.NA),]
	if(min(table(data[,id]))!=  max(table(data[,id])))   {
		n.rep<-max(table((data[,id])))
		id.nrep.differ<-which(table(data[,id]) !=n.rep)
                ID.NA<-names(id.nrep.differ)
	        data<-data[which(!data[,id]%in% ID.NA),]
	} 
   data[,id]<-factor(data[,id])
    }       
  }
  
  
  
  if(any(type.v==.dico[["txt_covariables"]])) {
    
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
#if(any(param %in% c(.dico[["txt_param_model"]], "Parametric", "param")))  
  if(any(options.out$param %in% c("param", .dico[["txt_param_model"]], "Parametric", "param"))){
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
    pred.rep<-paste(ez.principal, erreur,"|", id, ")")
  }
  
  if(!is.null(between) & !is.null(within)) predicteurs<-paste(pred.ind, "*",pred.rep) else {
    if(!is.null(between) & is.null(within)) {predicteurs<-paste0(pred.ind,"+Error(1|",id,")")} else 
    {predicteurs<-pred.rep}
  }
  modele<-paste0(DV, "~",cov1, predicteurs)
  Resultats[[.ez.anova.msg("title",27)]]<-modele
  modele<-as.formula(modele)  
  
  Resultats[[.ez.anova.msg("title",26)]]<-.stat.desc.out(X=DV, groupes=c(between, within), data=data, tr=.1, type=3, plot=T)
  
  
  aov.plus.in<-list()
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
  
  if(any(param %in% c(.dico[["txt_param_model"]],"param", "Parametric"))){
    if(any(Resultats[[2]][[1]]$sd==0)) Resultats[[.ez.anova.msg("title",30)]]<-.ez.anova.msg("msg",29)
    options(contrasts=c("contr.sum","contr.poly"))
    if(!is.null(cov)) factorize<-FALSE else factorize<-TRUE
    aov.out<-aov_4(as.formula(modele),data=data, es_aov=ES, type=SumS,factorize=factorize)
    residus<-data.frame(aov.out$lm$residuals)
    residus[,"match"] <-aov.out$data$wide[,id]
    if(!is.null(within)){ residus<-melt(residus, id.vars="match") 
                         names(residus)[3]<-'residu'
                          residus$match<-paste0(residus[,1], residus[,2])
                          data$match<-paste0(data[,id], data[,within[1]])
                          if(length(within)>1){
                             for(i in 2:length(within)){
                                 data$match<-paste0(data$match, "_", data[,within[i]])
                                                        }
                                               }
                          }else{
      names(residus)<-c('residu', "match")
      data$match<-data[,id]
      }

    data<-merge(x=data, y=residus, by="match")
    Resultats[[.ez.anova.msg("title",31)]]<-.normalite(data=data, X='residu', Y=NULL)
    
    if(!is.null(cov) & !is.null(between)){
      options(contrasts = c("contr.helmert", "contr.poly"))
      for(i in 1:length(cov)){
        aov(as.formula(paste0(cov[i], "~",pred.ind)), data=data)->aov.cov
        Anova(aov.cov, type="III")->aov.cov
        names(aov.cov)<-c("SC", .dico[["txt_df"]], "F", .dico[["txt_p_dot_val"]]) 
        
        Resultats[[.ez.anova.msg("title",32)]][[paste0(.ez.anova.msg("title",33), cov[i])]]<-aov.cov
        if(i==1) {paste(cov[1],"*")->cov2} else {paste0(cov2, cov[i],"*")->cov2}
      }
      aov(as.formula(paste0(DV, "~", cov2,pred.ind)), data=data)->aov.cov
      Anova(aov.cov, type="III")->aov.cov
      names(aov.cov)<-c("SC", .dico[["txt_df"]], "F", .dico[["txt_p_dot_val"]])
      Resultats[[.ez.anova.msg("title",32)]][[.ez.anova.msg("title",34)]]<-aov.cov
      
    }
    
    if(!is.null(between)){
      paste0(DV, "~",pred.ind)->modele2
      Levene<-leveneTest(as.formula(modele2),data=data) # test de Levene pour homogeneite des variances
      Levene<-round(unlist(Levene)[c(1,2,3,5)],3)
      names(Levene)<-c(.dico[["txt_df1"]],.dico[["txt_df2"]],"F",.dico[["txt_p_dot_val"]])
      Resultats[[.ez.anova.msg("title",35)]]<- Levene
    }
    
    c(unlist(within), between)->withinbetween
      
      if(length(withinbetween)==1) graph.modele<-paste0("~",withinbetween[1]) else{
      graph.modele<-paste0(withinbetween[1],"~",withinbetween[2])}
      if(length(withinbetween)>2){paste0(graph.modele, "|",withinbetween[3] )->graph.modele
        if(length(withinbetween)>3){ for(i in 4:length(withinbetween)){paste0(graph.modele, "*",withinbetween[i] )->graph.modele} 
          
        }} 
      
     try( Resultats$Figure<-emmip(aov.out,as.formula(graph.modele),CIs=T), silent=T)
    
    
    
    aov.out2<-summary(aov.out)
    if(!is.null(within) && any( sapply(data[,c(unlist(within))],nlevels)>2)) {
      aov.out2b<-round(aov.out2$sphericity.test,5)
      aov.out2b<-matrix(aov.out2b, ncol=2)
      dimnames(aov.out2b)<-list(dimnames(aov.out2$sphericity.test)[[1]], c("Stat", .dico[["txt_p_dot_val"]]))
      Resultats[[.ez.anova.msg("title",36)]]<-aov.out2b
    }

    aov.out3<-aov.out[[1]]
    aov.out3<-data.frame(aov.out3)
    names(aov.out3)<-c(.dico[["txt_df_num"]], .dico[["txt_df_denom"]], "CME", "F", ES, .dico[["txt_p_dot_val"]] )
    omega.out<-effectsize::omega_squared(aov.out$Anova)
    aov.out3<-cbind(aov.out3, omega.2=omega.out[match(rownames(aov.out3), omega.out$Parameter),2])
    
    Resultats[[.ez.anova.msg("title",37)]]<- aov.out3
    if(!is.null(within) && any( sapply(data[,c(unlist(within))],nlevels)>2)) {
      GG.HF<-data.frame(round(aov.out2$pval.adjustments,5))
      names(GG.HF)<-c("GG.eps", .dico[["txt_gg_p_value"]],"HF.eps", .dico[["txt_hf_p_value"]])
      Resultats[[.dico[["txt_greenhouse_geisser_huynn_feldt_correction"]]]]<-GG.HF}
    if(length(between)==1 & is.null(within) & is.null(cov)) {
      Welch<-oneway.test(as.formula(paste(DV,"~", between)),data=data)
      Welch<-round(data.frame("F"=Welch$statistic,"num"=Welch$parameter[1],"denom"=Welch$parameter[2],"p"=Welch$p.value),4)
      
      Resultats[[.ez.anova.msg("title",38)]]<-Welch
    }  
    
    em.out<-emmeans(aov.out, withinbetween)
    aov.plus.in$em.out<-em.out
    
    
    if(!is.list(contrasts) && any(contrasts %in% c(.dico[["txt_pairwise"]],  .dico[["txt_comparison_two_by_two"]] ))){
      pair<-pairs(em.out, adjust=p.adjust)
      pair<-summary(pair)
      names(pair)[which(names(pair)=="p.value")]<-.dico[["txt_p_dot_val"]]
      Resultats[[.ez.anova.msg("title",39)]]<-pair
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
      table.cont$R.2<-round(table.cont$t.ratio^2/(table.cont$t.ratio^2+table.cont$df),4)
        if(!is.null(between)) {
        grepl(paste(between,collapse = "|"),  table.cont[,1])->table.cont$d.Cohen
       round( ifelse(table.cont$d.Cohen==T, (2*table.cont$t.ratio)/(nlevels(data[,id])^0.5), table.cont$t.ratio/(nlevels(data[,id])^0.5)),4)->table.cont$d.Cohen}else{
          round(table.cont$t.ratio/((nlevels(data[,id]))^0.5),4)->table.cont$d.Cohen}
      
        names(table.cont)<-c(.dico[["txt_contrast"]],.dico[["txt_estimation"]], .dico[["txt_error_dot_standard_short"]], .dico[["txt_df"]],"t", .dico[["txt_p_dot_val"]], .dico[["txt_r_square"]], "d Cohen")

    
      
      
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
        names(Table.contrasts)<-c(.dico[["txt_estimator"]], .dico[["txt_df"]],"t", .dico[["txt_p_dot_val"]])
        Table.contrasts$t^2/(Table.contrasts$t^2+Table.contrasts$ddl)->Table.contrasts$R.2
        round(Table.contrasts$t/(nlevels(data[,id]))^0.5,4)->Table.contrasts$D.Cohen 
        dimnames(Table.contrasts)[[1]]<-table.cont[,1]
        Resultats[[.ez.anova.msg("title",40)]][[.ez.anova.msg("title",43)]]<-Table.contrasts
      } 
    }
  }

  ##### Bayes 
  if(any(param %in% c("bayes",.dico[["txt_bayesian_factors"]], .dico[["txt_bayesian_factors"]])) )  {
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
    
    
    if(class(BF.out)=='try-error') Resultats[[.ez.anova.msg("title",43)]]<-.ez.anova.msg("msg",30) else{
      BF.out<-extractBF(BF.out)
      BF.out<-BF.out[,1:2]
      BF.out<-BF.out[which(grepl(id, dimnames(BF.out)[[1]])==F),]
      options("scipen"=7, "digits"=4)
      Resultats[[.ez.anova.msg("title",44)]]<-BF.out
      
    }
  } 
  
  
  
  if(any(param %in% c( "non param", .dico[["txt_non_param_model"]], "Non parametric"))){
    if(!is.null(between)){
      KW<-kruskal.test(as.formula( paste0(DV, "~",between[1])), data = data)
      round(data.frame(KW$statistic,KW$parameter,KW$p.value),4)->KW
     
        names(KW)<-c("H",.dico[["txt_df"]],.dico[["txt_p_dot_val"]])
      
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
      names(friedman)<-c(.dico[["txt_chi_dot_squared"]],.dico[["txt_df"]],.dico[["txt_p_dot_val"]], .dico[["txt_kendall_w"]])
      Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",47)]]<-friedman
      ans<-frdAllPairsExactTest(y=data[,DV],groups=data[,within], blocks=data[,id], p.adjust = p.adjust)
      comp<-expand.grid(dimnames(ans$p.value))
      comp<- paste0(comp[,1],"-", comp[,2])
      F.MC<-data.frame(D.exact.test=c(ans$statistic), valeur.p=c(ans$p.value))
       dimnames(F.MC)[[1]]<-comp
       F.MC<-F.MC[complete.cases(F.MC),]
#      F.MC$p<-round.ps(F.MC$p)
      Resultats[[.ez.anova.msg("title",45)]][[.ez.anova.msg("title",48)]]<-F.MC
    }
  }
  
  
  if(any(param %in% c(.dico[["txt_robust_statistics"]], "robust", "Robust statistics - might take some time"))){
    if(length(between)==1 & is.null(within)){

      mediane<-med1way(as.formula( paste0(DV, "~",between[1])), data = data, iter= n.boot)

      if(class(mediane)!='try-error'){
        mediane<-c(mediane$test, mediane$crit.val, mediane$p.value)
        names(mediane)<-c("F", .dico[["txt_critical_dot_val"]],.dico[["txt_p_dot_val"]])
        Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",51)]]<-round(mediane,4)
        # revoir
        if(is.list(contrasts)){
          contrasts<-contrasts[[1]]
          robuste<-unstack(data, as.formula( paste0(DV, "~",between[1])))
          cont<-medpb(robuste,alpha=.05,nboot=n.boot,con=contrasts,bhop=FALSE)
          dimnames(cont$output)[[2]]<-c("Num.cont",.dico[["txt_contrast_dot_val"]],
                                          .dico[["txt_p_dot_val"]],.dico[["txt_critical_p_corrected"]],.dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]],
				       .dico[["txt_adjusted_p_dot_value"]])
          
          Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",42)]]<-cont$output
        }
        
        
      }else {
        Resultats[[.ez.anova.msg("title",49)]]<-.ez.anova.msg("msg",31)
        
      }
      
      
      AR1<-try( WRS2::t1way(as.formula(paste0(DV, "~",between)), tr=.2,data=data,nboot=n.boot),silent=T)
      if(class(AR1)!='try-error'){
        AR1<- data.frame(AR1[[2]],AR1[[3]],AR1[[1]], ifelse(round(AR1[[4]],3)==0,"<.001", round(AR1[[4]],3) ) ,
                 AR1[[5]], AR1[[6]][[1]], AR1[[6]][[2]])
        names(AR1)<-c(.dico[["txt_df_num"]],.dico[["txt_df_denom"]],
              "Stat",.dico[["txt_p_dot_val"]],
              .dico[["txt_effect_size_dot"]],.dico[["txt_effect_size_dot_inf"]], .dico[["txt_effect_size_dot_sup"]] )
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-AR1
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",30)]]<-.ez.anova.msg("msg",32)
        
        cont<-try(WRS2::lincon(as.formula(paste0(DV, "~",between)), data=data, tr=.2),silent=T)

        if(class(cont)!= 'try-error') {
          noms.out<-combn(cont$fnames, 2)
          noms.out<-paste0(noms.out[1,], " vs. ", noms.out[2,])
          dimnames(cont$comp)[[1]]<-noms.out
          cont<-cont$comp
          cont<-cont[,-c(1:2)]
           dimnames(cont)[[2]]<-c(.dico[["txt_contrast_dot_val"]],.dico[["txt_ci_inferior_limit_dot"]],
                 .dico[["txt_ci_superior_limit_dot"]],.dico[["txt_p_dot_val"]])
          Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",42)]] <-cont
        }
        
      }else{
        Resultats[[.ez.anova.msg("title",52)]]<-.ez.anova.msg("title",33)
      }

     AR1<-try(WRS2::t1waybt(as.formula(paste0(DV, "~",between)), tr=.2, nboot=n.boot,data=data),silent=T)
     if(class(AR1)!='try-error'){
       AR1<- data.frame(AR1[[1]],
        ifelse(round(AR1[[2]],3)==0, "<.001",round(AR1[[2]],3)),AR1[[3]],AR1[[4]])
        names(AR1)<-c("Stat",.dico[["txt_p_dot_val"]],.dico[["txt_var_explained_dot"]],
        .dico[["txt_effect_size_dot"]] )
                Resultats[[.ez.anova.msg("title",57)]][[.ez.anova.msg("title",51)]]<-AR1
        #Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",30)]]<-.ez.anova.msg("msg",32)
 
        cont<-try(WRS2::mcppb20(as.formula(paste0(DV, "~",between)),data=data,  tr=.2, nboot=n.boot),silent=T)
        if(class(cont)!= 'try-error') {
          noms.out<-combn(cont$fnames, 2)
          noms.out<-paste0(noms.out[1,], " vs. ", noms.out[2,])
          dimnames(cont$comp)[[1]]<-noms.out
          cont<-cont$comp
          cont<-cont[,-c(1:2)]
          dimnames(cont)[[2]]<-c(.dico[["txt_contrast_dot_val"]],.dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]],.dico[["txt_p_dot_val"]])
          Resultats[[.ez.anova.msg("title",57)]][[.ez.anova.msg("title",42)]] <-cont
        }
        
      }else{
        Resultats[[.ez.anova.msg("title",57)]]<-.ez.anova.msg("title",33)
      }
    }
    
    
    if(length(between)==2 & is.null(within)) { 
      
      try( WRS2::t2way(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, tr = 0.2), silent=T)->T2
      if(class(T2)!='try-error'){
        T2<-matrix(unlist(T2[c(1:6)]), ncol=2, byrow=T)
        dimnames(T2)[[2]]<-c(.dico[["txt_value"]], .dico[["txt_p_dot_val"]])
        c(names(data[,between]), paste(names(data[,between])[1],":",names(data[,between])[2]))->dimnames(T2)[[1]]
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-T2
      }
      mom<-try(
        WRS2::pbad2way(as.formula(paste0(DV, "~",between[1],"*",between[2])),data=data, est = "mom", nboot = n.boot),silent=T)
      if(class(mom)!='try-error')  {
        mom<-matrix(unlist(mom[c(2,4,6)]), ncol=1)
        dimnames(mom)<-list(c(between, paste0(between[1], ":",between[2])),c(.dico[["txt_p_dot_val"]]))
         Resultats[[.ez.anova.msg("title",53)]][[.ez.anova.msg("title",51)]]<-mom
      }
     
       mom<-try(
     WRS2::pbad2way(as.formula(paste0(DV, "~",between[1],"*",between[2])),data=data, est = "median", nboot = n.boot),silent=T)
  if(class(mom)!='try-error')  {
    mom<-matrix(unlist(mom[c(2,4,6)]), ncol=1)
    dimnames(mom)<-list(c(between, paste0(between[1], ":",between[2])),c(.dico[["txt_p_dot_val"]]))
    Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",51)]]<-mom
  }
      
      try(WRS2::mcp2a(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, est = "mom", nboot = n.boot), silent=T)->mediane
      if(class(mediane)!='try-error') {
        comp<-data.frame(psihat=c(mediane[[1]][[1]][[1]], mediane[[1]][[2]][[1]] , mediane[[1]][[3]][[1]]), 
                 valeur.p=c(mediane[[1]][[1]][[3]], mediane[[1]][[2]][[3]] , mediane[[1]][[3]][[3]]))
        med<-cbind(comp, matrix(c(mediane[[1]][[1]][[2]], mediane[[1]][[2]][[2]] , mediane[[1]][[3]][[2]]), ncol=2, byrow=T))
        names(med)<-c("psihat", .dico[["txt_p_dot_val"]], .dico[["txt_inferior_limit"]],.dico[["txt_ci_superior_limit"]])
        dimnames(med)[[1]]<-names(mediane[[2]])
        Resultats[[.ez.anova.msg("title",53)]][[.ez.anova.msg("title",42)]]<-med
      }
      
      try(mediane<-WRS2::mcp2a(as.formula(paste0(DV, "~",between[1],"*",between[2])), data=data, est = "median", nboot = n.boot), silent=T)
      if(class(mediane)!='try-error') {
        comp<-data.frame(psihat=c(mediane[[1]][[1]][[1]], mediane[[1]][[2]][[1]] , mediane[[1]][[3]][[1]]), 
                 valeur.p=c(mediane[[1]][[1]][[3]], mediane[[1]][[2]][[3]] , mediane[[1]][[3]][[3]]))
        med<-cbind(comp, matrix(c(mediane[[1]][[1]][[2]], mediane[[1]][[2]][[2]] , mediane[[1]][[3]][[2]]), ncol=2, byrow=T))
        names(med)<-c("psihat", .dico[["txt_p_dot_val"]], .dico[["txt_inferior_limit"]],.dico[["txt_ci_superior_limit"]])
        dimnames(med)[[1]]<-names(mediane[[2]])
        Resultats[[.ez.anova.msg("title",50)]][[.ez.anova.msg("title",42)]]<-med
      }
    }
    
    if(length(between)==3 & is.null(within)){
      tronquees<-try( WRS2::t3way(as.formula(paste0(DV, "~",between[1],"*",between[2],"*",between[3])), data=data, tr = 0.2), silent=T)
      if(class(tronquees)!='try-error') {
        tronquees<-round(matrix(unlist(tronquees[c(1:14)]), ncol=2, byrow=T), 4)
        colnames(tronquees)<-c("F", "p")
        rownames(tronquees)<- nice(aov.out)$Effect
        Resultats[[.ez.anova.msg("title",52)]]<-tronquees  
      }
    }
    if(length(within)==1 & is.null(between)){
      ANOVA.tr<-try( WRS2::rmanova(data$value,data[,within[[1]]] ,data[,id]), silent=T)
      if(class(ANOVA.tr)!='try-error'){
        
        ANOVA.tr<-round(data.frame(txt_test_dot_val= ANOVA.tr$test,txt_df1=ANOVA.tr$df1, txt_df2=ANOVA.tr$df2,txt_p_dot_val=ANOVA.tr$p.value),4)
        names(ANOVA.tr)<-c(.dico[["txt_test_dot_val"]], .dico[["txt_df1"]], .dico[["txt_df2"]], .dico[["txt_p_dot_val"]])
        
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]]<-ANOVA.tr
        if((nlevels(data[,within[[1]]]))>2) {
          WRS2::rmmcp(data[,DV],data[, within[[1]]],data[,id])->comp
          comp$call<-NULL
          Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",39)]]<-comp
        }else Resultats[[.ez.anova.msg("title",10)]]<-.ez.anova.msg("msg",33)
      }
      
    } 
    
    if(length(between)==1 & length(within)==1){
      modeleR<-as.formula(paste0(DV, "~", between,"*",  within))
      try(WRS2::tsplit( as.formula(modeleR), data[,id], data=data, tr = 0.2), silent=T)->tronquees
      if(class(tronquees)!='try-error'){
        tronquees2<-matrix(unlist(unlist(tronquees)[c(1:12)]),ncol=4, byrow=T)
        tronquees2<-data.frame(tronquees2)
        rownames(tronquees2)<-c(tronquees$varnames[2] , tronquees$varnames[3], paste0(tronquees$varnames[2],":",tronquees$varnames[3]))
        names(tronquees2)<-c("F", .dico[["txt_p_dot_val"]], .dico[["txt_df1"]], .dico[["txt_df2"]])
        Resultats[[.ez.anova.msg("title",52)]][[.ez.anova.msg("title",51)]] <-tronquees2
        WRS2::sppba(modeleR, data[,id], data=data, est = "mom", avg = TRUE, nboot = n.boot, MDIS = FALSE)->MoMa 
        WRS2::sppbb(modeleR, data[,id], data=data, est = "mom", nboot = n.boot)->MoMb
        WRS2::sppbi(modeleR, data[,id], data=data, est = "mom", nboot = n.boot)->MoMi 
        MoM<-data.frame("effet"= c(between,within[[1]],"interaction"), txt_p_dot_val=c(MoMa$p.value,MoMb$p.value, MoMi$p.value) )
	names(MoM) <- c(.dico[["txt_effect"]],.dico[["txt_p_dot_val"]])
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
  
  
  
  omega_sq <- function(aov_in, neg2zero=T){
    aovtab <- summary(aov_in)[[1]]
    n_terms <- length(aovtab[["Sum Sq"]]) - 1
    output <- rep(-1, n_terms)
    SSr <- aovtab[["Sum Sq"]][n_terms + 1]
    MSr <- aovtab[["Mean Sq"]][n_terms + 1]
    SSt <- sum(aovtab[["Sum Sq"]])
    for(i in 1:n_terms){
        SSm <- aovtab[["Sum Sq"]][i]
        DFm <- aovtab[["Df"]][i]
        output[i] <- (SSm-DFm*MSr)/(SSt+MSr)
        if(neg2zero & output[i] < 0){output[i] <- 0}
    }
    names(output) <- rownames(aovtab)[1:n_terms]

    return(output)
}
medpb<-function(x,alpha=.05,nboot=NA,grp=NA,est=median,con=0,bhop=FALSE,method='hoch',
                SEED=TRUE,...){
  #
  #   Multiple comparisons for  J independent groups using medians.
  #
  #   A percentile bootstrap method. 
  #   FWE controlled via argument method
  #   method =hoch  Hochberg;s method is used by default
  #
  #   The data are assumed to be stored in x
  #   which either has list mode or is a matrix.  In the first case
  #   x[[1]] contains the data for the first group, x[[2]] the data
  #   for the second group, etc. Length(x)=the number of groups = J.
  #   If stored in a matrix, the columns of the matrix correspond
  #   to groups.
  #
  #   est is the measure of location and defaults to the median
  #   ... can be used to set optional arguments associated with est
  #
  #   The argument grp can be used to analyze a subset of the groups
  #   Example: grp=c(1,3,5) would compare groups 1, 3 and 5.
  #
  #
  #   con can be used to specify linear contrasts; see the function lincon
  #
  #   Missing values are allowed.
  #
  con<-as.matrix(con)
  if(is.data.frame(x))x=as.matrix(x)
  if(is.matrix(x))x<-listm(x)
  if(!is.list(x))stop('Data must be stored in list mode or in matrix mode.')
  if(!is.na(sum(grp))){  # Only analyze specified groups.
    xx<-list()
    for(i in 1:length(grp))xx[[i]]<-x[[grp[i]]]
    x<-xx
  }
  J<-length(x)
  tempn<-0
  mvec<-NA
  for(j in 1:J){
    temp<-x[[j]]
    temp<-temp[!is.na(temp)] # Remove missing values.
    tempn[j]<-length(temp)
    x[[j]]<-temp
    mvec[j]<-est(temp,...)
  }
  Jm<-J-1
  #
  # Determine contrast matrix
  #
  if(sum(con^2)==0){
    ncon<-(J^2-J)/2
    con<-matrix(0,J,ncon)
    id<-0
    for (j in 1:Jm){
      jp<-j+1
      for (k in jp:J){
        id<-id+1
        con[j,id]<-1
        con[k,id]<-0-1
      }}}
  ncon<-ncol(con)
  dvec<-alpha/c(1:ncon)
  if(nrow(con)!=J)stop('Something is wrong with con; the number of rows does not match the number of groups.')
  #  Determine nboot if a value was not specified
  if(is.na(nboot)){
    nboot<-5000
    if(J <= 8)nboot<-4000
    if(J <= 3)nboot<-2000
  }
  # Determine critical values
  if(!bhop){
    if(alpha==.05){
      dvec<-c(.05,.025,.0169,.0127,.0102,.00851,.0073,.00639,.00568,.00511)
      if(ncon > 10){
        avec<-.05/c(11:ncon)
        dvec<-c(dvec,avec)
      }}
    if(alpha==.01){
      dvec<-c(.01,.005,.00334,.00251,.00201,.00167,.00143,.00126,.00112,.00101)
      if(ncon > 10){
        avec<-.01/c(11:ncon)
        dvec<-c(dvec,avec)
      }}
    if(alpha != .05 && alpha != .01){
      dvec<-alpha/c(1:ncon)
    }
  }
  if(bhop)dvec<-(ncon-c(1:ncon)+1)*alpha/ncon
  bvec<-matrix(NA,nrow=J,ncol=nboot)
  if(SEED)set.seed(2) # set seed of random number generator so that
  #             results can be duplicated.
  for(j in 1:J){
    data<-matrix(sample(x[[j]],size=length(x[[j]])*nboot,replace=TRUE),nrow=nboot)
    bvec[j,]<-apply(data,1,est,...) # Bootstrapped values for jth group
  }
  test<-NA
  bcon<-t(con)%*%bvec #ncon by nboot matrix
  tvec<-t(con)%*%mvec
  for (d in 1:ncon){
    tv<-sum(bcon[d,]==0)/nboot
    test[d]<-sum(bcon[d,]>0)/nboot+.5*tv
    if(test[d]> .5)test[d]<-1-test[d]
  }
  test<-2*test
  output<-matrix(0,ncon,7)
  dimnames(output)<-list(NULL,c('con.num','psihat','p.value','p.crit','ci.lower','ci.upper','adj.p.value'))
  temp2<-order(0-test)
  zvec<-dvec[1:ncon]
  sigvec<-(test[temp2]>=zvec)
  output[temp2,4]<-zvec
  icl<-round(dvec[ncon]*nboot/2)+1
  icu<-nboot-icl-1
  for (ic in 1:ncol(con)){
    output[ic,2]<-tvec[ic,]
    output[ic,1]<-ic
    output[ic,3]<-test[ic]
    temp<-sort(bcon[ic,])
    output[ic,5]<-temp[icl]
    output[ic,6]<-temp[icu]
  }
  num.sig<-sum(output[,3]<=output[,4])
  output[,7]=p.adjust(output[,3],method=method)
  
  list(output=output,con=con,num.sig=num.sig)
}

listm<-function(x){
  #
  # Store the data in a matrix or data frame in a new
  # R variable having list mode.
  # Col 1 will be stored in y[[1]], col 2 in y[[2]], and so on.
  #
  if(is.null(dim(x)))stop("The argument x must be a matrix or data frame")
  y<-list()
  for(j in 1:ncol(x))y[[j]]<-x[,j]
  y
}
