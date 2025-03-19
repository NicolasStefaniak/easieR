corr.matrice <-
  function(X=NULL, Y=NULL, Z=NULL,data=NULL, group=NULL,method="pearson",param=c("H0","FB"), save=F, outlier=c(.dico[["txt_complete_dataset"]]),n.boot=1,  rscale=0.354, info=T,
           p.adjust="holm",out.m=2, na.rm=NULL, html=T) {
    # X : character or vector. First set of variables
    # Y : character or vector. Second set of variables Must be NULL if Z is not
    # Z : character or vector. Names of the variables to control in partial correlation. Must be NULL if Y is not
    # data : dataset
    # group : character or vector. Names of the classifying variables
    # method : one among c("pearson", "spearman", "kendall")
    # param :  one or both among "H0" (null hypoethesis testing) et "FB"(bayesian factors)
    # save : logical. Must the analyses be saved ?
    # outlier : One among   c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]])
    # rscale : numeric. If not null, bayesian factors are computed. Can also be "moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]
    # info : logical. Must information be displayed in dialog box interface.
    # correction : character. Probability adjustement. See p.adjust for list of possibilities
    # out.m : 1 for deleting one observation at the time in outlier detection. 2 for all at the same time.
    # na.rm : character. How to deal with missing values ?
    # html : Logical. Should output be a HTML page ?

    corr.matrice.in<-function(X=NULL, Y=NULL, Z=NULL, group=NULL, data=NULL, p.adjust=NULL, rscale=0.354,save=F,outlier=.dico[["txt_complete_dataset"]], info=T, method="pearson", param=c("H0","FB"), n.boot=NULL){
      Resultats<-list()
      if(!is.null(X) & !is.null(data) & (is.null(Y) | is.null(Z))) {dial<-F
      if(is.null(Z)) choix<-.dico[["txt_correlations"]] else choix<-.dico[["txt_partial_and_semi_correlations"]]
      #if(!is.null(Y)) carre<-"rectangulaire" else carre<-.dico[["txt_square"]]
      if(!is.null(Y)) carre<-.dico[["txt_rectangular"]] else carre<-.dico[["txt_square"]]
      }  else {dial<-T
      choix<-NULL}

      if(is.null(choix) ){
        if(info) writeLines(.dico[["ask_type_correlation"]])
        choix<-dlgList(c(.dico[["txt_correlations"]], .dico[["txt_partila_correlations"]]), preselect=.dico[["txt_correlations"]], multiple = FALSE, title=.dico[["ask_corr_or_partial_correlations"]])$res
        if(length(choix)==0) return(NULL)
      }

      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]

      if(choix==.dico[["txt_correlations"]] & dial==T){
        writeLines(.dico[["desc_square_matrix_rectangular_matrix"]])
        carre<-dlgList(c(.dico[["txt_square"]], .dico[["txt_rectangular"]]), multiple = FALSE, title=.dico[["txt_matrix_type"]])$res
        if(length(carre)==0){Resultats<-corr.matrice.in()
        return(Resultats)}
      } else carre<-.dico[["txt_square"]]

      msg3<-.dico[["ask_first_variables_set"]]


      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title=.dico[["txt_variables"]], out=NULL)
      if(is.null(X)) {
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-X$data
      X1<-X$X
      if(carre==.dico[["txt_rectangular"]]){
        msg4<-.dico[["ask_second_variables_set"]]
        Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title=.dico[["txt_second_variables_set"]], out=X1)
        if(is.null(Y)) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X

      }
      if(choix==.dico[["txt_partila_correlations"]]){
        msg6<-.dico[["ask_control_variables"]]
        Z<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title=.dico[["txt_control_variables"]], out=c(X1,Y))
        if(is.null(Z)) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        data<-Z$data
        Z<-Z$X
      }


      if(dial){

        if(info==TRUE) writeLines(.dico[["desc_corr_group_analysis_spec"]])
        dlgList(c(.dico[["txt_yes"]], .dico[["txt_no"]]), preselect=.dico[["txt_no"]], multiple = FALSE, title=.dico[["ask_analysis_by_group"]])$res->par.groupe
        if(length(par.groupe)==0) {
          corr.matrice.in(X=NULL, Y=NULL, data=NULL,method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                          n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)
        } } else par.groupe<-.dico[["txt_no"]]
      msg5<-.dico[["ask_chose_ranking_categorial_factor"]]
      if(par.groupe==.dico[["txt_yes"]] || !is.null(group)){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title=.dico[["txt_variables"]], out=c(X1,Y,Z))
      if(length(group)==0) {   corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                               n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}
      data<-group$data
      group<-group$X
      if(any(ftable(data[,group])<3)){
        msgBox(.dico[["desc_need_at_least_three_observation_by_combination"]])
        corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                        n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)
      }
      }

      if(dial || length(outlier)>1 || outlier %in% c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]) ==FALSE){
        if(info) writeLines(.dico[["ask_analysis_on_complete_data_or_remove_outliers"]])
        outlier<- dlgList(c(.dico[["txt_complete_dataset"]], .dico[["txt_without_outliers"]]), preselect=c(.dico[["txt_complete_dataset"]]),
                          multiple = FALSE, title=.dico[["ask_results_desired"]])$res
        if(length(outlier)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)
        return(Resultats)}
      }
      if(dial || length(method)>1 || method %in% c("pearson", "spearman","kendall") ==FALSE){
        if(info) writeLines(.dico[["ask_correlations_type"]])
        method<-dlgList(c("pearson", "spearman","kendall"), preselect="pearson", multiple = FALSE, title=.dico[["ask_correlations_type"]])$res
        if(length(method)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                           n.boot=NULL, rscale=0.353)
        return(Resultats)}
      }


      if(is.null(Y) & is.null(Z)){

        if(!is.null(n.boot) && ((class(n.boot)!="numeric" & class(n.boot)!="integer") ||  n.boot%%1!=0 || n.boot<1)){
          msgBox(.dico[["desc_bootstraps_number_must_be_positive"]])
          n.boot<-NULL
        }
        while(is.null(n.boot)){
          writeLines(.dico[["ask_bootstrap_numbers_1_for_none"]])

          n.boot<-dlgInput(.dico[["ask_bootstraps_number"]], 1)$res
          if(length(n.boot)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, method=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                            n.boot=NULL, rscale=0.353)
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


      if((dial)|| !is.null(rscale) & ((is.numeric(rscale) & (rscale<0.1 | rscale>2)) || (!is.numeric(rscale) & rscale%in% c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]])==F))) {
        if(info) writeLines(.dico[["ask_null_hypothesis_tests_or_bayesian_factors"]])
        param<-dlgList(c(.dico[["txt_bayesian_factors"]],.dico[["txt_null_hypothesis_tests"]]), preselect=c(.dico[["txt_bayesian_factors"]],.dico[["txt_null_hypothesis_tests"]]), multiple = T, title=.dico[["ask_statistical_approach"]])$res
        if(length(param)==0) { Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                          n.boot=NULL, rscale=0.353)
        return(Resultats)}

        if(any(param==.dico[["txt_bayesian_factors"]]) | any(param=="FB")){
          if(info) writeLines(.dico[["ask_cauchy_apriori_distribution"]])

          rscale<-dlgList(c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]), preselect="moyen", multiple = F, title=.dico[["ask_distribution_type"]])$res
          if(length(rscale)==0) {
            Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                       n.boot=NULL, rscale=0.353)
            return(Resultats)
          }
          ifelse(rscale=="moyen", rscale<-2^0.5/4, ifelse(rscale==.dico[["txt_large"]], rscale<-0.5, ifelse(rscale==.dico[["txt_ultrawide"]], rscale<-2^0.5/2, rscale<-rscale)))} else rscale<-NULL
      }

      if(any(param==.dico[["txt_null_hypothesis_tests"]]) |any(param=="H0")){
        if(dial | length(p.adjust)!=1 || p.adjust %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")==FALSE){
          writeLines(.dico[["ask_correction_desired"]])
          dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect=NULL, multiple = FALSE, title=.dico[["ask_correction_type"]])$res->p.adjust
          if(length(p.adjust)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                              n.boot=NULL, rscale=0.353)->Resultats
          return(Resultats)}
        }
      } else p.adjust<-"none"
      if(dial | length(save)!=1 || !is.logical(save)){
        writeLines(.dico[["ask_save_results"]])
        save<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = TRUE, title=.dico[["ask_save_results"]])$res
        if(length(save)==0) {Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                                              n.boot=NULL, rscale=0.353)->Resultats
        return(Resultats)}

      }

      if(any(is.na(data[,c(X1,Y,Z)]))){
        msgBox(.dico[["ask_how_to_treat_missing_values"]])
        imp<- dlgList(c(.dico[["txt_do_nothing_keep_all_obs"]], .dico[["txt_delete_observations_with_missing_values"]], .dico[["txt_replace_by_mean"]],
                        .dico[["txt_replace_by_median"]],.dico[["txt_multiple_imputation_amelia"]]), preselect=FALSE, multiple = TRUE, title=.dico[["txt_missing_values_treatment"]])$res
        if(length(imp)==0){
          Resultats<-corr.matrice.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,
                                     n.boot=NULL, rscale=0.353)
          return(Resultats)
        }
        data1<-ez.imp(data[, c(X1,Y,Z)], imp=imp)
        data<-data.frame(data1, data[,group])
	names(data)<-c(names(data1), group)
      }

      Resultats$nom<- nom
      Resultats$data<-data
      Resultats$X<-X1
      if(exists("Y")) Resultats$Y<-Y
      if(exists("Z")) Resultats$Z<-Z
      if(exists("group")) Resultats$group<-group
      Resultats$method<- method
      Resultats$outlier<-outlier
      Resultats$param<-param
      Resultats$rscale<-rscale
      Resultats$n.boot<-n.boot
      Resultats$save<-save
      Resultats$p.adjust<-p.adjust
      return(Resultats)
    }



    corr.matrice.out<-function(data, X, Y, Z, p.adjust, method,save, rscale, n.boot, param){
      Resultats<-list()
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-easieR:::.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=F)
      if(method == "pearson"){
      Resultats[[.dico[["txt_multivariate_normality"]]]]<-easieR:::.normalite(data, c(X,Y,Z))
      }


      if(is.null(Z)){
        if(!is.null(Y))  {
          Y1<-as.data.frame(data[,Y])
          names(Y1)<-Y
        }else{
	Y1<-NULL	
	}
        X1<-as.data.frame(data[,X])
        names(X1)<-X
        corr.test(x=X1, y=Y1, use = .dico[["txt_pairwise"]],method=method,adjust=p.adjust, alpha=.05,ci=TRUE)->matrice
        r1<-round(matrice$r,3)


        if(is.null(Y)) r1[which(lower.tri(r1, diag = T))]<-"-"
        Resultats[[.dico[["txt_correlations_matrix"]]]]<-as.data.frame(r1)
	
	      
	      Resultats[["plot"]]<-plot

      } else{
        data[,c(X,Z)]->d2
        partial.r(d2, 1:length(X), (length(X)+1):length(d2))->matrice
        matrice<-corr.p(matrice, adjust=p.adjust, n=length(data[,1])-length(Z))

        r1<-round(matrice$r, 3)

        class(r1)<-"matrix"
        r1[which(lower.tri(r1, diag = T))]<-"-"
        Resultats[[.dico[["txt_partial_correlations_matrix"]]]] <-as.data.frame(r1)

      }

      class(r1)<-"matrix"
      dimnames(r1)[[1]]<-paste(dimnames(r1)[[1]], "r")
      matrice$n->Resultats[[.dico[["txt_sample_size"]]]]

      if(any(param=="H0")|any(param==.dico[["txt_null_hypothesis_tests"]])) {
	      paste(.dico[["desc_applied_correction_is"]],p.adjust)->Resultats$Correction[1]
        if(is.null(Y)) {Resultats$Correction[2]<-.dico[["desc_only_values_above_diagonal_are_adjusted_for_multiple_comp"]]
        		round(matrice$p,3)->r2}else{
		r2<-round(matrice$p.adj,3)
	}
        class(r2)<-c("matrix", "p.value")
	plot<-ggcorrplot(matrice$r, hc.order = F,  lab = TRUE,  p.mat= r2)
        Resultats[[.dico[["txt_probability_matrix"]]]]<-r2
	Resultats[["Figure"]]<-plot      
        dimnames(r2)[[1]]<-paste0(dimnames(r2)[[1]], ".p")
        if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-NA
        r1<-rbind(r1,r2)
      }
      if(method=="kendall") {
        r2<-round(sin(0.5*pi*matrice$r)^2,3) # from David A. Walker 2003 JMASM9: Converting Kendall's Tau For Correlational Or Meta-Analytic Analyses
        Resultats[[.dico[["txt_information"]]]]<-.dico[["desc_effect_size_by_walker"]]
      } else r2<-round(matrice$r^2,3)



      if(!is.null(rscale)){
        r2[which(r2==1)]<-0
        if(is.null(Z))  N<-length(data[,1]) else    N<-length(data[,1])-length(Z)
        matriceBF<-function(X){return(linearReg.R2stat(N=N, 1, X, rscale = rscale, simple = TRUE))}
        r3<-round(apply(X=r2,c(1,2), FUN=matriceBF),3)
        r3<-format(r3, scientific=T)
        if(is.null(Y)) r3[which(lower.tri(r3, diag = T))]<-"-"
        dimnames(r3)[[1]]<-paste0(dimnames(r3)[[1]], ".FB")
        Resultats[[.dico[["txt_bayesian_factors"]]]]<-as.data.frame(r3)
        r1<-rbind(r1, r3)
      }
      class(r2)<-"matrix"
      if(is.null(Y)) r2[which(lower.tri(r2, diag = T))]<-"-"
      Resultats[[.dico[["txt_r_squared_matrix"]]]] <-as.data.frame(r2)
      dimnames(r2)[[1]]<-paste(dimnames(r2)[[1]], "r^2")
      r1<-rbind(r1, r2)
      r1<-data.frame(r1)
	    if(is.null(Y)){
      r1$tri<-1:length(dimnames(r1)[[2]])
      r1<-r1[order(r1$tri), ]
	    r1<-r1[,-length(r1)]
      r1[is.na(r1)]<-"-"
		    }
      nice.mat<-list()
      nice.mat[[.dico[["txt_correlations_matrix"]]]]<-(r1)
      if(html) try(ez.html(nice.mat), silent =T)


      if(is.null(Y) & is.null(Z) & (!is.null(n.boot) && n.boot > 100)) round(cor.ci(data[,X], n.iter=n.boot, plot=FALSE)$ci,4)->Resultats[[.dico[["txt_confidence_interval_estimated_by_bootstrap"]]]] else  round(matrice$ci,4)->Resultats[[.dico[["txt_confidence_interval"]]]]
      names(Resultats[[length(Resultats)]])<-c(.dico[["txt_inferior_limit"]],"r",.dico[["txt_ci_superior_limit"]],.dico[["txt_p_dot_val"]])

      return(Resultats)

    }

    options (warn=-1)
    packages<-c('BayesFactor','nortest', 'psych', 'svDialogs', 'ggplot2', 'ggcorrplot')

    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=='try-error') quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data

    corr.options<-corr.matrice.in(X=X, Y=Y, Z=Z, data=data, group=group,p.adjust=p.adjust, param=param, outlier=outlier, save=save, info=T,  rscale=rscale, n.boot=n.boot)
    if(is.null(corr.options)) return(analyse())

    choix<-corr.options$choix
    X<-corr.options$X
    Y<-corr.options$Y
    Z<-corr.options$Z
    group<-corr.options$group
    data<-corr.options$data
    param<-corr.options$param
    rscale<-corr.options$rscale
    save<-corr.options$save
    outlier<-corr.options$outlier
    method<-corr.options$method
    p.adjust<-corr.options$p.adjust
    n.boot<-corr.options$n.boot

    if(outlier==.dico[["txt_without_outliers"]]){
	    # When matrix correlations > partial > without outliers  => missing Y value causes crash (VI.multiples no Y argumlent)
	    #print(corr.options)
	    #print(X)
	    #print(Y) # <- missing
	    #print(Z)
      inf<-VI.multiples(data, X=c(X,Y,Z))
      Resultats[[.dico[["txt_labeled_outliers"]]]]<-inf[[.dico[["txt_labeled_outliers"]]]]
      data<-inf$data
    }

    Resultats[[.dico[["txt_correlations_matrix"]]]]<-corr.matrice.out(data=data, X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,save=save,
							   rscale=rscale, n.boot=n.boot, param=param)



    if(!is.null(group))   {
      G<-data[,group]
      if(length(group)>1) G<-as.list(G)
      G<-split(data[,c(X,Y,Z)], G)
      for(i in 1:length(G)){
        resg<-corr.matrice.out(data=G[[i]], X=X, Y=Y, Z=Z, p.adjust=p.adjust, method=method,save=save, rscale=rscale, n.boot=n.boot, param=param)
        Resultats[[length(Resultats)+1]]<-resg
        names(Resultats)[length(Resultats)]<-names(G)[i]
      }
    }


    paste(X, collapse="','", sep="")->X
    if(!is.null(Y)) paste(Y, collapse="','", sep="")->Y
    if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
    if(!is.null(group)) paste(group, collapse="','", sep="")->group


    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("corr.matrice(X=c('", X,
                           "'), Y=", ifelse(!is.null(Y),paste0("c('",Y,"')"), "NULL"),
                           ", Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom, ", p.adjust='", p.adjust,
                           "', group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"),
                           ", param=c('", param, "'), save=", save, ",outlier=c('", outlier, "'), info=T, rscale=", ifelse(!is.null(rscale),rscale, "NULL"), ", n.boot=", n.boot, ")")

    .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))



    if(save) save(Resultats=Resultats, choix=paste(.dico[["txt_correlation_is"]], method), env=.e)
    ref1(packages)->Resultats[[.dico[["txt_references"]]]]
    if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
    }
