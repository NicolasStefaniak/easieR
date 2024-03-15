
regressions <-
  function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=txt_complete_dataset, inf=F, CV=F, select.m="none", method="p", step=NULL, group=NULL, criteria=0.15 , scale=T, dial=T, info=T,
           sauvegarde=F, n.boot=NULL, param="param", rscale=0.353, html=TRUE){



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
        if(info) writeLines(ask_chose_relation_between_vars_regressions_log)
        dlgList(c(txt_additive_effects, txt_interaction_effects, txt_specify_model), preselect=txt_regressions, multiple = TRUE, title=ask_which_regression_type)$res->link
        if(length(link)==0) return(NULL) } else link<-"none"

      if(length(Y)>1){
        msgBox(desc_only_one_dependant_variable_alllowed)
        Y<-NULL }
      if(any(link %in% c(txt_additive_effects, txt_interaction_effects))){
        msg3<-ask_chose_dependant_variable
        Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=FALSE, title=txt_dependant_variable, out=NULL)
        if(is.null(Y)) {
          regressions.in()->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X

        if(any(link==txt_additive_effects) || !is.null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-ask_chose_dependant_variable
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title=txt_additive_model_variables, out=Y)
          if(is.null(X_a)) {
            regressions.in()->Resultats
            return(Resultats)}
          data<-X_a$data
          X_a<-X_a$X

        }else X_a<-NULL

        if(any(link==txt_interaction_effects) || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
          msg3<-ask_chose_interaction_model_predictors
          X_i<-c()
          while(length(X_i)<2){
            X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title=txt_interactive_model_variables, out=c(X_a,Y))
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

      if(any(link==txt_specify_model)) {
        if(is.null(modele)) modele<-" "
        modele<-fix(modele)}
      modele<-as.formula(modele)
      variables<-terms(modele)
      variables<-as.character( attributes(variables)$variables)[-1]


      model.test<-try(model.matrix(modele, data), silent=T)
      if(any(class(model.test)=='try-error')) {
        msgBox(desc_incorrect_model)
        return(regressions.in())
      }


      data[complete.cases(data[,variables]),]->data
      msg.options1<-desc_param_test_is_classical_reg_robusts_are_m_estimator

      options<-.ez.options(options=c('choix',"outlier"), n.boot=n.boot,param=T, non.param=F, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial,
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

      Resultats[[txt_descriptive_statistics]]<-.stat.desc.out(X=variables, groupes=NULL, data=dtrgeasieR, tr=.1, type=3, plot=T)

       if(scale==T || scale==txt_center) {
         Resultats$info<-desc_centered_data_schielzeth_recommandations
      if(length(pred)>1) { which(!sapply(dtrgeasieR[,pred[which(pred %in% variables)]],class)%in%c("factor", "character"))->centre
        centre<-pred[centre]}else{centre<-NULL}
      if(!is.null(centre)){
         if(length(centre)==1) dtrgeasieR[,centre]-mean(dtrgeasieR[,centre],na.rm=T)->dtrgeasieR[,centre] else{
        sapply(X=dtrgeasieR[,centre], fun<-function(X){X-mean(X, na.rm=T)})->dtrgeasieR[,centre]
           }
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
      Resultats[[txt_normality_tests]]<-.normalite(data=dtrgeasieR, X='residu', Y=NULL)
      if(length(pred)>1)  {
        cont<-variables[which(!sapply(dtrgeasieR[,variables],class)%in%c("factor","character"))]
        Resultats[[txt_multivariate_normality]]<-.normalite(data=dtrgeasieR, X=cont, Y=NULL)
        ols_plot_resid_fit(lm.r1)
        FIV<-ols_coll_diag(lm.r1) # calcul du facteur d inflation de la variance
        FIV[[1]]<-data.frame(FIV[[1]])
        names(FIV)<-c(txt_multicolinearity_test, txt_proper_values_index)
        names(FIV$`Test de multicolinearite`)<-c(txt_variables, txt_tolerance, "FIV")
        Resultats[[txt_multicolinearity_tests]]<-FIV$`Test de multicolinearite`
        if(any(FIV$`Test de multicolinearite`$Tolerance==0)) {
          msgBox(desc_instable_model_high_multicolinearity)
          return(Resultats)
        }

        Resultats[[txt_linearity_graph_between_predictors_and_dependant_variable]]<-ols_plot_comp_plus_resid(lm.r1)
        Resultats[[txt_proper_values_index]]<-FIV$`Indice des valeurs propres`
        dwt(lm.r1, simulate=TRUE, method= "normal", reps=500)->DWT.results
        Resultats[[txt_durbin_watson_test_autocorr]]<-round(data.frame(txt_autocorrelation=DWT.results[[1]],
                                                                               txt_dw_statistic=DWT.results[[2]],txt_p_dot_val=DWT.results[[3]]),4)

        var.err<-ols_test_breusch_pagan(lm.r1, rhs=T)

        Resultats[[txt_breusch_pagan_test]]<-data.frame(chi=var.err$bp,
                                                                                                                 ddl=length(var.err$preds), valeur.p=var.err$p)

        try(ceresPlots(lm.r1, main=txt_ceres_graph_linearity), silent=T)
      }
      if(select.m!="none"){
        dtrgeasieR<<-dtrgeasieR

        select.m<-switch(select.m,txt_forward_step_ascending="Forward", txt_backward_step_descending=txt_backward, txt_bidirectionnal="Both",
                           "forward"="Forward", "bidirectional"="Both","backward"=txt_backward )
        
        if(method %in% c("F", txt_f_value, "p", txt_probability_value)){
           if(select.m=="Forward")  ols.out <- ols_step_forward_p(lm.r1,penter = criteria, details=F)
          if(select.m=="Backward") ols.out <- ols_step_backward_p(lm.r1, prem=criteria, details=F)
          if(select.m=="Both") ols.out <- ols_step_both_p(lm.r1,pent=criteria, details=F)
          }
       
         if(method %in% c(txt_aic_criterion,"AIC")){
          if(select.m=="Forward")  ols.out <- ols_step_forward_aic(lm.r1, details=F)
          if(select.m=="Backward")  ols.out <- ols_step_backward_aic(lm.r1, details=F)
          if(select.m=="Both")     ols.out <- ols_step_both_aic(lm.r1, details=F) 
           }     

          ols.frame<-data.frame(ols.out$metrics)
          reg.out<-ols_regress(ols.out$model)
          c(summary(ols.out$model)$sigma, summary(ols.out$model)$r.squared, summary(ols.out$model)$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
          pf(summary(ols.out$model)$fstatistic[1], summary(ols.out$model)$fstatistic[2],summary(ols.out$model)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
          c(significativite_modele , p.value)->modele.F # on combine les precedents 
          modele.F<-round(modele.F,3) # on arrondit les nombres a la 3e decimale
         names(modele.F)<-c(txt_residual_error, txt_r_dot_two, "F", txt_df_parenthesis_num, txt_df_parenthesis_denom,txt_p_dot_val)-># attribue le nom aux colonne
                   
          coef.table<-data.frame(b = round(reg.out$betas, 3),
                                 "Erreur Std."= format(round(reg.out$std_errors, 3)), 
                                  "Beta" = c(" ", round(reg.out$sbetas, 3)),
                                 t=round(reg.out$tvalues, 3),
                                 valeur.p =round(reg.out$pvalues, 3),
                                 lower=round(reg.out$conf_lm[, 1], 3),
                                   upper =round(reg.out$conf_lm[, 2], 3))
         names(coef.table)<-c("b",txt_error_dot_standard,"beta","t",
                              txt_p_dot_val,txt_confidence_interval_inferior_limit, txt_confidence_interval_superior_limit)

          select.name<-paste0(txt_selection_method,"-", method)                                 
          Resultats$selection$Selection<-ols.frame 
          Resultats$selection$ANOVA<-modele.F
          Resultats$selection[[txt_coeff_table]]<-coef.table
          names(Resultats)[length(Resultats)]<-select.name 
    
        }

   

        if(any(param=="Bayes")|any(param==txt_bayesian_factors)){

          BF.out<-try(regressionBF(modele, data=dtrgeasieR,progress=F, rscaleCont=rscale), silent=T)
          if(class(BF.out)!='try-error') {
            try(plot(BF.out) , silent=T)
            BF.out<-extractBF(BF.out)
            BF.out<-head(BF.out[order(BF.out[,1], decreasing=T), ])
            BF.out<-BF.out[,1:2]
            Resultats[[txt_selection_method_bayesian_factor]]<-BF.out
          } else Resultats[[txt_selection_method_bayesian_factor]]<-desc_selection_for_bayesian_factor_does_not_apply_to_complex_models
        }
        rm( "dtrgeasieR", envir = .GlobalEnv)
      

      if(!is.null(step)){

        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
        list()->modele.H1
        list()->formule.H1
        for(i in 1:length(step)){

          for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
          formule.H1[[i]]<-modele.H
          lm(modele.H, data=dtrgeasieR, na.action=na.exclude )->lm.H
          lm.H->modele.H1[[i]]}

        if(any(param=="param")|any(param==txt_param_tests)) {
          hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
          if(length(modele.H1)>2){
            for(i in 3: length(modele.H1)){
              hier<-paste0(hier, ",modele.H1[[", i, "]]")
            }
          }
          hier<-paste0(hier,")")
          hier<-eval(parse(text=hier))
          attributes(hier)$heading[1]<-txt_hierarchical_models_variance_analysis_table
          names(hier)<-c(txt_df_residual, "SC.resid",txt_df_effect, "SC", "F", txt_p_dot_val)
          Resultats[[txt_hierarchical_model_analysis]]<-hier



          c(summary(modele.H1[[1]])$sigma, summary(modele.H1[[1]])$r.squared, summary(modele.H1[[1]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
          pf(summary(modele.H1[[1]])$fstatistic[1], summary(modele.H1[[1]])$fstatistic[2],summary(modele.H1[[1]])$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
          c(significativite_modele , p.value)->modele_avec_outliers

          for(i in 2:(length(modele.H1))){
            c(summary(modele.H1[[i]])$sigma, summary(modele.H1[[i]])$r.squared, summary(modele.H1[[i]])$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
            pf(summary(modele.H1[[i]])$fstatistic[1], summary(modele.H1[[i]])$fstatistic[2],summary(modele.H1[[i]])$fstatistic[3], lower.tail=F)->valeur.p #permet de savoir si le F est significatif
            rbind(modele_avec_outliers, c(significativite_modele ,valeur.p))->modele_avec_outliers
          }
          round(modele_avec_outliers,3)->modele_avec_outliers
          c(txt_residual_error, txt_r_dot_two, "F", txt_df_parenthesis_1, txt_df_parenthesis_2,txt_p_dot_val)->dimnames(modele_avec_outliers)[[2]]
          paste(txt_step, 1:length(modele_avec_outliers[,1]))->dimnames(modele_avec_outliers)[[1]]
          Resultats[[txt_hierarchical_models_complete_model_sig_at_each_step]]<-modele_avec_outliers

        }

        if(any(param=="Bayes")|any(param==txt_bayesian_factors)) {
          BF<-lmBF(formula= as.formula(formule.H1[[1]]), data=dtrgeasieR, rscaleFixed=rscale)
          BF.modele<-extractBF(BF, onlybf=T)
          BF.hier<-c(NA)
          for(i in 2:length(formule.H1)){
            numBF<-lmBF(formula= as.formula(formule.H1[[i]]), data=dtrgeasieR, rscaleFixed=rscale)
            BF.modele<-c(BF.modele, extractBF(numBF, onlybf=T))
            denomBF<-lmBF(formula= as.formula(formule.H1[[i-1]]), data=dtrgeasieR, rscaleFixed=rscale)
            OddBF<-numBF/denomBF
            BF.hier<-c(BF.hier, extractBF(OddBF, onlybf=T))}

          BF.hier<-data.frame(desc_fb_ratio_between_models=BF.hier, txt_bayesian_factor_of_model= BF.modele)
          dimnames(BF.hier)[[1]]<- unlist(as.character(formule.H1))
          Resultats[[txt_bayesian_approach_hierarchical_models]]<-BF.hier
        }

      }
      # txt_param_test, txt_non_param_test,txt_robusts_tests_with_bootstraps, txt_bayesian_factors
      if(any(param=="param")|any(param==txt_param_tests)) {
        c(summary(lm.r1)$sigma, summary(lm.r1)$r.squared, summary(lm.r1)$fstatistic)->significativite_modele # fournit les residus, le R.deux et le F
        pf(summary(lm.r1)$fstatistic[1], summary(lm.r1)$fstatistic[2],summary(lm.r1)$fstatistic[3], lower.tail=F)->p.value #permet de savoir si le F est significatif
        c(significativite_modele , p.value)->modele.F # on combine les precedents
        round(modele.F,3)->modele.F # on arrondit les nombres a la 3e decimale
        c(txt_residual_error, txt_r_dot_two, "F", txt_df_parenthesis_num, txt_df_parenthesis_denom,txt_p_dot_val)->names(modele.F)# attribue le nom aux colonnes
        modele.F->Resultats$"Estimation  du modele global"


        data.frame(summary(lm.r1)$coefficients)->table # fournit le b, le t et la valeur de la probabilite. On le stocke dans table
        round(table[,1:4],3)->table # on arrondit les valeurs a 3 decimales

        beta<-coef(lm.r1)*sapply(data.frame(model.matrix(lm.r1)),sd) /sd(dtrgeasieR[,variables[1]])
        c("",round(beta[-1],5))->table$beta # fournit les betas qu on inclut a la table
        names(table)<-c("b",txt_error_dot_standard,"t",txt_p_dot_val,"beta")

        r_carre<- matrix(c(0,0,0),1)
        for(i in 1:length(mod)){
          rep(summary(mod[[i]])$r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre2
          summary(mod[[i]])$r.squared-r_carre[length(r_carre[,2]),1]->diff
          rep(diff, (length(coef(mod[[i]]))-length(r_carre[,1])))->diff
          rep(summary(mod[[i]])$adj.r.squared, (length(coef(mod[[i]]))-length(r_carre[,1])))->r_carre_adj

          round(cbind(r_carre2, diff, r_carre_adj), 4)->r_carre2
          rbind(r_carre,r_carre2 )->r_carre

        }

        dimnames(r_carre)<-list(ligne=NULL, c(txt_r_dot_two, txt_delta_r_squared, txt_r_dot_two_adjusted))
        data.frame(table,r_carre)->table
        table[is.na(table)]<-""
        table->Resultats[[txt_beta_table]]
        if(length(pred)>1){
          ols.corr<-try(ols_correlations(lm.r1), silent=T)
          if(any(class(ols.corr)!='try-error')){
          Resultats[[txt_variables_contribution_to_model]]<-ols.corr
          Resultats[[txt_added_variables_graph]] <-ols_plot_added_variable(lm.r1)}

        }
      }

      if(any(param=="Bayes")|any(param==txt_bayesian_factors)){

        lmBF(modele1, data=dtrgeasieR)->BF.out
        BF.table<-extractBF(BF.out)[1:2]
        if(length(pred)>1) { for(i in 2:length(pred)){
          modele1<-update(modele1, as.formula(paste0(".~.+",pred[i])))
          lmBF(modele1, data=dtrgeasieR)->BF.out
          BF.table<-rbind(BF.table, extractBF(BF.out)[1:2])
        }
        }
        Resultats[[txt_bayesian_factors]]<-BF.table

      }

      if(any(param==txt_robusts| any(param==txt_robusts_tests_with_bootstraps))){

        rlm(formula=modele, data=dtrgeasieR)->modele_robuste
        summary(modele_robuste)->res_modele_robuste
        (1-pt(abs(res_modele_robuste$coefficients[,3]), (length(dtrgeasieR[,1])-1-length(pred)), lower.tail=TRUE))*2->proba
        round(cbind(res_modele_robuste$coefficients, proba),3)->M_estimator
        data.frame(M_estimator)->M_estimator
        noms<-c(txt_b_m_estimator, "SE", "t", txt_p_dot_val)


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
            noms<-c(noms, txt_percentile_inferior_limit_dot, txt_percentile_superior_limit_dot)
          } else{
            noms<-c(noms, txt_bca_inferior_limit, txt_bca_superior_limit)
          }
          data.frame(M_estimator, round(intervalle,4))->M_estimator
        }
        names(M_estimator)<-noms
        Resultats[[txt_robusts_statistics]]<-M_estimator
      }


      if(CV) desc_cross_validation_issues

      return(Resultats)

    }
    options (warn=-1)
    .e <- environment()
    c('MASS','BayesFactor','boot','car','ggplot2','gsl', 'MBESS','olsrr','nortest','psych','svDialogs')->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(any(class(win)=='try-error')) quartz()
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
    if(reg.in.output$options$rscale) rscale<-reg.in.output$options$rscale/2 else rscale<-reg.in.output$options$rscale
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








    if(any(outlier==  txt_complete_dataset)){
      Resultats[[txt_complete_dataset]]<-regressions.out(dtrgeasieR=data, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)
      if(!is.null(group))   {
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-regressions.out(dtrgeasieR=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)

          R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Resultats[[txt_complete_dataset]][[txt_group_analysis]]<-R1
      }

    }
    if(any(outlier==txt_identifying_outliers)|any(outlier==txt_without_outliers)|inf==T){
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
        writeLines(desc_obs_with_asterisk_are_outliers)
        View(data)
        suppression<-"yes"
        outliers<-data.frame()
        nettoyees<-data
        while(suppression=="yes"){

          cat (ask_press_enter_to_continue)
          line <- readline()
          sup<-NA
          while(is.na(sup)){
            sup <- dlgInput(ask_obs_to_remove, 0)$res
            if(length(sup)==0) return(regressions())
            strsplit(sup, ":")->sup
            tail(sup[[1]],n=1)->sup
            as.numeric(sup)->sup
            if(is.na(sup)) msgBox(ask_enter_number_of_to_be_removed_variable)
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
        Resultats$"information"[[desc_outliers_identified_on_4_div_n]]
      }
      nettoyees->>nettoyees
      length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
      if(any(outlier== txt_identifying_outliers)){
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retire"=N_retire, txt_percent_removed_obs=Pourcentage_retire)->Resultats[[txt_identified_outliers_synthesis]]
        if(length(outliers)!=0) Resultats[[txt_identifying_outliers]][[desc_identified_outliers]]<-outliers

      }
      if(any(outlier== txt_without_outliers)) {
        if(N_retire!=0 | all(outlier!=txt_complete_dataset)){
          Resultats[[txt_without_outliers]]<-regressions.out(dtrgeasieR=nettoyees, modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                                                     sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)

          if(!is.null(group))   {
            R1<-list()
            G<-nettoyees[,group]
            if(length(group)>1) G<-as.list(G)
            G<-split(nettoyees, G)
            for(i in 1:length(G)){
              resg<-regressions.out(dtrgeasieR=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group, criteria=criteria , scale=scale,
                                    sauvegarde=sauvegarde, n.boot=n.boot, param=param, rscale=rscale)

              R1[[length(R1)+1]]<-resg
              names(R1)[length(R1)]<-names(G)[i]
            }
            Resultats[[txt_without_outliers]][[txt_group_analysis]]<-R1
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
    .add.result(Resultats=Resultats, name =paste(txt_multiple_regressions_dot, Sys.time() ))
    if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix=txt_multiple_regressions_dot, env=.e)
    Resultats[[txt_references]]<-ref1(packages)
    if(html) ez.html(Resultats)

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
    if(info)   writeLines(ask_center_numeric_variables)
    scale<-dlgList(c(txt_center, txt_non_centered), multiple = FALSE, title=ask_center)$res
    if(length(scale)==0) return(NULL)
    scale<-ifelse(scale==txt_center,T,F)
  }
  Resultats$scale<-scale
  if(dial || !is.logical(inf) || !is.logical(CV)) {
    writeLines(ask_specify_other_options_regressions)
    autres.options<-c(txt_cross_validation,txt_influence_method,  txt_none)
    if(dim(model.matrix(modele, data))[2]>2) autres.options<-c(txt_selection_methods, txt_hierarchical_models, autres.options)
    if(length(step2)<length(data))  autres.options<-c(txt_groups_analysis,autres.options)

    autres.options<- dlgList( autres.options, preselect=c(txt_none), multiple = TRUE, title=ask_other_options)$res
    if(length(autres.options)==0) return(.regressions.options(data=data, modele=modele))
    # if(any(autres.options==txt_none)) return(Resultats)
    if(any(autres.options==txt_influence_method) ) Resultats$inf<-T else  Resultats$inf<-F
    if(any(autres.options==txt_cross_validation) ) Resultats$CV<-T else Resultats$CV<-F
  }else{Resultats$inf<-inf
  Resultats$CV<-CV
  autres.options<-txt_none
  }


  if(any(autres.options==txt_groups_analysis) || !is.null(group)) {

    msg5<-ask_chose_categorial_ranking_factor
    group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=T, message=msg5,  multiple=FALSE, title=txt_groups_variables, out=step2)
    if(length(group)==0) { return(.regressions.options(data=data, modele=modele))}
    data<-group$data
    group<-group$X
    ftable(data[,group])->groupe.check
    if(any(is.na(groupe.check)) || min(groupe.check)<(length(dimnames(model.matrix(as.formula(modele), data))[[2]])+10)) {
      msgBox(desc_at_least_10_obs_needed)
      return(groupe.check)
    }
  }

  if(any(autres.options==txt_selection_methods) || select.m!="none" & length(select.m)!=1 | !select.m%in%c("none","forward", "backward", "bidirectional",txt_forward_step_ascending,
                                                                                                             txt_backward_step_descending, txt_bidirectionnal)){
    if(info) writeLines(ask_chose_selection_method)
    select.m<- dlgList(c(txt_forward_step_ascending,txt_backward_step_descending, txt_bidirectionnal),
                       preselect=NULL, multiple = FALSE, title=txt_method_choice)$res
    if(length(select.m)==0) return(.regressions.options(data=data, modele=modele))
  }
  if(!is.null(method)){
    if(any(autres.options==txt_selection_methods)   || (select.m!="none" && !method%in%c("AIC", "p", "F", txt_f_value,txt_probability_value, txt_aic_criterion)) ){
      if(info) writeLines(ask_selection_method)
      method<- dlgList(c(txt_f_value,txt_probability_value, txt_aic_criterion),
                       preselect=c(txt_f_value), multiple = FALSE, title=txt_method_choice)$res
      if(length(method)==0) return(.regressions.options(data=data, modele=modele))
    }

    if(select.m!="none" & (method==txt_f_value | method=="F")){
      if(!is.null(criteria) && (!is.numeric(criteria) || criteria<1)) {msgBox(desc_specify_f_value)
        criteria<-NULL}

      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput(ask_f_value, 4)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria<1) {criteria<-NULL
          msgBox(desc_specify_f_value)
          }
          criteria<-df(criteria, df1=1, df2=(length(data[,1])-1-length(step1)), log = FALSE)
        }
      }
    }

    if(select.m!="none" & (method==txt_probability_value | method=="p")){
      if(dial | !is.null(criteria) && (!is.numeric(criteria) || criteria<0 || criteria>1)) {msgBox(desc_specify_probability_value)
        criteria<-NULL}
      if(is.null(criteria)) {
        while(is.null(criteria)){
          criteria <- dlgInput(ask_probability_value, 0.15)$res
          if(length(criteria)==0) return(.regressions.options(data=data, modele=modele))
          strsplit(criteria, ":")->criteria
          tail(criteria[[1]],n=1)->criteria
          as.numeric(criteria)->criteria
          if(is.na(criteria) || criteria>1 || criteria<0 ) {criteria<-NULL
          msgBox(desc_specify_probability_value)}
        }
      }

    }
  }
  if(any(autres.options==txt_hierarchical_models)| !is.null(step)) {

    if(!is.null(step) ){
      st1<-unlist(step)
      if(any(table(st1>1))) st1<-txt_error
      if(any(!st1%in%step1 ))st1<-txt_error
      if(st1==txt_error){
        msgBox(desc_issue_in_hierarchical_regression)
        step<-NULL
      }
    }
    if(is.null(step)){
      if(info) writeLines(ask_chose_variables)
      step<-list()
      step[[1]]<- dlgList(step1, preselect=NULL, multiple = TRUE, title=txt_variables_from_step)$res
      if(length(step[[1]])==0) return(.regressions.options(data=data, modele=modele))
      setdiff(step1,step[[1]])->step1

      while(length(step1!=0)){
        step[[length(step)+1]]<-dlgList(step1, multiple = TRUE,title=txt_variables_from_step)$res
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
