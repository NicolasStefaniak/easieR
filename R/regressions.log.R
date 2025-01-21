regressions.log <-
  function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
           sauvegarde=F,proba=F, html=T){

    logisticPseudoR2s <- function(LogModel) {
      dev <- LogModel$deviance
      nullDev <- LogModel$null.deviance
      modelN <-  length(LogModel$fitted.values)
      R.l <-  1 -  dev / nullDev
      R.cs <- 1- exp ( -(nullDev - dev) / modelN)
      R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
      return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
    }
    reg.log.in<-function(data=NULL, modele=NULL, Y=NULL, X_a=NULL, X_i=NULL, outlier=NULL, inf=T, select.m="none", step=NULL, group=NULL, scale=T, dial=T, info=T,
                         sauvegarde=F,proba=F){

      options (warn=-1)
      Resultats<-list()
      if(is.null(data) | is.null(modele))  {dial<-TRUE}else dial<-F
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]


      if(dial && is.null(modele)){
        if(info) writeLines(.dico[["ask_chose_relation_between_vars_regressions_log"]])
        dlgList(c(.dico[["txt_additive_effects"]], .dico[["txt_interaction_effects"]], .dico[["txt_specify_model"]]), preselect=.dico[["txt_regressions"]], multiple = TRUE, title=.dico[["ask_which_regression_type"]])$res->link
        if(length(link)==0) return(NULL)} else link<-"none"

      if(length(Y)>1){
        msgBox(.dico[["desc_only_one_dependant_variable_alllowed"]])
        Y<-NULL }
      if(any(link %in% c(.dico[["txt_additive_effects"]], .dico[["txt_interaction_effects"]]))){
        msg3<-.dico[["ask_chose_dependant_variable"]]
        Y<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=FALSE, title=.dico[["txt_dependant_variable"]], out=NULL)
        if(is.null(Y)) {
          reg.log.in()->Resultats
          return(Resultats)}
        data<-Y$data
        Y<-Y$X

        if(length(unique(data[,Y]))!=2) {
          msg1<-paste(.dico[["desc_your_dependant_variable_has"]], length(unique(data[,Y])), .dico[["desc_must_be_dichotomic"]] )
          msgBox(msg1)
          if(class(data[,Y]) %in%c("numeric","integer")){
            dlgMessage(.dico[["ask_convert_dependant_variable_to_dichotomic"]],"yesno")$res->conv

            if(conv=="no") return(reg.log.in())  else{
              if(info) writeLines(.dico[["ask_criterion_for_dichotomy"]])
              dlgList(c(.dico[["txt_median"]], .dico[["txt_threshold"]]), preselect=.dico[["txt_median"]], multiple = FALSE, title=.dico[["ask_coding_criterion"]])$res->codage
              if(length(codage)==0) return(reg.log.in())
              if(codage==.dico[["txt_median"]]) data[,Y]<-ifelse(data[,Y]>median(data[,Y]),1, 0)
              View(data)
              readline()
              if(codage==.dico[["txt_threshold"]]) {
                seuil<-NA
                while(is.na(seuil)){
                  seuil<-dlgInput(.dico[["ask_separation_value"]], median(data[,Y]))$res
                  if(length(seuil)==0) return(reg.log.in())
                  strsplit(seuil, ":")->seuil
                  tail(seuil[[1]],n=1)->seuil
                  as.numeric(seuil)->seuil
                  if(is.na(seuil) || seuil>max(data[,Y]) || seuil<min(data[,Y])) {msgBox(.dico[["desc_value_must_be_numeric"]])
                    Y<-NA}
                }
                data[,Y]<-ifelse(data[,Y]>seuil,1, 0)

              } # seuil
            }
          }
          if(class(data[,Y]) %in%c("factor","character")){
            dlgMessage(.dico[["ask_regroup_modalities"]],"yesno")$res->reg
            if(reg=="no") return(reg.log.in()) else {
              if(info) writeLines(.dico[["ask_linebase_modalities"]])
              reg<- dlgList(levels(data[,Y]), preselect=NULL, multiple = TRUE, title=.dico[["txt_modalities_to_regroup"]])$res
              setdiff(levels(data[,Y]),reg)->reste
              data[,Y]<-ifelse(data[,Y]%in%reg, 0,1)
              data[,Y]<-factor(data[,Y])
            }
          }
        }


        if(any(link==.dico[["txt_additive_effects"]]) || !is.null(X_a)| any(X_a %in% names(data)==F)) {
          msg3<-.dico[["ask_chose_dependant_variable"]]
          X_a<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title=.dico[["txt_additive_model_variables"]], out=Y)
          if(is.null(X_a)) {
            reg.log.in()->Resultats
            return(Resultats)}
          data<-X_a$data
          X_a<-X_a$X

        }else X_a<-NULL

        if(any(link==.dico[["txt_interaction_effects"]]) || !is.null(X_i) & (length(X_i)<2 | any(X_i %in% names(data)==F))) {
          msg3<-.dico[["ask_chose_interaction_model_predictors"]]
          X_i<-c()
          while(length(X_i)<2){
            X_i<-.var.type(X=Y, info=info, data=data, type=NULL, check.prod=F, message=msg3,  multiple=TRUE, title=.dico[["txt_interactive_model_variables"]], out=c(X_a,Y))
            if(is.null(X_i)) {
              reg.log.in()->Resultats
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



      if(any(link==.dico[["txt_specify_model"]])) modele<-fix(modele)
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels

      if(dial){
        if(length(pred>1)){
          pred.ord<-c()
          while(length(pred)!=0){
            if(info)  writeLines(.dico[["ask_variables_order_for_max_likelihood"]])
            V1<-dlgList(pred, multiple = FALSE,title=.dico[["ask_variable_at_this_point"]])$res
            c(pred.ord,V1)->pred.ord
            setdiff(pred,V1)->pred}
        }else pred.ord<-pred

        paste0(Y," ~ ", pred.ord[1])->modele
        if(length(pred.ord)>1) for(i in 2 : length(pred.ord)) paste0(modele, "+", pred.ord[i])-> modele
        modele<-as.formula(modele)}

      model.test<-try(model.matrix(modele, data), silent=T)
      if(any(class(model.test)=='try-error')) {
        msgBox(.dico[["desc_incorrect_model"]])
        return(reg.log.in())
      }


      data[complete.cases(data[,variables]),]->data
      options<-.ez.options(options=c("outlier"), n.boot=NULL,param=F, non.param=F, robust=F, Bayes=F, msg.options1=NULL, msg.options2=NULL, info=info, dial=dial,
                           choix=NULL,sauvegarde=sauvegarde, outlier=outlier, rscale=NULL)
      if(is.null(options)) return(reg.log.in())

      reg.options<- .regressions.options(data=data, modele=modele, CV=FALSE, inf=inf, select.m=select.m, method=NULL, criteria=NULL, step=step, group=group, scale=scale, dial=dial,info=info)
      if(is.null(reg.options)) return(reg.log.in())

      if(dial){
        if(info) writeLines(.dico[["ask_integrate_probabilities_to_dataset"]])
        dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title=.dico[["ask_probabilities"]])$res->proba

      }

      Resultats$proba<-proba
      Resultats$data<-data
      Resultats$nom<-nom
      Resultats$modele<-modele
      Resultats$options<-options
      Resultats$reg.options<-reg.options
      return(Resultats)

    }

    reg.log.out<-function(data=NULL, modele=NULL,  select.m="none", step=NULL, scale=T, nom=NULL,proba=F){

      Resultats<-list()
      variables<-terms(as.formula(modele))
      variables<-as.character( attributes(variables)$variables)[-1]
      pred<-attributes(terms(as.formula(modele)))$term.labels
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-.stat.desc.out(X=variables, groupes=NULL, data=data, tr=.1, type=3, plot=T)

      if(scale==T || scale==.dico[["txt_center"]]) {
	  Resultats$info<-.dico[["desc_centered_data_schielzeth_recommandations"]]
	  variables[-1]->pred2
	  if(length(pred2)>1) { which(!sapply(data[,pred2[which(pred2 %in% variables)]],class)%in%c("factor", "character"))->centre
	  centre<-pred2[centre]}else{centre<-NULL}
	  if(!is.null(centre)){
	      if(length(centre)==1) data[,centre]-mean(data[,centre],na.rm=T)->data[,centre] else{
		  sapply(X=data[,centre], FUN = function(X){X-mean(X, na.rm=T)})->data[,centre]
	      }

	  }
      }
      if(class(data[,variables[1]])=="character") factor(data[,variables[1]])->data[,variables[1]]

      if(!is.null(step)){

        as.formula(paste0(variables[1]," ~ ",step[[1]][1]))->modele.H
        list()->modele.H1
        list()->formule.H1
        for(i in 1:length(step)){

          for(j in 1:length(step[[i]])){update(modele.H, as.formula(paste0(".~. + ",step[[i]][j])))->modele.H}
          formule.H1[[i]]<-modele.H
          glm(modele.H, data=data, na.action=na.exclude , family="binomial")->lm.H
          lm.H->modele.H1[[i]]}

        hier<-paste0("anova(modele.H1[[1]],modele.H1[[2]]")
        if(length(modele.H1)>2){
          for(i in 3: length(modele.H1)){
            hier<-paste0(hier, ",modele.H1[[", i, "]]")
          }
        }
        hier<-paste0(hier,")")
        hier<-eval(parse(text=hier))

        attributes(hier)$heading[1]<-.dico[["txt_hierarchical_models_deviance_table"]]
        round(1-pchisq(hier$Deviance,hier$Df,lower.tail=F),4)->hier$valeur.p
        names(hier)<-c(.dico[["txt_df_residual"]], "Deviance.resid",.dico[["txt_df_effect"]], .dico[["txt_deviation"]], .dico[["txt_p_dot_val"]])
        Resultats[[.dico[["txt_hierarchical_model_analysis"]]]]<-hier
      }





      mod<-list()
      modele1<-as.formula(paste0(variables[1], "~", pred[1]))
      glm(modele1, data=data, family="binomial")->glm.r1
      mod <- list()
      glm.r1->mod[[1]]
      if(length(pred)>1) {
        for(i in 2:length(pred)){update(glm.r1, as.formula(paste0(".~.+",pred[i])))->glm.r1
          glm.r1->mod[[i]]}
      }

      Amelioration_du_MV<-anova(mod[[length(mod)]])

      summary(mod[[length(mod)]])->resultats
      as(resultats$call,"character")->texte
      paste(.dico[["desc_tested_model_is"]] , texte[2])->Resultats[[.dico[["txt_test_model"]]]]

      cbind(rms::vif(mod[[length(mod)]]), 1/rms::vif(mod[[length(mod)]]))->MC
      dimnames(MC)[[2]]<-c(.dico[["txt_inflation_variance_factor"]], .dico[["txt_tolerance"]])
      round(MC,4)->Resultats[[.dico[["txt_multicolinearity_test"]]]]

      ddl<-sum(Amelioration_du_MV$Df[2:length(Amelioration_du_MV$Df)])
      chi.carre.modele<-Amelioration_du_MV$`Resid. Dev`[1]-Amelioration_du_MV$`Resid. Dev`[length(Amelioration_du_MV$`Resid. Dev`)]
      valeur.p<-round(1-pchisq(chi.carre.modele,ddl),4)
      Pseudo.R.carre<-logisticPseudoR2s(mod[[length(mod)]])
      mod.glob<-data.frame(chi.carre.modele, ddl, valeur.p,Pseudo.R.carre[1],Pseudo.R.carre[2],Pseudo.R.carre[3])
      names(mod.glob)<-c(.dico[["txt_chi_dot_squared_model"]], .dico[["txt_df"]], .dico[["txt_p_dot_val"]],.dico[["txt_hosmer_lemeshow_r_2"]],
                        .dico[["txt_cox_snell_r_2"]],"Nagelkerke R^2")
      Resultats[[.dico[["txt_model_significance"]]]]<-mod.glob


      #Amelioration_du_MV$chi.deux.prob<-1-pchisq(Amelioration_du_MV$Deviance, Amelioration_du_MV$Df)
      Amelioration_du_MV<-round(Amelioration_du_MV,4)
      names(Amelioration_du_MV)<-c(.dico[["txt_df_predictor"]], "MV",.dico[["txt_df_residuals"]],"MV residuel",.dico[["txt_p_dot_val"]])
      Resultats[[.dico[["desc_improve_likelihood_for_each_variable"]]]]<-data.frame(Amelioration_du_MV)

      data.frame(resultats$coefficients)->table
      (table$z.value)^2->table$Wald.statistic
      exp(table$Estimate)->table$Odd.Ratio
      round(table,4)->table
      names(table)<-c("b",.dico[["txt_error_dot_standard"]],.dico[["txt_z_dot_val"]],"p.Wald", "Wald",.dico[["txt_odd_ratio_dot"]])
      cbind(table, round(exp(confint(mod[[length(mod)]])),4))->table
      table$interpretation<-ifelse(table$Odd.ratio>=1,paste(table$Odd.ratio, .dico[["desc_times_more"]]), paste(round(1/table$Odd.ratio,4), .dico[["desc_times_less"]]))
      table->Resultats[[.dico[["txt_coeff_table"]]]]

      R_sq<-NULL
      for(i in 1:length(mod)){logisticPseudoR2s(mod[[i]])->R_squared
        rbind(R_sq, R_squared)->R_sq}
      diff(R_sq,lag=1)->R_sq[2.]
      dimnames(R_sq)[[1]]<-pred
      dimnames(R_sq)[[2]]<-c(.dico[["txt_hosmer_lemeshow_r_2"]],.dico[["txt_cox_snell_r_2"]],"Nagelkerke R^2")
      R_sq->Resultats[[.dico[["txt_pseudo_r_square_delta"]]]]

      if(proba=="TRUE")	{
        round(fitted(mod[[length(mod)]]),4)->data[[.dico[["txt_predicted_probabilities"]]]]
        head(data)
        print(nom)
        assign(x=nom, value=data, envir=.GlobalEnv)}

      if(select.m!="none"){
        #select.m<-switch(select.m,txt_forward_step_ascending="forward", txt_backward_step_descending="backward", txt_bidirectionnal="both",
        #                 "forward"="forward", "bidirectional"="both","backward"="backward" )
        if (select.m==.dico[["txt_forward_step_ascending"]]) "forward" -> select.m
        else if (select.m==.dico[["txt_backward_step_descending"]]) "backward" -> select.m
	else if (select.m==.dico[["txt_bidirectionnal"]]) "both" -> select.m
        else if (select.m=="forward") "forward" -> select.m
	else if (select.m=="bidirectional") "both" -> select.m
	else if (select.m=="backward") "backward" -> select.m

        if(  select.m=="forward") { mod0<-as.formula(paste0(as.character(modele)[2], "~1"))
          glm.0<-glm(mod0, data=data, family="binomial")
        }
        glm(modele, data=data, family="binomial")->glm.r1

        if(  select.m=="forward") { steps<-stepAIC(glm.0, direction=select.m, scope =list(upper=glm.r1,
    lower = glm.0)) }else{
        steps<-stepAIC(glm.r1, direction=select.m)
        }
        Resultats[[.dico[["txt_selection_method_akaike"]]]]<-steps$anova
       # modele<-as.formula(attributes(steps$anova)$heading[5])
      }

      return(Resultats)



    }


    c('boot','car','psych', 'mlogit','svDialogs','rms','MASS')->packages
    if(class(data)=="data.frame") deparse(substitute(data))->data
    options (warn=-1)
    .e <- environment()
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    Resultats<-list()
    reg.in.output<-reg.log.in(data=data, modele=modele, Y=Y, X_a=X_a, X_i=X_i, outlier=outlier, inf=inf,
                              select.m=select.m,  step=step, group=group,  scale=scale, info=info, sauvegarde=sauvegarde, proba=proba)
    if(is.null(reg.in.output)) return(choix.reg())
    data<-reg.in.output$data
    nom<-reg.in.output$nom
    modele<-reg.in.output$modele
    outlier<-reg.in.output$options$desires
    sauvegarde<-reg.in.output$options$sauvegarde
    scale<-reg.in.output$reg.options$scale
    inf<-reg.in.output$reg.options$inf
    step<-reg.in.output$reg.options$step
    select.m<-reg.in.output$reg.options$select.m
    group<-reg.in.output$reg.options$group
    proba<-reg.in.output$proba

    if(!is.null(reg.in.output$reg.options$CV) && reg.in.output$reg.options$CV==TRUE) print(.dico[["desc_cross_validation_is_not_yet_supported"]])

    if(any(outlier==  .dico[["txt_complete_dataset"]])){
      Resultats[[.dico[["txt_complete_dataset"]]]]<-  reg.log.out(data=data, modele=modele,  select.m=select.m, step=step, scale=scale, proba=proba, nom=nom)
      if(!is.null(group))   {
        R1<-list()
        G<-data[,group]
        if(length(group)>1) G<-as.list(G)
        G<-split(data, G)
        for(i in 1:length(G)){
          resg<-  try(reg.log.out(data=G[[i]], modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba), silent=T)
          if(class(resg)=='try-error')   R1[[length(R1)+1]]<-.dico[["desc_insufficient_obs"]] else R1[[length(R1)+1]]<-resg
          names(R1)[length(R1)]<-names(G)[i]
        }
        Resultats[[.dico[["txt_complete_dataset"]]]][[.dico[["txt_group_analysis"]]]]<-R1
      }

    }
    if(any(outlier==.dico[["txt_identifying_outliers"]])|any(outlier==.dico[["txt_without_outliers"]])|inf==T){

      lm.r1<-glm(modele, data, na.action=na.exclude ,family="binomial")
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
        writeLines(.dico[["desc_obs_with_asterisk_are_outliers"]])
        View(data)
        suppression<-"yes"
        outliers<-data.frame()
        nettoyees<-data
        while(suppression=="yes"){

          cat (.dico[["ask_press_enter_to_continue"]])
          line <- readline()
          sup<-NA
          while(is.na(sup)){
            sup <- dlgInput(.dico[["ask_obs_to_remove"]], 0)$res
            if(length(sup)==0) return(regressions())
            strsplit(sup, ":")->sup
            tail(sup[[1]],n=1)->sup
            as.numeric(sup)->sup
            if(is.na(sup)) msgBox(.dico[["desc_you_must_give_obs_number"]])
          }
          if(sup==0) suppression<-"no" else {
            rbind(outliers, nettoyees[sup,])->outliers
            nettoyees[-sup,]->nettoyees
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
        Resultats$"information"[[.dico[["desc_outliers_identified_on_4_div_n"]]]]
      }
      nettoyees->>nettoyees

      if(any(outlier== .dico[["txt_identifying_outliers"]])){
        length(data[,1])-length(nettoyees[,1])->N_retire # identifier le nombre d observations retirees sur la base de la distance de cook
        paste(N_retire/length(data[,1])*100,"%")->Pourcentage_retire # fournit le pourcentage retire
        data.frame("N.retirees"=N_retire, txt_percentage_removed_obs=Pourcentage_retire)->Resultats[[.dico[["txt_identified_outliers_synthesis"]]]]
        if(length(outliers)!=0) Resultats[[.dico[["txt_identifying_outliers"]]]][[.dico[["desc_identified_outliers"]]]]<-outliers

      }
      if(any(outlier== .dico[["txt_without_outliers"]])) {
        if(N_retire!=0 | all(outlier!=.dico[["txt_complete_dataset"]])){
          so<- try(reg.log.out(data=nettoyees,modele=modele,  select.m=select.m, step=step, scale=scale,proba=proba, nom=paste0(nom,.dico[["txt_dot_cleaned"]])),silent=T)
          if(class(so)=='try-error') Resultats[[.dico[["txt_without_outliers"]]]]<-.dico[["desc_removing_outliers_weakens_sample_size"]] else{
            Resultats[[.dico[["txt_without_outliers"]]]]<-so

            if(!is.null(group))   {
              R1<-list()
              G<-nettoyees[,group]
              if(length(group)>1) G<-as.list(G)
              G<-split(nettoyees, G)
              for(i in 1:length(G)){
                resg<- try( reg.log.out(data=G[[i]], modele=modele,  VC=VC, select.m=select.m, method=method, step=step, group=group,  scale=scale,proba=proba), silent=T)

                if(class(resg)=='try-error')   R1[[length(R1)+1]]<-.dico[["desc_insufficient_obs"]] else R1[[length(R1)+1]]<-resg
                names(R1)[length(R1)]<-names(G)[i]
              }
              Resultats[[.dico[["txt_without_outliers"]]]][[.dico[["txt_group_analysis"]]]]<-R1
            }
          }

        }
      }
    }


    paste(outlier, collapse="','", sep="")->outlier
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
    Resultats$Call<-paste0("regressions.log(data=", nom, ",modele=",  modele, ",outlier=c('", outlier, "'),inf=", inf, ",select.m='", select.m,"',step=", ifelse(!is.null(step), step.call,"NULL"),
                           ",group=", ifelse(is.null(group), "NULL", paste0("c('",group,"')")),",dial=T, info=T,sauvegarde=", sauvegarde,",proba=",proba ,")")


    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste(.dico[["txt_log_regression_dot"]], Sys.time() ))
    if(sauvegarde)   if(sauvegarde) save(Resultats=Resultats, choix=.dico[["txt_log_regression_dot"]], env=.e)
    Resultats[[.dico[["txt_references"]]]]<-ref1(packages)
    if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)

  }
