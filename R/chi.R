chi <-
  function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=TRUE, n.boot=NULL, priorConcentration =1,
           SampleType=NULL,fixedMargin=NULL, choix2=c(.dico[["txt_non_parametric_test"]],.dico[["txt_robusts_tests_with_bootstraps"]], .dico[["txt_bayesian_factors"]]) ,rscale=2^0.5/2, html=T){
    # X = character or vector.  First set of variables
    # Y = character or vector. Second set of variables
    # Effectifs = character. Name of weighting variable. Must be positive integer
    # p = vector of probabilities. Must be equal to 1. The lenght must be equel to number of levels of X
    # choix = character. One among .dico[["txt_chi_adjustement"]], .dico[["txt_chi_independance"]], or .dico[["txt_mcnemar_test"]]
    # data = name of the dataframe
    # B = number of bootstrap fro computing p.values by Monte-Carlo simulation
    # priorConcentration : prior concentration paramter, set to 1 by default (see ?contingencyTableBF)
    # SampleType : the sampling plan (see details)
    # fixedMargin : for the independent multinomial sampling plan, which margin is fixed ("rows" or "cols")
    # rscale : prior scale. A number of preset values can be given as strings
    chi.in<-function(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL){
      if(!is.null(choix)) dial<-F else dial<-T
      if(is.null(choix) || (choix %in%c(.dico[["txt_chi_adjustement"]], .dico[["txt_chi_independance"]], .dico[["txt_mcnemar_test"]])==FALSE)){
        if(info) writeLines(.dico[["ask_chi_squared_type"]])
        choix<- dlgList(c(.dico[["txt_chi_adjustement"]], .dico[["txt_chi_independance"]], .dico[["txt_mcnemar_test"]]), preselect=.dico[["txt_chi_independance"]], multiple = FALSE, title=.dico[["txt_chi_squared_type"]])$res
        if(length(choix)==0) return(NULL)
      }

      choix.data(data=data, info=info, nom=T)->data
      if(length(data)==0) return(NULL)
      data[[1]]->nom
      data[[2]]->data
      msg3<-.dico[["ask_first_categorical_set"]]
      if(choix==.dico[["txt_chi_independance"]]) multiple<-T else multiple<-F
      X<-.var.type(X=X, info=info, data=data, type="factor", check.prod=F, message=msg3,  multiple=multiple, title=.dico[["txt_variables"]], out=NULL)
      if(is.null(X)) {
        chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
        return(Resultats)}
      X$data->data
      X$X->X

      if(choix!=.dico[["txt_chi_adjustement"]]){
        msg4<-.dico[["ask_second_categorical_set"]]
        Y<-.var.type(X=Y, info=info, data=data, type="factor", check.prod=F, message=msg4,  multiple=multiple, title=.dico[["txt_variables"]], out=NULL)
        if(is.null(Y)) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        Y$data->data
        Y$X->Y
        if(choix==.dico[["txt_mcnemar_test"]] & any(sapply(data[,c(X,Y)],nlevels)!=2)) {
          msgBox(.dico[["desc_mcnemar_need_2x2_table_yours_are_different"]])
          print(table(data[,X], data[,Y], dnn=c(X,Y)))
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)
        }
      }

      if(dial){
        if(info==T) writeLines(.dico[["ask_ponderate_analysis_by_a_sample_var"]])
        Effectifs<-dlgList(c(.dico[["txt_yes"]], .dico[["txt_no"]]), multiple = F, preselect=.dico[["txt_no"]], title=.dico[["ask_specify_sample"]])$res
        if(length(Effectifs)==0) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        if(Effectifs==.dico[["txt_no"]]) Effectifs<-NULL}

      if(!is.null(Effectifs)){
        msg5<-.dico[["ask_chose_sample_variables"]]
        .var.type(X=Effectifs, info=T, data=data, type="integer", message=msg5,multiple=F, title=.dico[["ask_specify_sample_variable"]], out=c(X, Y))->Effectifs
        if(is.null(Effectifs)) {
          chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
          return(Resultats)}
        Effectifs$X->Effectifs
      }

      # check variable
      if(!is.null(Effectifs)) sum(data[,Effectifs])->tot else length(data[,1])->tot
      if(choix!=.dico[["txt_chi_adjustement"]]) {
        expand.grid(X, Y)->comb
        comb[which(as.vector(comb[,1])!=as.vector(comb[,2])),]->comb
        if(any(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)){
          which(apply(comb, 1, function(x) prod(sapply(data[,x],nlevels)))>tot)->trop
          for(i in length(trop):1){
            msg6<-paste0(.dico[["desc_insufficient_sample_for_combinations_between"]], comb[trop[i],1], .dico[["desc_and_variable_y"]], comb[trop[i],2], .dico[["desc_this_analysis_will_not_be_performed"]])
            msgBox(msg6)
            comb[ -which(dimnames(comb)[[1]]==names(trop)[i]),]->comb
          }
          if(length(comb[,1])==0) {
            msgBox(.dico[["desc_no_analysis_can_be_performed_given_your_data"]])
            return(NULL)
          }
        }
      }

      if(choix==.dico[["txt_chi_adjustement"]]) {
        if(dial==F & is.null(p)) rep(1/nlevels(data[,X]),times=nlevels(data[,X]))->p
        if(sum(p)!=1 | any(p)>1 | any(p)<0) p<-NULL

        while(is.null(p)){
          if(info==T) writeLines(.dico[["ask_probabilities_for_modalities"]])
          dlgForm(setNames(as.list(rep(1/nlevels(data[,X]),times=nlevels(data[,X]))), levels(data[,X])), .dico[["desc_probabilities_vector_please_no_fraction"]])$res->niveaux
          stack(niveaux)[,1]->p
          if(sum(p)!=1 ||length(p)!=nlevels(data[,X]) | any(p)>1 | any(p)<0){
            if( dlgMessage(.dico[["desc_proba_sum_is_not_one_or_not_enough_proba"]],"okcancel")$res=="cancel") {
              chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
              return(Resultats)} else return(NULL)
        }
        }
        }
      if(choix==.dico[["txt_mcnemar_test"]]) robust<-F else robust<-T
      if(choix==.dico[["txt_chi_adjustement"]]) Bayes<-F else Bayes<-T
      msg.options<-.dico[["desc_in_that_case_non_parametric_is_classical_chi_squared"]]
      .ez.options(options='choix', n.boot=n.boot,param=F, non.param=T, robust=robust, Bayes=Bayes, msg.options1=NULL, msg.options2=msg.options, info=T, dial=dial, choix=choix2)->Options
      if(is.null(Options)){  chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
        return(Resultats)}
      if(dial==T || any(SampleType %in% c("poisson", "jointMulti","hypergeom", "indepMulti"))==F || SampleType=="indepMulti" & any(fixedMargin %in% c("rows","cols"))==F){

        if(any(Options$choix==.dico[["txt_bayesian_factors"]]) && choix== .dico[["txt_chi_independance"]] ){
          if(info==T) {
            writeLines(.dico[["ask_sampling_type"]])
            cat(.dico[["desc_if_non_fixed_sample_poisson_law"]])
            print(matrix(c(100,50,200,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))

            writeLines(.dico[["desc_distribution_is_joint_multinomial"]])
            print(matrix(c(100,100,100,100), nrow=2, ncol=2, dimnames=list(c("A.1", "A.2"), c("B.1", "B.2")) ))

            writeLines(.dico[["desc_distribution_is_independant_multinomial"]])
            print(matrix(c(15,40,55, 85,60,145, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
            writeLines(.dico[["desc_identical_option_total_sample"]])
            writeLines(.dico[["desc_distribution_is_hypergeometric_when"]])
            print(matrix(c(15,85,100, 85,15,100, 100,100,200), nrow=3, ncol=3, dimnames=list(c("A.1", "A.2", "total"), c("B.1", "B.2", "total")) ))
          }
          SampleType<-c()
          FM<-c()
          for(i in 1:length(comb[,1])){

            if(nlevels(data[,as.character(comb[i,1])])==2 && nlevels(data[,as.character(comb[i,2])])==2) possible<- c(.dico[["txt_poisson_total_not_fixed_sample"]], .dico[["txt_jointmulti_total_fixed_sample"]],
                                                                                                                      paste(.dico[["txt_indepmulti_total_fixed_rows_cols"]], comb[i,1]),
                                                                                                                      paste(.dico[["txt_indepmulti_fixed_sample_rows_cols"]], comb[i,2]),
                                                                                                                      .dico[["txt_hypergeom_total_sample_fixed_rows_cols"]]) else {
                                                                                                                        possible<- c(.dico[["txt_poisson_total_not_fixed_sample"]], .dico[["txt_jointmulti_total_fixed_sample"]],
                                                                                                                                     paste(.dico[["txt_indepmulti_total_fixed_rows_cols"]], comb[i,1]),
                                                                                                                                     paste(.dico[["txt_indepmulti_fixed_sample_rows_cols"]], comb[i,2]))
                                                                                                                      }
            SampleType1<-dlgList(possible, preselect=.dico[["txt_total_sample_not_fixed"]], multiple = FALSE, title=paste(.dico[["txt_experimental_pan_between"]], comb[i,1], .dico[["desc_and"]],comb[i,2], "?"))$res
            if(length(SampleType1)==0) {chi.in(X=NULL, Y=NULL, Effectifs=NULL, p=NULL, choix=NULL, data=NULL, info=T, n.boot=NULL, SampleType=NULL, FM=NULL, choix2=NULL)->Resultats
              return(Resultats)}
            ifelse(SampleType1 == paste(.dico[["txt_indepmulti_total_fixed_rows_cols"]], comb[i,1]), fixedMargin<-"rows",
                   ifelse(SampleType1 == paste(.dico[["txt_indepmulti_fixed_sample_rows_cols"]], comb[i,2]), fixedMargin<-"cols", fixedMargin<-0))
            FM<-c(FM,fixedMargin )
            #ST<- switch(SampleType1, txt_poisson_total_not_fixed_sample= "poisson",
            #            txt_jointmulti_total_fixed_sample="jointMulti",
            #            "hypergeom -  Effectif total fixe pour les lignes et les colonnes"= "hypergeom")
	    if (SampleType1==.dico[["txt_poisson_total_not_fixed_sample"]]) "poisson"->ST
	    if (SampleType1==.dico[["txt_jointmulti_total_fixed_sample"]]) "jointMulti"->ST
	    if (SampleType1==.dico[["txt_hypergeom_total_sample_fixed_rows_cols"]]) "hypergeom"->ST
            if(SampleType1==paste(.dico[["txt_indepmulti_total_fixed_rows_cols"]], comb[i,1])) ST<-"indepMulti"
            if(SampleType1==paste(.dico[["txt_indepmulti_fixed_sample_rows_cols"]], comb[i,2])) ST<-"indepMulti"
            SampleType<-c(SampleType, ST)
          }

        }
      }

      list()->Resultats
      Resultats$analyse<-choix
      Resultats$data<-data
      Resultats$nom.data<-nom
      if(choix==.dico[["txt_chi_adjustement"]]) Resultats$Variables<-X else Resultats$Variables<-comb
      Resultats$Effectifs<-Effectifs
      Resultats$p<-p
      Resultats$choix<-Options$choix
      Resultats$n.boot<-Options$n.boot
      Resultats$SampleType<-SampleType
      Resultats$fixedMargin<-FM
      return(Resultats)
      }
    Cramer<-function(chi.r){
      x<-chi.r$statistic
      n<-sum(chi.r$expected)
      dims<-dim(chi.r$expected)
      V<-round((x/((min(dims)-1)*n))^0.5,3)
      V.sq<-round(V^2,3)
      resultats<-data.frame("V"=V, "V.carre"=V.sq)
      names(resultats) <-c("V", .dico[["txt_cramer_v_square"]])
      return(resultats)}
    chi.out<-function(data=NULL, X=NULL, Y=NULL, p=NULL, choix=NULL, Effectifs=NULL, n.boot=NULL, SampleType=NULL,
                      fixedMargin=NULL, choix2=NULL, rscale=2^0.5/2,priorConcentration=1){
      Resultats<-list()
      if(choix==.dico[["txt_chi_adjustement"]]){
        if(!is.null(Effectifs)){
          tapply(data[,Effectifs], data[,X],sum,na.rm=TRUE)->tab
          rbind(tab,p, p*sum(data[,Effectifs]))->Distribution} else {
            table(data[,X])->tab
            rbind(tab, p, sum(tab)*p)->Distribution}
        dimnames(Distribution)[[1]]<-c(.dico[["txt_observed"]], .dico[["txt_probabilities"]],.dico[["txt_expected"]])
        Resultats[[.dico[["txt_synthesis_table"]]]]<-Distribution
        chi<-chisq.test(tab, p=p, B=n.boot)
        Resultats[[.dico[["txt_chi_dot_squared_adjustment"]]]]<-data.frame(chi.deux=round(chi$statistic,3), ddl=chi$parameter)
        names(Resultats[[.dico[["txt_chi_dot_squared_adjustment"]]]])<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]])
        if(any(choix2== .dico[["txt_non_parametric_test"]])) Resultats[[.dico[["txt_chi_dot_squared_adjustment"]]]][[.dico[["txt_p_dot_val"]]]]<-round(chi$p.value,4)
        if(!is.null(n.boot) && n.boot>1){
          Resultats[[.dico[["txt_chi_dot_squared_adjustment"]]]][[.dico[["txt_p_estimation_with_monter_carlo"]]]]<-round(chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value,4)}

      }
      if((choix!=.dico[["txt_chi_adjustement"]])){
        if (is.null(Effectifs)) tab<-table(data[,X],data[ ,Y], dnn=c(X, Y))else {
          tab<-tapply(data[,Effectifs],list(data[,X],data[,Y]),sum,na.rm=TRUE)
          tab[is.na(tab)] <- 0
          as.table(tab)->tab
          names(attributes(tab)$dimnames)<-c(X,Y)
        }
        # graphique

  if(is.null(Effectifs)) {
    V1<-data[,X]
	  V2<-data[,Y]
    }else{
      data<-data[,c(X,Y,Effectifs )]
      data<-data[rep(1:nrow(data),times = data[,Effectifs]),1:2]
      V1<-data[,X]
	    V2<-data[,Y]
    }
	Resultats[["txt_mosaic_plot"]]<-ggplot() +
  			geom_mosaic(aes(x = product(V1, V2), fill=V1), na.rm=TRUE) +
  			labs(x=X, y=Y, fill=X )+
  			theme_mosaic()

        Resultats[[.dico[["txt_observed_sample"]]]]<-table.margins(tab)

        if(choix==.dico[["txt_chi_independance"]]){
          mon.chi<-chisq.test(tab, B=n.boot, correct=F)
          Resultats[[.dico[["txt_expected_sample"]]]]<-mon.chi$expected
          if(any(choix2 %in% c(.dico[["txt_non_parametric_test"]],.dico[["txt_robusts_tests_with_bootstraps"]])))    {
            #SY<-data.frame( txt_chi_dot_squared=round(mon.chi$statistic,4),
            #                txt_df=mon.chi$parameter, Cramer(mon.chi))
            # names(SY)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], "V.Cramer",  'V.2')
		  SY<-data.frame( txt_chi_dot_squared=round(mon.chi$statistic,4),
                            txt_df=mon.chi$parameter)
		SY<-cbind(SY, Cramer(mon.chi))		 
            if(any(choix2==.dico[["txt_non_parametric_test"]])) SY[[.dico[["txt_p_dot_val"]]]]<-round(mon.chi$p.value,4)
            try(fisher.test(tab),silent=T)->fisher
            if(class(fisher)!='try-error') SY$Fisher.Exact.Test=round(fisher$p.value,4)
            if(all(dim(tab)==2)){
              mon.chi<-chisq.test(tab, B=n.boot, correct=T)
              AY<-data.frame(txt_chi_dot_squared=round(mon.chi$statistic,4),txt_df=mon.chi$parameter,   Cramer(mon.chi),valeur.p=round(mon.chi$p.value,4) ,Fisher.Exact.Test="" )
              names(AY)<-c(.dico[["txt_chi_dot_squared"]],.dico[["txt_df"]],"V.Cramer",  V.sq, .dico[["txt_p_dot_val"]],"Fisher.Exact.Test")
              if(any(choix2==.dico[["txt_non_parametric_test"]])) AY[[.dico[["txt_p_dot_val"]]]]<-round(mon.chi$p.value,4)
              SY<-rbind(SY, AY)
              dimnames(SY)[[1]]<-c(.dico[["txt_without_yates_correction"]], .dico[["txt_with_yates_correction"]])
            } else dimnames(SY)[[1]][1]<-c(.dico[["txt_without_yates_correction"]])
            if(!is.null(n.boot) && n.boot>1){
              SY[[.dico[["txt_p_value_with_monte_carlo"]]]]<-chisq.test(tab, B=n.boot, simulate.p.value=T, correct=F)$p.value
            }
            Resultats[[.dico[["txt_principal_analysis"]]]]<-SY
            # Rapport de vraisemblance
            RV<-2* sum(mon.chi$observed[which(mon.chi$observed!=0)] *
                         log(mon.chi$observed[which(mon.chi$observed!=0)]/mon.chi$expected[which(mon.chi$observed!=0)],base=exp(1)))
            PRV<-pchisq(RV, mon.chi$parameter, ncp = 0, lower.tail = F, log.p = FALSE)
            p<-mon.chi$observed/sum(mon.chi$observed)
            q<-mon.chi$expected/sum(mon.chi$expected)
            RVES<-(-1/(log(min(q[which(p!=0)]), base=exp(1)))) *sum(p *log(p[which(p!=0)]/q[which(p!=0)], base=exp(1))) # ES from JOHNSTON et al. 2006
            RV<-data.frame(txt_chi_dot_squared=RV, txt_df=mon.chi$parameter, txt_p_dot_val=round(PRV,4), txt_effect_size_dot=round(RVES,4))
            names(RV)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], .dico[["txt_p_dot_val"]], .dico[["txt_effect_size_dot"]])
            Resultats[[.dico[["txt_likelihood_ratio_g_test"]]]]<-RV
          }
          # facteur bayesien
          if(any(choix2==.dico[["txt_bayesian_factors"]])) {
            if(!is.null(fixedMargin) && fixedMargin==0) fixedMargin<-NULL
            bf<-contingencyTableBF(tab, sampleType = SampleType, fixedMargin = fixedMargin, priorConcentration=priorConcentration)
            bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",round(extractBF(bf, onlybf=T),4)))
            bf<-data.frame(txt_bayesian_factor=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)),SampleType))
	    names(bf) <- c(.dico[["txt_bayesian_factor"]])
            dimnames(bf)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]], .dico[["txt_type"]])
            Resultats[[.dico[["txt_bayesian_factor"]]]]<-bf
          }

          # Odd ratio
          as.matrix(tab)->tab
          if(all(dim(tab)>2) |any(mon.chi$observed==0)) {
            .dico[["desc_odd_ratio_cannot_be_computed"]]->Resultats[[.dico[["txt_odd_ratio"]]]]
          }else{
            if(length(tab[1,])>2) tab<-apply(tab,1, rev)
            Resultats[[.dico[["txt_odd_ratio"]]]]<- oddsratio.wald(x=tab,conf.level = 0.95,rev = c("neither"),correction = FALSE,verbose = FALSE)$measure
          }
          if(any(choix2 %in% c(.dico[["txt_non_parametric_test"]],.dico[["txt_robusts_tests_with_bootstraps"]])))      {
            if(is.null(SY[[.dico[["txt_p_value_with_monte_carlo"]]]])) p<-SY[[.dico[["txt_p_dot_val"]]]] else p<-SY[[.dico[["txt_p_value_with_monte_carlo"]]]]
            if(any(p<0.05))  {
              round(mon.chi$residuals,3)->Resultats[[.dico[["txt_residue"]]]]
              round((mon.chi$observed-mon.chi$expected)/(mon.chi$expected^0.5),3)->Resultats[[.dico[["txt_residue_standardized"]]]]
              round(mon.chi$stdres,3)->Resultats[[.dico[["txt_residue_standardized_adjusted"]]]]
              p.adjust(2*(1-pnorm(abs(Resultats[[.dico[["txt_residue_standardized_adjusted"]]]]))), method="holm")->valeur.p
              matrix(valeur.p, nrow=nrow(tab))->valeur.p
              dimnames(tab)->dimnames(valeur.p)
              round(valeur.p,4)->Resultats[[.dico[["txt_residues_significativity_holm_correction"]]]]
            }
          }
          round(table.margins(prop.table(mon.chi$observed))*100,1)->Resultats[[.dico[["txt_percentage_total"]]]]
          round(sweep(addmargins(mon.chi$observed, 1, list(list(All = sum, N = function(x) sum(x)^2/100))), 2,apply(mon.chi$observed, 2, sum)/100, "/"), 1)->Resultats[[.dico[["txt_percentage_col"]]]]
          round(sweep(addmargins(mon.chi$observed, 2, list(list(All = sum, N = function(x) sum(x)^2/100))), 1,apply(mon.chi$observed, 1, sum)/100, "/"), 1)->Resultats[[.dico[["txt_percentage_row"]]]]

        }
        if(choix==.dico[["txt_mcnemar_test"]]){
          if(any(choix2== .dico[["txt_non_parametric_test"]]))    {
            MCN<-mcnemar.test(tab, correct=F)
            MCN<-data.frame(txt_chi_dot_squared=round(MCN$statistic,3), txt_df=MCN$parameter, txt_p_dot_val= round(MCN$p.value,4))
            names(MCN)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], .dico[["txt_p_dot_val"]])
            MCN2<-mcnemar.test(tab, correct=T)
            MCN2<-data.frame(txt_chi_dot_squared=round(MCN2$statistic,3), txt_df=MCN2$parameter, txt_p_dot_val= round(MCN2$p.value,4))
            names(MCN2)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], .dico[["txt_p_dot_val"]])
            MCN<-rbind(MCN, MCN2)
            dimnames(MCN)[[1]]<-c(.dico[["txt_mcnemar_test_without_yates_correction"]], .dico[["txt_mcnemar_test_with_continuity_correction"]] )
            MCN->Resultats[[.dico[["txt_mcnemar_test_with_yates_correction"]]]] # test de McNemar
          }
          if(any(choix2==.dico[["txt_bayesian_factors"]])) {
            bf<-proportionBF(y=tab[1,2], tab[1,2]+tab[2,1], p=0.5,rscale=rscale)
            erreur<-bf@numerator[[1]]@analysis$properror
            erreur<-ifelse(erreur<.0001, "<0.0001", erreur)
            bf<-ifelse(extractBF(bf, onlybf=T)>1000, ">1000", ifelse(extractBF(bf, onlybf=T)<.001, "<0.001",extractBF(bf, onlybf=T)))
            samples =proportionBF(y = tab[1,2], N = tab[1,2]+tab[2,1], p = .5, posterior = TRUE, iterations = 10000)
            plot(samples[,"p"])
            bf<-data.frame(txt_bayesian_factor=c(bf, ifelse(class(bf)=="character", "<0.001", round(1/bf,4)), erreur, rscale))
            names(bf)<-c(.dico[["txt_bayesian_factor"]])
            dimnames(bf)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]], .dico[["txt_error"]], "rscale")
            Resultats[[.dico[["txt_bayesian_factors"]]]]<-bf
          }

          if( all(dimnames(tab)[[1]]==dimnames(tab)[[2]])) {
		  Resultats$Avertissement<- .dico[["desc_cells_for_mcnemar"]]
	  } else {
		  Resultats$Avertissement<-.dico[["ask_mcnemar_repeated_measure"]]
	  }
	}

      }
      return(Resultats)
    }

    c('svDialogs', 'epitools', 'BayesFactor', 'ggplot2','ggmosaic')->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    .e <- environment()
    Resultats<-list()


    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data

    chi.in(X=X, Y=Y, Effectifs=Effectifs,p=p, choix=choix, data=data, info=info, n.boot=n.boot, SampleType=SampleType, FM=fixedMargin, choix2=choix2)->chi.options
    if(is.null(chi.options)) return(analyse())
    if(chi.options$analyse!=.dico[["txt_chi_adjustement"]]){
      try( windows(record=T), silent=T)->win
      if(class(win)=='try-error') quartz()
    }


    if(class(chi.options$Variables)=="data.frame") {
      X<- chi.options$Variables[,1]
      Y<- chi.options$Variables[,2]
    } else {X<-chi.options$Variables
    Y<-NULL}

    if(length(X)>1) Resultats[[.dico[["txt_alpha_warning"]]]]<-paste(.dico[["desc_alpha_increased_with_value_equals_to"]], 100*(1-0.95^length(X)), "%", sep=" ")
    for(i in 1:length(X)) {
      as.character(X[i])->Xi
      as.character(Y[i])->Yi
      chi.results<-chi.out(data=chi.options$data, X=Xi, Y=Yi,p=chi.options$p, choix=chi.options$analyse,
                           Effectifs =chi.options$Effectifs, n.boot=chi.options$n.boot, choix2=chi.options$choix,
                           SampleType=chi.options$SampleType[i],  fixedMargin=chi.options$fixedMargin[i], rscale=rscale, priorConcentration =priorConcentration)
      Resultats[[i]]<-chi.results
      if(chi.options$analyse==.dico[["txt_chi_adjustement"]]) nom<-paste(.dico[["desc_chi_squared_adjustment_on_variable_x"]], X, sep =" ")
      if(chi.options$analyse==.dico[["txt_chi_independance"]]) nom<-paste(.dico[["txt_chi_results_between_var_x"]], Xi,
                                                         .dico[["desc_and_variable_y"]], Yi,sep=" ")
      if(chi.options$analyse==.dico[["txt_mcnemar_test"]]) nom<-paste(.dico[["txt_mcnemar_results_between_var_x"]], Xi,
                                                            .dico[["desc_and_variable_y"]], Yi,sep=" ")
      names(Resultats)[i]<-nom
    }

    paste(unique(X), collapse="','", sep="")->X
    if(!is.null(Y)) paste(unique(Y), collapse="','", sep="")->Y
    paste(chi.options$choix, collapse="','", sep="")->choix2
    paste(chi.options$p, collapse=",", sep="")->p
    if(!is.null(chi.options$SampleType)) paste(chi.options$SampleType, collapse="','", sep="")->SampleType
    paste(chi.options$fixedMargin, collapse="','", sep="")->FM
    paste0("chi(X=c('", X,ifelse(!is.null(Y), paste0("'),Y=c('", Y, "')"), "'), Y=NULL"),
           ifelse(is.null(chi.options$Effectifs),",Effectifs=NULL", paste0(",Effectifs='", chi.options$Effectifs, "'")),
           ifelse(!is.null(Y), ", p=NULL", paste0(", p=c(", p,")")),
           ", choix='", chi.options$analyse, "',data=", chi.options$nom.data, ",info=", info, ",n.boot=", ifelse(is.null(chi.options$n.boot), "NULL",chi.options$n.boot) ,
           ",priorConcentration =" ,priorConcentration, ",SampleType=", ifelse(is.null(chi.options$SampleType), 'NULL', paste0("c('",SampleType,"')")),
           ",fixedMargin=", ifelse(is.null(chi.options$fixedMargin), 'NULL', paste0("c('",FM,"')")), ",choix2=c('",choix2,
           "'),rscale=", round(rscale,3), ")")->Resultats$Call
    .add.history(data=chi.options$data, command=Resultats$Call, nom=chi.options$nom)
    .add.result(Resultats=Resultats, name =paste(chi.options$analyse, Sys.time() ))


    ref1(packages)->Resultats[[.dico[["txt_references"]]]]
    ### Obtenir les Resultats
    if(html) try(ez.html(Resultats))
    return(Resultats)
    }
