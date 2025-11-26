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
        if (is.null(Effectifs)) {
          tab<-table(data[,X],data[ ,Y], dnn=c(X, Y)) 
           V1<<-data[,X]
	         V2<<-data[,Y]
          }else {
          tab<-tapply(data[,Effectifs],list(data[,X],data[,Y]),sum,na.rm=TRUE)
          tab[is.na(tab)] <- 0
          tab<-as.table(tab)
          names(attributes(tab)$dimnames)<-c(X,Y)
          data<-data[,c(X,Y,Effectifs )]
          data<-data[rep(1:nrow(data),times = data[,Effectifs]),1:2]
          V1<<-data[,X]
	        V2<<-data[,Y]

        }
        # graphique


	
        Resultats[[.dico[["txt_observed_sample"]]]]<-table.margins(tab)

       local( {
        graph<<-ggplot() +
  			geom_mosaic(aes(x = product(V1, V2), fill=V1), na.rm=TRUE) +
  			labs(x=X, y=Y, fill=X )+
  			theme_mosaic()
      })
      Resultats[[.dico[["txt_mosaic_plot"]]]]<-graph

        if(choix==.dico[["txt_chi_independance"]]){
          mon.chi<-chisq.test(tab, B=n.boot, correct=F)
          Resultats[[.dico[["txt_expected_sample"]]]]<-mon.chi$expected
          if(any(choix2 %in% c(.dico[["txt_non_parametric_test"]],.dico[["txt_robusts_tests_with_bootstraps"]])))    {
            #SY<-data.frame( txt_chi_dot_squared=round(mon.chi$statistic,4),
            #                txt_df=mon.chi$parameter, Cramer(mon.chi))
            # names(SY)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], "V.Cramer",  'V.2')
		        SY<-data.frame( txt_chi_dot_squared=round(mon.chi$statistic,4),
                            txt_df=mon.chi$parameter)
            names(SY)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]])
		        if(any(choix2==.dico[["txt_non_parametric_test"]])) SY[[.dico[["txt_p_dot_val"]]]]<-round(mon.chi$p.value,4)
            try(fisher.test(tab),silent=T)->fisher
            if(class(fisher)!='try-error') SY$Fisher.Exact.Test=round(fisher$p.value,4)
            SY<-cbind(SY, Cramer(mon.chi))		 
            
            if(all(dim(tab)==2)){
              mon.chi<-chisq.test(tab, B=n.boot, correct=T)
              AY<-data.frame(txt_chi_dot_squared=round(mon.chi$statistic,4),txt_df=mon.chi$parameter,
              valeur.p=round(mon.chi$p.value,4) ,Fisher.Exact.Test="" )
              names(AY)<-c(.dico[["txt_chi_dot_squared"]],.dico[["txt_df"]], .dico[["txt_p_dot_val"]],"Fisher.Exact.Test")
              AY<-cbind(AY, Cramer(mon.chi))   
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
            MCN$g.Cohen<-(tab[1,2]/(tab[1,2]+tab[2,1])-0.5)
            MCN2<-mcnemar.test(tab, correct=T)
            MCN2<-data.frame(txt_chi_dot_squared=round(MCN2$statistic,3), txt_df=MCN2$parameter, txt_p_dot_val= round(MCN2$p.value,4))
            names(MCN2)<-c(.dico[["txt_chi_dot_squared"]], .dico[["txt_df"]], .dico[["txt_p_dot_val"]])
            MCN2$g.Cohen<-(tab[1,2]/(tab[1,2]+tab[2,1])-0.5)
            MCN<-rbind(MCN, MCN2)
            dimnames(MCN)[[1]]<-c(.dico[["txt_mcnemar_test_without_yates_correction"]], .dico[["txt_mcnemar_test_with_continuity_correction"]] )
            MCN->Resultats[[.dico[["txt_mcnemar_test"]]]] # test de McNemar
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

prodcalc <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, offset = offset) {

  vars <- parse_product_formula(stats::as.formula(formula))
#browser()
  if (length(vars$wt) == 1) {
    data$.wt <- data[[vars$wt]]
  } else {
    data$.wt <- 1
  }
  margin <- getFromNamespace("margin", "productplots")

  wt <- margin(data, vars$marg, vars$cond)
  wt2 <- margin(data, c(vars$marg, vars$cond)) # getting margins
  #browser()
  #wt$.n <- wt2$.wt

  if (na.rm) {
    wt <- wt[stats::complete.cases(wt), ]
  }


  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- lapply(divider, match.fun)

  max_wt <- if (scale_max) NULL else 1

  df <- divide(wt, divider = rev(divider), cascade = cascade, max_wt = max_wt, offset = offset)
#  browser()
  wt2 <- dplyr::rename(wt2, .n=".wt")
  dplyr::left_join(df, wt2, by = setdiff(names(wt2), ".n"))
}
partd <- function(x) {
  d <- attr(x, "d")
  if (!is.null(d)) d else 1
}

# Convenience function to create bounds
bound <- function(t = 1, r = 1, b = 0, l = 0) {
  data.frame(t = t, r = r, b = b, l = l)
}


divide <- function(data, bounds = bound(), divider = list(productplots::hbar), level = 1, cascade = 0, max_wt = NULL, offset = offset) {
  d <- partd(divider[[1]])
  if (ncol(data) == d + 1) {
    return(divide_once(data, bounds, divider[[1]], level, max_wt, offset))
  }
  # In divide we work with the opposite order of variables to margin -
  # so we flip and then flip back
  margin <- getFromNamespace("margin", "productplots")

  parent_data <- margin(data, rev(seq_len(d)))
  parent_data <- parent_data[, c(rev(seq_len(d)), d + 1)]

  parent <- divide_once(parent_data, bounds, divider[[1]], level, max_wt, offset)
  parentc <- parent
  parentc$l <- parent$l + cascade
  parentc$b <- parent$b + cascade
  parentc$r <- parent$r + cascade
  parentc$t <- parent$t + cascade

  if (is.null(max_wt)) {
    max_wt <- max(margin(data, d + 1, seq_len(d))$.wt, na.rm = TRUE)
  }

#  browser()
#  pieces <- split(data, data[,seq_len(d)]) # this one doesn't deal well with NAs
  pieces <- as.list(getFromNamespace("dlply", asNamespace("plyr"))(data, seq_len(d))) #



  children <- purrr::map_df(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1],
                        level = level + 1, cascade = cascade, max_wt = max_wt, offset = offset)

    labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
    cbind(labels, partition)
  })

  # children <- plyr::ldply(seq_along(pieces), function(i) {
  #   piece <- pieces[[i]]
  #   partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1],
  #                       level = level + 1, cascade = cascade, max_wt = max_wt, offset = offset)
  #
  #   labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
  #   cbind(labels, partition)
  # })
  dplyr::bind_rows(parent, children)
}

# @param data data frame giving partitioning variables and weights.  Final
#   column should be called .wt and contain weights
divide_once <- function(data, bounds, divider, level = 1, max_wt = NULL, offset) {
  d <- partd(divider)
  # Convert into vector/matrix/array for input to divider function
  if (d > 1) {
    data[-ncol(data)] <- lapply(data[-ncol(data)], addNA, ifany = TRUE)
    wt <- tapply(data$.wt, data[-ncol(data)], identity)
    # This ensures that the order of the data matches the order tapply uses
    data <- as.data.frame.table(wt, responseName = ".wt")
  } else {
    wt <- data$.wt
  }

  wt <- wt / sum(wt, na.rm = TRUE)
  if (is.null(max_wt)) max_wt <- max(wt, na.rm = TRUE)

  partition <- divider(wt, bounds, offset, max = max_wt)
  cbind(data, partition, level = level)
}
																
squeeze <- function(pieces, bounds = bound()) {
  scale_x <- function(x) x * (bounds$r - bounds$l) + bounds$l
  scale_y <- function(y) y * (bounds$t - bounds$b) + bounds$b

  pieces$l <- scale_x(pieces$l)
  pieces$r <- scale_x(pieces$r)
  pieces$b <- scale_y(pieces$b)
  pieces$t <- scale_y(pieces$t)
  pieces
}


rotate <- function(data) {
   l <- b <- t <- r <- NULL # visible binding
   dplyr::rename(data, b=l, t=r, l=b, r=t)
 }

#' Spine partition: divide longest dimension.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
spine <- function(data, bounds, offset = offset, max = NULL) {
  w <- bounds$r - bounds$l
  h <- bounds$t - bounds$b

  if (w > h) {
    hspine(data, bounds, offset, max)
  } else {
    vspine(data, bounds, offset, max)
  }
}


#' Horizontal spine partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hspine <- function(data, bounds, offset = offset, max = NULL) {
  n <- length(data)
  # n + 1 offsets

  if (ncol(bounds)>4)  offsets <- ((c(0, rep(1, n - 1), 0) * offset))/sqrt((bounds$level+.1))
  else offsets <- (c(0, rep(1, n - 1), 0) * offset)

  data <- data * (1 - sum(offsets))

  widths <- as.vector(t(cbind(data, offsets[-1])))
  widths[is.na(widths)] <- 0

  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = 1
  )
  squeeze(locations, bounds)
}

#' Vertical spine partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vspine <- function(data, bounds, offset = offset, max = NULL) {
  rotate(hspine(data, rotate(bounds), offset, max = max))
}

#' Horizontal bar partition: width constant, height varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
hbar <- function(data, bounds, offset = 0.02, max = NULL) {
  if (is.null(max)) max <- 1

  n <- length(data)
  # n + 1 offsets
  offsets <- c(0, rep(1, n - 1), 0) * offset

  width <- (1 - sum(offsets)) / n
  heights <- data / max

  widths <- as.vector(t(cbind(width, offsets[-1])))
  pos <- c(offsets[1], cumsum(widths)) / sum(widths)
  locations <- data.frame(
    l = pos[seq(1, 2 * n - 1, by = 2)],
    r = pos[seq(2, 2 * n, by = 2)],
    b = 0,
    t = heights
  )
  squeeze(locations, bounds)
}

#' Vertical bar partition: height constant, width varies.
#'
#' @param data bounds data frame
#' @param bounds bounds of space to partition
#' @param offset space between spines
#' @param max maximum value
#' @export
vbar <- function(data, bounds, offset = 0.02, max = NULL) {
 rotate(hbar(data, rotate(bounds), offset, max = max))
}



.directions <- c("vertical", "horizontal")

#' Template for a mosaic plot.
#' A mosaic plot is composed of spines in alternating directions.
#'
#' @param direction direction of first split
#' @export
mosaic <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) rev(rep(splits, length = n))
}


#' Template for a double decker plot.
#' A double decker plot is composed of a sequence of spines in the same
#' direction, with the final spine in the opposite direction.
#'
#' @param direction direction of first split
#' @export
ddecker <- function(direction = "h") {
  direction <- match.arg(direction, .directions)
  if (direction == "horizontal") {
    splits <- c("hspine", "vspine")
  } else {
    splits <- c("vspine", "hspine")
  }

  function(n) c(splits[2], rep(splits[1], length = n - 1))
}


																geom_mosaic_jitter <- function(mapping = NULL, data = NULL, stat = "mosaic_jitter",
                               position = "identity", na.rm = FALSE,  divider = mosaic(),
                               offset = 0.01, drop_level = FALSE, seed = NA,
                               show.legend = NA, inherit.aes = FALSE, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

    #browser()

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    aes_x <- rlang::eval_tidy(mapping$x)
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }

  aes_colour <- mapping$colour
  var_colour <- ""
  if (!is.null(aes_colour)) {
    aes_colour <- rlang::quo_text(mapping$colour)
    var_colour <- paste0("x__colour__", aes_colour)
    if (aes_colour %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_colour)
      var_x[idx] <- var_colour
    } else {
      mapping[[var_colour]] <- mapping$colour
    }
  }


  #  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")

    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }


  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    aes_conds <- rlang::eval_tidy(mapping$conds)
    mapping$conds <- structure(1L, class = "productlist")
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMosaicJitter,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      drop_level = drop_level,
      seed = seed,
      ...
    )
  )
}

#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate select
GeomMosaicJitter <- ggplot2::ggproto(
  "GeomMosaicJitter", ggplot2::Geom,
  setup_data = function(data, params) {
    #cat("setup_data in GeomMosaic\n")
    #browser()
    data
  },
  # required_aes = c("xmin", "xmax", "ymin", "ymax"),
  # default_aes = ggplot2::aes(width = 0.1, linetype = "solid", fontsize=5,
  #                            shape = 19, colour = NA,
  #                            size = 1, fill = "grey30", alpha = 1, stroke = 0.1,
  #                            linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes =  ggplot2::aes(
    shape = 19, colour = "grey30", size = 1, fill = NA,
    alpha = NA, stroke = 1, linewidth=.1, weight = 1
  ),

  draw_panel = function(data, panel_scales, coord) {
    #cat("draw_panel in GeomMosaic\n")
    # browser()
    # if (all(is.na(data$colour)))
    #   data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    # adjust the point placement for the size of the points.
    # .pt is defined in ggplot2 as 72.27 / 25.4
    dx <- grid::convertX(unit(.pt, "points"), "npc", valueOnly = TRUE)
    dy <- grid::convertY(unit(.pt, "points"), "npc", valueOnly = TRUE)
    # check out stroke and .stroke
    # mapping shape?
    #browser()
    # scale x and y coordinates to the correct place between (xmin+dx, xmax-dx) and
    # (ymin+dy, ymax-dy)
    scale_01_to_xy <- function(value, min_val, max_val) {
      # assumes that value is between 0 and 1
      value*(max_val-min_val) + min_val
    }
    data <- mutate(data,
      # could give some bit of space between any outline of a point and the
      # end of the interval
      x = scale_01_to_xy(x, xmin+1*(size)*dx, xmax-1*(size)*dx),
      y = scale_01_to_xy(y, ymin+1*(size)*dy, ymax-1*(size)*dy)
    )

    # points <- tidyr::unnest(points, coords)

    # sub$fill <- NA
    # sub$size <- sub$size/10

      ggplot2:::ggname("geom_mosaic_jitter", grobTree(
      #GeomRect$draw_panel(sub, panel_scales, coord),
      GeomPoint$draw_panel(data, panel_scales, coord)
    ))
  },

  check_aesthetics = function(x, n) {
    #browser()
    ns <- vapply(x, length, numeric(1))
    good <- ns == 1L | ns == n


    if (all(good)) {
      return()
    }

    stop(
      "Aesthetics must be either length 1 or the same as the data (", n, "): ",
      paste(names(!good), collapse = ", "),
      call. = FALSE
    )
  },

  draw_key = ggplot2::draw_key_point
)

																
#' Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plot is a convenient graphical summary of the conditional distributions
#' in a contingency table and is composed of spines in alternating directions.
#'
#'
#' @inheritParams ggplot2::layer
#' @param divider Divider function. The default divider function is mosaic() which will use spines in alternating directions. The four options for partitioning:
#' \itemize{
#' \item \code{vspine} Vertical spine partition: width constant, height varies.
#' \item \code{hspine}  Horizontal spine partition: height constant, width varies.
#' \item \code{vbar} Vertical bar partition: height constant, width varies.
#' \item \code{hbar}  Horizontal bar partition: width constant, height varies.
#' }
#' @param offset Set the space between the first spine
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#'
#' data(titanic)
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class), fill = Survived))
#' # good practice: use the 'dependent' variable (or most important variable)
#' # as fill variable
#'
#' # if there is only one variable inside `product()`,
#' # `product()` can be omitted
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = Class, fill = Survived))
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Age), fill = Survived))
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class), conds = product(Age), fill = Survived))
#'
#' # if there is only one variable inside `product()`,
#' # `product()` can be omitted
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = Class, conds = Age, fill = Survived))
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Survived, Class), fill = Age))
#'
#' # Just excluded for timing. Examples are included in testing to make sure they work
#' \dontrun{
#' data(happy)
#'
#' ggplot(data = happy) + geom_mosaic(aes(x = product(happy)), divider="hbar")
#'
#' ggplot(data = happy) + geom_mosaic(aes(x = product(happy))) +
#'   coord_flip()
#'
#' # weighting is important
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(happy)))
#'
#' ggplot(data = happy) + geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy)) +
#'   theme(axis.text.x=element_text(angle=35))
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health, sex, degree), fill=happy),
#'   na.rm=TRUE)
#'
#' # here is where a bit more control over the spacing of the bars is helpful:
#' # set labels manually:
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
#'   scale_x_productlist("Age", labels=c(17+1:72))
#'
#' # thin out labels manually:
#' labels <- c(17+1:72)
#' labels[labels %% 5 != 0] <- ""
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset=0) +
#'   scale_x_productlist("Age", labels=labels)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy, conds = product(sex)),
#'   divider=mosaic("v"), na.rm=TRUE, offset=0.001) +
#'   scale_x_productlist("Age", labels=labels)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(age), fill=happy), na.rm=TRUE, offset = 0) +
#'   facet_grid(sex~.) +
#'   scale_x_productlist("Age", labels=labels)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)),
#'   divider=mosaic("h"))
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight = wtssall, x = product(happy, finrela, health)), offset=.005)
#'
#' # Spine example
#' ggplot(data = happy) +
#'  geom_mosaic(aes(weight = wtssall, x = product(health), fill = health)) +
#'  facet_grid(happy~.)
#' } # end of don't run

geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                        show.legend = NA, inherit.aes = FALSE, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

  # browser()

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    if (grepl("product", rlang::quo_text(mapping$x))) {
      aes_x <- rlang::eval_tidy(mapping$x)
    } else aes_x <- list(rlang::quo_get_expr(mapping$x))
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }

  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")
    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }

  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    if (grepl("product", rlang::quo_text(mapping$conds))) {
      aes_conds <- rlang::eval_tidy(mapping$conds)
    } else aes_conds <- list(rlang::quo_get_expr(mapping$conds))
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))

    mapping$conds <- structure(1L, class = "productlist")
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMosaic,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}

#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
GeomMosaic <- ggplot2::ggproto(
  "GeomMosaic", ggplot2::Geom,
  setup_data = function(data, params) {
    #cat("setup_data in GeomMosaic\n")
    #browser()
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                             shape = 19, colour = NA,
                             size = .1, fill = "grey30", alpha = .8, stroke = 0.1,
                             linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),

  draw_panel = function(data, panel_scales, coord) {
    #cat("draw_panel in GeomMosaic\n")
    #browser()
    if (all(is.na(data$colour)))
      data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  },

  check_aesthetics = function(x, n) {
    #browser()
    ns <- vapply(x, length, numeric(1))
    good <- ns == 1L | ns == n


    if (all(good)) {
      return()
    }

    stop(
      "Aesthetics must be either length 1 or the same as the data (", n, "): ",
      paste(names(!good), collapse = ", "),
      call. = FALSE
    )
  },

  draw_key = ggplot2::draw_key_polygon
)	

																squeeze <- getFromNamespace("squeeze", "productplots")

# #' Internal helper function
# #'
# #' function copied directly from the productplots package
# #' @param table data table
# #' @param marginals variables used in margins of mosaic specification
# #' @param conditionals variables conditioned upon in mosaic specification
# #' @return weighted data table
# #' @author Hadley Wickham
# margin <- function(table, marginals = c(), conditionals = c()) {
#   if (is.numeric(marginals))    marginals    <- names(table)[marginals]
#   if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]
#
#   marginals <- rev(marginals)
#   conditionals <- rev(conditionals)
#
#   weighted.table <- getFromNamespace("weighted.table", "productplots")
#
#
#   marg <- weighted.table(table[c(conditionals, marginals)], table$.wt)
#
#   if (length(conditionals) > 0) {
#     # Work around bug in ninteraction
#     cond <- marg[conditionals]
#     cond[] <- lapply(cond, addNA, ifany = TRUE)
#     marg$.wt <- stats::ave(marg$.wt, id(cond), FUN = function(x) x / sum(x, na.rm=TRUE))
#   }
#
#   marg$.wt[is.na(marg$.wt)] <- 0
#   marg
# }

# #' Internal helper function
# #'
# #' function copied directly from the productplots package
# #' @param vars data frame
# #' @param wt vector of weights
# #' @author Hadley Wickham
# weighted.table <- function(vars, wt = NULL) {
#   # If no weight column, give constant weight
#   if (is.null(wt)) {
#     wt <- rep(1, nrow(vars))
#     wt <- wt/sum(wt, na.rm= TRUE)
#   }
#
#   # Ensure missing values are counted
#   vars[] <- lapply(vars, addNA, ifany = TRUE)
#
#   # Need to reverse order of variables because as.data.frame works in the
#   # opposite way to what I want
#   sums <- tapply(wt, rev(vars), sum, na.rm = TRUE)
#
#   df <- as.data.frame.table(sums, responseName = ".wt")
#   # Missing values represent missing combinations in the original dataset,
#   # i.e. they have zero weight
#   df$.wt[is.na(df$.wt)] <- 0
#   df[, c(rev(seq_len(ncol(df) - 1)), ncol(df)) ]
# }
																
																
	#' Helper function for determining scales
#'
#' Used internally to determine class of variable x
#' @param x variable
#' @return character string "productlist"
#' @importFrom ggplot2 scale_type
#' @export
scale_type.productlist <- function(x) {
  #  cat("checking for type productlist\n")
  #browser()
  "productlist"
}




#' Determining scales for mosaics
#'
#' @param name set to pseudo waiver function `product_names` by default.
#' @inheritParams ggplot2::continuous_scale
#' @export
scale_x_productlist <- function(name = ggplot2::waiver(), breaks = product_breaks(),
                                minor_breaks = NULL, labels = product_labels(),
                                limits = NULL, expand = ggplot2::waiver(), oob = scales::censor,
                                na.value = NA_real_, transform = "identity",
                                position = "bottom", sec.axis = ggplot2::waiver()) {
  #browser()
  sc <- ggplot2::continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    palette = identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, transform = transform,
    guide = ggplot2::waiver(), position = position, super = ScaleContinuousProduct
  )

#browser()
  # if (!ggplot2::waiver(sec.axis)) {
  #   if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
  #   is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
  #   if (is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
  #   sc$secondary.axis <- sec.axis
  # }
  sc
}

#' @rdname scale_x_productlist
#' @param sec.axis specify a secondary axis
#' @export
scale_y_productlist <- function(name = ggplot2::waiver(), breaks = product_breaks(),
                                minor_breaks = NULL, labels = product_labels(),
                                limits = NULL, expand = ggplot2::waiver(), oob = scales::censor,
                                na.value = NA_real_, transform = "identity",
                                position = "left", sec.axis = ggplot2::waiver()) {
  #browser()
  sc <- ggplot2::continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "ylower", "ymiddle", "yupper"),
    palette = identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, transform = transform,
    guide = ggplot2::waiver(), position = position, super = ScaleContinuousProduct
  )

  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
    if (is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}


#' @rdname scale_x_productlist
#' @export
ScaleContinuousProduct <- ggplot2::ggproto(
  "ScaleContinuousProduct", ScaleContinuousPosition,
  train =function(self, x) {
    #cat("train in ScaleContinuousProduct\n")
    #cat("class of variable: ")
    #cat(class(x))
    #browser()
    if (is.list(x)) {
      x <- x[[1]]
      if ("Scale" %in% class(x)) {
        #browser()
        # re-assign the scale values now that we have the information - but only if necessary
        if (is.function(self$breaks)) self$breaks <- x$breaks
        if (is.function(self$labels)) self$labels <- x$labels
        if (is.waive(self$name)) {
          self$product_name <- gsub("x__alpha__", "", x$name)
          self$product_name <- gsub("x__fill__", "", self$product_name)
          self$product_name <- gsub("x__", "", self$product_name)
          self$product_name <- gsub("conds\\d__", "", self$product_name)
        }
        #cat("\n")
        return()
      }
    }
    if (is.discrete(x)) {
      self$range$train(x=c(0,1))
      #cat("\n")
      return()
    }
    self$range$train(x)
    #cat("\n")
  },
  map = function(self, x, limits = self$get_limits()) {
    #cat("map in ScaleContinuousProduct\n")
    #browser()
    if (is.discrete(x)) return(x)
    if (is.list(x)) return(0) # need a number
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  dimension = function(self, expand = c(0, 0)) {
    #cat("dimension in ScaleContinuousProduct\n")
    c(-0.05,1.05)
  },
  make_title = function(..., self) {
    title <- ggproto_parent(ScaleContinuousPosition, self)$make_title(...)
    if (isTRUE(title %in% self$aesthetics)) {
      title <- self$product_name
    }
    else title
  }
)		


																#' @rdname geom_mosaic_jitter
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{xmin}{location of bottom left corner}
#' \item{xmax}{location of bottom right corner}
#' \item{ymin}{location of top left corner}
#' \item{ymax}{location of top right corner}
#' }
#' @export
stat_mosaic_jitter <- function(mapping = NULL, data = NULL, geom = "mosaic_jitter",
                               position = "identity", na.rm = FALSE,  divider = mosaic(),
                               show.legend = NA, inherit.aes = TRUE, offset = 0.01,
                               drop_level = FALSE, seed = NA, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    aes_x <- rlang::eval_tidy(mapping$x)
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }


  #  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")

    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }


  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    aes_conds <- rlang::eval_tidy(mapping$conds)
    mapping$conds <- structure(1L, class = "productlist")
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaicJitter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      drop_level = drop_level,
      seed = seed,
      ...
    )
  )
}


#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
StatMosaicJitter <- ggplot2::ggproto(
  "StatMosaicJitter", ggplot2::Stat,
  #required_aes = c("x"),
  non_missing_aes = c("weight", "size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  setup_params = function(data, params) {
    #cat("setup_params from StatMosaic\n")
    #browser()
    # if (!is.null(data$y)) {
    #   stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
    # }
    params
  },

  setup_data = function(data, params) {
    #cat("setup_data from StatMosaic\n")
    #browser()

    data
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, drop_level=FALSE, seed = NA, divider, offset) {
    #cat("compute_panel from StatMosaic\n")
    #browser()

    #    vars <- names(data)[grep("x[0-9]+__", names(data))]
    vars <- names(data)[grep("x__", names(data))]
    conds <- names(data)[grep("conds[0-9]+__", names(data))]


    if (length(vars) == 0) formula <- "1"
    else formula <-  paste(vars, collapse="+")



    formula <- paste("weight~", formula)

    if (length(conds) > 0) formula <- paste(formula, paste(conds, collapse="+"), sep="|")

    df <- data
    if (!in_data(df, "weight")) {
      df$weight <- 1
    }


    res <- prodcalc(df, formula=as.formula(formula),
                    divider = divider, cascade=0, scale_max = TRUE,
                    na.rm = na.rm, offset = offset)

    # browser()

    # consider 2nd weight for points
    if (in_data(df, "weight2")) {
      formula2 <- str_replace(formula, "weight", "weight2")
      res2 <- prodcalc(df, formula = as.formula(formula2), divider = divider,
                       cascade = 0, scale_max = TRUE, na.rm = na.rm, offset = offset)
      res$.n2 <- res2$.n
    }


    # need to set x variable - I'd rather set the scales here.
    prs <- parse_product_formula(as.formula(formula))
    p <- length(c(prs$marg, prs$cond))
    if (is.function(divider)) divider <- divider(p)

    # the level at which things are labelled could be made a parameter.
    # At the moment the deepest level is being labelled.
    dflist <- list(data=subset(res, level==max(res$level)), formula=as.formula(formula), divider=divider)
    scx <- productplots::scale_x_product(dflist)
    scy <- productplots::scale_y_product(dflist)


    # res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    # res <- subset(res, level==max(res$level))

    # export the variables with the data - terrible hack
    # res$x <- list(scale=scx)
    # if (!is.null(scales$y)) {
    #   # only set the y scale if it is a product scale, otherwise leave it alone
    #   if ("ScaleContinuousProduct" %in% class(scales$y))
    #     res$y <- list(scale=scy)
    # }

    # XXXX add label for res
    cols <- c(prs$marg, prs$cond)

    if (length(cols) > 1) {
      df <- res[,cols]
      df <- tidyr::unite(df, "label", cols, sep="\n")

      res$label <- df$label
    } else res$label <- as.character(res[,cols])


    res$x <- list(scale=scx)
    if (!is.null(scales$y)) {
      # only set the y scale if it is a product scale, otherwise leave it alone
      if ("ScaleContinuousProduct" %in% class(scales$y))
        res$y <- list(scale=scy)
    }

    # merge res with data:
    # is there a fill/alpha/color variable?
    fill_idx <- grep("x__fill", names(data))
    if (length(fill_idx) > 0) {
      fill_res_idx <- grep("x__fill", names(res))
      res$fill <- res[[fill_res_idx]]
    }
    alpha_idx <- grep("x__alpha", names(data))
    if (length(alpha_idx) > 0) {
      alpha_res_idx <- grep("x__alpha", names(res))
      res$alpha <- res[[alpha_res_idx]]
    }
    colour_idx <- grep("x__colour", names(data))
    if (length(colour_idx) > 0) {
      colour_res_idx <- grep("x__colour", names(res)) # find what comes after __colour
      res$colour <- res[[colour_res_idx]]
    }

    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    # browser()

    # generate points
    # consider 2nd weight for point
    if (in_data(res, ".n2")) {
      res$.n <- res$.n2
    }

    sub <- subset(res, level==max(res$level))
    if(drop_level) {
      ll <- subset(res, level==max(res$level)-1)
      sub <- dplyr::left_join(select(sub, -(xmin:ymax)), select(ll, contains("x__"), xmin:ymax, -contains("col")))
    }


# create a set of uniformly spread points between 0 and 1 once, when the plot is created.
# the transformation to the correct scale happens in compute panel.

    # altered from ggrepel:
    # Make reproducible if desired.
    if (!is.null(seed) && is.na(seed)) {
      seed <- sample.int(.Machine$integer.max, 1L)
    }

    points <- subset(sub, sub$.n>=1)
    points <- tidyr::nest(points, data = -label)
    points <- with_seed_null(seed,
      dplyr::mutate(
        points,
        coords = purrr::map(data, .f = function(d) {
          data.frame(
            x = runif(d$.n, min = 0, max = 1),
            y = runif(d$.n, min = 0, max = 1),
            dplyr::select(d, -x, -y)
          )
        })
      ))

    points <- tidyr::unnest(points, coords)
    # browser()

    points
  }
)


																#' @rdname geom_mosaic
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{x}{location of center of the rectangle}
#' \item{y}{location of center of the rectangle}
#' }
#' @export
stat_mosaic_text <- function(mapping = NULL, data = NULL, geom = "Text",
                        position = "identity", na.rm = FALSE,  divider = mosaic(),
                        show.legend = NA, inherit.aes = TRUE, offset = 0.01, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    aes_x <- rlang::eval_tidy(mapping$x)
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }


  #  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")

    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }


  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    aes_conds <- rlang::eval_tidy(mapping$conds)
    mapping$conds <- structure(1L, class = "productlist")
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaicText,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}

#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
StatMosaicText <- ggplot2::ggproto(
  "StatMosaicText", ggplot2::Stat,
  #required_aes = c("x"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    #cat("setup_params from StatMosaic\n")
    #browser()
    # if (!is.null(data$y)) {
    #   stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
    # }
    params
  },

  setup_data = function(data, params) {
    #cat("setup_data from StatMosaic\n")
    #browser()

    data
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, divider, offset) {

    first_stage <- StatMosaic$compute_panel(data, scales, na.rm=FALSE, divider, offset)

     # if (all(is.na(first_stage$colour)))
       # first_stage$colour <- scales::alpha(first_stage$fill, first_stage$alpha) # regard alpha in colour determination

     # browser()
     sub <- subset(first_stage, level==max(first_stage$level))
       text <- subset(sub, .n > 0) # do not label the obs with weight 0
     text <- tidyr::nest(text, data = -label)

     text <-
       dplyr::mutate(
         text,
         coords = purrr::map(data, .f = function(d) {
           data.frame(
             x = (d$xmin + d$xmax)/2,
             y = (d$ymin + d$ymax)/2,
             #size = 2.88,
             angle = 0,
             hjust = 0.5,
             vjust = 0.5,
             alpha = NA,
             family = "",
             fontface = 1,
             lineheight = 1.2,
             dplyr::select(d, -any_of(c("x", "y", "alpha")))
           )
         })
       )

     text <- tidyr::unnest(text, coords)

     # sub$fill <- NA
     # sub$colour <- NA
     # sub$size <- sub$size/10

     text

  }
)

																#' @rdname geom_mosaic
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{xmin}{location of bottom left corner}
#' \item{xmax}{location of bottom right corner}
#' \item{ymin}{location of top left corner}
#' \item{ymax}{location of top right corner}
#' }
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "mosaic",
                        position = "identity", na.rm = FALSE,  divider = mosaic(),
                        show.legend = NA, inherit.aes = TRUE, offset = 0.01, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    if (grepl("product", rlang::quo_text(mapping$x))) {
      aes_x <- rlang::eval_tidy(mapping$x)
    } else aes_x <- list(rlang::quo_get_expr(mapping$x))
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }

  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")
    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }

  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    if (grepl("product", rlang::quo_text(mapping$conds))) {
      aes_conds <- rlang::eval_tidy(mapping$conds)
    } else aes_conds <- list(rlang::quo_get_expr(mapping$conds))
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))

    mapping$conds <- structure(1L, class = "productlist")
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaic,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}


#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
StatMosaic <- ggplot2::ggproto(
  "StatMosaic", ggplot2::Stat,
  #required_aes = c("x"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    #cat("setup_params from StatMosaic\n")
    #browser()
    # if (!is.null(data$y)) {
    #   stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
    # }
    params
  },

  setup_data = function(data, params) {
    #cat("setup_data from StatMosaic\n")
    #browser()

    data
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, divider, offset) {
#    cat("compute_panel from StatMosaic\n")
#       browser()

    #    vars <- names(data)[grep("x[0-9]+__", names(data))]
    vars <- names(data)[grep("x__", names(data))]
    conds <- names(data)[grep("conds[0-9]+__", names(data))]


    if (length(vars) == 0) formula <- "1"
    else formula <-  paste(vars, collapse="+")



    formula <- paste("weight~", formula)

    if (length(conds) > 0) formula <- paste(formula, paste(conds, collapse="+"), sep="|")

    df <- data
    if (!in_data(df, "weight")) {
      df$weight <- 1
    }


    res <- prodcalc(df, formula=as.formula(formula),
                    divider = divider, cascade=0, scale_max = TRUE,
                    na.rm = na.rm, offset = offset)


    # need to set x variable - I'd rather set the scales here.
    prs <- parse_product_formula(as.formula(formula))
    p <- length(c(prs$marg, prs$cond))
    if (is.function(divider)) divider <- divider(p)

    # the level at which things are labelled could be made a parameter.
    # At the moment the deepest level is being labelled.
    dflist <- list(data=subset(res, level==max(res$level)), formula=as.formula(formula), divider=divider)
    #browser()
    scx <- productplots::scale_x_product(dflist)
    scy <- productplots::scale_y_product(dflist)


    # res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    res <- subset(res, level==max(res$level))

    # export the variables with the data - terrible hack
    # res$x <- list(scale=scx)
    # if (!is.null(scales$y)) {
    #   # only set the y scale if it is a product scale, otherwise leave it alone
    #   if ("ScaleContinuousProduct" %in% class(scales$y))
    #     res$y <- list(scale=scy)
    # }
    # XXXX add label for res
    cols <- c(prs$marg, prs$cond)


    if (length(cols) > 1) {
      df <- res[,cols]
      df <- tidyr::unite(df, "label", cols, sep="\n")

      res$label <- df$label
    } else res$label <- as.character(res[,cols])
    #   browser()

    res$x <- list(scale=scx)
    if (!is.null(scales$y)) {
      # only set the y scale if it is a product scale, otherwise leave it alone
      if ("ScaleContinuousProduct" %in% class(scales$y))
        res$y <- list(scale=scy)
    }

    # merge res with data:
    # is there a fill variable?
    fill_idx <- grep("x__fill", names(data))
    if (length(fill_idx) > 0) {
      fill_res_idx <- grep("x__fill", names(res))
      res$fill <- res[[fill_res_idx]]
    }
    alpha_idx <- grep("x__alpha", names(data))
    if (length(alpha_idx) > 0) {
      alpha_res_idx <- grep("x__alpha", names(res))
      res$alpha <- res[[alpha_res_idx]]
    }


    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    res
  }
)

																#' Theme for mosaic plots
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#' \code{theme_mosaic} provides access to the regular ggplot2 theme, but removes any
#' background, most of the gridlines, and ensures an aspect ratio of 1 for better
#' viewing of the mosaics.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @examples
#' library(ggmosaic)
#' data(happy)
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE) +
#'   theme_mosaic()
#'
#' @name theme_mosaic
NULL
#' @export
#' @import ggplot2
theme_mosaic <- function (base_size = 11, base_family = "")
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      aspect.ratio = 1
    )
}


																
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

parse_product_formula <- getFromNamespace("parse_product_formula", "productplots")

#' Wrapper for a list
#'
#' @param ... Unquoted variables going into the product plot.
#' @export
#' @examples
#' data(titanic)
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Survived, Class), fill = Survived))
product <- function(...) {
  rlang::exprs(...)
}

is.formula <- function (x) inherits(x, "formula")

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

product_names <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_breaks <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_labels <- function() {
  function(x) {
    #cat(" in product_labels\n")
    #browser()

    unique(x)
  }
}

is.waive <- function(x) inherits(x, "waiver")




## copied from ggplot2
with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}
