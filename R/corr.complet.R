corr.complet <-
  function(X=NULL, Y=NULL, Z=NULL,data=NULL,  group=NULL, param=c(.dico[["txt_param_test"]], .dico[["txt_non_param_test"]],.dico[["txt_robusts_tests_with_bootstraps"]], .dico[["txt_bayesian_factors"]]),
           save=F, outlier=c("complete", "id", "removed"),  z=NULL, info=T, n.boot=NULL, rscale=0.353, html=T)

  {options (warn=-1)
    corr.complet.in<-function(X=NULL, Y=NULL,Z=NULL, data=NULL, group=NULL, param=NULL, outlier=NULL, save=NULL, info=T,n.boot=NULL, rscale=0.707)
    {
      Resultats<-list()
      if(!is.null(X) & !is.null(data) & !is.null(Y)) {
	      dial<-F
	      if(is.null(Z)) choix<-.dico[["txt_correlations"]]
	      else choix<-.dico[["txt_partial_and_semi_correlations"]]
      } else {
	      dial<-T
	      choix<-NULL
      }

      if(is.null(choix)){
        if(info) writeLines(.dico[["ask_type_correlation"]])
        choix<-dlgList(c(.dico[["txt_correlations"]], .dico[["txt_partial_and_semi_correlations"]]), preselect=.dico[["txt_correlations"]], multiple = FALSE, title=.dico[["ask_simple_or_partial_corr"]])$res
        if(length(choix)==0) return(NULL)
      }
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]

      msg3<-.dico[["ask_chose_variable_x_axis"]]
      msg4<-.dico[["ask_chose_variable_y_axis"]]

      X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title=.dico[["txt_x_axis_variables"]], out=NULL)
      if(is.null(X)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      }
      data<-X$data
      X1<-X$X

      Y<-.var.type(X=Y, info=info, data=data, type="numeric", check.prod=F, message=msg4,  multiple=T, title=.dico[["txt_y_axis_variables"]], out=X1)
      if(is.null(Y)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      }
      data<-Y$data
      Y<-Y$X
      if(choix==.dico[["txt_partial_and_semi_correlations"]]) {
        msg6<-.dico[["ask_control_variables"]]
        Z<-.var.type(X=Z, info=info, data=data, type="numeric", check.prod=F, message=msg6,  multiple=T, title=.dico[["txt_control_variables"]], out=c(X1,Y))
        if(is.null(Z)) {
          corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)
	}
        data<-Z$data
        Z<-Z$X
      }

      if(dial) {
        if(info==TRUE) writeLines(.dico[["desc_corr_group_analysis_spec"]])
        dlgList(c(.dico[["txt_yes"]], .dico[["txt_no"]]), preselect=.dico[["txt_no"]], multiple = FALSE, title=.dico[["ask_analysis_by_group"]])$res->par.groupe
        if(length(par.groupe)==0) {
          corr.complet.in(X=NULL, Y=NULL, data=NULL,param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,n.boot=NULL, rscale=0.707)->Resultats
          return(Resultats)
        }

        msg5<-.dico[["ask_chose_ranking_categorial_factor"]]
        if(par.groupe==.dico[["txt_yes"]]) {
		group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=TRUE, title=.dico[["txt_variables"]], out=c(X1,Y,Z))
		if(length(group)==0) {
			corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL,n.boot=NULL, rscale=0.707)->Resultats
			return(Resultats)
		}
        	data<-group$data
        	group<-group$X
        	if(any(ftable(data[,group])<3)) {
        	  msgBox(.dico[["desc_need_at_least_three_observation_by_combination"]])
        	  corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL, n.boot=NULL, rscale=0.707)->Resultats
        	  return(Resultats)
        	}
        }
      }

      msg.options1<-.dico[["desc_param_is_BP"]]
      msg.options2<- .dico[["desc_non_param_are_rho_and_tau"]]

      options<-.ez.options(options=c('choix',"outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial, choix=param,sauvegarde=save, outlier=outlier, rscale=rscale)
      if(is.null(options)) {
        corr.complet.in(X=NULL, Y=NULL, data=NULL, param=NULL, outlier=NULL, save=NULL, info=T, group=NULL, n.boot=NULL, rscale=0.707)->Resultats
        return(Resultats)
      }

      Resultats$choix<-choix
      Resultats$nom<- nom
      Resultats$data<-data
      Resultats$X<-X1
      Resultats$Y<-Y
      if(exists("Z")) Resultats$Z<-Z
      if(exists("group")) Resultats$group<-group
      Resultats$options<-options
      return(Resultats)
    }


    corr.complet.out<-function(X=NULL, Y=NULL, Z=NULL, data=NULL, choix=NULL, group=NULL, param=NULL,n.boot=NULL, rscale=0.353) {
      boot_BP<-function(data,i)cor(data[ , X1][i], data[ , Y1][i], use="complete.obs", method="pearson")
      boot_Spearman<-function(data,i)cor(data[ ,X1][i], data[ , Y1][i], use="complete.obs", method="spearman")
      boot_BPSP<-function(data,i)cor(data[ , X][i], data[ , Y1][i], use="complete.obs", method="pearson")
      boot_SpearmanSP<-function(data,i)cor(data[ ,X][i], data[ , Y1][i], use="complete.obs", method="spearman")
      list()->Resultats
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-.stat.desc.out(X=c(X,Y,Z), groupes=NULL, data=data, tr=.1, type=3, plot=T)
      if(!is.null(group)) {
	      Resultats[[.dico[["txt_descriptive_statistics_by_group"]]]]<-.stat.desc.out(X=c(X,Y,Z), groupes=group, data=data, tr=.1, type=3, plot=T)
      }

      if(choix== .dico[["txt_correlations"]]) {
        title<-.dico[["txt_BP_correlation"]]
        title2<-.dico[["txt_rho"]]
        X1<-X
        Y1<-Y} else {
          title<-.dico[["txt_partial_corr_BP"]]
          title2<-.dico[["txt_partial_rho"]]
          modele1<-as.formula(paste0(X,"~",Z[1]))
          modele2<-as.formula(paste0(Y,"~", Z[1]))
          if(length(Z)>1) for(i in 2:length(Z)){
            modele1<-update(modele1, as.formula(paste0(".~.+",Z[i])))
            modele2<-update(modele2, as.formula(paste0(".~.+",Z[i])))
          }
          lm.r1<-lm(modele1, data)
          lm.r2<-lm(modele2, data)
          data$'residus1'<-lm.r1$residuals
          data$'residus2'<-lm.r2$residuals
          X1<-'residus1'
          Y1<-'residus2'
        }
      modele<-as.formula(paste0(X1,"~",Y1))
      lm.r<-lm(modele,na.action=na.exclude,data=data)
      resid(lm.r)->data$'residus' # recuperation du residu sur le modele lineaire

      if(any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]) | any(param=="param") | any(param==.dico[["txt_param_tests"]]))  {
        Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X='residus', Y=NULL)
        graphiques<-list()
        p<-ggplot(data)
        p<-p+ eval(parse(text=paste0("aes(x=", X,", y=", Y,")"))) + geom_point()
        p<-p+ geom_smooth(method=lm)
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        graphiques[[1]]<-p
        #print(p)
        if(!is.null(group)){
          p1<-p+eval(parse(text=paste0("aes(color=",group[1] ,")")))
          if(length(group)>1) {p1<-p1+eval(parse(text=paste0("aes(shape=",group[2],")"))) }
          if(length(group)>2){
            for(i in 3:length(group)){
              if(i==3) paste0(".~", group[3])->panneau
              if(i==4) paste0(group[4],"~", group[3])->panneau
              if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
              if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
            }
            p1<-p1+ facet_grid(as.formula(panneau), labeller=label_both)
          }
          graphiques[[2]]<-p1
        }
        Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_graphics"]]]]<-graphiques
      }

      if(any(param=="param") | any(param==.dico[["txt_param_tests"]])){
        if(choix!=.dico[["txt_correlations"]]) {
          cor.part<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")[1:3],
                           spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "pearson")[1:3])
          cor.part$estimate^2->cor.part[[.dico[["txt_r_dot_square"]]]]
          round(cor.part, 4)->cor.part
          cor.part$ddl<-(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "pearson")$n-2-length(Z))
          dimnames(cor.part)<-list(c(.dico[["txt_partial_corr_BP"]],.dico[["txt_semi_BP"]]), c("Correlation", .dico[["txt_p_dot_val"]], "test.t", .dico[["txt_r_dot_square"]],.dico[["txt_df"]]))
          Resultats[[.dico[["txt_partial_semi_BP"]]]]<-cor.part
        } else {
          BP<-cor.test(data[, X1], data[ ,Y1], method = "pearson")
          Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_BP_correlation"]]]]<-round(data.frame("r"=BP$estimate,txt_r_dot_two=BP$estimate^2, txt_ci_inferior_limit=BP$conf.int[1],txt_ci_superior_limit=BP$conf.int[2], "t"=BP$statistic, txt_df=BP$parameter, txt_p_dot_val=BP$p.value),4)
          names(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_BP_correlation"]]]])<-c("r",.dico[["txt_r_dot_two"]], .dico[["txt_ci_inferior_limit"]],.dico[["txt_ci_superior_limit"]], "t", .dico[["txt_df"]], .dico[["txt_p_dot_val"]])
        }

        if(!is.null(group)){
          if(choix==.dico[["txt_correlations"]]) {
            corr.g<-function(X2){
              return(data.frame(BP.r= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$estimate,
                                BP.ddl= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$parameter,
                                BP.t= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$statistic,
                                BP.p= cor.test(X2[, X1], X2[ ,Y1], method = "pearson")$p.value))}} else {
                                  corr.g<-function(X2){ return(data.frame(BP.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$estimate,
                                                                          BP.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z])$n-2-length(Z),
                                                                          BP.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$statistic,
                                                                          BP.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "pearson")$p.value))}
                                }

          BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
          BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T), 4)
          if(length(group)==1) {gr.l<-expand.grid(levels(data[,group]))
          names(gr.l)<-group}else { gr.l<-sapply(data[,group],levels)
          gr.l<-data.frame(gr.l)
          gr.l<-expand.grid(gr.l)
          }

          dimnames(BPgroup)[[2]]<- c("BP.r", "BP.ddl", "BP.t", "BP.p")
          BPgroup<-data.frame(gr.l,BPgroup )
          if(choix!=.dico[["txt_correlations"]]) {
		  Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_partial_corr_BP_by_group"]]]]<-BPgroup
	  } else {
		  Resultats[[.dico[["txt_BP_correlation_by_group"]]]]<-BPgroup
	  }
        }
      }
      if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])){

        graphiques<-list()
        p<-ggplot(data)
        p<-p+ eval(parse(text=paste0("aes(x=rank(data$", X,"), y=rank(data$", Y,"))"))) + geom_point()
        p<-p+ labs(x =X, y=Y)
        p<-p+ geom_smooth(method=lm)
        p<-p+theme(plot.title = element_text(size = 12))+ggtitle(title2)
        p<-p+theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))
        graphiques[[1]]<-p
        #print(p)
        if(!is.null(group)){
          p1<-p+eval(parse(text=paste0("aes(color=",group[1] ,")")))
          if(length(group)>1) {p1<-p1+eval(parse(text=paste0("aes(shape=",group[2],")"))) }
          if(length(group)>2){
            for(i in 3:length(group)){
              if(i==3) paste0(".~", group[3])->panneau
              if(i==4) paste0(group[4],"~", group[3])->panneau
              if(i>3 & i%%2!=0) paste0(panneau, "+", group[i])->panneau
              if(i>4 & i%%2==0) paste0(group[i], "+", panneau)->panneau
            }
            p1<-p1+ facet_grid(as.formula(panneau), labeller=label_both)
          }
          graphiques[[2]]<-p1
        }
        Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_graphics"]]]]<-graphiques

        if(choix!=.dico[["txt_correlations"]]) {
          spear<-rbind( pcor.test(data[,X], data[ ,Y], data[ , Z], method = "spearman")[1:3],spcor.test(data[,X], data[ ,Y], data[ ,Z], method = "spearman")[1:3])
          tau<-rbind(pcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3],spcor.test(data[,X], data[ ,Y], data[ , Z], method = "kendall")[1:3])
          spear<-round(spear,4)
          tau<-round(tau,4)
          #spear$estimate^2->spear$r.carre
          spear$estimate^2->spear[[.dico[["txt_r_dot_square"]]]]
          round(spear, 4)->cor.part
          dimnames(spear)<-list(c(.dico[["txt_partial_rho"]],.dico[["txt_semi_partial_rho"]]), c("rho", .dico[["txt_p_dot_val"]], "t", .dico[["txt_r_dot_square"]]))
          Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_partial_semi_partial_rho"]]]]<-spear
          tau<-round(tau,4)
          dimnames(tau)<-list(c(.dico[["txt_kendall_partial_tau"]],.dico[["txt_kendall_semipartial_tau"]]), c("tau", .dico[["txt_p_dot_val"]], "z"))
          Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_kendall_partial_semipartial_tau"]]]]<-tau
        } else { Spear<-cor.test(data[,X1], data[ ,Y1], method = "spearman", exact=T, continuity=T)
        cor.test(data[,X1], data[ ,Y1], method = "kendall")->Kendall
        Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_rho"]]]]<-round(data.frame("rho"=Spear$estimate,txt_rho_dot_square=Spear$estimate^2,"S"=Spear$statistic,txt_p_dot_val=Spear$p.value),4)
        names(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_rho"]]]])<-c("rho",.dico[["txt_rho_dot_square"]],"S",.dico[["txt_p_dot_val"]])
        round(data.frame("tau"=Kendall$estimate,"z"=Kendall$statistic,txt_p_dot_val=Kendall$p.value),4)->Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_kendall_tau"]]]]
        c("tau","z",.dico[["txt_p_dot_val"]])->names(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_kendall_tau"]]]])
	}

        if(!is.null(group)){
          if(choix==.dico[["txt_correlations"]]) {corr.g<-function(X2){ return(data.frame(Sp.r= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$estimate,
                                                                             Sp.p= cor.test(X2[, X1], X2[ ,Y1], method = "spearman")$p.value,
                                                                             Kendall.r= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$estimate,
                                                                             Kendall.p= cor.test(X2[, X1], X2[ ,Y1], method = "kendall")$p.value))}
          } else {
            corr.g<-function(X2){ return(data.frame(Spearman.r= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                    Spearman.ddl= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z],method="spearman")$n-2-length(Z),
                                                    Spearman.t= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$estimate,
                                                    Spearman.p= pcor.test(X2[, X], X2[ ,Y],X2[ ,Z], method = "spearman")$p.value))
            			}
	}

        BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
        BPgroup<-round(matrix(unlist(BPgroup), ncol=4, byrow=T),4)
        if(length(group)==1) {
		gr.l<-expand.grid(levels(data[,group]))
        	names(gr.l)<-group
	} else {
		gr.l<-sapply(data[,group],levels)
        	gr.l<-data.frame(gr.l)
		gr.l<-expand.grid(gr.l)
        }
        if(choix!=.dico[["txt_correlations"]]){
          dimnames(BPgroup)[[2]]<- c("Spearman.rho", .dico[["txt_spearman_df"]], "Spearman.t", "Spearman.p")
          BPgroup<-data.frame(gr.l,BPgroup )
          Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_partial_spearman_by_group"]]]]<-BPgroup
        } else {
		dimnames(BPgroup)[[2]]<- c( "Spearman.r", "Spearman.p", "Tau.Kendall.r", "Tau.Kendall.p")
        	BPgroup<-data.frame(gr.l,BPgroup )
        	Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_spearman_kendall_corr_by_group"]]]]<-BPgroup
	}
       }
      }

      if(any(param=="robust"| any(param==.dico[["txt_robusts_tests_with_bootstraps"]]))) {
        boot_BP_results<-boot(data, boot_BP, n.boot)
        if(!is.null(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_BP_correlation"]]]])) {
          try(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_BP_correlation"]]]][[.dico[["txt_bca_inferior_limit"]]]]<-round( boot.ci(boot_BP_results)$bca[,4],4), silent=T)
          try(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_BP_correlation"]]]][[.dico[["txt_bca_superior_limit"]]]]<-round( boot.ci(boot_BP_results)$bca[,5],4),silent=T)
        } else if(!is.null(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_partial_semi_BP"]]]])) {
          boot_BPSP_results<-boot(data, boot_BPSP, n.boot)
          try(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_partial_semi_BP"]]]][[.dico[["txt_bca_inferior_limit"]]]]<-round( c(boot.ci(boot_BP_results)$bca[,4], boot.ci(boot_BPSP_results)$bca[,4]),4),silent=T)
          try(Resultats[[.dico[["txt_param_tests"]]]][[.dico[["txt_partial_semi_BP"]]]][[.dico[["txt_bca_superior_limit"]]]]<-round( c(boot.ci(boot_BP_results)$bca[,5], boot.ci(boot_BPSP_results)$bca[,5]) ,4), silent=T)
        #} else try(Resultats[[.dico[["txt_robust_analysis"]]]][[.dico[["txt_bootstrap_on_BP"]]]]<-round(data.frame(txt_bca_inferior_limit= boot.ci(boot_BP_results)$bca[,4], txt_bca_superior_limit=boot.ci(boot_BP_results)$bca[,5] ), 4),silent=T)
        } else {
		try(Resultats[[.dico[["txt_robust_analysis"]]]][[.dico[["txt_bootstrap_on_BP"]]]]<-round(data.frame(txt_bca_inferior_limit= boot.ci(boot_BP_results)$bca[,4], txt_bca_superior_limit=boot.ci(boot_BP_results)$bca[,5] ), 4),silent=T)
		try(names(Resultats[[.dico[["txt_robust_analysis"]]]][[.dico[["txt_bootstrap_on_BP"]]]])<-c(.dico[["txt_bca_inferior_limit"]], .dico[["txt_bca_superior_limit"]]))
	}

        if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])) {
          boot_Spearman_results<-boot(data, boot_Spearman, n.boot)
          if(!is.null(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_rho"]]]])) {
            try(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_rho"]]]][[.dico[["txt_bca_inferior_limit"]]]]<-round( boot.ci(boot_Spearman_results)$bca[,4],4), silent=T)
            try(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_rho"]]]][[.dico[["txt_bca_superior_limit"]]]]<-round( boot.ci(boot_Spearman_results)$bca[,5],4), silent=T)
          } else{
            boot_SpearmanSP_results<-boot(data, boot_SpearmanSP, n.boot)

            try(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_partial_semi_partial_rho"]]]][[.dico[["txt_bca_inferior_limit"]]]]<-round(c( boot.ci(boot_Spearman_results)$bca[,4], boot.ci(boot_SpearmanSP_results)$bca[,4]),4), silent=T)
            try(Resultats[[.dico[["txt_non_parametric_test"]]]][[.dico[["txt_partial_semi_partial_rho"]]]][[.dico[["txt_bca_superior_limit"]]]]<-round(c( boot.ci(boot_Spearman_results)$bca[,5], boot.ci(boot_SpearmanSP_results)$bca[,5]),4), silent=T)
          }

        }
      }

      if(any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]) ){

        BF<-regressionBF(modele, data=data, rscaleCont=rscale )
        sample<-posterior(BF, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame(txt_bayesian_factor=c(ifelse(BF$bf>10000,">10000", round(BF$bf,5)),
                                            ifelse(1/BF$bf>10000, ">10000", round((1/BF$bf),5))), txt_error=round(c( BF$error, BF$error),5))
	names(BF)<-c(.dico[["txt_bayesian_factor"]],.dico[["txt_error"]])

        dimnames(BF)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]])
        # what is the t-value for the data?
        r2Val <-cor.test(data[,X1],data[,Y1])$estimate
        BF$r<-r2Val
        r2Val<-r2Val^2
        #BF$r.carre<-r2Val
        BF[[.dico[["txt_r_dot_square"]]]]<-r2Val
        Resultats[[.dico[["txt_bayesian_factors"]]]][[.dico[["txt_bayesian_factors_for_BP"]]]]<-BF

        if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])) {
          data2<-sapply(data[,c(X,Y,Z)], rank, ties.method="average", na.last="keep")
          data2<-data.frame(data2)
          if(choix!=.dico[["txt_correlations"]]){
            lm.r1<-lm(modele1, data2)
            lm.r2<-lm(modele2, data2)
            data2$'residus1'<-lm.r1$'residuals'
            data2$'residus2'<-lm.r2$'residuals'
          }

          BFS<-regressionBF(modele, data=data2, rscaleCont=rscale )
          BFS<-extractBF(BFS, onlybf=F)
          BFS<-data.frame(txt_bayesian_factor=c(ifelse(BFS$bf>10000,">10000", round(BFS$bf,5)),
                                               ifelse(1/BFS$bf>10000, ">10000", round((1/BFS$bf),5))), txt_error=round(c( BFS$error, BF$error),5))

	  names(BFS)<-c(.dico[["txt_bayesian_factor"]],.dico[["txt_error"]])
          dimnames(BFS)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]])
          Resultats[[.dico[["txt_bayesian_factors"]]]][[.dico[["txt_bayesian_factors_for_spearman"]]]]<-BFS

        }

        if(!is.null(group)){

          corr.g<-function(X2){  BF<-regressionBF(modele, X2, rscaleCont=rscale ,progress=F)
          BF<-extractBF(BF, onlybf=F)
          #return(data.frame(txt_bayesian_factor=round(BF$bf,5), txt_error=round(BF$error,5)))
          current_df <- data.frame(txt_bayesian_factor=round(BF$bf,5), txt_error=round(BF$error,5))
	  names(current_df) <- c(.dico[["txt_bayesian_factor"]],.dico[["txt_error"]])
          return(current_df)
	  }

          BPgroup<-by(data=data, INDICES=data[,group], FUN=corr.g)
          BPgroup<-round(matrix(unlist(BPgroup), ncol=2, byrow=T), 4)
          dimnames(BPgroup)[[2]]<- c("FB", .dico[["txt_error"]])
          if(length(group)==1) {gr.l<-expand.grid(levels(data[,group]))
          names(gr.l)<-group}else gr.l<-expand.grid(sapply(data[,group],levels))
          BPgroup<-data.frame(gr.l,BPgroup )

          if( any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])){
            BFgroupS<-by(data=data2, INDICES=data[,group], FUN=corr.g)
            BFgroupS<-matrix(unlist(BFgroupS), ncol=2, byrow=T)
            BPgroup<-cbind(BPgroup, BFgroupS)
            names(BPgroup)<-c(group, "FB.BP",.dico[["txt_error_BP"]], "FB.Spearman", .dico[["txt_error_spearman"]])
          }
          BPgroup->Resultats[[.dico[["txt_bayesian_factors"]]]][[.dico[["txt_bayesian_factor_by_group"]]]]
        }

        plot(sample)
        bfs<-c()
        for (i in 5:length(data[,X1])) {
          bfm <- regressionBF(modele, data=data[1:i,],progress=F, rscaleCont=0.353)
          bfl <- regressionBF(modele, data=data[1:i,], progress=F, rscaleCont=0.5)
          bful <- regressionBF(modele,data=data[1:i,], progress=F, rscaleCont=0.707)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }

        SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs,
                        "rscale"=as.factor(rep(c("moyen - 0.353", .dico[["txt_large_half"]], .dico[["txt_ultrawide_val"]]), length.out= 3*(length(data[,X])-4) )))
        SBF$rscale<-relevel(SBF$rscale, ref=2)
        Resultats[[.dico[["txt_bayesian_factors_sequential"]]]]<-.plotSBF(SBF)

        ##### Debut du graphique  Bayes Factor Robustness Check


        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)

        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x) exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = x, simple = T)))

        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.353, simple = T))->r1
        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.5, simple = T))->r2
        exp(linearReg.R2stat(N=length(data[,1]), p=1, R2=r2Val, rscale = 0.707, simple = T))->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        if(max(bayesFactors)>10^40) bayesFactors[which(bayesFactors>10^40)]<-10^40
        if(r1>10^40) r1<-10^40
        if(r2>10^40) r2<-10^40
        if(r3>10^40) r3<-10^40
        seq(min(bayesFactors),  max(bayesFactors), length.out = 5)->axe2
        format(axe2, scientific=T)->axe2b
        par(mar = c(4, 10, 0.5, 0.5), mgp = c(8, 1, 0))
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48", ylim= c(min(bayesFactors), max(bayesFactors)),
             yaxt = "n"    , xaxt = "n",  xlab = .dico[["txt_cauchy_prior_width"]] , ylab = .dico[["txt_bayes_factor_10"]])
        axis(2, labels=axe2b, at=axe2, las=2)
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))



        # add the BF at the default Cauchy point
        points(2^0.5/4, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(0.5, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5/2, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.353 - medium", "r = 0.5 - wide ", "r = 0.707 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")


      }

      return(Resultats)
    }


    # package supprime "plyr",
    packages<-c('BayesFactor', 'boot', 'ggplot2', 'ppcor', 'outliers', 'psych', 'svDialogs')

    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=='try-error') quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data

    corr.options<-corr.complet.in(X=X, Y=Y,Z=Z, data=data, group=group, param=param, outlier=outlier, save=save, info=T, n.boot=n.boot, rscale=rscale)
    if(is.null(corr.options)) return(analyse())
    choix<-corr.options$choix
    X<-corr.options$X
    Y<-corr.options$Y
    Z<-corr.options$Z
    group<-corr.options$group
    data<-corr.options$data
    param<-corr.options$options$choix
    if(corr.options$options$rscalei==T) rscale<-corr.options$options$rscale/2 else rscale<-corr.options$options$rscale
    n.boot<-corr.options$options$n.boot
    save<-corr.options$options$sauvegarde
    outlier<-corr.options$options$desires

    expand.grid(X,Y)->XY
    for(i in 1:length(XY[,1]))
    {
      X1<-as.character(XY[i,1])
      Y1<-as.character(XY[i,2])
      data1<-data[complete.cases(data[,c(Y1,X1,Z)]),]
      R1<-list()
      if(any(outlier%in%  c(.dico[["txt_complete_dataset"]], "complete"))){
        R1[[.dico[["txt_complete_dataset"]]]]<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=data1, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
      }
      if(any(outlier%in%c(.dico[["txt_identifying_outliers"]],"id"))|
         any(outlier%in%c(.dico[["txt_without_outliers"]], "removed"))){
        modele<-as.formula(paste0(X1,"~",Y1))
        if(!is.null(Z)){for(i in 1:length(Z))      modele<-update(modele, as.formula(paste0(".~.+",Z[i])))}
        data1$'residu'<-resid(lm(modele, data=data1))
        critere<-ifelse(is.null(z), "Grubbs", "z")
        valeurs.influentes(X='residu', critere=critere,z=z, data=data1)->influentes
      }
      if(any(outlier%in% c("id",.dico[["txt_identifying_outliers"]]))){influentes->R1[[.dico[["txt_outliers_values"]]]]}
      if(any(outlier%in%c("removed", .dico[["txt_without_outliers"]]))) {
        #if(length(influentes$'observations influentes')!=0 |
        #if(length(influentes[[.dico[["txt_outliers"]]]])!=0 |
        #if(influentes[[.dico[["txt_outliers_synthesis"]]]]$Synthese[1]!=0 |
        if(influentes[[.dico[["txt_outliers_synthesis"]]]][[.dico[["txt_synthesis"]]]][1]!=0 |
           ! any(outlier %in% c(.dico[["txt_complete_dataset"]],"complete"))){
          get('nettoyees', envir=.GlobalEnv)->nettoyees
          R1[[.dico[["txt_without_outliers"]]]]<-corr.complet.out(X=X1, Y=Y1,Z=Z, data=nettoyees, choix=choix, group=group, param=param, n.boot=n.boot, rscale=rscale)
        }
      }
      Resultats[[i]]<-R1
      names(Resultats)[i]<-paste(.dico[["txt_correlation_between_var_x"]], X1, .dico[["desc_and_variabe"]], Y1)
    }

    paste(X, collapse="','", sep="")->X
    paste(Y, collapse="','", sep="")->Y
    if(!is.null(Z)) paste(Z, collapse="','", sep="")->Z
    if(!is.null(group)) paste(group, collapse="','", sep="")->group


    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("corr.complet(X=c('", X,
                           "'), Y=c('", Y,
                           "'), Z =", ifelse(!is.null(Z),paste0("c('",Z,"')"), "NULL"), ",data=",  corr.options$nom,
                           ", group=", ifelse(!is.null(group),paste0("c('",group,"')"), "NULL"),
                           ", param=c('", param, "'), save=", save, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                           ", info=T, rscale=", rscale,
                           ", n.boot=", ifelse(is.null(n.boot), "NULL",n.boot),", html=", html, ")")

    .add.history(data=data, command=Resultats$Call, nom=corr.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))

    if(save){ try(ez.html(Resultats, html=F), silent=T) }

    ref1(packages)->Resultats[[.dico[["txt_references"]]]]
    if(html) try(ez.html(Resultats), silent=T)
    ### Obtenir les Resultats
    return(Resultats)
  }
