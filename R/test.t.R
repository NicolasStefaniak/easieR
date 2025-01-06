test.t <-
  function(X=NULL, Y=NULL, group=NULL, choix=NULL,
           sauvegarde=F, outlier=c(.dico[["txt_complete_dataset"]],  .dico[["txt_identifying_outliers"]],.dico[["txt_without_outliers"]]),  z=NULL, data=NULL,
           alternative="two.sided", mu=NULL, formula=NULL, n.boot=NULL,
           param=c(.dico[["txt_param_test"]], .dico[["txt_non_param_test"]],.dico[["txt_robusts_tests_with_bootstraps"]],
                   .dico[["txt_bayesian_factors"]]), info=TRUE, rscale=0.707, html=T){
    # X : Character specifying the dependant variable in dataframe.
    # Y : character specifying either a two levels factor in dataframe or a numeric variable if paired is TRUE
    # group : Factor vector allowing to decompose analysis by group in one sample t test
    # choix : Character. One among c(.dico[["txt_comparison_to_norm"]], .dico[["txt_two_paired_samples"]],.dico[["txt_two_independant_samples"]])
    # sauvegarde : logical. Should the results be saved ?
    # outlier : character. One or several possibilities among c(.dico[["txt_complete_dataset"]],   .dico[["txt_identifying_outliers"]], .dico[["txt_without_outliers"]])
    # z : if NULL and the identification/exclusion of outlier is desired, outlier are identified on Grubbs' test. If z is numeric, outliers are identified on abs(z)
    # data : data on which analysis has to be performed.
    # alternative : one among c("greater", "lower", "two.sided"). Two sided is default.
    # formula : a formula of the form dependant.variable~independant.variable
    # n.boot : number of bootstrap. Must be a positive value
    # param : character vector with one or several choices among c(.dico[["txt_param_test"]], .dico[["txt_non_param_test"]],.dico[["txt_robusts_tests_with_bootstraps"]], .dico[["txt_bayesian_factors"]])
    # info : logical. If dialog box are used, Should information be printed in the console
    # rscale : if desc_bayesian_factors_chosen_inparam", rscale is the prior scale. See t.testBF for more information

    #### 5 fonctions qui seront appelees pour realiser l'analyse
    test.t.in<-function(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                        formula=NULL,n.boot=NULL, rscale=NULL, mu=NULL){

      Resultats<-list()
      if(!is.null(choix)) dial<-F else dial<-T
      if(is.null(choix) || (choix %in%c(.dico[["txt_comparison_to_norm"]], .dico[["txt_two_paired_samples"]],.dico[["txt_two_independant_samples"]])==FALSE)){
        if(info) writeLines(.dico[["ask_t_test_type"]])
        choix<-dlgList(c(.dico[["txt_comparison_to_norm"]], .dico[["txt_two_paired_samples"]],
                         .dico[["txt_two_independant_samples"]]), preselect=NULL, multiple = FALSE, title=.dico[["txt_t_test_choice"]])$res
        if(length(choix)==0) return(NULL)
      }
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(NULL)
      nom<-data[[1]]
      data<-data[[2]]
      if(is.null(Y) || class(data[,Y]) == "factor") format<-"long" else format<-.dico[["txt_large"]]

      if(is.null(formula)){
        if(choix==.dico[["txt_two_paired_samples"]]){
          if(dial){
            if(info==TRUE){
              temps1<-1:3
              temps2<-4:6
              data.frame(txt_time1=temps1,txt_time2=temps2)->large
              data.frame(c(rep(.dico[["txt_time1"]],3),rep(.dico[["txt_time2"]], 3)), 1:6)->long
              names(long)<-c("moment","mesure")
              writeLines(.dico[["desc_this_is_large_format"]])
              print(large)
              writeLines(.dico[["desc_this_is_long_format"]])
              print(long)}
            format<-dlgList(c(.dico[["txt_large"]], "long"), preselect=.dico[["txt_large"]], multiple = FALSE, title=.dico[["ask_data_format"]])$res
            if(length(format)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
          }}
        if(format==.dico[["txt_large"]]) {
          msg3<-.dico[["ask_time1"]]
          msg4<-.dico[["ask_time2"]]
          title1<-.dico[["txt_time_1"]]
          title2<-.dico[["txt_time_2"]]
        } else{
          msg3<-.dico[["ask_chose_dependant_variable"]]
          msg4<-.dico[["ask_independant_variable"]]
          title1<-.dico[["txt_dependant_variables"]]
          title2<-.dico[["txt_independant_variable"]]

        }

        if(choix==.dico[["txt_two_paired_samples"]]) {multiple<-F
        if(length(X)>1){
          msgBox(.dico[["desc_single_dependant_variable_allowed_in_paired_t"]])
          X<-NULL }}else multiple<-T
          X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=multiple, title=title1, out=NULL)
          if(is.null(X)) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          data<-X$data
          X1<-X$X

          if(choix!=.dico[["txt_comparison_to_norm"]]){
            if(choix==.dico[["txt_two_paired_samples"]] && format==.dico[["txt_large"]]) type<-"numeric" else type<-"factor"
            Y<-.var.type(X=Y, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=FALSE, title=title2, out=X1)
            if(is.null(Y)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)}
            data<-Y$data
            Y<-Y$X
            if(class(data[,Y])=="factor" && nlevels(data[,Y])!=2) {
              msgBox(.dico[["desc_two_modalities_for_independante_categorial_variable"]])
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)
            }
          }
      } else {
        X1<-as.character(formula[2])
        Y<-as.character(formula[3])
      }





      if(choix==.dico[["txt_two_paired_samples"]]){
        if(format==.dico[["txt_large"]]){
          if(dial){
            if(info==TRUE)writeLines(.dico[["ask_independant_variable_name"]])
            nomVI <- dlgInput(.dico[["ask_independant_variable_name"]], "Moment")$res
            if(length(nomVI)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
            strsplit(nomVI, ":")->nomVI
            tail(nomVI[[1]],n=1)->nomVI
            if(info==TRUE) writeLines(.dico[["ask_dependant_variable_name"]])
            nomVD <- dlgInput(.dico[["ask_dependant_variable_name"]], .dico[["txt_result"]])$res
            if(length(nomVD)==0) {
              Resultats<-test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                                   formula=NULL,n.boot=NULL, rscale=NULL)
              return(Resultats)
            }
          } else {
            nomVD<-.dico[["txt_result"]]
            nomVI<-"Moment"
          }
          strsplit(nomVD, ":")->nomVD
          tail(nomVD[[1]],n=1)->nomVD
          data[complete.cases(data[,c(X1, Y)]),]->data
          data$IDeasy<-paste0("p", 1:length(data[,X1]))
          melt(data=data, measure.vars=c(X1,Y) , variable.name=nomVI, value.name=nomVD)->data
          assign(x=paste0(nom,".format.long"), value=data, envir=.GlobalEnv)
          X1<-nomVD
          Y<-nomVI
        }
        if(format=="long") {
          if( length(unique(table(data[,Y])))!=1) {
            msgBox(.dico[["desc_non_equal_independant_variable_modalities_occurrence"]])
            msg4<-.dico[["ask_id_variable"]]
            ID<-.var.type(X=NULL, info=info, data=data, type=type, check.prod=F, message=msg4,  multiple=multiple, title=.dico[["txt_id_variable"]], out=c(X1,Y))
            if(is.null(ID)) {
              test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
              return(Resultats)}
            ID<-ID$X
            ID.fail<-names(which(table(data[,ID])!=2))
            data<-data[which(data[,ID]!=ID.fail),]
            data<-data[order(data[,c(Y,ID)]), ]
          } else {
            data[order(data[,Y]),]->data
            data$IDeasy<-rep(paste0("p", 1:(length(data[,X1])/2)), 2)
          }
        }

      }

      if(choix==.dico[["txt_comparison_to_norm"]]){
        writeLines(.dico[["ask_specify_norm_value"]])
        if(class(mu) !="numeric") mu<-NA
        while(is.na(mu)){
          mu <- dlgInput(.dico[["ask_norm_value"]], 0)$res
          if(length(mu)==0) {
		  test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                                        formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
		  return(Resultats)
	  }
          strsplit(mu, ":")->mu
          tail(mu[[1]],n=1)->mu
          as.numeric(mu)->mu
          if(is.na(mu)) msgBox(.dico[["desc_norm_must_be_numeric"]])
        }
        if(dial){


          if(info==TRUE) writeLines(.dico[["desc_bilateral_superior_inferior_test_t"]])
          dlgList(c(.dico[["txt_bilateral"]], .dico[["txt_superior"]], .dico[["txt_inferior"]]), preselect=NULL, multiple = FALSE, title=.dico[["txt_means_comparison"]])$res->alternative
          if(length(alternative)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)
          #} else car::recode(alternative, "'Bilateral'= 'two.sided';'Superieur'='greater'; 'Inferieur'='less'")->alternative # TODO check here for troubles translation
          } else {
		  if      (alternative == .dico[["txt_bilateral"]]) { 'two.sided' -> alternative }
		  else if (alternative == .dico[["txt_superior"]])  { 'greater' -> alternative }
		  else if (alternative == .dico[["txt_inferior"]])  { 'less' -> alternative }
		  else { print('[ERROR] Unknown alternative (./test.t.R)') }
	  }

          if(info==TRUE) writeLines(.dico[["desc_corr_group_analysis_spec"]])
          dlgList(c(.dico[["txt_yes"]], .dico[["txt_no"]]), preselect=.dico[["txt_no"]], multiple = FALSE, title=.dico[["ask_analysis_by_group"]])$res->par.groupe
          if(length(par.groupe)==0) {
            test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative="two.sided",
                      formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)
          }
          msg5<-.dico[["ask_chose_categorial_ranking_factor"]]
          if(par.groupe==.dico[["txt_yes"]]){group<-.var.type(X=group, info=info, data=data, type="factor", check.prod=F, message=msg5,  multiple=FALSE, title=.dico[["txt_variables"]], out=X1)
          if(length(group)==0) { test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative='two.sided',
                                           formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
            return(Resultats)}
          data<-group$data
          group<-group$X
          }
        }
      }
      msg.options1<- .dico[["desc_param_is_t_test"]]
      msg.options2<- .dico[["desc_non_param_is_wilcoxon_or_mann_withney"]]

      options<-.ez.options(options=c('choix',"outlier"), n.boot=n.boot,param=T, non.param=T, robust=T, Bayes=T, msg.options1=msg.options1, msg.options2=msg.options2, info=info, dial=dial,
                           choix=param,sauvegarde=sauvegarde, outlier=outlier, rscale=rscale)
      if(is.null(options)){
        test.t.in(X=NULL, Y=NULL, data=NULL, choix=NULL, param=NULL, outlier=NULL, sauvegarde=NULL, info=T, group=NULL,alternative='two.sided',
                  formula=NULL,n.boot=NULL, rscale=NULL)->Resultats
        return(Resultats)
      }
      Resultats$choix<-choix
      Resultats$nom<-ifelse(format==.dico[["txt_large"]], paste0(nom,".format.long"), nom)
      Resultats$data<-data
      Resultats$X<-X1
      if(exists("Y")) Resultats$Y<-Y
      if(exists("mu")) Resultats$mu<-mu
      if(exists(.dico[["txt_alternative"]])) Resultats$alternative<-alternative
      if(exists("group")) Resultats$group<-group
      Resultats$options<-options
      return(Resultats)
      }

      norme<-function(X, mu, data, param=c("param", "non param", .dico[["txt_robusts"]]), group=NULL, alternative='two.sided', n.boot=NULL, rscale=0.707){
      if(class(data)!="data.frame") {data<-data.frame(data)
                                     names(data)[1]<-X}
      Resultats<-list()
      .e <- environment()
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-.stat.desc.out(X=X, groupes=NULL, data=data, tr=.1, type=3, plot=F)
      cutoff <- data.frame(x = c(-Inf, Inf), y = mu, cutoff = factor(mu) )
      p2<- ggplot(data)
      p2<-p2+ eval(parse(text=paste0("aes(x=factor(0), y=", X,")"))) + geom_violin()
      p2<-p2+geom_line(aes( x, y, linetype = cutoff ), cutoff)
      p2<-p2+ labs( x=" ")
      p2<-p2 + stat_summary(fun.data=data_summary,geom="pointrange", color="red", size=0.50,position=position_dodge(0.9))
      p2<-p2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=1/4)
      p2<-p2 + theme(legend.position="none")
      p2<-p2+theme(plot.title = element_text(size = 12))+ggtitle(.dico[["txt_mean_sd"]])
      # print(p2)
      Resultats[[.dico[["txt_descriptive_statistics"]]]]$Graphique<-p2

      if(!is.null(group)) {Resultats[[.dico[["txt_descriptive_statistics_by_group"]]]]<-.stat.desc.out(X=X, groupes=group, data=data, tr=.1, type=3, plot=T) }
      if(any(param=="param") | any(param==.dico[["txt_param_tests"]])){
        Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X=X, Y=NULL)
        t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(data[,X], mu=mu)->dc
        data.frame("t test"=round(ttest$statistic,3), txt_df=ttest$parameter, txt_p_dot_val=round(ttest$p.value,4), txt_ci_inferior_limit_dot=ttest$conf.int[[1]], txt_ci_superior_limit_dot=ttest$conf.int[[2]],
                   txt_r_dot_square=round(R_carre,4), txt_cohen_d=round(dc,3))->ttest
        c("t test", .dico[["txt_df"]], .dico[["txt_p_dot_val"]], .dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]], .dico[["txt_r_dot_square"]], .dico[["txt_cohen_d"]])->names(ttest)
        dimnames(ttest)[1]<-" "
        ttest->Resultats[[.dico[["txt_student_t_test_norm"]]]]
        if(!is.null(group)){
          data<-data[complete.cases(data[,group]),]
          func <- function(data, moy=mu){
            t.test(data, mu = moy)->ttest
            ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
            cohensD(data[,1], mu=moy)->dc
            current_df <- data.frame(test.t=round(ttest$statistic,3),
                              ddl=ttest$parameter,
                              valeur.p=round(ttest$p.value,4),
                              IC.inf=ttest$conf.int[[1]],
                              IC.sup=ttest$conf.int[[2]],
                              txt_r_dot_square=round(R_carre,4),
                              D.Cohen=round(dc,3))
	    names(current_df) <- c("test.t",.dico[["txt_df"]],.dico[["txt_p_dot_val"]],.dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_r_dot_square"]],.dico[["txt_cohen_d"]])
	    return(current_df)
	  }
          data.frame(data[,X])->Y

          ddply(.data=Y, .(data[,group]), func)->t.groupes
          t.groupes->Resultats[[.dico[["txt_student_t_by_group"]]]]}}

      if(any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]) ){
        if(all(param!="param") & all(param!=.dico[["txt_param_tests"]])) Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X=X, Y=NULL)

        BF<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame(txt_bayesian_factor=c(round(BF$bf,5), round((1/BF$bf),5)), txt_error=round(c( BF$error, BF$error),5))
        names(BF)<-c(.dico[["txt_bayesian_factor"]], .dico[["txt_error"]])
        dimnames(BF)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]])
        Resultats[[.dico[["txt_bayesian_factors"]]]]<-BF
        if(!is.null(group)){
          func <- function(data, moy=mu, scale=rscale){
            ttestBF(data, mu = moy, rscale=scale)->BF
            BF<-extractBF(BF, onlybf=F)
            return(data.frame(txt_bayesian_factor=round(BF$bf,5), txt_error=round(BF$error,5)))
          }
          BFgroup<-tapply(X=data[,X], data[,group], func,scale=rscale, moy=mu)
          BFgroup<-matrix(unlist(BFgroup), ncol=2, byrow=T)
          dimnames(BFgroup)<-list(levels(data[,group]), c("FB", .dico[["txt_error"]]))
          BFgroup->Resultats[[.dico[["txt_bayesian_factor_by_group"]]]]
        }
        samples<-ttestBF(x = data[,X], mu=mu , paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,"mu"])


        bfs<-c()
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=0.707)
          bfl <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1)
          bful <- ttestBF(x = data[,X][1:i], mu=mu,paired=FALSE, rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }

        SBF<-data.frame("n"=rep(5:length(data[,X]), each=3 ),"BF"= bfs,
                        "rscale"=factor(rep(c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]), length.out= 3*(length(data[,X])-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats[[.dico[["txt_bayesian_factors_sequential"]]]]<-.plotSBF(SBF)

        ##### Debut du graphique  Bayes Factor Robustness Check

        # what is the t-value for the data?
        tVal <-  t.test(data[,X], mu = mu, paired = FALSE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)

        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x)
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))

        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48",
             ylim = c(0, max(bayesFactors)), xaxt = "n",
             xlab = .dico[["txt_cauchy_prior_width"]], ylab = .dico[["txt_bayes_factor_10"]])
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")

      }

      if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])){

        wilcox.test(x= data[,X], y = NULL, alternative = alternative, mu = mu, paired = FALSE, exact = T,
                    conf.int = TRUE, conf.level = 0.95)
        WT<-wilcox.test(data[,X],y=NULL, mu=mu, alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, txt_p_dot_val=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        txt_ci_inferior_limit_dot=WT$conf.int[1],txt_ci_superior_limit_dot=WT$conf.int[2])
        names(Resultats$Wilcoxon) <- c("Wilcoxon W", .dico[["txt_p_dot_val"]], "z", "r", .dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]])

        if(!is.null(group)){
          func <- function(data,Y=X, moy=mu, alt=alternative){
            WT<-wilcox.test(data[,Y],mu=moy, alternative=alt)
            if(alt!="two.sided") abs( qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
            r<-z/(length(data[,X]))^0.5
            return(data.frame(Wilcoxon.W=WT$statistic, valeur.p=round(WT$p.value,4), z=round(z,4), r=round(r,4)))
          }

          ddply(.data=data, .(data[, group]), func)->Wilcox.groupes
          Wilcox.groupes->Resultats[[.dico[["txt_wilcoxon_by_group"]]]]
        }
      }

      if(any(param==.dico[["txt_robusts"]]| any(param==.dico[["txt_robusts_tests_with_bootstraps"]]))){
        try( round(unlist(WRS::trimci(data[,X],tr=.2,alpha=.05, null.value=mu)),4), silent=T)->m.tr
        if(class(m.tr)!='try-error'){
          names(m.tr)<-c(.dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]], .dico[["txt_truncated_m"]],"test.t", "se",.dico[["txt_p_dot_val"]],"n")
          #m.tr->Resultats$'Test sur la moyenne tronquee a 0.2'
          m.tr->Resultats[[.dico[["txt_truncated_mean_0_2"]]]]
          data[,X]->x
          try(WRS::trimcibt(x, tr=.2,alpha=.05,nboot=n.boot,plotit=T,op=3)$ci, silent=T)->trimci
          try(WRS::mestci(x,alpha=.05,nboot=n.boot,bend=1.28,os=F),silent=T)->M.estimator
          try(WRS:: momci(x,alpha=.05,nboot=n.boot),silent=T)->MoM
          IC.robustes<-data.frame()
          if(class(trimci)!='try-error') {IC.robustes<-rbind(IC.robustes,trimci)
          dimnames(IC.robustes)[[1]][1]<-.dico[["txt_bootstrap_t_method"]]}
          if(class(M.estimator)!='try-error') {IC.robustes<-rbind(IC.robustes,M.estimator$ci)
          dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator"}
          if(class(MoM)!='try-error') {IC.robustes<-rbind(IC.robustes,MoM$ci)
          dimnames(IC.robustes)[[1]][length(IC.robustes[,1])]<-"M-estimator modifie"}
          if(all(dim(IC.robustes)!=0)) names(IC.robustes )<-c(.dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]])
          Resultats[[.dico[["txt_robusts_statistics"]]]]<-IC.robustes
          c(.dico[["desc_bootstrap_t_adapt_to_truncated_mean"]],
            .dico[["desc_this_index_is_prefered_for_most_cases"]],
            .dico[["desc_truncature_on_m_estimator_adapts_to_sample"]])->Resultats$infos
        } else Resultats[[.dico[["txt_robusts_statistics"]]]]<-.dico[["desc_robusts_statistics_could_not_be_computed_verify_WRS"]]
      }

      return(Resultats)
    }
    apparies<-function(X, Y, data=NULL, param=c("param", "non param", .dico[["txt_robusts"]]),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Resultats<-list()
      .e <- environment()
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      large<-data.frame("t1"=data[which(data[,Y]==levels(data[,Y])[1]), X], "t2"=data[which(data[,Y]==levels(data[,Y])[2]), X])
      if(any(param=="param") | any(param==.dico[["txt_param_tests"]])){
        large$diff<--large$t2-large$t1
        Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=large, X="diff", Y=NULL)
        t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)->ttest
        ttest$statistic^2/( ttest$statistic^2+ ttest$parameter)->R_carre
        cohensD(x= large[,1], y=large[,2], method="paired")->dc
        data.frame("t test"= round(ttest$statistic,3), txt_df= ttest$parameter, txt_p_dot_val= round(ttest$p.value,4), txt_ci_inferior_limit_dot= ttest$conf.int[[1]],
                   txt_ci_superior_limit_dot=ttest$conf.int[[2]], txt_r_dot_square=round(R_carre,4), txt_cohen_d=round(dc,3))->ttest
        c("t test", .dico[["txt_df"]], .dico[["txt_p_dot_val"]], .dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]], .dico[["txt_r_dot_square"]], .dico[["txt_cohen_d"]])->names(ttest)
        dimnames(ttest)[1]<-" "
        ttest->Resultats[[.dico[["txt_student_t_test_paired"]]]]}
      if(any(param=="param") | any(param==.dico[["txt_param_tests"]], any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]))) {
        # realisation du graphique
        X1<-which(names(data)==X)
        nonaj<-ggplot(data)
        nonaj<- nonaj+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        # aes(x=data[,Y], y=data[,X1]))+labs(x=Y, y=X)+
        nonaj<- nonaj+ stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        nonaj<-nonaj+theme(plot.title = element_text(size = 12))+ggtitle(.dico[["txt_non_adjusted_data"]])
        # realisation du graphique ajuste propose par Loftus et Masson 1994 (pour plus d informations voir l article)
        Resultats[[.dico[["txt_mean_sd_for_non_adjusted_data"]]]]<-nonaj
        large$meanD2<-(large[ ,1]+large[ ,2])/2
        mean(large$meanD2)->GMean
        GMean-large$meanD2->large$adj
        large$adjM1<-large[ ,1]+large$adj
        large$adjM2<-large[ ,2]+large$adj
        data[,paste0(X, .dico[["txt_dot_adjusted"]])]<-c(large$adjM1,large$adjM2)

        aj<-ggplot(data)
        aj<-aj+eval(parse(text=paste0("aes(x=", Y, ", y=", names(data)[length(data)],")")))
        aj<-aj+labs(x=Y, y=X)+stat_summary(fun.y=mean, geom="bar",
                                           fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        aj<-aj+theme(plot.title = element_text(size = 12))+ggtitle(.dico[["txt_adjusted_data_loftus_masson"]])
        Resultats[[.dico[["txt_mean_sd_for_adjusted_data"]]]]<-aj
        .multiplot(nonaj,aj, cols=2 )
      }

      if(any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]) ){
        if(all(param!="param") & all(param!=.dico[["txt_param_tests"]])) Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame(txt_bayesian_factor=c(round(BF$bf,5), round((1/BF$bf),5)), txt_error=round(c( BF$error, BF$error),5))
        names(BF)<-c(.dico[["txt_bayesian_factor"]], .dico[["txt_error"]])
        dimnames(BF)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]])
        Resultats[[.dico[["txt_bayesian_factors"]]]]<-BF

        samples<-ttestBF(x=data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], y=data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X] , paired=TRUE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])


        bfs<-c()
        for (i in 5:(length(data[,X])/2)) {
          bfm <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE, rscale=0.707)
          bfl <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1)
          bful <- ttestBF(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X][1:i], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X][1:i] , paired=TRUE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }

        SBF<-data.frame("n"=rep(5:(length(data[,X])/2), each=3 ),"BF"= bfs,
                        "rscale"=factor(rep(c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]), length.out= 3*((length(data[,X])/2)-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats[[.dico[["txt_bayesian_factors_sequential"]]]]<-.plotSBF(SBF)

        ##### Debut du graphique  Bayes Factor Robustness Check

        # what is the t-value for the data?
        tVal <-  t.test(data[,X]~data[,Y], paired = TRUE, conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)

        # get the Bayes factor for each prior value
        bayesFactors <- sapply(cauchyRates, function(x)
          exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = x)[['bf']]))

        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = length(data[,X]), rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48",
             ylim = c(0, max(bayesFactors)), xaxt = "n",
             xlab = .dico[["txt_cauchy_prior_width"]], ylab = .dico[["txt_bayes_factor_10"]])
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")

      }
      if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])) {
        WT<-wilcox.test(as.formula(paste0(X, "~",Y)), paired=T,data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats$Wilcoxon<- data.frame("Wilcoxon W"=WT$statistic, txt_p_dot_val=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                        txt_ci_inferior_limit_dot=WT$conf.int[1],txt_ci_superior_limit_dot=WT$conf.int[2])
        names(Resultats$Wilcoxon)<- c("Wilcoxon W", .dico[["txt_p_dot_val"]], "z", "r", .dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]])
      }

      if(any(param==.dico[["txt_robusts"]]| any(param==.dico[["txt_robusts_tests_with_bootstraps"]])) ){
        try(WRS::yuend(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=.2),silent=T)->moy.tr
        if(class(moy.tr)!='try-error'){
          round(unlist(moy.tr),3)->moy.tr
          names(moy.tr)<-c(.dico[["txt_ci_inferior"]],.dico[["txt_ci_superior"]], .dico[["txt_p_dot_val"]], .dico[["txt_mean1"]], .dico[["txt_mean2"]], .dico[["txt_difference"]],"se", "Stat", "n", .dico[["txt_df"]])
          if(n.boot>99){
            WRS::ydbt(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X], tr=0.2, nboot=n.boot)->moy.tr.bt
            moy.tr->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_comparison_on_truncated_means"]]]]
            round(unlist(moy.tr.bt),4)->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_student_bootstrap_on_truncated_means"]]]]
            if(length(data[,1])>20) {
              try({WRS::bootdpci(data[ which(data[ ,Y]==levels(data[ ,Y])[1]) ,X], data[ which(data[ ,Y]==levels(data[ ,Y])[2]) ,X],
                                                   nboot=n.boot, BA=T)$output[,2:6]->Mest
                names(Mest)<-c(.dico[["txt_statistic"]], .dico[["txt_p_dot_val"]], "p.crit", "CI inf", "CI sup")
              Mest->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_bca_bootstrap_on_m_estimator"]]]]}
                , silent=T)
              }}} else Resultats[[.dico[["txt_robusts_statistics"]]]]<-.dico[["desc_robusts_statistics_could_not_be_computed"]]
      }





      return(Resultats)
    }
    indpdts<-function(X, Y, data, param=c("param", "non param",.dico[["txt_robusts"]]),alternative="two.sided", n.boot=NULL, rscale=0.707){
      Resultats<-list()
      .e <- environment()
      Resultats[[.dico[["txt_descriptive_statistics"]]]]<-.stat.desc.out(X=X, groupes=Y, data=data, tr=.1, type=3, plot=T)
      as.formula(paste0(X," ~ ",Y))->modele
      if(any(param=="param") | any(param==.dico[["txt_param_tests"]])){
        Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X=X, Y=Y)
        Levene<-car::leveneTest(data[ ,X], data[ ,Y]) # test de Levene pour homogeneite des variances
       Levene<- round(unlist(Levene)[c(1,2,3,5)],3)
        names(Levene)<-c(.dico[["txt_df1"]],.dico[["txt_df2"]],"F",.dico[["txt_p_dot_val"]])
       Resultats[[.dico[["txt_levene_test_verifying_homogeneity_variances"]]]]<- Levene
       student<- t.test(modele, data=data, alternative=alternative,  var.equal=TRUE, conf.level=0.95)
       R.deux<- round(student$statistic^2/(student$statistic^2+student$parameter),3)
       d_cohen<-round(cohensD(modele , data=data, method = "pooled"),3)
       student<- data.frame(student[9], round(student$statistic,3), student$parameter, round(student$p.value,3), round(student$conf.int[1],4),
                   round(student$conf.int[2],4),  R.deux, d_cohen)
        corrige<-t.test(modele, data=data, alternative=alternative,  var.equal=FALSE, conf.level=0.95)
        R.deux.corr<-corrige$statistic^2/(corrige$statistic^2+corrige$parameter)
        d_cohen.corr<-cohensD(modele , data=data, method = "unequal")
        data.frame(corrige[9], round(corrige$statistic,3), round(corrige$parameter,3), round(corrige$p.value,3), round(corrige$conf.int[1],4),
                   round(corrige$conf.int[2],4),  R.deux, d_cohen)->corrige
        names(student)<-c("modele", "test t", .dico[["txt_df"]], .dico[["txt_p_dot_val"]], .dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]],.dico[["txt_r_dot_square"]],.dico[["txt_cohen_d"]])
        names(corrige)<- c("modele", "test t", .dico[["txt_df"]], .dico[["txt_p_dot_val"]], .dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]],.dico[["txt_r_dot_square"]],.dico[["txt_cohen_d"]])
        student<-rbind(student, corrige)
        dimnames(student)[[1]]<-c(.dico[["txt_without_welch_correction"]],.dico[["txt_with_welch_correction"]])
        student->Resultats[[.dico[["txt_student_t_independant"]]]]
        p<-ggplot(data)
        p<-p+eval(parse(text=paste0("aes(x=", Y, ", y=", X,")")))
        p<-p+  stat_summary(fun.y=mean, geom="bar",fill="grey", colour="White")+stat_summary(fun.data="mean_sdl", geom="errorbar", position=position_dodge(width=0.90), width=0.2)
        Resultats[[.dico[["txt_graphic_mean_sd"]]]]<-p

      }
      if(any(param=="Bayes") | any(param==.dico[["txt_bayesian_factors"]]) ){
        if(all(param!="param") & all(param!=.dico[["txt_param_tests"]])) Resultats[[.dico[["txt_normality_tests"]]]]<-.normalite(data=data, X=X, Y=Y)
        BF<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale)
        BF<-extractBF(BF, onlybf=F)
        BF<-data.frame(txt_bayesian_factor=c(round(BF$bf,5), round((1/BF$bf),5)), txt_error=round(c( BF$error, BF$error),5))
        names(BF)<-c(.dico[["txt_bayesian_factor"]], .dico[["txt_error"]])
        dimnames(BF)[[1]]<-c(.dico[["txt_supports_alternative"]], .dico[["txt_supports_null"]])
        Resultats[[.dico[["txt_bayesian_factors"]]]]<-BF

        samples<-ttestBF(formula=modele,data=data, paired=FALSE, rscale=rscale, posterior=T, iterations = ifelse(is.null(n.boot), 1000, n.boot))
        plot(samples[,1:4])


        bfs<-c()
        tab<-table(data[,Y])
        data1<-data.frame(X=c(data[which(data[,Y]==levels(data[,Y])[1] ),X], data[which(data[,Y]==levels(data[,Y])[2] ),X]), id=c(1:tab[1],1:tab[2]),
                          Y=c(rep(levels(data[,Y])[1], tab[1]), rep(levels(data[,Y])[2], tab[2])))
        data1<-data1[order(data1$id),]
        for (i in 5:length(data[,X])) {
          bfm <- ttestBF(formula=X~Y,data=data1[1:i,], paired=FALSE, rscale=0.707)
          bfl <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1)
          bful <- ttestBF(formula=X~Y,data=data1[1:i,] , paired=FALSE,  rscale=1.41)
          bfs<-c(bfs, extractBF(bfm, onlybf=T), extractBF(bfl, onlybf=T), extractBF(bful, onlybf=T))
        }

        SBF<-data.frame("n"=rep(5:(length(data[,X])), each=3 ),"BF"= bfs,
                        "rscale"=factor(rep(c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]), length.out= 3*(length(data[,X])-4) )))
        names(SBF)<-c("n", "BF", "rscale")
        reorder( c("moyen", .dico[["txt_large"]], .dico[["txt_ultrawide"]]),levels(SBF$rscale))->levels(SBF$rscale)
        Resultats[[.dico[["txt_bayesian_factors_sequential"]]]]<-.plotSBF(SBF)

        ##### Debut du graphique  Bayes Factor Robustness Check

        # what is the t-value for the data?
        tVal <-  t.test(formula=modele, data=data,  conf.level = 0.95, alternative=alternative)$statistic
        # how many points in the prior should be explored?
        nPoints <- 1000
        # what Cauchy rates should be explored?
        cauchyRates <- seq(from = 0.01, to = 1.5, length.out = 1000)
        # what effect sizes should be plotted?
        effSize <- seq(from = -2, to = 2, length.out = 1000)

        # get the Bayes factor for each prior value

        bayesFactors <- sapply(cauchyRates, function(x)
          exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = x)[['bf']]))

        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 0.707)[['bf']])->r1
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1)[['bf']])->r2
        exp(ttest.tstat(t = tVal, n1 = tab[1], n2=tab[2], rscale = 1.41)[['bf']])->r3
        plotWidth <- round(seq(from = 1, to = nPoints, length.out = 1), 0)
        # do the Bayes factor plot
        plot(cauchyRates, bayesFactors, type = "l", lwd = 2, col = "gray48",
             ylim = c(0, max(bayesFactors)), xaxt = "n",
             xlab = .dico[["txt_cauchy_prior_width"]], ylab = .dico[["txt_bayes_factor_10"]])
        abline(h = 0, lwd = 1)
        abline(h = 6, col = "black", lty = 2, lwd = 2)
        axis(1, at = seq(0, 1.5, 0.25))
        # add the BF at the default Cauchy point
        points(0.707, r1, col = "black", cex = 1.5, pch = 21, bg = "black")
        points(1, r2, col = "black", pch = 21, cex = 1.3, bg = "gray")
        points(2^0.5, r3, col = "black", pch = 21, cex = 1.3, bg = "white")
        # add legend
        legend(x="topright", legend = c("r = 0.707 - medium", "r = 1 - wide ", "r = 1.41 - ultrawide"),
               pch = c(21, 21), lty = c(NA, NA), lwd = c(NA, NA), pt.cex = c(1, 1),
               col = c("black", "black"), pt.bg = c("black", "gray", "white"), bty = "n")

      }
      if(any(param=="non param")| any(param==.dico[["txt_non_parametric_test"]])) {
        WT<-wilcox.test(modele, data=data, alternative=alternative, conf.int=T, conf.level=0.95)
        if(alternative!="two.sided")  abs(qnorm(WT$p.value))->z else abs(qnorm(WT$p.value/2))->z
        r<-z/(length(data[,X]))^0.5
        Resultats[[.dico[["txt_mann_whitney_test"]]]]<- data.frame("Wilcoxon W"=WT$statistic, txt_p_dot_val=round(WT$p.value,4), "z"=round(z,4), "r"=round(r,4),
                                                                 txt_ci_inferior_limit_dot=WT$conf.int[1],txt_ci_superior_limit_dot=WT$conf.int[2])
        names(Resultats[[.dico[["txt_mann_whitney_test"]]]])<- c("Wilcoxon W", .dico[["txt_p_dot_val"]], "z", "r", .dico[["txt_ci_inferior_limit_dot"]],.dico[["txt_ci_superior_limit_dot"]])
      }

      if(any(param==.dico[["txt_robusts"]]| any(param==.dico[["txt_robusts_tests_with_bootstraps"]])) ){
        data[which(data[,Y]==levels(data[,Y])[1]),]->g1 # on cree une base de Donnees avec le groupe 1 uniquement (sans valeur aberrantes)
        data[which(data[,Y]==levels(data[,Y])[2]),]->g2 # on cree une base de Donnees avec le groupe 2 uniquement (sans valeur aberrantes)
        try(WRS::yuen(g1[,X],g2[,X]), silent=T)->yuen.modele### fournit la probabilite associee a des moyennes tronquees.Par defaut, la troncature est de 0.20
        if(class(yuen.modele)!='try-error'){
          round(unlist(yuen.modele),4)->yuen.modele
          cbind(yuen.modele[1:2], yuen.modele[3:4])->yuen.desc
          dimnames(yuen.desc)[[1]]<-levels(data[,Y])
          dimnames(yuen.desc)[[2]]<-c("n", .dico[["txt_truncated_means"]])
          yuen.desc->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_descriptive_statistics"]]]]

          yuen.modele[c(5,6,8,9,10,11,12,7)]->yuen.modele
          names(yuen.modele)<-c(.dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]],
                                .dico[["txt_difference"]],"Err-type","Stat", .dico[["txt_threshold"]], .dico[["txt_df"]],.dico[["txt_p_dot_val"]])
          yuen.modele->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_analysis_on_truncated_means"]]]]
          if(n.boot>99){
            WRS2::yuenbt(modele, data= data, nboot=n.boot, side=T)->yuen.bt.modele ### fournit la probabilite associee a des moyennes tronquees apres un bootstrap.
            yuen.bt.modele<-round(data.frame(test = yuen.bt.modele$test,
                                             ddl = yuen.bt.modele$df,
                                             valeur.p = yuen.bt.modele$p.value,
                                             lim.inf.IC = yuen.bt.modele$conf.int[1],
                                             lim.sup.IC = yuen.bt.modele$conf.int[2]),3)
            yuen.bt.modele->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_bootstrap_t_method_on_truncated_means"]]]]
            WRS::pb2gen(g1[,X],g2[,X], nboot=n.boot)->pb2gen.modele### calcule le bootstrap sur le M-estimateur et fournit l intervalle de confiance.
            round(unlist(pb2gen.modele)[1:6],4)->pb2gen.modele
            names(pb2gen.modele)<-c("M.estimator.G1", "M.estimator.G2", "diff", .dico[["txt_ci_inferior_limit_dot"]], .dico[["txt_ci_superior_limit_dot"]], .dico[["txt_p_dot_val"]])
            pb2gen.modele->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_percentile_bootstrap_on_m_estimators"]]]]
            Resultats[[.dico[["txt_robusts_statistics"]]]]$Informations<-c(.dico[["desc_percentile_bootstrap_prefered_for_small_samples"]],
                                               .dico[["desc_for_bigger_samples_bootstrap_t_prefered"]])
          }

          easieR::ks(g1[,X],g2[,X],w=F,sig=T)->KS
          round(unlist(KS),4)->KS
          names(KS)<-c("KS", .dico[["txt_critical_dot_threshold"]],.dico[["txt_p_dot_val"]])
          KS->Resultats[[.dico[["txt_robusts_statistics"]]]][[.dico[["txt_kolmogorov_smirnov_comparing_two_distrib"]]]]
        }else Resultats[[.dico[["txt_robusts_statistics"]]]]<-.dico[["desc_robusts_statistics_could_not_be_computed_verify_WRS"]]


      }

      return(Resultats)
    }
    data_summary <- function(x) {
      m <- mean(x)
      ymin <- m-sd(x)
      ymax <- m+sd(x)
      return(c(y=m,ymin=ymin,ymax=ymax))
    }
    #### 5 fonctions qui seront appelees pour realiser l'analyse
    options (warn=-1)
    # chargement des packages
    packages<-c('BayesFactor', 'svDialogs', 'outliers', 'nortest','psych', 'lsr','ggplot2', 'reshape2', 'car', 'plyr')
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())
    try(library("WRS"),silent=T)
    .e <- environment()
    Resultats<-list()
    try( windows(record=T), silent=T)->win
    if(class(win)=='try-error') quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data
    test.t.options<-test.t.in(X=X, Y=Y, data=data, choix=choix, param=param, outlier=outlier, sauvegarde=sauvegarde, info=info, group=group,alternative=alternative,
                              formula=formula,n.boot=n.boot, rscale=rscale, mu=mu)
    if(is.null(test.t.options)) return(analyse())

    choix<-test.t.options$choix
    X<-test.t.options$X
    Y<-test.t.options$Y
    mu<-test.t.options$mu
    group<-test.t.options$group
    data<-test.t.options$data
    alternative<-test.t.options$alternative
    group<-test.t.options$group
    param<-test.t.options$options$choix
    rscale<-test.t.options$options$rscale
    n.boot<-test.t.options$options$n.boot
    sauvegarde<-test.t.options$options$sauvegarde
    outlier<-test.t.options$options$desires

    for(i in 1 : length(X)) {


      if(choix==.dico[["txt_two_paired_samples"]]){
        diffs<-data[which(is.na(data[,X])), "IDeasy"]
        if(length(diffs)==0) data->data1 else data[which(data$IDeasy!=diffs), ]->data1
      } else  {
        data1<-data[complete.cases(data[,c(Y,X[i])]),]
      }




      X1<-X[i]
      R1<-list()
      if(any(outlier==  .dico[["txt_complete_dataset"]])){
        if (choix==.dico[["txt_comparison_to_norm"]]) {
		R1[[.dico[["txt_complete_dataset"]]]]<-norme(X=X1, mu=mu, data=data1, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale)
	}
        if (choix==.dico[["txt_two_paired_samples"]]) {
		R1[[.dico[["txt_complete_dataset"]]]]<-apparies(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale)
	}
        if (choix==.dico[["txt_two_independant_samples"]])	{
		R1[[.dico[["txt_complete_dataset"]]]]<-indpdts(X=X1, Y=Y, data=data1, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale)
	}
      }

      if(any(outlier==.dico[["txt_identifying_outliers"]])|any(outlier==.dico[["txt_without_outliers"]])){
        if(choix==.dico[["txt_comparison_to_norm"]]) {
          if(class(data1)!="data.frame"){
		  data1<-data.frame(data1)
                  names(data1)[1]<-X1
	  }
          data1$residu<-data1[,X1]
	} else {
		data1$residu<-unlist(tapply(data1[,X1], data1[,Y], scale, center=T, scale=F))
	}
        critere<-ifelse(is.null(z), "Grubbs", "z")
        valeurs.influentes(X='residu', critere=critere,z=z, data=data1)->influentes
      }
      if(any(outlier== .dico[["txt_identifying_outliers"]])){influentes->R1[[.dico[["txt_outliers_values"]]]]}
      if(any(outlier== .dico[["txt_without_outliers"]])) {
        if(influentes[[.dico[["txt_outliers_synthesis"]]]][[.dico[["txt_synthesis"]]]][1]!=0 | all(outlier!=.dico[["txt_complete_dataset"]])){
          if(choix==.dico[["txt_two_paired_samples"]]){
            setdiff(data$IDeasy,influentes[[.dico[["txt_outliers"]]]]$IDeasy)->diffs
            data[which(data$IDeasy%in%diffs), ]->nettoyees
          } else  get('nettoyees', envir=.GlobalEnv)->nettoyees

          ### Regler le souci pour les echantillons apparies
          if (choix==.dico[["txt_comparison_to_norm"]]) {
		  R1[[.dico[["txt_without_outliers"]]]]<-norme(X=X1, mu=mu, data=nettoyees, param=param, group=group, alternative=alternative, n.boot=n.boot, rscale=rscale)
	  }
	  if (choix==.dico[["txt_two_paired_samples"]]) {
		  R1[[.dico[["txt_without_outliers"]]]]<-apparies(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale)
	  }
	  if (choix==.dico[["txt_two_independant_samples"]]) {
		  R1[[.dico[["txt_without_outliers"]]]]<-indpdts(X=X1, Y=Y, data=nettoyees, param=param,alternative=alternative, n.boot=n.boot, rscale=rscale)
	  }
        }
      }
      Resultats[[i]]<-R1
    }

    names(Resultats)<-paste(.dico[["txt_analysis_on_variable"]], X)

    paste(unique(X), collapse="','", sep="")->X
    paste(outlier,  collapse="','", sep="")->outlier
    paste(param,  collapse="','", sep="")->param
    Resultats$Call<-paste0("test.t(X=c('", X,
                           "'), Y=", ifelse(!is.null(Y),paste0("'",Y,"'"), "NULL"),
                           ",group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"),
                           ", choix='", choix,
                           "', sauvegarde = ", sauvegarde, ",outlier=c('", outlier, "'),z=", ifelse(!is.null(z),z, "NULL"),
                           ", data=", test.t.options$nom, ",alternative='", alternative, "', mu=", ifelse(!is.null(mu),mu, "NULL"),
                           ",formula =NULL, n.boot=", ifelse(is.null(n.boot), "NULL", n.boot), ",param=c('", param, "'),info=T, rscale=", rscale, ")"
    )
    .add.history(data=data, command=Resultats$Call, nom=test.t.options$nom)
    .add.result(Resultats=Resultats, name =paste(choix, Sys.time() ))

    if(sauvegarde){save(Resultats=Resultats ,choix =choix, env=.e)}

    ref1(packages)->Resultats[[.dico[["txt_references"]]]]
    if(html) ez.html(Resultats)
    ### Obtenir les Resultats
    return(Resultats)

    }
ksties.crit<-function(x,y,alpha=.05){
  #
  # Compute a critical value so that probability coverage is approximately
  # 1-alpha 
  #
  n1<-length(x)
  n2<-length(y)
  START=sqrt(0-log(alpha/2)*(n1+n2)/(2*n1*n2))
  crit=optim(START,ksties.sub,x=x,y=y,alpha=alpha,lower=.001,upper=.86,method='Brent')$par
  crit
}



			       
ks<-function(x,y,w=FALSE,sig=TRUE,alpha=.05){
  #  Compute the Kolmogorov-Smirnov test statistic
  #
  #  w=T computes the weighted version instead.
  #
  #  sig=T indicates that the exact  level is to be computed.
  #  If there are ties, the reported Type I  error probability is exact when
  #  using the unweighted test, but for the weighted test the reported
  #  level is too high.
  #
  #  This function uses the functions ecdf, kstiesig, kssig and kswsig
  #
  #  This function returns the value of the test statistic, the approximate .05
  #  critical value, and the exact level if sig=T.
  #
  #  Missing values are automatically removed
  #
  x<-x[!is.na(x)]
  y<-y[!is.na(y)]
  n1 <- length(x)
  n2 <- length(y)
  w<-as.logical(w)
  sig<-as.logical(sig)
  tie<-logical(1)
  siglevel<-NA
  z<-sort(c(x,y))  # Pool and sort the observations
  tie=FALSE
  chk=sum(duplicated(x,y))
  if(chk>0)tie=TRUE
  v<-1   # Initializes v
  for (i in 1:length(z))v[i]<-abs(ecdf(x,z[i])-ecdf(y,z[i]))
  ks<-max(v)
  if(!tie)crit=ks.crit(n1=n1,n2=n2,alpha=alpha)
  else crit=ksties.crit(x,y,alpha=alpha)
  if(!w && sig && !tie)siglevel<-kssig(length(x),length(y),ks)
  if(!w && sig && tie)siglevel<-kstiesig(x,y,ks)
  if(w){
    crit=ksw.crit(length(x),length(y),alpha=alpha)
    for (i in 1:length(z)){
      temp<-(length(x)*ecdf(x,z[i])+length(y)*ecdf(y,z[i]))/length(z)
      temp<-temp*(1.-temp)
      v[i]<-v[i]/sqrt(temp)
    }
    v<-v[!is.na(v)]
    ks<-max(v)*sqrt(length(x)*length(y)/length(z))
    if(sig)siglevel<-kswsig(length(x),length(y),ks)
    if(tie && sig)
      warning(paste("Ties were detected. The reported significance level of the
weighted Kolmogorov-Smirnov test statistic is not exact."))
  }
  list(test=ks,critval=crit,p.value=siglevel)
}

ks.crit<-function(n1,n2,alpha=.05){
  #
  # Compute a critical value so that probability coverage is approximately
  # 1-alpha 
  #
  START=sqrt(0-log(alpha/2)*(n1+n2)/(2*n1*n2))
  crit=optim(START,ks.sub,n1=n1,n2=n2,alpha=alpha,lower=.001,upper=.86,method='Brent')$par
  crit
}

ks.sub<-function(crit,n1,n2,alpha){
  v=kssig(n1,n2,crit)
  dif=abs(alpha-v)
  dif
}


ksw.crit<-function(n1,n2,alpha=.05){
  #
  # Compute a critical value so that probability coverage is 
  # >= 1-alpha while being close as possible to 1-alpha
  #
  if(alpha>.1)stop('The function assumes alpha is at least .1')
  crit=2.4
  del=.05
  pc=.12
  while(pc>alpha){
    crit=crit+.05
    pc=kswsig(n1,n2,crit)
  }
  crit
}

ecdf<-function(x,val){
#  compute empirical cdf for data in x evaluated at val
#  That is, estimate P(X <= val)
#
ecdf<-length(x[x<=val])/length(x)
ecdf
}

kswsig<-function(m,n,val){
#
#    Compute significance level of the weighted
#    Kolmogorov-Smirnov test statistic
#
#    m=sample size of first group
#    n=sample size of second group
#    val=observed value of test statistic
#
mpn<-m+n
cmat<-matrix(0,m+1,n+1)
umat<-matrix(0,m+1,n+1)
for (i in 1:m-1){
for (j in 1:n-1)cmat[i+1,j+1]<-abs(i/m-j/n)*sqrt(m*n/((i+j)*(1-(i+j)/mpn)))
}
cmat<-ifelse(cmat<=val,1,0)
for (i in 0:m){
for (j in 0:n)if(i*j==0)umat[i+1,j+1]<-cmat[i+1,j+1]
else umat[i+1,j+1]<-cmat[i+1,j+1]*(umat[i+1,j]+umat[i,j+1])
}
term<-lgamma(m+n+1)-lgamma(m+1)-lgamma(n+1)
kswsig<-1.-umat[m+1,n+1]/exp(term)
kswsig
}

kstiesig<-function(x,y,val){
  #
  #    Compute significance level of the  Kolmogorov-Smirnov test statistic
  #    for the data in x and y.
  #    This function allows ties among the  values.
  #    val=observed value of test statistic
  #
  m<-length(x)
  n<-length(y)
  z<-c(x,y)
  z<-sort(z)
  cmat<-matrix(0,m+1,n+1)
  umat<-matrix(0,m+1,n+1)
  for (i in 0:m){
    for (j in 0:n){
      if(abs(i/m-j/n)<=val)cmat[i+1,j+1]<-1e0
      k<-i+j
      if(k > 0 && k<length(z) && z[k]==z[k+1])cmat[i+1,j+1]<-1
    }
  }
  for (i in 0:m){
    for (j in 0:n)if(i*j==0)umat[i+1,j+1]<-cmat[i+1,j+1]
    else umat[i+1,j+1]<-cmat[i+1,j+1]*(umat[i+1,j]+umat[i,j+1])
  }
  term<-lgamma(m+n+1)-lgamma(m+1)-lgamma(n+1)
  kstiesig<-1.-umat[m+1,n+1]/exp(term)
  kstiesig
}


kssig<-function(m,n,val){
  #
  #    Compute significance level of the  Kolmogorov-Smirnov test statistic
  #    m=sample size of first group
  #    n=sample size of second group
  #    val=observed value of test statistic
  #
  cmat<-matrix(0,m+1,n+1)
  umat<-matrix(0,m+1,n+1)
  for (i in 0:m){
    for (j in 0:n)cmat[i+1,j+1]<-abs(i/m-j/n)
  }
  cmat<-ifelse(cmat<=val,1e0,0e0)
  for (i in 0:m){
    for (j in 0:n)if(i*j==0)umat[i+1,j+1]<-cmat[i+1,j+1]
    else umat[i+1,j+1]<-cmat[i+1,j+1]*(umat[i+1,j]+umat[i,j+1])
  }
  term<-lgamma(m+n+1)-lgamma(m+1)-lgamma(n+1)
  kssig<-1.-umat[m+1,n+1]/exp(term)
  kssig=max(0,kssig)
  kssig
}

ksties.crit<-function(x,y,alpha=.05){
  #
  # Compute a critical value so that probability coverage is approximately
  # 1-alpha 
  #
  n1<-length(x)
  n2<-length(y)
  START=sqrt(0-log(alpha/2)*(n1+n2)/(2*n1*n2))
  crit=optim(START,ksties.sub,x=x,y=y,alpha=alpha,lower=.001,upper=.86,method='Brent')$par
  crit
}

ksties.sub<-function(crit,x,y,alpha){
  v=kstiesig(x,y,crit)
  dif=abs(alpha-v)
  dif
}
