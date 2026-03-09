analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c(.dico[["txt_descriptive_statistics"]],.dico[["txt_chi_squared"]],.dico[["txt_correlations"]],
              .dico[["txt_student_t"]], .dico[["txt_anova_ancova"]],
              .dico[["txt_regressions"]],
              .dico[["txt_analysis_factor_component"]],
              .dico[["txt_fiability_analysis"]]), preselect=NULL, multiple = FALSE, title=.dico[["ask_analysis_type"]])$res->choix
    if(length(choix)==0) return(easieR())
    if(choix==.dico[["txt_chi_squared"]]) Resultats<-chi(html=html)
    if(choix==.dico[["txt_student_t"]]) Resultats<-test.t(html=html)
    if(choix==.dico[["txt_anova_ancova"]]) {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
	      Resultats<-ez.anova(html=html)
      } else {
	      html=html
	      dlgList(c(.dico[["txt_principal_analysis"]],
			.dico[["txt_complementary_results"]]),
		        preselect=NULL,
			multiple = FALSE,
			title=.dico[["ask_analysis_type"]])$res->choix
	      if(choix==.dico[["txt_principal_analysis"]]) {
		      ez.anova(html=html)->Resultats
	      } else {
		      aov.plus(html=html)->Resultats
	      }
      }
    }
    if(choix==.dico[["txt_correlations"]]) Resultats<-choix.corr(html=html)
    if(choix==.dico[["txt_regressions"]]) Resultats<-choix.reg(html=html)
    #if(choix==.dico[["txt_logistic_regressions"]]) Resultats<-regressions.log()
    if(choix==.dico[["txt_analysis_factor_component"]]) Resultats<-factor.an(html=html)
    if(choix==.dico[["txt_fiability_analysis"]]) Resultats<-fiabilite(html=html)
    if(choix==.dico[["txt_descriptive_statistics"]]) Resultats<-stat.desc(html=html)
    return(Resultats)
  }
