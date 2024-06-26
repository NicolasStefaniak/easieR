analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c(.dico[["txt_descriptive_statistics"]],.dico[["txt_chi_squared"]],.dico[["txt_correlations"]],
              .dico[["txt_student_t"]], .dico[["txt_anova_ancova"]],
              .dico[["txt_regressions"]],
              .dico[["txt_analysis_factor_component"]],
              .dico[["txt_fiability_analysis"]]), preselect=NULL, multiple = FALSE, title=.dico[["ask_analysis_type"]])$res->choix
    if(length(choix)==0) return(easieR())
    if(choix==.dico[["txt_chi_squared"]]) chi(html=html)->Resultats
    if(choix==.dico[["txt_student_t"]]) test.t(html=html)->Resultats
    if(choix==.dico[["txt_anova_ancova"]]) {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
	      ez.anova(html=html)->Resultats
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
    if(choix==.dico[["txt_correlations"]]) choix.corr(html=html)->Resultats
    if(choix==.dico[["txt_regressions"]]) choix.reg(html=html)->Resultats
    #if(choix==.dico[["txt_logistic_regressions"]]) regressions.log()->Resultats
    if(choix==.dico[["txt_analysis_factor_component"]]) factor.an(html=html)->Resultats
    if(choix==.dico[["txt_fiability_analysis"]]) fiabilite(html=html)->Resultats
    if(choix==.dico[["txt_descriptive_statistics"]]) stat.desc(html=html)->Resultats
    return(Resultats)
  }
