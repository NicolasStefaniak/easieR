analyse <-
  function(html=T){options (warn=-1)
    require(svDialogs)
    dlgList(c(txt_descriptive_statistics,txt_chi_squared,txt_correlations,
              txt_student_t, txt_anova_ancova,
              txt_regressions,
              txt_analysis_factor_component,
              txt_fiability_analysis), preselect=NULL, multiple = FALSE, title=ask_analysis_type)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix==txt_chi_squared) chi(html=html)->Resultats
    if(choix==txt_student_t) test.t(html=html)->Resultats
    if(choix==txt_anova_ancova) {
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
	      ez.anova(html=html)->Resultats
      } else {
	      html=html
	      dlgList(c(txt_principal_analysis,
			txt_complementary_results),
		        preselect=NULL,
			multiple = FALSE,
			title=ask_analysis_type)$res->choix
	      if(choix==txt_principal_analysis) {
		      ez.anova(html=html)->Resultats
	      } else {
		      aov.plus(html=html)->Resultats
	      }
      }
    }
    if(choix==txt_correlations) choix.corr(html=html)->Resultats
    if(choix==txt_regressions) choix.reg(html=html)->Resultats
    #if(choix==txt_logistic_regressions) regressions.log()->Resultats
    if(choix==txt_analysis_factor_component) factor.an(html=html)->Resultats
    if(choix==txt_fiability_analysis) fiabilite(html=html)->Resultats
    if(choix==txt_descriptive_statistics) stat.desc(html=html)->Resultats
    return(Resultats)
  }
