#choix.corr <-
#  function(html=T){options (warn=-1)
#    c( "svDialogs")->packages
#    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
#      require(packages)}
#    writeLines("l'analyse detaillee permet d'avoir les statistiques descriptives, les tests de normalite, le nuage de points,
#               \n des statistiques robustes, l'ensemble des coefficients de correlations.
#               \n la matrice de correlation permet de contrôler l'erreur de 1e espece et est adaptee pour un grand nombre de correlations
#               \n la comparaison de correlations permet de comparer 2 correlations dependantes ou independantes
#               \n Le choix + autre correlations + permet d'avoir les correlation tetrachoriques et polychoriques")
#    dlgList(c(.dico[["txt_detailed_corr_analysis"]],
#              .dico[["txt_correlations_matrix"]],
#              .dico[["txt_compare_two_correlations"]],
#              .dico[["txt_other_correlations"]]), preselect=NULL, multiple = FALSE, title=.dico[["ask_which_analysis"]])$res->choix
#    if(length(choix)==0) return(analyse())
#    switch(choix,
#           txt_detailed_corr_analysis=corr.complet(html=html)->Resultats,
#           txt_correlations_matrix= corr.matrice(html=html)->Resultats,
#           txt_compare_two_correlations= comp.corr(html=html)->Resultats,
#           txt_other_correlations= tetrapoly(html=html)->Resultats
#    )
#    return(Resultats)
#  }
choix.corr <-
  function(html=T){options (warn=-1)
    c('svDialogs')->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    writeLines(.dico[["desc_corr_detailed_analysis"]])
    dlgList(c(.dico[["txt_detailed_corr_analysis"]],
              .dico[["txt_correlations_matrix"]],
              .dico[["txt_compare_two_correlations"]],
              .dico[["txt_other_correlations"]]), preselect=NULL, multiple = FALSE, title=.dico[["ask_which_analysis"]])$res->choix
    if(length(choix)==0) return(analyse())

    if(choix==.dico[["txt_detailed_corr_analysis"]]) corr.complet(html=html)->Resultats
    if(choix==.dico[["txt_correlations_matrix"]]) corr.matrice(html=html)->Resultats
    if(choix==.dico[["txt_compare_two_correlations"]]) comp.corr(html=html)->Resultats
    if(choix==.dico[["txt_other_correlations"]]) tetrapoly(html=html)->Resultats
    return(Resultats)
  }
