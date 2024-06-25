preprocess <-
  function(){
    choix<-dlgList(c(.dico[["txt_ranks_upper"]], .dico[["txt_imput_missing_values"]],
                     .dico[["txt_select_obs"]],.dico[["txt_select_variables"]],.dico[["txt_center_or_center_reduce"]],.dico[["txt_order"]],
                     .dico[["txt_mathematical_operations_on_variables"]],.dico[["txt_dynamic_crossed_table"]],
                    .dico[["txt_long_or_large_format"]]), multiple = F, preselect=.dico[["txt_ranks_lower"]], title=.dico[["ask_what_to_do"]])$res
    if(length(choix)==0) return(easieR())
    if (choix==.dico[["txt_ranks_upper"]])  ez.rank()->Resultats
    if (choix==.dico[["txt_imput_missing_values"]]) ez.imp()->Resultats
    if (choix==.dico[["txt_select_obs"]]) selectionO()->Resultats
    if (choix==.dico[["txt_select_variables"]]) SelectionV()->Resultats
    if (choix==.dico[["txt_center_or_center_reduce"]])  Centrer.red()->Resultats
    if (choix==.dico[["txt_order"]])  trier()->Resultats
    if (choix==.dico[["txt_mathematical_operations_on_variables"]])  maths()->Resultats
    if (choix==.dico[["txt_long_or_large_format"]]) ez.reshape()->Resultats
    if (choix==.dico[["txt_dynamic_crossed_table"]]) {
      try(library('rpivotTable'), silent=T)->test2
      if(class(test2)== 'try-error') return(ez.install())
      return( rpivotTable(choix.data(nom=F)))
    }
    return(Resultats)
  }
