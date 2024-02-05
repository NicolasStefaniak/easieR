preprocess <-
  function(){
    choix<-dlgList(c(txt_ranks_upper, txt_imput_missing_values,
                     txt_select_obs,txt_select_variables,txt_center_or_center_reduce,txt_order,
                     txt_mathematical_operations_on_variables,txt_dynamic_crossed_table,
                    txt_long_or_large_format), multiple = F, preselect=txt_ranks_lower, title=ask_what_to_do)$res
    if(length(choix)==0) return(easieR())
    if (choix==txt_ranks_upper)  ez.rank()->Resultats
    if (choix==txt_imput_missing_values) ez.imp()->Resultats
    if (choix==txt_select_obs) selectionO()->Resultats
    if (choix==txt_select_variables) SelectionV()->Resultats
    if (choix==txt_center_or_center_reduce)  Centrer.red()->Resultats
    if (choix==txt_order)  trier()->Resultats
    if (choix==txt_mathematical_operations_on_variables)  maths()->Resultats
    if (choix==txt_long_or_large_format) ez.reshape()->Resultats
    if (choix==txt_dynamic_crossed_table) {
      try(library('rpivotTable'), silent=T)->test2
      if(class(test2)== 'try-error') return(ez.install())
      return( rpivotTable(choix.data(nom=F)))
    }
    return(Resultats)
  }
