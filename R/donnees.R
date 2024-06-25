donnees <-
  function(){options (warn=-1)
    require(svDialogs)
    choix<- c(.dico[["txt_import_data"]], .dico[["txt_view_data"]], .dico[["txt_import_results"]],.dico[["txt_export_data"]], .dico[["txt_compile_report"]])
    if( 'RGtk2Extras' %in% installed.packages()) choix<-c(.dico[["txt_new_data_set"]], choix)
    title<-.dico[["ask_what_to_do"]]
    dlgList(choix, preselect=NULL, multiple = FALSE,
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c(.dico[["txt_new_data_set"]], .dico[["txt_new_data_set"]])) blank.data()->Resultats
    if(choix %in% c(.dico[["txt_view_data"]], .dico[["txt_view_data"]])) voir()->Resultats
    if(choix %in% c(.dico[["txt_import_results"]], .dico[["txt_import_results"]])) import.results()->Resultats
    if(choix %in% c(.dico[["txt_import_data"]], .dico[["txt_import_data"]]) ) import()->Resultats
    if(choix %in% c(.dico[["txt_export_data"]], .dico[["txt_export_data"]])) exporterD()->Resultats
    if(choix %in% c(.dico[["txt_compile_report"]], .dico[["txt_compile_report"]])) {
	    ez.report()
            Resultats<-NULL
    }
    return(Resultats)
  }

