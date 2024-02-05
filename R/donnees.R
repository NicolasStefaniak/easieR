donnees <-
  function(){options (warn=-1)
    require(svDialogs)
    choix<- c(txt_import_data, txt_view_data, txt_import_results,txt_export_data, txt_compile_report)
    if( 'RGtk2Extras' %in% installed.packages()) choix<-c(txt_new_data_set, choix)
    title<-ask_what_to_do
    dlgList(choix, preselect=NULL, multiple = FALSE,
            title=title)$res->choix
    if(length(choix)==0) return(easieR())
    if(choix %in% c(txt_new_data_set, txt_new_data_set)) blank.data()->Resultats
    if(choix %in% c(txt_view_data, txt_view_data)) voir()->Resultats
    if(choix %in% c(txt_import_results, txt_import_results)) import.results()->Resultats
    if(choix %in% c(txt_import_data, txt_import_data) ) import()->Resultats
    if(choix %in% c(txt_export_data, txt_export_data)) exporterD()->Resultats
    if(choix %in% c(txt_compile_report, txt_compile_report)) {
	    ez.report()
            Resultats<-NULL
    }
    return(Resultats)
  }

