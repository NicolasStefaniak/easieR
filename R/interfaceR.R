interfaceR <-
  function(){
    options (warn=-1)
    packages <- c('svDialogs')
    lapply(packages, require,character.only=T)
    Resultats <- list()



    choix <- dlgList(c(.dico[["txt_get_working_dir"]],
		       .dico[["txt_specify_working_dir"]],
		       .dico[["txt_remove_object_in_memory"]],
                       .dico[["txt_list_of_objects_in_mem"]],
		       .dico[["txt_search_for_new_function"]],
		       .dico[["txt_packages_update"]],
		       .dico[["txt_verify_packages_install"]],
		       .dico[["txt_select_language"]]
		       ), preselect=NULL, multiple = FALSE, title=.dico[["ask_what_is_your_choice"]])$res
    while(length(choix)==0) return(easieR())


           if (choix==.dico[["txt_get_working_dir"]]) Resultats[[.dico[["txt_working_dir"]]]] <- getwd()
           if (choix==.dico[["txt_list_of_objects_in_mem"]]) Resultats[[.dico[["txt_objects_in_mem"]]]] <- ls(envir=.GlobalEnv)
           if (choix==.dico[["txt_specify_working_dir"]]) {
             repertoire <- dlgDir(title=.dico[["ask_chose_the_working_dir"]])$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Resultats[[.dico[["txt_new_dir"]]]] <- paste(.dico[["desc_working_dir_is_now"]], repertoire)
           }
           if (choix==.dico[["txt_remove_object_in_memory"]]) {
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title=.dico[["txt_object_to_remove"]])$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Resultats <- list()
             Resultats[[.dico[["desc_list_of_objects_still_in_mem"]]]] <- ls(envir=.GlobalEnv)
           }
           if (choix==.dico[["txt_search_for_new_function"]]) {
             require(sos)
             writeLines(.dico[["desc_to_find_new_analysis_search_in_english"]])
             critere <- dlgInput(.dico[["ask_which_analysis_you_looking_for"]], .dico[["desc_search_here"]])$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Resultats<- findFn(critere)
             return(Resultats)
           }
           if (choix==.dico[["txt_packages_update"]]) {update.packages(ask=FALSE)}
           if (choix==.dico[["txt_verify_packages_install"]]) vef.pack()->Resultats[[.dico[["txt_packages_verification"]]]]

	   if (choix==.dico[["txt_select_language"]]) {select_language()}

           if (choix==.dico[["txt_search_for_new_function"]]) packages<-c(packages, 'sos')
    Resultats$ref<- ref1(packages)
    return(Resultats)
  }
