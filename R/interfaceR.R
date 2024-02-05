interfaceR <-
  function(){
    options (warn=-1)
    packages <- c('svDialogs')
    lapply(packages, require,character.only=T)
    Resultats <- list()



    choix <- dlgList(c(txt_get_working_dir,
		       txt_specify_working_dir,
		       txt_remove_object_in_memory,
                       txt_list_of_objects_in_mem,
		       txt_search_for_new_function,
		       txt_packages_update,
		       txt_verify_packages_install,
		       txt_select_language
		       ), preselect=NULL, multiple = FALSE, title=ask_what_is_your_choice)$res
    while(length(choix)==0) return(easieR())


           if (choix==txt_get_working_dir) Resultats[[txt_working_dir]] <- getwd()
           if (choix==txt_list_of_objects_in_mem) Resultats[[txt_objects_in_mem]] <- ls(envir=.GlobalEnv)
           if (choix==txt_specify_working_dir) {
             repertoire <- dlgDir(title=ask_chose_the_working_dir)$res
             if(length(repertoire)==0) repertoire <- getwd()
             setwd(repertoire)
             Resultats[[txt_new_dir]] <- paste(desc_working_dir_is_now, repertoire)
           }
           if (choix==txt_remove_object_in_memory) {
             ls(envir=.GlobalEnv)->tout
             Filter( function(x) 'function' %in% class( get(x) ), ls(envir=.GlobalEnv) )->fonctions
             tout[!is.element(tout,fonctions)]->tout
             X<-dlgList(tout, multiple = TRUE, title=txt_object_to_remove)$res
             if(length(X)==0) return(easieR())
             rm(list=X, envir=.GlobalEnv)
             Resultats <- list()
             Resultats[[desc_list_of_objects_still_in_mem]] <- ls(envir=.GlobalEnv)
           }
           if (choix==txt_search_for_new_function) {
             require(sos)
             writeLines(desc_to_find_new_analysis_search_in_english)
             critere <- dlgInput(ask_which_analysis_you_looking_for, desc_search_here)$res
             if(length(critere)==0) return(easieR())
             critere <- strsplit(critere, ":")
             critere <- tail(critere[[1]],n=1)
             Resultats<- findFn(critere)
             return(Resultats)
           }
           if (choix==txt_packages_update) {update.packages(ask=FALSE)}
           if (choix==txt_verify_packages_install) vef.pack()->Resultats[[txt_packages_verification]]

	   if (choix==txt_select_language) {select_language()}

           if (choix==txt_search_for_new_function) packages<-c(packages, 'sos')
    Resultats$ref<- ref1(packages)
    return(Resultats)
  }
