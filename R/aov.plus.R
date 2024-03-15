aov.plus <-
  function(aov.plus.list=NULL, info=T, html=T){
    options (warn=-1)
    packages<-c('svDialogs','emmeans')
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
      .e <- environment()

    if(is.null(aov.plus.list)){
      Filter( function(x) 'aovplus' %in% class( get(x) ), ls(envir=.GlobalEnv))->nom1
      if(length(nom1)==0) {
        writeLines(desc_no_compatible_object_in_mem_for_aov)
        return(ez.anova())}
      if(length(nom1)==1)  aov.plus.list<-get(nom1) else{
        if(info=='TRUE') writeLines(ask_wanted_model)
        nom1 <- dlgList(nom1, multiple = FALSE, title=ask_model)$res
        if(length(nom1)==0) {nom1<-NULL
        aov.plus.list<-NULL}
        if(!is.null(nom1))  aov.plus.list<-get(nom1)
      }
    }



    if(length(aov.plus.list)==3){
      writeLines(ask_complete_or_outliers)
      type<-dlgList(names(aov.plus.list)[2:3], multiple = FALSE, title=ask_which_data_to_analyse)$res
      if(length(type)==0) return(desc_user_exited_aov_plus)
      if(type==txt_complete_dataset) aov.plus.list[[2]]->aov.plus.list else aov.plus.list[[2]]->aov.plus.list
    }else aov.plus.list[[2]]->aov.plus.list


    writeLines(desc_this_function_means_and_sd_adjusted_interaction_effect_possible)
    choix<-dlgList(c(txt_means_adjusted_standard_errors,txt_contrasts),
                   multiple = TRUE, title=ask_which_data_to_analyse)$res
    if(length(choix)==0) return(analyse())

    Resultats<-list()
    noms<-names(summary(aov.plus.list[["em.out"]]))[which(sapply(summary(aov.plus.list[["em.out"]]),class) =="factor")]

    if(any(choix==txt_means_adjusted_standard_errors)){
      writeLines(ask_which_factors_combination_for_adjust_means)
      facteurs<-dlgList(noms, multiple = TRUE, title=ask_what_to_print)$res
      if(length(facteurs)==0) return(aov.plus())
      formula<-paste0('~',facteurs[[1]])
    if(length(facteurs)>1){
      for(i in 2:length(facteurs)){
        formula<-paste(formula, '+', facteurs[i])
      }}
      recordPlot()->graphe
      Resultats[[txt_adjusted_means_graph]]<-emmip(object= aov.plus.in[[txt_complete_dataset]]$em.out,as.formula(formula) , CIs=T)
      em.out<-emmeans(object= aov.plus.in[[txt_complete_dataset]]$em.out,as.formula(formula), CIs=T)
      Resultats[[txt_adjusted_means]]<-data.frame(em.out)

    }

    if(any(choix==txt_contrasts)){
      writeLines(ask_specify_contrasts)
      if(length(choix)==0) return(aov.plus())
      p.adjust<-dlgList(c("holm", "hochberg", "hommel", "bonferroni", "fdr","tukey","scheffe",
                "sidak","dunnettx","mvt" ,"none" ), preselect="holm", multiple = FALSE, title=ask_correction_type)$res
      if(length(p.adjust)==0) p.adjust<-"none"

        cont.data<-data.frame(aov.plus.in[[txt_complete_dataset]]$em.out)
        cont.data<-cont.data[, noms]
        cont.data<-fix(cont.data)
        suppress<-which(colSums(is.na(cont.data)) > 0)
        if(length(suppress>0)) cont.data<-cont.data[,-suppress]
        Resultats$Contrates$coefficients<-cont.data
        emm.out<-contrast(aov.plus.in[[txt_complete_dataset]]$em.out,
                                                method= list(cont.data[, which(sapply(cont.data, class)=="numeric")]), adjust=p.adjust)
        emm.out<-data.frame(emm.out)
        names(emm.out)[6]<-txt_p_dot_val # TODO translation

        emm.out$contrast<-names(cont.data)[which(sapply(cont.data, class)=="numeric")]
        Resultats$Contrates$contrastes<-emm.out
        }
        ref1(packages)->Resultats[[desc_references]]
        .add.result(Resultats=Resultats, name =paste(txt_anova_plus, Sys.time() ))
#    if(sauvegarde==T) save(Resultats=Resultats ,choix ="Resultats.aov.plus", env=.e)
     if(html) ez.html(Resultats)
      return(Resultats)

  }
