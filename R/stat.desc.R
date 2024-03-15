stat.desc <-
  function(X=NULL, groupes=NULL, data=NULL, tr=.1, type=3, plot=T, ref=T, save=F, html=T){
    # X = vecteur de variables dont il faut faire les statistiques descriptives (character)
    # groupes = vecteur de variables dont il faut decomposer les statistiques descriptives par sous-groupe (character)
    # data = nom de la base de donnees
    # tr = troncature
    # type = type d'asymetrie et d'aplatissement a calculer (voir ?psych::describe)
    # plot = logical. Show the plot ?
    # ref = logical. Show packages used in this function ?
    # save = logical. Should the output be saved in rtf and R file ?
    stat.desc.in<-function(x=NULL, groupes=NULL, data=NULL, tr=.1, type=3, save=NULL){
      list()->Resultats
      choix.data(data=data, info=TRUE, nom=T)->data
      if(length(data)==0) { return(NULL)} else {
        data[[1]]->nom1
        data[[2]]->data}
      # choix X
      if(!is.null(x)) dial<-F else dial<-T

      msg1<-ask_variables_for_description_statistics
      .var.type(X=X, info=T, data=data, type=NULL, message=msg1,multiple=T, title=ask_variable)->X1
      if(is.null(X1)) return(NULL)
      X1$X->x
      setdiff(names(data), x)->diff
      if(length(diff)==0 & !is.null(groupes)) {
        msgBox(desc_cannot_group_variables_because_not_described)
        groupes<-NULL
      }

      if(length(diff)>0){
        if(dial){
          writeLines(ask_subgroups)
          groupes<-dlgList(c(txt_yes, txt_no), multiple = F, preselect=txt_no, title=ask_specify_groups)$res
          if(length(groupes)==0) {stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
            return(Resultats)}
          if(groupes==txt_no) groupes<-NULL
        }

        if(!is.null(groupes)){
          msg2<-ask_variables_used_for_groups
          .var.type(X=groupes, info=T, data=data, type="factor", message=msg2,multiple=T, title="Variable(s)  groupes ?", out=x)->groupes
          if(is.null(groupes)){
            stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
            return(Resultats)
          }
          groupes$data->data
          groupes$X->groupes
        }
      }

      if(dial==T | tr>1 | tr<0 | (type %in% 1:3==F) ) {
        writeLines(desc_flattening_and_asymetry_configurable)
        options<-dlgList(c(txt_yes, txt_no), multiple = F, preselect=txt_no, title=ask_specify_other_options)$res
        if(length(options)==0) {
          stat.desc.in(x=X, groupes=NULL, data=NULL, tr=tr, type=type,save=save)->Resultats
          return(Resultats)
        }
        if(options==txt_yes) {opts2<-NA
        while(any(is.na(opts2))){
          #dlgForm(list(txt_troncature_num=0.1, "Type de skew et kurtosis, doit se situer entre 1 et 3:NUM"=3),  ask_troncature_threshold)$res->opts2
	  name <- c(txt_troncature_num,desc_skew_and_kurtosis_between_1_and_3)
	  vals <- c(0.1, 3)
	  Form <- setNames(as.list(vals), name)
          dlgForm(Form,  ask_troncature_threshold)$res->opts2
          if(opts2[[1]]>0.5 | opts2[[1]]<0 ) NA->opts2[[1]] else tr<-opts2[[1]]
          if(opts2[[2]]%in% 1:3)  type<-opts2[[2]]  else opts2[[2]]<-NA

        }
        }
        #      ez.options(save=T)$sauvegarde->save
      }
      Resultats$data<-data
      Resultats$nom1<-nom1
      Resultats$X<-x
      Resultats$groupes<-groupes
      Resultats$tr<-tr
      Resultats$type<-type
      Resultats$sauvegarde<-save
      return(Resultats)
    }



    options (warn=-1)
    c( 'ggplot2', 'psych', 'svDialogs')->packages
    lapply(packages, require, character.only=T)
    list()->Resultats
    .e <- environment()
    try( windows(record=T), silent=T)->win
    if(class(win)=='try-error') quartz()
    if(!is.null(data) & class(data)!="character") deparse(substitute(data))->data

    stat.desc.in(x=X, groupes=groupes, data=data, tr=tr, type=type,save=save)->data.in
    if(is.null(data.in)) return(analyse())
    .stat.desc.out(X=data.in$X, groupes=data.in$groupes,data=data.in$data,plot=plot, tr=data.in$tr, type=data.in$type)->Resultats
    paste(data.in$X, collapse="','", sep="")->X
    if(is.null(data.in$groupes)) paste0("'), groupes = NULL, data=")->groupes else { paste(data.in$groupes, collapse="','", sep="")->groupes
      paste0("'), groupes =c('",groupes ,"'), data=")->groupes}
    paste0("stat.desc(X=c('", X, groupes, data.in$nom1, ",tr=" , tr, ",type=", type, ", plot=", plot, ", ref=", ref,", html=",html, ")")->Resultats$Call
    .add.history(data=data.in$data, command=Resultats$Call, nom=data.in$nom1)
    .add.result(Resultats=Resultats, name =paste(txt_descriptive_statistics, Sys.time() ))

    if(data.in$sauvegarde==TRUE) save(Resultats=Resultats ,choix =paste(desc_descriptive_statistics_on,data.in$nom1 ), env=.e)
    if(ref) ref1(packages)->Resultats[[desc_references]]
    if(html) try(ez.html(Resultats), silent=T)
    return(Resultats)
  }
