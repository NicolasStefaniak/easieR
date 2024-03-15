tetrapoly <-
  function(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator="two.step", output="cor", imp=NULL, html=T){
    # data : dataframe
    # X : vector of variables names
    # sauvegarde : bolean. Should analysis be saved ?
    # ord : Character. names of variables considered as ordinal. The other are considered as continuous.
    # info : bolean. Should information be printed in the console during analysis ?
    # group : character. Name of the factor variable
    # estimator : see ?lavCor for information
    # output : see ?lavCor for information
    # html : logical. Should output be a HTML page ?
    options (warn=-1)
    c('lavaan', 'svDialogs')->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())

    .e<- environment()
    Resultats<-list()

    if(is.null(data) | is.null(X))  {dial<-TRUE
    if(info) writeLines(ask_correlation_type)
    dlgList(c(txt_polyc_correlations, txt_mixt_correlations), preselect=NULL, multiple = FALSE, title=ask_correlations_type)$res->method
    if(length(method)==0) return(choix.corr())
    } else dial<-F


    if(dial || class(data)!="data.frame"){
      data<-choix.data(data=data, info=info, nom=T)
      if(length(data)==0) return(choix.corr())
      nom<-data[[1]]
      data<-data[[2]]
    }else{
      deparse(substitute(data))->nom
    }


    msg3<-ask_variabels_for_polyc_tetra_mixt_corr
    X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title=ask_variables, out=NULL)
    if(is.null(X)) {
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)}
    data<-X$data
    X<-X$X

    if(!is.null(ord) & any(ord %in%X==F)||(dial && method==txt_mixt_correlations ) ){
      if(info) writeLines(ask_ordinal_variables)
      ord<-dlgList(X, preselect=X, multiple = TRUE, title=ask_ordinal_variables)$res
      if(length(ord)==0){
        Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Resultats)
      }
    } else ord<-X
    if(any(is.na(data[,X]))) {
      if(is.null(imp))  {msgBox(ask_how_to_treat_missing_values)
        imp<- dlgList(c(txt_do_nothing_keep_all_obs, txt_delete_observations_with_missing_values,txt_replace_by_median,txt_multiple_imputation_amelia),
                      preselect=FALSE, multiple = TRUE, title=ask_missing_values_treatment)$res}
      if(length(imp)==0){
        Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Resultats)
      }
      data1<-ez.imp(data[, X], imp=imp, ord= ord)
      data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),group])
    }
    if(dial || !is.logical(sauvegarde)){
      sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title=ask_save_results)$res
      if(length(sauvegarde)==0) {
        Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
        return(Resultats)
      }
    }
    Resultats[[txt_tetra_polyc_corr_matrix_or_mixt]]<-lavCor(data[,c(X,group)], ordered=ord,estimator=estimator, group=group,  missing="default", output=output)
    paste(X, collapse="','", sep="")->X
    if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
    Resultats$Call<-paste0("tetrapoly(data=", nom,",X=c('", X,"'),sauvegarde=", sauvegarde, ",ord=", ifelse(!is.null(ord),paste0("c('",ord,"')"), "NULL"),
                           ",info=T, group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), ",estimator='", estimator, "',output='", output, "')")

    .add.history(data=data, command=Resultats$Call, nom=nom)
    .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))


    if(sauvegarde) save(Resultats=Resultats, choix="cor.polychorique", env=.e)

    ref1(packages)->Resultats[[txt_references]]
    if(html) ez.html(Resultats)
    return(Resultats) }
