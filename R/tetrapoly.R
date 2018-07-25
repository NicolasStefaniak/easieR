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
  c("lavaan", "svDialogs")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  
  .e<- environment()
  Resultats<-list()

  if(is.null(data) | is.null(X))  {dial<-TRUE
  if(info) writeLines("Veuillez choisir le type de corrélations que vous désirez réaliser. Pour les variables dichotomiques, les corrélations seront des corrélations tétrachoriques")
  dlgList(c("corrélations polychoriques", "corrélations mixtes"), preselect=NULL, multiple = FALSE, title="Type de corrélations ?")$res->method
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
  
  
  msg3<-"Veuillez choisir les variables dont il faut réaliser les corrélations polychorique/tétrachorique/mixte."
  X<-.var.type(X=X, info=info, data=data, type="numeric", check.prod=F, message=msg3,  multiple=T, title="Variable-s ", out=NULL)
  if(is.null(X)) {
    Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
    return(Resultats)}
  data<-X$data
  X<-X$X
  
  if(!is.null(ord) & any(ord %in%X==F)||(dial && method=="corrélations mixtes" ) ){
    if(info) writeLines("Veuillez choisir les variables ordinales.")
    ord<-dlgList(X, preselect=X, multiple = TRUE, title="Variables ordinales ?")$res
    if(length(ord)==0){
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
  } else ord<-X
  if(any(is.na(data[,X]))) {
  if(is.null(imp))  {msgBox("Des valeurs manquantes ont été détectées. Comment voulez-vous les traiter ? Garder l'ensemble des observations peut biaiser les résultats.")
    imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes","Remplacer par la médiane","Multiple imputation - Amelia"), 
                  preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes ?")$res}
    if(length(imp)==0){
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
    data1<-ez.imp(data[, X], imp=imp, ord= ord)
    data<-data.frame(data1, data[which(dimnames(data)[[1]] %in% dimnames(data1)[[1]]),group])
  }  
  if(dial || !is.logical(sauvegarde)){
    sauvegarde<- dlgList(c(TRUE, FALSE), preselect=FALSE, multiple = FALSE, title="Voulez-vous sauver les résultats ?")$res
    if(length(sauvegarde)==0) {
      Resultats<-tetrapoly(data=NULL,X=NULL, sauvegarde=F, ord=NULL ,info=T, group=NULL, estimator=estimator, output=output)
      return(Resultats)
    }
  }
  Resultats$"Matrice de corrélation tétrachorique/polychorique ou mixte"<-lavCor(data[,c(X,group)], ordered=ord,estimator=estimator, group=group,  missing="default", output=output)
  paste(X, collapse="','", sep="")->X
  if(!is.null(ord)) paste(ord, collapse="','", sep="")->ord
  Resultats$Call<-paste0("tetrapoly(data=", nom,",X=c('", X,"'),sauvegarde=", sauvegarde, ",ord=", ifelse(!is.null(ord),paste0("c('",ord,"')"), "NULL"),
                         ",info=T, group=", ifelse(!is.null(group),paste0("'",group,"'"), "NULL"), ",estimator='", estimator, "',output='", output, "')")
  
  .add.history(data=data, command=Resultats$Call, nom=nom)
  .add.result(Resultats=Resultats, name =paste("cor.polychorique", Sys.time() ))  
  
  
  if(sauvegarde) save(Resultats=Resultats, choix="cor.polychorique", env=.e)
  
  ref1(packages)->Resultats$Références
  if(html) ez.html(Resultats)
  return(Resultats) }
