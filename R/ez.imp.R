ez.imp <-
function(data=NULL, imp="median", ord=NULL, id=NULL, noms=NULL, info=T){
  # data : data.frame 
  # imp : one among "rm", "mean", "median", "amelia"
  # ord : if imp is amelia, names of ordinal variables
  # id : if imp is amelia, names of id variables
  # noms : if imp is amelia, names of nominal variables
  
  packages<-c("Amelia",  "svDialogs")
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)== "try-error") return(ez.install())
  .e <- environment()
  if(is.null(data)) { dial<-T
  data<-choix.data(data=data, info=info, nom=T)
  if(length(data)==0) return(NULL)
  nom<-data[[1]]
  data<-data[[2]]
  } else {dial<-F
  deparse(substitute(data))->nom  }
  nom<-paste0(nom,".complet")
  
  if(dial || imp%in% c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes", "Remplacer par la moyenne",
                       "Remplacer par la médiane","Multiple imputation - Amelia","rien","rm", "mean","median", "amelia") == FALSE){
    writeLines("Nombre de valeurs manquantes par variable. Comment voulez-vous les traiter ?")
    print(sapply(data, function(x) sum(length(which(is.na(x))))) )
    
    imp<- dlgList(c("Ne rien faire - Garder l'ensemble des observations", "Suppression des observations avec valeurs manquantes", "Remplacer par la moyenne",
                    "Remplacer par la médiane","Multiple imputation - Amelia"), preselect=FALSE, multiple = TRUE, title="Traitement des valeurs manquantes")$res
    if(length(imp)==0){
      return(NULL)
    }
  }
  if(length(imp)==0) return(NULL)
  if(imp == "Ne rien faire - Garder l'ensemble des observations" || imp=="rien") return(data)
  if(imp== "Suppression des observations avec valeurs manquantes"|| imp=="rm"){
    data<-data[complete.cases(data),]
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp=="Remplacer par la moyenne"|| imp=="mean"){
    for(i in 1 : length(data)) {data[which(is.na(data[,i])),i]<-mean(data[,i], na.rm=T)}
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp== "Remplacer par la médiane"|| imp=="median"){
    for(i in 1 : length(data)) {data[which(is.na(data[,i])),i]<-median(data[,i], na.rm=T)}
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  if(imp== "Multiple imputation - Amelia"|| imp=="amelia"){
    amelia(x=data, m = 1, p2s = 0,frontend = FALSE, idvars = id,
           ts = NULL, cs = NULL, polytime = NULL, splinetime = NULL, intercs = FALSE,
           lags = NULL, leads = NULL, startvals = 0, tolerance = 0.0001,
           logs = NULL, sqrts = NULL, lgstc = NULL, noms = noms, ords = ord,
           incheck = TRUE, collect = FALSE, arglist = NULL, empri = NULL,
           priors = NULL, autopri = 0.05, emburn = c(0,0), bounds = NULL,
           max.resample = 100, overimp = which(is.na(data), arr.ind = T), boot.type = "ordinary",
           parallel = c("no", "multicore", "snow"),
           ncpus = getOption("amelia.ncpus", 1L), cl = NULL)->data.am
    data.am$imputations$imp1->data
    
    if(dial)  assign(nom, data, envir=.GlobalEnv)
  }
  return(data)
}
