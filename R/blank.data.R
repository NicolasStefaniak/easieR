blank.data <-
function(){
  options (warn=1)
  options(scipen=999)
  # 3. choix du groupe de fonctions
  c("svDialogs", "RGtk2Extras")->packages
  try(lapply(packages, library, character.only=T), silent=T)->test2
  if(class(test2)=="try-error") {try(install.packages("RGtk2Extras"),silent=T)->test2
    if(class(test2)=="try-error") msgBox("Désolé, vous devez utiliser une version 3.4 ou supérieure de R pour utiliser cette fonction")
  }
  fichier <- dlgInput("Quel nom voulez-vous donner à vos données ?", "data1")$res
  if(length(fichier)==0) return(imp.exp())
  fichier <- strsplit(fichier, ":")
  fichier <- tail(fichier[[1]],n=1)
  if(grepl("[^[:alnum:]]", fichier)) {
    writeLines("Des caractères non autorisés ont été utilisés pour le nom. Ces caractères ont été remplacés par des points")
    gsub("[^[:alnum:]]", ".", fichier)->fichier
  }
  data1<-data.frame()
  
  
  win <- gtkWindowNew()
  obj <- gtkDfEdit(data1, dataset.name=deparse(substitute(fichier)))
  win$add(obj)
  
  # assign(fichier, win$add(obj))
  return(ref1(packages))
  
  
}
