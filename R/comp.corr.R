comp.corr <-
function(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE){options (warn=-1) 
  #xy : value of the correlation between x and y
  #xz : value of the correlation between x and z
  #yz : value of the correlation between y and z. Should be null for independant comparisons et having a value for paired.
  # n : sample size for the correlation xy.
  # n2 : sample size for the correlation xz. 
  # twotailed : logical. Should the estimation of p be one(FALSE) or twotailed (TRUE). 
  
  c("psych", "svDialogs")->packages
  if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages) 
    require(packages)} 
  list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats
  
  if((all(c(xy, yz, xz)<=1) & all(c(xy, yz, xz)>=-1)) & 
     all(c(n,n2)>0) & all(c(n,n2)%%1==0)) {
    paired.r(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->r
  } else {
    msgBox("Les valeurs des corrélations doivent être comprises entre -1 et 1/n
           et les effectifs doivent être des entiers positifs")
  }
  
  if(exists("r") && length(r$p)!=0 && !is.na(r$p)) {
    Resultats$"comparaison des deux corrélations"<-r
    Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
    data1<-data.frame()
    .add.history(data=data1, command=Resultats$call, nom=paste("comparaison des corrélations XY=", xy, "et YZ =", yz ))
    .add.result(Resultats=Resultats, name =paste("comparaison de corrélations", Sys.time() ))
    Resultats$"Références"<-ref1(packages)
    return(Resultats)
  } else{
    type<- dlgList(c("Corrélations appariées", "Corrélations indépendantes"), preselect=FALSE, multiple = TRUE, title="Comparaison de deux corrélations")$res
    if(length(type)==0) return(choix.corr())
    
    if(type=="Corrélations indépendantes") {
      Form <- list(
        "Corrélation entre XY:NUM" = 0,
        "N de la corrélation XY:NUM" = 100,
        "Corrélation entre XZ:NUM" = 0,
        "N de la corrélation XZ:NUM" = 100)
    }else{
      Form <- list(
        "Corrélation entre XY:NUM" = 0,
        "Corrélation entre XZ:NUM" = 0,
        "Corrélation entre YZ:NUM" = 0,
        "Taille de l'échantillon:NUM" = 100)
    }
    
    value<-dlgForm(Form, "Veuillez entrer les différentes valeurs")$res
    if(any(is.na(value))) {
      msgBox("Toutes les valeurs entrées ne sont pas numérique. Veuillez entrer des valeurs numériques uniquement")
      comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
      return(Resultats)
    }
    xy<-value$"Corrélation entre XY"
    xz<-value$"Corrélation entre XZ"
    yz<-value$"Corrélation entre YZ"
    if(type==  "Corrélations appariées"){n<-value$"Taille de l'échantillon"} else {
      n<-value$"N de la corrélation XY"
      n2<-value$"N de la corrélation XZ"
    }
    comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
    return(Resultats)
  }
  }
