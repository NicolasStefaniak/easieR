comp.corr <-
  function(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE, html=FALSE){options (warn=-1) 
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
      msgBox("Les valeurs des correlations doivent etre comprises entre -1 et 1/n
             et les effectifs doivent etre des entiers positifs")
    }
    
    if(exists("r") && length(r$p)!=0 && !is.na(r$p)) {
      Resultats$"comparaison des deux correlations"<-r
      Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
      data1<-data.frame()
      .add.history(data=data1, command=Resultats$call, nom=paste("comparaison des correlations XY=", xy, "et YZ =", yz ))
      .add.result(Resultats=Resultats, name =paste("comparaison de correlations", Sys.time() ))
      Resultats$"References"<-ref1(packages)
      return(Resultats)
    } else{
      type<- dlgList(c("Correlations appariees", "Correlations independantes"), preselect=FALSE, multiple = TRUE, title="Comparaison de deux correlations")$res
      if(length(type)==0) return(choix.corr())
      
      if(type=="Correlations independantes") {
        Form <- list(
          "Correlation entre XY:NUM" = 0,
          "N de la correlation XY:NUM" = 100,
          "Correlation entre XZ:NUM" = 0,
          "N de la correlation XZ:NUM" = 100)
      }else{
        Form <- list(
          "Correlation entre XY:NUM" = 0,
          "Correlation entre XZ:NUM" = 0,
          "Correlation entre YZ:NUM" = 0,
          "Taille de l'echantillon:NUM" = 100)
      }
      
      value<-dlgForm(Form, "Veuillez entrer les differentes valeurs")$res
      if(any(is.na(value))) {
        msgBox("Toutes les valeurs entrees ne sont pas numerique. Veuillez entrer des valeurs numeriques uniquement")
        comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
        return(Resultats)
      }
      xy<-value$"Correlation entre XY"
      xz<-value$"Correlation entre XZ"
      yz<-value$"Correlation entre YZ"
      if(type==  "Correlations appariees"){n<-value$"Taille de l'echantillon"} else {
        n<-value$"N de la correlation XY"
        n2<-value$"N de la correlation XZ"
      }
      comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
      if(html) html<-FALSE
      return(Resultats)
    }
    }

