comp.corr <-
  function(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE, html=FALSE){options (warn=-1)
    #xy : value of the correlation between x and y
    #xz : value of the correlation between x and z
    #yz : value of the correlation between y and z. Should be null for independant comparisons et having a value for paired.
    # n : sample size for the correlation xy.
    # n2 : sample size for the correlation xz.
    # twotailed : logical. Should the estimation of p be one(FALSE) or twotailed (TRUE).

    c('psych', 'svDialogs')->packages
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats # cree une liste appelee Resultats dans laquelle on va stocker les Resultats

    if((all(c(xy, yz, xz)<=1) & all(c(xy, yz, xz)>=-1)) &
       all(c(n,n2)>0) & all(c(n,n2)%%1==0)) {
      paired.r(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->r
    } else {
      msgBox(.dico[["desc_corr_values_must_be_between_min_1_and_1"]])
    }

    if(exists("r") && length(r$p)!=0 && !is.na(r$p)) {
      Resultats[[.dico[["txt_comparison_of_two_correlations"]]]]<-r
      Resultats$call<-paste("comp.corr(xy=", xy, ",xz=", xz, ",yz=",yz, ",n=", n, ",n2=", n2, ",twotailed=",twotailed, ")")
      data1<-data.frame()
      .add.history(data=data1, command=Resultats$call, nom=paste(.dico[["txt_comparisons_XY"]], xy, .dico[["txt_and_YZ"]], yz ))
      .add.result(Resultats=Resultats, name =paste(.dico[["txt_correlations_comparison"]], Sys.time() ))
      Resultats[[.dico[["txt_references"]]]]<-ref1(packages)
      return(Resultats)
    } else{
      type<- dlgList(c(.dico[["txt_apparied_correlations"]], .dico[["txt_independant_correlations"]]), preselect=FALSE, multiple = FALSE, title=.dico[["txt_compare_two_correlations"]])$res
      if(length(type)==0) return(choix.corr())

      if(type==.dico[["txt_independant_correlations"]]) {
	name <- c(.dico[["txt_XY_correlation"]], .dico[["txt_N_of_XY_corr"]], .dico[["txt_XZ_correlation"]], .dico[["txt_N_of_XZ_corr"]])
	vals <- c(0, 100, 0, 100)
	Form <- setNames(as.list(vals), name)

      }else{
	name <- c(.dico[["txt_XY_correlation"]], .dico[["txt_XZ_correlation"]], .dico[["txt_YZ_correlation"]], .dico[["txt_sample_size"]])
	vals <- c(0, 0, 0, 100)
	Form <- setNames(as.list(vals), name)
      }

      # For Unix users: The native form dialog box (dlgForm) is available only if you install 'yad'
      # moreover, dlgForm is working as a dialog box only on Linux (see ?dlgForm)
      value<-dlgForm(Form, .dico[["ask_enter_different_values"]])$res
      if(any(is.na(value))) {
        msgBox(.dico[["desc_some_values_are_not_numeric"]])
        comp.corr(xy=NULL, xz=NULL, yz=NULL, n=NULL, n2=NULL,twotailed=TRUE)->Resultats
        return(Resultats)
      }
      xy<-value[[.dico[["txt_XY_correlation"]]]]
      xz<-value[[.dico[["txt_XZ_correlation"]]]]
      yz<-value[[.dico[["txt_YZ_correlation"]]]]
      if(type==.dico[["txt_apparied_correlations"]]) {
	      n<-value[[.dico[["txt_sample_size"]]]]
      } else {
              n<-value[[.dico[["txt_N_of_XY_corr"]]]]
              n2<-value[[.dico[["txt_N_of_XZ_corr"]]]]
      }
      comp.corr(xy=xy, xz=xz, yz=yz, n=n, n2=n2,twotailed=twotailed)->Resultats
      if(html) html<-FALSE
      return(Resultats)
    }
    }

