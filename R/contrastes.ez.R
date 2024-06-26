contrastes.ez <-
  function(longdata, inter=NULL, intra=NULL){
    Resultats<-list()
    writeLines(.dico[["desc_all_contrasts_description"]])
    type.cont<- dlgList(c(.dico[["txt_apriori"]],  .dico[["txt_comparison_two_by_two"]], .dico[["txt_none"]]), preselect=.dico[["txt_apriori"]],multiple = FALSE, title=.dico[["ask_which_contrasts"]])$res
    if(length(type.cont)==0) return(NULL)
    Resultats$type.cont<-type.cont
    c(inter, unlist(intra))->interintra
    if(type.cont==.dico[["txt_apriori"]]) {
      contrastes<-list()
      writeLines(.dico[["desc_you_can_choose_contrasts_you_want"]])
      cont.exemple<-list()
      contr.helmert(3)->cont.exemple$Orthogonaux
      apply(contr.helmert(3), 2, rev)->cont.exemple$Orthogonaux.inverses
      contr.poly(3)->cont.exemple$Polynomiaux
      contr.treatment(3, contrasts = TRUE, sparse = FALSE)->cont.exemple$comparaison.ligne.de.base
      print(cont.exemple)

      for (i in 1:length(interintra)){
        if(i>1) {
          type.cont2<- dlgList(c(.dico[["txt_orthogonals"]], .dico[["txt_orthogonals_inverse"]], .dico[["txt_polynomials"]],.dico[["txt_compare_to_baseline"]], .dico[["ask_specify_contrasts"]]),
                               preselect=c(.dico[["txt_orthogonals"]]), multiple = FALSE, title=paste(.dico[["ask_which_contrasts_for_variable"]],names(longdata[interintra])[i],"?"))$res} else {
                                 type.cont2<- dlgList(c(.dico[["txt_orthogonals"]], .dico[["txt_orthogonals_inverse"]], .dico[["txt_polynomials"]],.dico[["txt_compare_to_baseline"]],
                                                        .dico[["ask_specify_contrasts"]]),preselect=c(.dico[["txt_orthogonals"]]), multiple = FALSE, title=paste(.dico[["ask_which_contrasts_for_variable"]],names(longdata[interintra])[i],"?"))$res
                               }
        if(length(type.cont2)==0) return(contrastes.ez())
        if(type.cont2==.dico[["txt_orthogonals"]]) contr.helmert(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
        if(type.cont2==.dico[["txt_orthogonals_inverse"]]) apply(contr.helmert(nlevels(longdata[,interintra[i]])), 2, rev)->contrastes[[i]]
        if(type.cont2==.dico[["txt_polynomials"]])  contr.poly(nlevels(longdata[,interintra[i]]))->contrastes[[i]]
        if(type.cont2==.dico[["txt_compare_to_baseline"]]) {
          base<- dlgList(levels(longdata[, interintra[i]]), preselect=levels(longdata[,interintra[i]])[1],
                         multiple = FALSE, title=.dico[["ask_baseline"]])$res
          which(levels(longdata[, interintra[i]])==base)->base
          contr.treatment(levels(longdata[, interintra[i]]), base = base, contrasts = TRUE, sparse = FALSE)->contrastes[[i]]
        }
        if(type.cont2==.dico[["ask_specify_contrasts"]]){
          ortho<-FALSE
          while(ortho!=TRUE){
            matrix(rep(0,times=nlevels(longdata[,interintra[i]])*(nlevels(longdata[,interintra[i]])-1)), nrow=nlevels(longdata[,interintra[i]]))->contrastes3
            dimnames(contrastes3)[[1]]<-levels(longdata[,interintra[i]])
            dimnames(contrastes3)[[2]]<-paste(.dico[["txt_contrast"]], 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
            fix(contrastes3)->contrastes3
            if(any(colSums(contrastes3)!=0)|(nlevels(longdata[,interintra[i]])>2 & max(rle(c(contrastes3))$lengths)>2*(nlevels(longdata[,interintra[i]])-2))) ortho<-FALSE else {
              test.out<-rep(1, length(contrastes3[,1]))
              for(j in 1:length(contrastes3[1,])) {contrastes3[,j]*test.out->test.out}
              if(sum(test.out)==0) ortho<-TRUE else ortho<-FALSE}
            if(ortho==FALSE) {dlgMessage(.dico[["ask_contrast_must_respect_ortho"]], "yesno")$res->cont
              if(cont=="no") return(contrastes.ez(longdata=longdata, inter=inter, intra=intra ))  }
            contrastes[[i]]<-contrastes3

          }

        }

        dimnames(contrastes[[i]])[[2]]<-paste(.dico[["txt_contrast"]], 1:(nlevels(longdata[,interintra[i]])-1), sep=".")
      }
      names(contrastes)<-interintra
      Resultats$contrastes<-contrastes

    }
    if(type.cont== .dico[["txt_comparison_two_by_two"]]){
      list()->p.adjust
      writeLines(.dico[["ask_which_correction"]])
      dlgList(c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"), preselect="holm", multiple = FALSE, title=.dico[["ask_correction_type"]])$res->p.adjust
      if(length(p.adjust)==0) return(contrastes.ez())
      Resultats$p.adjust<-p.adjust
    }
    return(Resultats)
  }
