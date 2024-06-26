maths <-
  function(info=TRUE){
    options (warn=-1)
    packages<-c('svDialogs')
    #faire l analyse par groupe # regler le probleme des noms
    if(any(lapply(packages, require, character.only=T))==FALSE)  {install.packages(packages)
      require(packages)}
    list()->Resultats

    choix.data(nom=TRUE)->data1
    if(length(data1)==0) {return(preprocess())}
    data1[[1]]->nom1
    data1[[2]]->data
    if(info=="TRUE") writeLines(.dico[["ask_which_mathematical_operation"]])
    dlgList(c(.dico[["txt_additions"]],.dico[["txt_multiplication"]], .dico[["txt_division"]], .dico[["txt_substraction"]],.dico[["txt_col_mean"]], .dico[["txt_exponant_or_root"]],
              .dico[["txt_logarithm"]], .dico[["txt_exponential"]],.dico[["txt_absolute_value"]],.dico[["txt_complex_model"]]), preselect=.dico[["txt_additions"]], multiple = FALSE, title=.dico[["ask_which_operation"]])$res->choix
    if(length(choix)==0) return(preprocess())

    variable<-function(multiple=TRUE){
      X<-dlgList(c(names(data), .dico[["txt_cancel"]]), multiple = multiple, title=.dico[["txt_variables"]])$res
      if(any(sapply(data[,X], class)=="factor")) {writeLines(.dico[["desc_at_least_one_var_is_not_num"]])
        writeLines(str(data))
        return(maths())}
      return(X)}

    valeur<-function(info=TRUE, out=NULL){
      # info : logique pour determiner les informations relatives aux parametres doivent s'afficher dans la console
      # out : valeur renvoyee si valeur non numerique ou annulation
      if(info) writeLines(.dico[["ask_value_for_operation"]])
      msg<-"no"
      while(msg=="no" ){
        valeur1 <- dlgInput(.dico[["ask_which_value_for_operation"]], out)$res
        if(length(valeur1)!=0){
          strsplit(valeur1, ":")->valeur1
          if(class(valeur1)=="list") {  tail(valeur1[[1]],n=1)->valeur1}
          if(grepl("/",valeur1)) apply(sapply(strsplit(valeur1, split = "/"), as.numeric), 2, function(x) x[1] / x[2])->valeur1
          if(valeur1=="e") valeur1<-exp(1)
          as.numeric(valeur1)->valeur1
          msg<-"yes"} else return(out)
        if(is.na(valeur1) ) { dlgMessage(.dico[["ask_cancel_entered_value_not_num"]], "yesno")$res->msg
          if(msg=="yes") return(out)}

      }
      return(valeur1)
    }
    nom<-function(data,info, nom1){
      if(info=="TRUE") writeLines(.dico[["ask_new_variable_name"]])
      variable<-dlgInput(.dico[["ask_variable_name"]],"nouvelle.variable")$res
      if(length(variable)==0) variable<-"nouvelle.variable"
      strsplit(variable, ":")->variable
      tail(variable[[1]],n=1)->variable
      if(grepl("[^[:alnum:]]", variable)) {
      writeLines(.dico[["desc_unauthorized_char_replaced"]])
      gsub("[^[:alnum:]]", ".", variable)->variable
}


      names(data)<-c(names(data)[1:(length(data)-1)], variable)
      assign(nom1, data, envir=.GlobalEnv)
      Resultats<-paste(.dico[["desc_the_variable_upper"]], variable, .dico[["desc_has_been_added_to"]], nom1)
      return(Resultats)}

    if(choix==.dico[["txt_additions"]]) {
      if(info=="TRUE") writeLines(.dico[["desc_if_you_select_both_operations_value_will_be_added_to_chose_cols"]])
      dlgList(c(.dico[["txt_add_of_cols"]],.dico[["txt_add_of_specific_value"]]), preselect=.dico[["txt_add_of_cols"]], multiple = TRUE, title=.dico[["ask_which_operation"]])$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== .dico[["txt_add_of_specific_value"]])){
        variable()->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        valeur(info=info)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]+valeur1)->data
        if(valeur1>0)      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, .dico[["txt_plus"]], valeur1, sep=".") else names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, .dico[["txt_minus"]], abs(valeur1), sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, .dico[["desc_has_been_added_to_variable"]], X)->Resultats
      }

      if(any(choix2== .dico[["txt_add_of_cols"]])) {
        if(info=="TRUE") writeLines(.dico[["ask_variables_to_add"]])
        variable()->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"+",X1[1])->X2
          X1[-1]->X1}
        rowSums(data[,X])->data$nouvelle_variable
        if(info=="TRUE") writeLines(.dico[["desc_you_can_still_add"]])
        valeur(info=info, out=0)->valeur1
        if(valeur1!=0) {data$nouvelle_variable+valeur1->data$nouvelle_variable
          paste(X2, "+", valeur1)->X2}
        writeLines(paste(.dico[["desc_you_did_this_operation"]], X2))
        writeLines(.dico[["ask_add_value_to_total"]])
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }

    if(choix==.dico[["txt_multiplication"]]){
      if(info=="TRUE") writeLines(.dico[["desc_if_you_select_both_operations_value_will_be_multiplied_to_chose_cols"]])
      dlgList(c(.dico[["txt_cols_multiplication"]],.dico[["txt_specific_val_multiplication"]]), preselect=.dico[["txt_cols_multiplication"]], multiple = TRUE, title=.dico[["ask_which_operation"]])$res->choix2
      if(length(choix2)==0) return(maths())
      if(any(choix2== .dico[["txt_specific_val_multiplication"]])){
        if(info=="TRUE") writeLines(.dico[["ask_variables_to_multiply"]])
        variable()->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        valeur(info=info, out=NULL)->valeur1
        if(is.null(valeur1)) return(maths())
        data.frame(data, data[,X]*valeur1)->data
        names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, .dico[["txt_multiplied_by"]], valeur1, sep=".")
        assign(nom1, data, envir=.GlobalEnv)
        paste(valeur1, .dico[["desc_has_multiplied_variables"]], X)->Resultats
      }

      if(any(choix2== .dico[["txt_cols_multiplication"]])) {
        variable()->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())

        X->X1
        X2<-X1[1]
        X1[-1]->X1
        while(length(X1)!=0){paste(X2,"*",X1[1])->X2
          X1[-1]->X1}
        1*data[,X[1]]->nouvelle
        for(i in 1:(length(X)-1)) nouvelle*data[,X[i+1]]->nouvelle
        data.frame(data, nouvelle)->data

        if(info=="TRUE") writeLines(.dico[["desc_you_can_still_multiply"]])
        valeur(info=info, out=1)->valeur1
        if(valeur1!=1) {data$nouvelle*valeur1->data$nouvelle
          paste(X2, "*", valeur1)->X2}
        writeLines(paste(.dico[["desc_you_did_this_operation"]], X2))
        nom(data=data, info=info,nom1=nom1)->Resultats
      }
    }
    if(choix==.dico[["txt_division"]]){
      if(info=="TRUE") writeLines(.dico[["ask_numerator_variable_or_value"]])
      numer<-dlgList(c(.dico[["txt_value"]], .dico[["txt_variable"]]), multiple = FALSE, title=.dico[["txt_numerator"]])$res
      if(length(numer)==0) return(maths())
      if(numer==.dico[["txt_value"]]) valeur(info=info, out=1)->X else{
        if(info=="TRUE") writeLines(.dico[["ask_numerator_variable"]])
        variable(multiple=FALSE)->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        data[,X]->X
      }

      if(info=="TRUE") writeLines(.dico[["ask_denominator_variable_or_value"]])
      denom<-dlgList(c(.dico[["txt_value"]], .dico[["txt_variable"]]), multiple = FALSE, title=.dico[["txt_denominator"]])$res
      if(length(denom)==0) return(maths())
      if(denom==.dico[["txt_value"]]) valeur(info=info, out=1)->Y else{
        if(info=="TRUE") writeLines(.dico[["ask_denominator_variable"]])
        variable(multiple=FALSE)->Y
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        data[,Y]->Y
        if(any(Y)==0) writeLines(.dico[["desc_at_least_one_denom_is_zero"]])
      }
      X/Y->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }

    if(choix==.dico[["txt_substraction"]]) {
      if(info=="TRUE") writeLines(.dico[["ask_chose_values_on_left_of_minus_symbol"]])
      if(info=="TRUE") writeLines(.dico[["ask_positive_val_variable_or_value"]])
      numer<-dlgList(c(.dico[["txt_value"]], .dico[["txt_variable"]]), multiple = FALSE, title=.dico[["txt_positive_values"]])$res
      if(length(numer)==0) return(maths())
      if(numer==.dico[["txt_value"]]) valeur(info=info, out=0)->X else{
        if(info=="TRUE") writeLines("Veuillez selectionner la -les- variable(s) a gauche du symbole *moins*")
        variable(multiple=TRUE)->X
        if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
        data[,X]->X1
        data.frame(X1)->X1
      }

      if(info=="TRUE") writeLines(.dico[["ask_minus_left_hand_variables"]])
      denom<-dlgList(c(.dico[["txt_value"]], .dico[["txt_variable"]]), multiple = FALSE, title=.dico[["txt_negative_values"]])$res
      if(length(denom)==0) return(maths())
      if(denom==.dico[["txt_value"]]) valeur(info=info, out=0)->Y else{
        if(info=="TRUE") writeLines(.dico[["ask_minus_right_hand_variables"]])
        Y<-NULL
        while(is.null(Y)){
          variable(multiple=TRUE)->Y
          if(length(Y)==0|| any(Y==.dico[["txt_cancel"]])) return(maths())
          data[,Y]->Y1
          data.frame(Y1)->Y1
          if(length(X1)!=1 & length(Y1)!=1 & length(X1)!=length(Y1)) {
            writeLines(.dico[["desc_one_or_same_number_cols_on_both_sides_only"]])
            Y<-NULL} else Y<-Y
        }
        }
      X1-Y1->new.var
      names(new.var)<-paste0(X, ".moins.", Y)
      data<-data.frame(data, new.var)
      assign(nom1, data, envir=.GlobalEnv)
      #nom(data=data, info=info,nom1=nom1)->Resultats
      Resultats<-.dico[["desc_operation_succesful"]]
      }

    if(choix==.dico[["txt_col_mean"]])  {
      if(info=="TRUE") writeLines(.dico[["ask_variables_to_mean"]])
      X<-variable()
      if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
      rowMeans(data[,X])->data$nouvelle_variable
      nom(data=data, info=info,nom1=nom1)->Resultats
    }
    if(choix== .dico[["txt_exponant_or_root"]]){
      if(info=="TRUE") writeLines(.dico[["ask_variables_to_exp"]])
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
      if(info=="TRUE") writeLines(.dico[["ask_specify_exponant_value"]])
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines(.dico[["desc_entered_value_not_num"]])
        return(maths())}
      data.frame(data, data[,X]^Y)->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(X, .dico[["txt_exponant"]], Y, sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(.dico[["desc_the_variable_lower"]], X, .dico[["desc_has_been_put_to_the_power_of"]], Y)->Resultats

    }
    if(choix== .dico[["txt_logarithm"]]){
      if(info=="TRUE") writeLines(.dico[["ask_variables_to_log"]])
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
      if(info=="TRUE") writeLines(.dico[["ask_log_base"]])
      valeur(info=info)->Y
      if(class(Y)!="numeric") {writeLines(.dico[["desc_entered_value_not_num"]])
        return(maths())}
      if(Y<0) {writeLines(.dico[["desc_neg_log_impossible"]])
        return(maths()) }
      data.frame(data, log(data[,X], base=Y))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("log.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(.dico[["desc_log_with_base"]], Y, .dico[["desc_has_been_applied_to_variable"]], X)->Resultats
    }
    if(choix== .dico[["txt_exponential"]]){
      if(info=="TRUE") writeLines(.dico[["ask_variables_used_for_exponential"]])
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
      data.frame(data, exp(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste("exp.", X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(.dico[["desc_exponential_has_been_applied_to_var"]], X)->Resultats
    }
    if(choix== .dico[["txt_absolute_value"]]){
      if(info=="TRUE") writeLines(.dico[["ask_variables_to_abs"]])
      variable(multiple=TRUE)->X
      if(length(X)==0|| any(X==.dico[["txt_cancel"]])) return(maths())
      data.frame(data, abs(data[,X]))->data
      names(data)[(length(data)-(length(X)-1)):length(data)]<-paste(.dico[["txt_absolute_dot_val"]], X,  sep=".")
      assign(nom1, data, envir=.GlobalEnv)
      paste(.dico[["desc_abs_val_applied_to_var"]], X)->Resultats
    }
    if(choix== .dico[["txt_complex_model"]]){
      writeLines(.dico[["desc_expression_must_be_correct_example"]])
      print(paste(names(data)[1],"^2+5"), quote=FALSE)
      print(names(data))
      valeur1 <- dlgInput(.dico[["ask_model"]])$res
      if(length(valeur1)==0) return(maths())
      strsplit(valeur1, ":")->valeur1
      tail(valeur1[[1]],n=1)->valeur1
      try(eval(parse(text=valeur1), envir=data), silent=TRUE)->nouvelle
      if(class(nouvelle)=='try-error') {writeLines(.dico[["desc_model_contains_error"]])
        return(maths())} else nouvelle->data$nouvelle

      nom(data=data,info=info, nom1=nom1)->Resultats

    }
    View(data)
    return(Resultats)
    }
