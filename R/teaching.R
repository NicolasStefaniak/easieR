teaching <-
  function(){
    tcl<-function(){
      clt.examp(1)
      msgBox(ask_are_you_ready)
      for(i in 1:50){
        clt.examp(i*2)
        Sys.sleep(1)
      }
    }

    c('psych', 'svDialogs', 'TeachingDemos', 'tkrplot')->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== 'try-error') return(ez.install())

    choix <- dlgList(c(txt_understanding_confidance_interval, txt_understanding_alpha_and_power,
                       txt_understanding_corr,
                       txt_understanding_central_limit_theorem,txt_understanding_corr_2,
                       txt_understanding_prev_sens_specificity,
                       txt_understanding_prev_sens_specificity_2,
                       txt_understanding_negative_positive_predic_power,
                       txt_understanding_bayesian_inference,
                       txt_understanding_likelihood,
                       txt_understanding_heterogenous_variance_effects), preselect=NULL, multiple = FALSE, title=ask_what_do_you_want)$res
    if(length(choix)==0) return(easieR())

           if (choix==txt_understanding_confidance_interval) ci.examp() # peut etre completer par des arguments
           if (choix==txt_understanding_central_limit_theorem) tcl()
           if (choix==txt_understanding_prev_sens_specificity) plotFagan2()
           if (choix==txt_understanding_bayesian_inference) plotFagan()
           if (choix==txt_understanding_likelihood) mle.demo() #des arguments peuvent etre utilises
           if (choix==txt_understanding_alpha_and_power) run.power.examp(hscale=1.5, vscale=1.5, wait=FALSE)
           if (choix==txt_understanding_corr) put.points.demo()
           if (choix==txt_understanding_heterogenous_variance_effects) {
             writeLines(desc_with_two_equal_means_ratio_must_be_5_percent)
             run.Pvalue.norm.sim()
           }
           if (choix==txt_understanding_prev_sens_specificity_2) roc.demo()
           if (choix==txt_understanding_corr_2) run.cor2.examp()
           if (choix==txt_understanding_negative_positive_predic_power) {
             for(i in seq(1,11,2)) {
               SensSpec.demo(sens=0.95, spec=0.99, prev=0.01, step=i) # on peut modifier sensibilite et specificite
               if( interactive() ) {
                 readline(ask_press_enter_to_continue)
               }
             }
           }



    ref1(packages)->Resultats
    return(Resultats)

           }
