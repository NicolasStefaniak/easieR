save <-
  function(Resultats=NULL, choix, env=.GlobalEnv){options (warn=-1)
    # Resultats = object that must be saved
    # choix = name of the file
    # env = environment in which to find the object
    packages<-c('svDialogs', 'htmltools' )
    test2<-try(lapply(packages, library, character.only=T), silent=T)
    if(class(test2)== 'try-error') return(ez.install())
    Resultats <- list()

    if(is.null(Resultats) & exists("ez.results")) Resultats<-ez.results else return(.dico[["desc_no_result_saved"]])
    ez.html <-function(ez.results=Resultats)

    fileHTML<-file.path("file:/", tempdir(), "easieR/Rapport.easieR.html") # TODO not working on Windows I guess?
    fileNAME<-dlgInput(.dico[["ask_filename"]])$res
    fileNAME<-strsplit(fileNAME, ":")
    fileNAME<-tail(fileNAME[[1]],n=1)
    save_html(html=fileHTML, file=fileNAME, background = "white")

    Resultats<-NULL
    Resultats$SAUVEGARDE<-paste(.dico[["desc_data_saved_in"]], getwd())

  }
