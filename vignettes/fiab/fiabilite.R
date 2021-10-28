fiabilite<-function (X = NULL, Y = NULL, data = NULL, choix = NULL, ord = NULL, 
          outlier = "Donnees completes", keys = NULL, n.boot = NULL, 
          sauvegarde = F, imp = NULL, html = TRUE) 
{
  options(warn = -1)
  packages <- c("svDialogs", "psych", "lavaan")
  test2 <- try(lapply(packages, library, character.only = T), 
               silent = T)
  if (class(test2) == "try-error") 
    return(ez.install())
  .e <- environment()
  rev<-FALSE
  Resultats <- list()
  if (is.null(data) | is.null(X)) {
    dial <- TRUE
  }
  else dial <- F
  if (dial || is.null(choix) || length(choix) != 1 || choix %in% 
      c("Alpha de Cronbach", "alpha", "ICC", 
        "CCK", "Correlation intra-classe", "Coefficient de concordance de Kendall") == 
      FALSE) {
    dial <- T
    writeLines("Veuillez choisir l'analyse que vous desirez realiser.")
    choix <- dlgList(c("Alpha de Cronbach", "Correlation intra-classe", 
                       "Coefficient de concordance de Kendall"), preselect = NULL, 
                     multiple = FALSE, title = "Quelle analyse voulez-vous realiser?")$res
    if (length(choix) == 0) 
      return(analyse())
  }
  if (dial || class(data) != "data.frame") {
    data <- choix.data(data = data, nom = T)
    if (length(data) == 0) 
      return(analyse())
    nom <- data[[1]]
    data <- data[[2]]
  }
  else {
    nom <- deparse(substitute(data))
  }
  if (choix == "CCK" | choix == "Coefficient de concordance de Kendall") {
    msg3 <- "Veuillez choisir le premier juge"
    type <- "factor"
    title <- "Juge 1"
    multiple <- T
  }
  else {
    multiple <- T
    msg3 <- "Veuillez choisir les variables que vous desirez analyser."
    type <- "numeric"
    title <- "variables"
  }
  X <- easieR:::.var.type(X = X, data = data, type = type, check.prod = F, 
                 message = msg3, multiple = multiple, title = title, out = NULL)
  if (is.null(X)) {
    Resultats <- fiabilite()
    return(Resultats)
  }
  data <- X$data
  X <- X$X
  if (choix %in% c("Alpha de Cronbach", "Correlation intra-classe", 
                   "ICC", "alpha")) {
    if (dial || length(outlier) > 1 || outlier %in% c("Donnees completes", 
                                                      "Donnees sans valeur influente") == FALSE) {
      writeLines("Desirez-vous l'analyse sur les donnees completes ou sur les donnees pour lesquelles les valeurs influentes ont ete enlevees ?")
      writeLines("les valeurs influentes sont identifiees sur la base de la distance de Mahalanobis avec un seuil du chi a 0.001")
      outlier <- dlgList(c("Donnees completes", "Donnees sans valeur influente"), 
                         preselect = "Donnees completes", multiple = FALSE, 
                         title = "Quels resultats voulez-vous obtenir ?")$res
      if (length(outlier) == 0) {
        Resultats <- fiabilite()
        return(Resultats)
      }
    }
    if (outlier == "Donnees sans valeur influente") {
      inf <- VI.multiples(data[, X])
      Resultats$"Valeurs considerees comme influentes" <- inf$"Valeurs considerees comme influentes"
      data <- inf$data
    }
    if (choix %in% c("Alpha de Cronbach", "alpha")) {
      if (dial) {
        writeLines("Veuillez preciser le type de variables. Des correlations tetra/polychoriques seront realisees sur les variables ordinales et Bravais-Pearson sur les variables continues")
        type <- dlgList(c("dichotomiques/ordinales", 
                          "continues", "mixte"), preselect = NULL, 
                        multiple = FALSE, title = "Nature des variables ?")$res
        if (length(type) == 0) {
          Resultats <- fiabilite()
          return(Resultats)
        }
      }
      else {
        if (is.null(ord)) 
          type <- "continues"
        else type <- "dichotomiques/ordinales"
      }
      if (dial) {
        writeLines("Y a-t-il des items inverses ?")
        rev <- dlgList(c(TRUE, FALSE), multiple = FALSE, 
                       title = "items inverses?")$res
        if (length(rev) == 0) {
          Resultats <- fiabilite()
          return(Resultats)
        }
      }
      if (rev == "TRUE" || !is.null(keys) && any(keys %in% 
                                                 X == FALSE)) {
        writeLines("Veuillez preciser les items inverses")
        keys <- dlgList(X, multiple = TRUE, title = "items inverses?")$res
        if (length(keys) == 0) {
          Resultats <- fiabilite()
          return(Resultats)
        }
      }
      else keys <- NULL
      if (type == "continues") {
        if (!is.null(n.boot) && ((class(n.boot) != "numeric" & 
                                  class(n.boot) != "integer") || n.boot%%1 != 
                                 0 || n.boot < 1)) {
          msgBox("Le nombre de bootstrap doit etre un nombre entier positif")
          n.boot <- NULL
        }
        while (is.null(n.boot)) {
          writeLines("Veuillez preciser le nombre de bootstrap. Pour ne pas avoir de bootstrap, choisir 1")
          n.boot <- dlgInput("Nombre de bootstrap ?", 
                             1)$res
          if (length(n.boot) == 0) {
            Resultats <- fiabilite()
            return(Resultats)
          }
          n.boot <- strsplit(n.boot, ":")
          n.boot <- tail(n.boot[[1]], n = 1)
          n.boot <- as.numeric(n.boot)
          if (is.na(n.boot) || n.boot%%1 != 0 || n.boot < 
              1) {
            msgBox("Le nombre de bootstrap doit etre un nombre entier positif")
            n.boot <- NULL
          }
        }
        cron <- psych::alpha(data[, X], keys = keys, 
                             n.iter = n.boot)
      }
      else {
        n.boot <- 0
        if (type == "mixte") {
          writeLines("Veuillez preciser les variables ordinales ?")
          ord <- dlgList(X, multiple = TRUE, title = "Variables ordinales ?")$res
          if (length(ord) == 0) {
            Resultats <- fiabilite()
            return(Resultats)
          }
        }
        else ord <- X
        Matrice <- tetrapoly(data = data[, X], X = X, 
                             info = T, ord = ord, group = NULL, estimator = "two.step", 
                             output = "cor", imp = imp, html = F)[[1]]
        if (all(class(Matrice) != "matrix")) {
          sortie <- dlgMessage("Vous essayez de faire un alpha sur autre chose qu'un matrice. Voulez-vous sortir de cette analyse?", 
                               type = "yesno")$res
          if (sortie == "yes") 
            return(analyse())
          else Matrice <- tetrapoly(data = data[, X], 
                                    X = X, info = T, ord = ord, group = NULL, 
                                    estimator = "two.step", output = "cor", 
                                    imp = "rm")[[1]]
        }
        cron <- psych::alpha(Matrice, keys = keys, n.obs = length(data[, 
                                                                       1]))
      }
      Resultats$"Alpha de Cronbach sur la totalite de l'echelle" <- round(cron$total, 
                                                                          3)
      if (n.boot > 1) 
        Resultats$"Intervalle de confiance base sur le bootstrap" <- cron$boot.ci
      a1 <- cron$total[, 1]
      ase <- cron$total[, 6]
      Resultats$"Intervalle de confiance base sur l'erreur standard de l'alpha" <- data.frame(Lim.inf.IC.95 = a1 - 
                                                                                                1.96 * ase, alpha = a1, Lim.sup.IC.95 = a1 + 
                                                                                                1.96 * ase)
      Resultats$"fiabilite par item supprime" <- round(data.frame(cron$alpha.drop, 
                                                                  cron$item.stats), 3)
    }
    if (choix == "Correlation intra-classe" | choix == 
        "ICC") {
      ICC.out <- psych::ICC(data[, X], missing = FALSE)
      Resultats$"correlation intra-classe" <- ICC.out[[1]]
      names(Resultats$"correlation intra-classe") <- c("type", 
                                                       "ICC", "F", "ddl1", "ddl2", 
                                                       "valeur.p", "lim.inf", "lim.sup")
      Resultats$informations <- paste("le nombre de juge =", 
                                      length(X), "et le nombre d'observations =", 
                                      ICC.out$n.obs)
    }
  }
  if (choix == "Coefficient de concordance de Kendall") {
    msg4 <- "Veuilez choisir le second juge"
    Y <- .var.type(X = Y, data = data, type = type, check.prod = F, 
                   message = msg4, multiple = F, title = "Juge 2", 
                   out = X)
    if (is.null(Y)) {
      Resultats <- fiabilite()
      return(Resultats)
    }
    data <- Y$data
    Y <- Y$X
    CK.out <- cohen.kappa(data[, c(X, Y)], w = NULL, n.obs = NULL, 
                          alpha = 0.05)
    dimnames(CK.out$confid) <- list(c("Coefficient kappa non pondere", 
                                      "Coefficient kappa pondere"), c("lim.inf", 
                                                                      "estimation", "lim.sup"))
    Resultats$"Coefficient de concordance de Kendall" <- round(CK.out$confid, 
                                                               3)
    Resultats$Accord <- CK.out$agree
    Resultats$information <- paste("le nombre d'observations =", 
                                   CK.out$n.obs)
  }
  if (dial) 
    sauvegarde <- dlgList(c("TRUE", "FALSE"), 
                          preselect = "FALSE", multiple = FALSE, title = "voulez-vous sauvegarder?")$res
  if (length(sauvegarde) == 0) {
    Resultats <- fiabilite()
    return(Resultats)
  }
  X <- paste(X, collapse = "','", sep = "")
  if (!is.null(ord)) 
    ord <- paste(ord, collapse = "','", sep = "")
  if (!is.null(keys)) 
    keys <- paste(ord, collapse = "','", sep = "")
  Resultats$Call <- paste0("fiabilite(X=c('", X, "'),Y=", 
                           ifelse(is.null(Y), "NULL", paste0("'", Y, 
                                                             "'")), ",data=", nom, ",choix='", 
                           choix, "',ord=", ifelse(!is.null(ord), paste0("c('", 
                                                                         ord, "')"), "NULL"), ",outlier='", 
                           outlier, "', keys=", ifelse(!is.null(keys), paste0("c('", 
                                                                              keys, "')"), "NULL"), ",n.boot=", 
                           ifelse(!is.null(n.boot), n.boot, "NULL"), ", sauvegarde=", 
                           sauvegarde, ")")
  easieR:::.add.history(data = data, command = Resultats$Call, nom = nom)
  easieR:::.add.result(Resultats = Resultats, name = paste("cor.polychorique", 
                                                  Sys.time()))
  if (sauvegarde) 
    save(Resultats = Resultats, choix = choix, env = .e)
  Resultats$References <- ref1(packages)
  if (html) 
    try(ez.html(Resultats), silent = T)
  return(Resultats)
}