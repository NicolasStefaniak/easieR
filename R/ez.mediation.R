ez.mediation <-
  function(info=T){
    options (warn=-1) 
    mediation.effect.bar.plot2 <-function (x, mediator, dv, main = "Mediation Effect Bar Plot", 
                                           width = 1, left.text.adj = 0, right.text.adj = 0, rounding = 3, 
                                           file = "", save.pdf = FALSE, save.eps = FALSE, save.jpg = FALSE){
      Mediation.Results <- mediation(x = x, mediator = mediator, 
                                     dv = dv, conf.level = 0.95,complete.set=TRUE)
      observed.c <- Mediation.Results$Y.on.X$Regression.Table[2, 
                                                              1]
      observed.c.prime <- Mediation.Results$Y.on.X.and.M$Regression.Table[2, 
                                                                          1]
      max.possible.c <- sqrt(var(dv))/sqrt(var(x))
      if (observed.c < 0) 
        max.possible.c <- -max.possible.c
      if (width < 1) {
        width <- 0.5 * (1 - width)
      }
      if (width > 1) {
        width <- 0.5 * (1 + width)
      }
      if (save.pdf == TRUE) {
        if (save.eps == TRUE) 
          stop("Only one file format for saving figure may be used at a time (you have both PDF and EPS specified).")
        if (save.jpg == TRUE) 
          stop("Only one file format for saving figure may be used at a time (you have both PDF and JPG specified).")
      }
      if (save.eps == TRUE) {
        if (save.jpg == TRUE) 
          stop("Only one file format for saving figure may be used at a time (you have both EPS and JPG specified).")
      }
      if (save.pdf == TRUE | save.eps == TRUE | save.jpg == TRUE) {
        no.file.name <- FALSE
        if (file == "") {
          file <- "mediation.effect.bar.plot"
          no.file.name <- TRUE
        }
      }
      if (save.pdf == TRUE) 
        pdf(file = paste(file, ".pdf", sep = ""))
      if (save.eps == TRUE) 
        jpeg(filename = paste(file, ".eps", sep = ""))
      if (save.jpg == TRUE) 
        jpeg(filename = paste(file, ".jpg", sep = ""))
      plot(c(-2, 2), seq(0, 1), ylab = "", xlab = "", xaxt = "n", 
           yaxt = "n", bty = "n", type = "n", main = main)
      segments(x0 = -0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 1)
      segments(x0 = 0.5 * width, y0 = 0, x1 = 0.5 * width, y1 = 1)
      segments(x0 = 0.5 * width, y0 = 0, x1 = -0.5 * width, y1 = 0)
      segments(x0 = 0.5 * width, y0 = 1, x1 = -0.5 * width, y1 = 1)
      segments(x0 = 0.5 * width, y0 = observed.c/max.possible.c, 
               x1 = -0.5 * width, y1 = observed.c/max.possible.c)
      segments(x0 = 0.5 * width, y0 = observed.c.prime/max.possible.c, 
               x1 = -0.5 * width, y1 = observed.c.prime/max.possible.c)
      rect(xleft = -0.5 * width, ybottom = 0, xright = 0.5 * width, 
           ytop = observed.c.prime/max.possible.c, density = 10, 
           angle = 45, border = NA)
      rect(xleft = -0.5 * width, ybottom = observed.c.prime/max.possible.c, 
           xright = 0.5 * width, ytop = observed.c/max.possible.c, 
           density = 10, angle = 135, border = NA)
      if (left.text.adj == 0) {
        left.text.adj <- -0.5 * width - (0.5 * width/3)
      }
      if (left.text.adj != 0) {
        left.text.adj <- -0.5 * width - (0.5 * width/3) + left.text.adj
      }
      if (right.text.adj == 0) {
        right.text.adj <- 0.5 * width + (0.5 * width/20)
      }
      if (right.text.adj != 0) {
        right.text.adj <- 0.5 * width + (0.5 * width/20) + right.text.adj
      }
      use.this <- round(max.possible.c, rounding)
      text(x = right.text.adj * 1.3, y = 1, bquote(paste(plain("max possible"), 
                                                         phantom(x), italic(c) == .(use.this))))
      use.this <- round(observed.c, rounding)
      text(x = left.text.adj, y = observed.c/max.possible.c, bquote(paste(plain(observed), 
                                                                          phantom(x), italic(c) == .(use.this))))
      use.this <- round(observed.c.prime, rounding)
      text(x = left.text.adj, y = observed.c.prime/max.possible.c, 
           bquote(paste(plain(observed), phantom(x), italic(c), 
                        phantom(x), plain(prime) == .(use.this))))
      use.this <- round(observed.c - observed.c.prime, rounding)
      text(x = right.text.adj, y = observed.c/max.possible.c - 
             observed.c.prime/max.possible.c, bquote(italic(ab) == 
                                                       .(use.this)))
      segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
               x1 = right.text.adj * 0.6, y1 = observed.c.prime/max.possible.c)
      segments(x0 = right.text.adj * 0.6, y0 = observed.c/max.possible.c, 
               x1 = right.text.adj * 0.55, y1 = observed.c/max.possible.c)
      segments(x0 = right.text.adj * 0.6, y0 = observed.c.prime/max.possible.c, 
               x1 = right.text.adj * 0.55, y1 = observed.c.prime/max.possible.c)
      text(x = right.text.adj * 0.8, y = 0, "zero")
      if (save.pdf == TRUE) {
        dev.off()
        if (no.file.name == TRUE) 
          print(paste("'mediation.effect.bar.plot.pdf' file saved at the directory", 
                      getwd()))
      }
      if (save.eps == TRUE) {
        dev.off()
        if (no.file.name == TRUE) 
          print(paste("'mediation.effect.bar.plot.eps' file saved at the directory", 
                      getwd()))
      }
      if (save.jpg == TRUE) {
        dev.off()
        if (no.file.name == TRUE) 
          print(paste("'mediation.effect.bar.plot.jpg' file saved at the directory", 
                      getwd()))
      }
    }
    
    
    
    .e <- environment()
    c("boot", "MBESS","QuantPsyc", "svDialogs")->packages
    try(lapply(packages, library, character.only=T), silent=T)->test2
    if(class(test2)== "try-error") return(ez.install())
    Resultats<-list()
    dlgList(c("Effets de mediation simple", 
              "Effet de mediation distante"), preselect=NULL, multiple = FALSE, title="Quel type de mediation ?")$res->choix
    if(length(choix)==0) return(analyse())
    choix.data(nom=T)->data
    if(is.null(data)) return(ez.mediation())
    data[[1]]->nom
    data[[2]]->data
    listes<-data.frame(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), names(data))
    if(info) writeLines("veuillez preciser le predicteur")
    X<-dlgList(paste(names(data), "(format :", sapply(data, class), ")", sep=" "), multiple = F, 
               title="Predicteur")$res
    if(length(X)==0) return(ez.mediation())
    subset(listes, listes[,1] %in% X)[,2]->X
    as.character(X)->X
    if(info) writeLines("veuillez choisir le mediateur")
    Mediator<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                      title="Mediateur")$res
    if(length(Mediator)==0) return(ez.mediation())
    subset(listes, listes[,1] %in% Mediator)[,2]->Mediator
    as.character(Mediator)->Mediator
    if(choix=="Effet de mediation distante"){
      writeLines("veuillez preciser le second mediateur.")
      Mediator2<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, title="Mediateur 2")$res
      if(length(Mediator2)==0) return(ez.mediation())
      subset(listes, listes[,1] %in% Mediator2)[,2]->Mediator2
      as.character(Mediator2)->Mediator2
    }
    
    if(info) writeLines("veuillez choisir la variable dependante")
    VD<-dlgList(c(paste(names(data), "(format :", sapply(data, class), ")", sep=" ")), multiple = F, 
                title="Variable dependante")$res
    subset(listes, listes[,1] %in% VD)[,2]->VD
    as.character(VD)->VD
    writeLines("veuillez preciser le nombre de bootstrap. Un minimum de 500 est idealement requis. Peut prendre du temps pour N>1000")
    n.boot<-dlgInput("Nombre de bootstrap ?", 1)$res
    if(length(n.boot)==0) n.boot<-"0"
    strsplit(n.boot, ":")->n.boot
    tail(n.boot[[1]],n=1)->n.boot
    as.numeric(n.boot)->n.boot
    if(!is.na(n.boot) && any(n.boot>50)) bootstrap<-TRUE else bootstrap<-FALSE
    
    if(choix=="Effets de mediation simple"){
      mediation(data[,X], data[,Mediator], data[,VD], conf.level = 0.95, bootstrap = bootstrap, B = n.boot, which.boot="both", save.bs.replicates=TRUE, complete.set=TRUE)->mediation.out
      for(i in 1:length(mediation.out)){
        if(class(mediation.out[[i]])== "list") for(j in 1 : length(mediation.out[[i]])){
          round(mediation.out[[i]][[j]], 4)->mediation.out[[i]][[j]]} else {
            round(mediation.out[[i]], 4)->mediation.out[[i]]}
      }  
      Resultats$Analyse.mediation<-mediation.out
      Resultats$Information<-"Pour une description detaillee des resultats, ?mediation"
      mediation.effect.bar.plot2(data[,X], data[,Mediator], data[,VD],main = "Mediation Effect Bar Plot", width = 1, left.text.adj = 0,right.text.adj = 0, rounding = 3, file = "", save.pdf = FALSE,save.eps = FALSE, save.jpg = FALSE)
    }else { data2<-data[,c(X, Mediator, Mediator2, VD)]
    names(data2)<-c("x", "m1","m2","y")
    distal.med(data2)->results
    data.frame(results)->results
    round(as.numeric(as.character(results$Effect)),4)->results$Effect
    round(as.numeric(as.character(results$SE)),4)->results$SE
    round(as.numeric(as.character(results[,3])),3)->results$t.ratio
    round(as.numeric(as.character(results$Med.Ratio)),4)->results$Med.Ratio 
    names(results)<-c("Effet", "Erreur.st","test.t", "Ratio.med")
    results->Resultats$"Mediation a distance"
    Resultats$Information<-"Pour une description detaillee des resultats, ?distal.med"
    distmed.boot <- boot(data2, distInd.ef, R=n.boot)
    boot.ci(distmed.boot, conf=.95, type=c("basic","perc", "norm"))->IC.boot
    round(matrix(c(IC.boot$normal[,2:3],IC.boot$basic[,4:5],IC.boot$percent[,4:5]), ncol=2 ),4)->IC.boot
    dimnames(IC.boot)[[1]]<-c("normal","basic","percentile")
    dimnames(IC.boot)[[2]]<-c("limite.inf","limite.sup")
    IC.boot->Resultats$"Intervalle de confiance estime par bootstrap"}
    
    dlgList(c("TRUE","FALSE"), preselect="FALSE", multiple = FALSE, title="voulez-vous sauvegarder?")$res->sauvegarde
    if(length(sauvegarde)==0) sauvegarde<-FALSE  
    if(sauvegarde) save(Resultats=Resultats, choix=choix, env=.e)
    ref1(packages)->Resultats$"References" 
    ez.html(Resultats)
    return(Resultats)  
    
  }
