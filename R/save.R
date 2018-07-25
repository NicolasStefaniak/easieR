save <-
function(Resultats, choix, env=.GlobalEnv){options (warn=-1)
  # Resultats = object that must be saved
  # choix = name of the file 
  # env = environment in which to find the object
  require(rtf)
  gsub(":",".",date())->date
  output<-paste(choix,date, ".doc")
  rtf<-RTF(output,width=30,height=20,font.size=12,omi=c(1,1,1,1))
  
  to.rtf<-function(Resultats, X=1){
    for(i in 1:length(Resultats)){
      names(Resultats)[[i]]->titres
      addHeader(rtf,title=titres, font.size=(22-2*X), TOC.level=i)
      if(any(class(Resultats[[i]])=="chr")|any(class(Resultats[[i]])=="character")) {addText.RTF(rtf, Resultats[[i]])
        addNewLine(rtf, n=2)
      }
      if(any(class(Resultats[[i]])=="matrix") && any(class(Resultats[[i]])=="table")) class(Resultats[[i]])<-"matrix"
      if(any(class(Resultats[[i]])=="matrix")) {
        data.frame(Resultats[[i]]) ->essai
        round(essai,4)->essai
        addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(essai)) ))
        addNewLine(rtf, n=2)
      }
      
      if(any(class(Resultats[[i]])=='bibentry')){"voir console"->console
        addText.RTF(rtf, console)
        addNewLine(rtf, n=2)
      }
      
      
      if(any(class(Resultats[[i]])=="data.frame") && length(Resultats[[i]])!=0) {
        if(any(sapply(Resultats[[i]], class)=="numeric")) Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"]<-lapply(Resultats[[i]][,sapply(Resultats[[i]], class)=="numeric"],round,4)
        addTable(rtf,Resultats[[i]], row.names=TRUE,col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]]))))
        addNewLine(rtf, n=2)
      }
      
      
      if(any(class(Resultats[[i]])=="table")) {
        matrix(Resultats[[i]], ncol=ncol(Resultats[[i]]))->essai
        data.frame(essai)->essai
        dimnames(Resultats[[i]])[[2]]->names(essai)
        dimnames(Resultats[[i]])[[1]]->dimnames(essai)[[1]]
        unlist(lapply(names(essai), function(x) max(nchar(x))))->largeur
        addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=c(1,largeur/10) )
        addNewLine(rtf, n=2)
      }
      if(any(class(Resultats[[i]])=="numeric") ) {
        if(length(Resultats[[i]])==1) {
          addText.RTF(rtf, Resultats[[i]])
          addNewLine(rtf, n=2)}else{
            
            round(matrix(Resultats[[i]],nrow=1),4)->essai
            dimnames(essai)[[2]]<-names(Resultats[[i]])
            dimnames(essai)[[1]]<-list()
            addTable(rtf,essai,row.names=TRUE, col.justify= "C",header.col.justify="C",col.widths=rep(1.0,(1+length(Resultats[[i]])) ))
            addNewLine(rtf, n=2)
          }
      }
      
      if(any(class(Resultats[[i]])=="list") ){
        Resultats[[i]]->Y
        to.rtf(Y, X=X+1)
      }
    }
  }
  to.rtf(Resultats, 1)
  done(rtf) 
  
  data<-get("data", env=env)
  data->Resultats$donnees
  date()->date
  gsub(":",".",date)->date
  dput(Resultats, file=paste(choix, date,".txt"))
  Resultats[[length(Resultats)]]<-NULL
  Resultats$SAUVEGARDE<-paste("les donnees sont sauvegardees dans", getwd())
  
}
