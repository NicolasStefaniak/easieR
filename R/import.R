import <-
  function(file=NULL, dir=NULL, type=NULL, header=T, info=TRUE, dec=".", sep=";",na.strings="NA",sheet=NULL, name="dataset"){
    # file : Character. Name of the file to import
    # dir : Character. Directory in which the file is stored
    # type : One among "csv", "txt", "excel", "SPSS"
    # header : Logical. Is the name of the variables on the first row ? 
    # info : logical
    # dec : Character. The character used in the file for decimal points.
    # sep : The field separator character. Values on each line of the file are separated by this character. 
    # na.strings :a character vector of strings which are to be interpreted as NA values. Blank fields are also considered to be missing values in logical, integer, numeric and complex fields.    
    # sheet:  Character. The name of the sheet for excel files.   
    # name : Character. Name of the R object in which to store the data. 
    
    
    options (warn=-1)
    c("svDialogs",  "readxl","foreign", "textclean")->packages
    lapply(packages, require,character.only=T)
    Resultats <- list()
    if(info==TRUE) print("Dans quel format est enregistre votre fichier ?")
    if(!is.null(type)){
      type<-switch(type, 
                   "csv"="Fichier CSV","Fichier CSV"="Fichier CSV" ,
                   "txt"="Fichier txt","Fichier txt"="Fichier txt" ,
                   "excel"="Fichier Excel","Fichier Excel"="Fichier Excel", 
                   "SPSS" =  "fichier SPSS","fichier SPSS"="fichier SPSS" )
    }
    if(is.null(type)) type <- dlgList(c("Fichier CSV", "Fichier txt", "Fichier Excel", "fichier SPSS"), preselect="Fichier Excel", multiple = FALSE, title="Format du fichier?")$res
    if(length(type)==0) return(donnees())
    
    if(!is.null(dir)) try(setwd(dir), silent=T)
    if(!is.null(file) && file.exists( file)) {file<-file 
    dial<-F}else {dial<-T
    file <- try(file.choose(), silent=TRUE)
    if(class(file)=="try-error") return(import())
    setwd(dirname(file))
    basename(file)->file
    }
    
    
    
    if(type!="fichier SPSS"){
      if(dial | (dec %in% c(".",","))==FALSE | (sep %in% c(" ", "\t",";",","))==FALSE |!is.logical(header)){
        if(info==TRUE) print("Est-ce que le nom des variables est sur la premiere ligne de votre base de donnees ? Choisir TRUE si c'est le cas")
        header <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title="Nom de variables?")$res
        if(length(header)==0) return(import())
        
        if(info==TRUE) print("Si certaines donnees sont manquantes, comment sont-elles definies ? Vous pouvez laisser NA si les cellules sont vides")
        na.strings <- dlgInput("Par quelle valeur sont definies les valeurs manquantes ?", "NA")$res
        if(length(na.strings)==0) na.strings <- "NA"
        na.strings <- strsplit(na.strings, ":")
        na.strings <- tail(na.strings[[1]],n=1)
        
        
        if(type=="Fichier CSV"|type=="Fichier txt"){
          if(info==TRUE) print("Lors de l'enregistrement de votre fichier, quel est l'indice de separation des colonnes ?")
          sep <- dlgList(c("espace","tab","point virgule","virgule"), preselect="point virgule", multiple = FALSE, title="Separateur de colonnes")$res
          if(length(sep)==0) return(import())
          m1 <- matrix(c("espace","tab","point virgule","virgule"," ","\t",";",","),nrow=4)
          sep <- subset(m1, m1[,1] %in% sep)[,2]
          
          if(info==TRUE) print("Si certaines donnees contiennent des decimales, quel est le symbole indiquant la decimale ?")
          dec <- dlgList(c("point", "virgule"), preselect=NULL, multiple = FALSE, title="Separateur de decimales")$res
          if(length(dec)==0) return(import())
          m1 <- matrix(c("point", "virgule",".",","),nrow=2)
          dec <- subset(m1, m1[,1] %in% dec)[,2]  
        }
      }
    }
    if(type=="fichier SPSS") {
      #basename(file)->file
      data1<-read.spss(file, to.data.frame=TRUE)
      col.char <-sapply(data1, is.factor)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(type=="Fichier CSV") data1 <- read.csv2(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type=="Fichier txt") data1 <- read.table(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type=="Fichier Excel"){
      basename(file)->file
      writeLines("Veuillez specifier la feuille de calcul que vous souhaitez importer")
      if(is.null(sheet) || (sheet %in%  excel_sheets(file))==FALSE){
        eval(parse(text=paste0("  dlgList( excel_sheets('",file,
                               "'), preselect=FALSE, multiple = FALSE, title='Quelle feuille ?')$res->sheet")))
        if(length(sheet)==0) return(import())    
      }
      
      eval(parse(text=paste0("data1 <- read_excel(path='", file, "', sheet='", sheet, "', col_names=as.logical(", header, "), na='", na.strings, "')")))
      col.char <-sapply(data1, is.character)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(dial)  { 
      if(type=="Fichier Excel") name<-sheet else name<-file
      name <- dlgInput("Quel nom voulez-vous donner aux donnees ?", name)$res
    if(length(name)==0) name <- "data1"
    name <- strsplit(name, ":")
    name <- tail(name[[1]],n=1)
    }
    if(grepl("[^[:alnum:]]", name)) {
      writeLines("Des caracteres non autorises ont ete utilises pour le nom. Ces caracteres ont ete remplaces par des points")
      gsub("[^[:alnum:]]", ".", name)->name
    }
     nameV<-replace_non_ascii(names(data1))
     names(data1)<-nameV
     data1<-data.frame(data1)
    
    if(any(nchar(names(data1))>30)) {
      dlgMessage("Certaines variables ont des noms particulierement longs pouvant gener la lecture. Voulez-vous les raccourcir?", "yesno")$res->rn
      if(rn=="yes"){
        which(nchar(names(data1))>30)->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput("Quel nom voulez-vous attribuer a", colnames(data1)[rn[i]])$res 
          if(length(rn2)!=0){
            strsplit(rn2, ":")->rn2
            tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
          }
          
        }
      }
    }
    
    if(any( grepl("[^[:alnum:][:space:]_.]", names(data1)))) {
      writeLines("Evitez les espaces ainsi que les signes de ponctuations, a l'exception . et _ ")
      dlgMessage("Certaines noms de variables contiennent des caracteres speciaux pouvant creer des bugs. Voulez-vous renommer ces variables ?", "yesno")$res->rn
      if(rn=="yes"){
        grep("[^[:alnum:][:space:]_.]", names(data1))->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput("Quel nom voulez-vous attribuer a", colnames(data1)[rn[i]])$res 
          strsplit(rn2, ":")->rn2
          tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
        }
      }
    }
    
    if(any(is.na(data1))){
      writeLines("Nombre de valeurs manquantes par variable")
      print(sapply(data1, function(x) sum(length(which(is.na(x))))) )
    }
    
    
    assign(x=name, value=data1, envir=.GlobalEnv)
    try(View(data1, "Vos donnees"), silent=T)
    str(data1)
    Resultats <- "les donnees ont ete importees correctement"
    call.txt<-paste0("import(file='", file, "',dir='",getwd(),"',type='",type,"',dec='",dec, 
                     "',sep='",sep,"',na.strings='", na.strings,"',sheet=" ,
                     ifelse(is.null(sheet), "NULL",paste0("'", sheet,"'")),",name='",name,"')")
    Resultats$call<-call.txt
    .add.history(data=data1, command=Resultats$Call, nom=name)
    return(Resultats)
    
  }
