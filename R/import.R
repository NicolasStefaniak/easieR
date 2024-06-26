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
    c('svDialogs',  'readxl','foreign', 'textclean')->packages
    lapply(packages, require,character.only=T)
    Resultats <- list()
    if(info==TRUE) print(.dico[["ask_file_format_to_import"]])
    if(!is.null(type)){
      if (type=="csv")   .dico[["txt_csv_file"]] -> type
      if (type=="txt")   .dico[["txt_txt_file"]] -> type
      if (type=="excel") .dico[["txt_excel_file"]] -> type
      if (type=="SPSS")  .dico[["txt_spss_file"]] -> type
    }
    if(is.null(type)) type <- dlgList(c(.dico[["txt_csv_file"]], .dico[["txt_txt_file"]], .dico[["txt_excel_file"]], .dico[["txt_spss_file"]]), preselect=.dico[["txt_excel_file"]], multiple = FALSE, title=.dico[["ask_file_format"]])$res
    if(length(type)==0) return(donnees())

    if(!is.null(dir)) try(setwd(dir), silent=T)
    if(!is.null(file) && file.exists( file)) {file<-file
    dial<-F}else {dial<-T
    if (Sys.info()[['sysname']] == 'Linux') {
    	require('tcltk')
    	file <- try(tk_choose.files(), silent=TRUE)
    } else {
    	file <- try(file.choose(), silent=TRUE)
    }
    if(class(file)=='try-error') return(import())
    setwd(dirname(file))
    basename(file)->file
    }



    if(type!=.dico[["txt_spss_file"]]){
      if(dial | (dec %in% c(".",","))==FALSE | (sep %in% c(" ", "\t",";",","))==FALSE |!is.logical(header)){
        if(info==TRUE) print(.dico[["ask_headers_in_database"]])
        header <- dlgList(c(TRUE, FALSE), preselect=TRUE, multiple = FALSE, title=.dico[["ask_variables_names"]])$res
        if(length(header)==0) return(import())

        if(info==TRUE) print(.dico[["ask_missing_values_value_na_on_empty"]])
        na.strings <- dlgInput(.dico[["ask_value_for_missing_values"]], "NA")$res
        if(length(na.strings)==0) na.strings <- "NA"
        na.strings <- strsplit(na.strings, ":")
        na.strings <- tail(na.strings[[1]],n=1)


        if(type==.dico[["txt_csv_file"]]|type==.dico[["txt_txt_file"]]){
          if(info==TRUE) print(.dico[["ask_col_separation_index"]])
          sep <- dlgList(c(.dico[["txt_space"]],"tab",.dico[["txt_semicolon"]],.dico[["txt_comma"]]), preselect=.dico[["txt_semicolon"]], multiple = FALSE, title=.dico[["txt_col_separator"]])$res
          if(length(sep)==0) return(import())
          m1 <- matrix(c(.dico[["txt_space"]],"tab",.dico[["txt_semicolon"]],.dico[["txt_comma"]]," ","\t",";",","),nrow=4)
          sep <- subset(m1, m1[,1] %in% sep)[,2]

          if(info==TRUE) print(.dico[["ask_decimal_symbol"]])
          dec <- dlgList(c(.dico[["txt_dot"]], .dico[["txt_comma"]]), preselect=NULL, multiple = FALSE, title=.dico[["txt_decimal_separator"]])$res
          if(length(dec)==0) return(import())
          m1 <- matrix(c(.dico[["txt_dot"]], .dico[["txt_comma"]],".",","),nrow=2)
          dec <- subset(m1, m1[,1] %in% dec)[,2]
        }
      }
    }
    if(type==.dico[["txt_spss_file"]]) {
      #basename(file)->file
      data1<-read.spss(file, to.data.frame=TRUE)
      col.char <-sapply(data1, is.factor)
      if(any(col.char)) data1[col.char] <- lapply(data1[which(col.char)], factor)
    }
    if(type==.dico[["txt_csv_file"]]) data1 <- read.csv2(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type==.dico[["txt_txt_file"]]) data1 <- read.table(file, header=as.logical(header), sep=sep, dec=dec, na.strings=na.strings)
    if(type==.dico[["txt_excel_file"]]){
      basename(file)->file
      writeLines(.dico[["ask_specify_datasheet_to_import"]])
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
      if(type==.dico[["txt_excel_file"]]) name<-sheet else name<-file
      name <- dlgInput(.dico[["ask_name_for_dataset"]], name)$res
    if(length(name)==0) name <- "data1"
    name <- strsplit(name, ":")
    name <- tail(name[[1]],n=1)
    }
    if(grepl("[^[:alnum:]]", name)) {
      writeLines(.dico[["desc_unauthorized_char_replaced"]])
      gsub("[^[:alnum:]]", ".", name)->name
    }
     nameV<-replace_non_ascii(names(data1))
     names(data1)<-nameV
     data1<-data.frame(data1)

    if(any(nchar(names(data1))>30)) {
      dlgMessage(.dico[["ask_shorten_long_variables_names"]], "yesno")$res->rn
      if(rn=="yes"){
        which(nchar(names(data1))>30)->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput(.dico[["ask_name_to_attribute_to"]], colnames(data1)[rn[i]])$res
          if(length(rn2)!=0){
            strsplit(rn2, ":")->rn2
            tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
          }

        }
      }
    }

    if(any( grepl("[^[:alnum:][:space:]_.]", names(data1)))) {
      writeLines(.dico[["desc_avoid_spaces_and_punctuations"]])
      dlgMessage(.dico[["ask_rename_variables_with_special_char"]], "yesno")$res->rn
      if(rn=="yes"){
        grep("[^[:alnum:][:space:]_.]", names(data1))->rn
        for(i in 1:length(rn)) {
          rn2<- dlgInput(.dico[["ask_name_to_attribute_to"]], colnames(data1)[rn[i]])$res
          strsplit(rn2, ":")->rn2
          tail(rn2[[1]],n=1)->colnames(data1)[rn[i]]
        }
      }
    }

    if(any(is.na(data1))){
      writeLines(.dico[["desc_number_of_missing_values"]])
      print(sapply(data1, function(x) sum(length(which(is.na(x))))) )
    }


    assign(x=name, value=data1, envir=.GlobalEnv)
    try(View(data1, .dico[["txt_your_data"]]), silent=T)
    str(data1)
    Resultats <- .dico[["desc_succesfully_imported"]]
    call.txt<-paste0("import(file='", file, "',dir='",getwd(),"',type='",type,"',dec='",dec,
                     "',sep='",sep,"',na.strings='", na.strings,"',sheet=" ,
                     ifelse(is.null(sheet), "NULL",paste0("'", sheet,"'")),",name='",name,"')")
    Resultats$call<-call.txt
    .add.history(data=data1, command=Resultats$Call, nom=name)
    return(Resultats)

  }
