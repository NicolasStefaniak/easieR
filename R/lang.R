#import_dict <- function(lang) {
#  .dico <<- new.env(parent=emptyenv())
#  if (lang=='Français') {
#    dictionnary <- file("./Dict_FR.txt","r")
#    lines <- readLines(dictionnary)
#    for (line in lines) {
#      s <- strsplit(line, " ::::: ")
#      if (length(s[[1]]) == 2) { assign(s[[1]][[1]], s[[1]][[2]], envir=.dico) }
#    }
#  } else if (lang=='English') {
#    dictionnary <- file("./Dict_EN.txt","r")
#    lines <- readLines(dictionnary)
#    for (line in lines) {
#      s <- strsplit(line, " ::::: ")
#      if (length(s[[1]]) == 2) { assign(s[[1]][[1]], s[[1]][[2]], envir=.dico) }
#    }
#  } else {
#    dictionnary <- file("./Dict_EN.txt","r")
#    lines <- readLines(dictionnary)
#    for (line in lines) {
#      s <- strsplit(line, " ::::: ")
#      if (length(s[[1]]) == 2) { assign(s[[1]][[1]], s[[1]][[2]], envir=.dico) }
#    }
#  }
#}

load_language <- function(lang='auto') {
  if (lang=='auto') {
    if(grepl('=fr_',Sys.getlocale()) | grepl('French',Sys.getlocale())) {
	    #import_dict("Français")
	    load_fr_FR()
            print('[INFO] Version française chargée.')
    } else {
	    #import_dict("English")
	    load_en_EN()
            print('[INFO] English language loaded (default).')
    }
  } else {
	if (lang=='Français') {
	    load_fr_FR()
	    #import_dict("Français")
            print('Version française chargée.')
	} else if (lang=='English') {
	    load_en_EN()
	    #import_dict("English")
            print('English language loaded (default).')
	} else {
	    load_en_EN()
	    #import_dict("English")
	    print('Not available. English language loaded (default).')
	}
  }
}

select_language <- function() {
  require(svDialogs)
  lang <- dlgList(c('English',
		    'Français'),
		  preselect=NULL,
		  multiple = FALSE,
		  title=.dico[["ask_what_is_your_choice"]])$res
  if (length(lang)!=0) {load_language(lang=lang) }
  return(easieR())
}
