load_language <- function(lang = "auto") {
  
  detect_lang <- function() {
    
    # 1. Essai LANG (Mac/Linux)
    lang_env <- Sys.getenv("LANG")
    if (nzchar(lang_env)) return(lang_env)
    
    # 2. Essai LC_MESSAGES
    loc_msg <- Sys.getlocale("LC_MESSAGES")
    if (!is.na(loc_msg) && nzchar(loc_msg)) return(loc_msg)
    
    # 3. Fallback Windows (le plus fiable ici)
    return(Sys.getlocale())
  }
  
  if (lang == "auto") {
    
    loc <- detect_lang()
    
    # debug utile
    # print(loc)
    
    if (grepl("fr", loc, ignore.case = TRUE)) {
      load_fr_FR()
      message("[INFO] Version française chargée.")
    } else {
      load_en_EN()
      message("[INFO] English language loaded (default).")
    }
    
  } else {
    
    if (lang == "Français") {
      load_fr_FR()
      message("Version française chargée.")
      
    } else if (lang == "English") {
      load_en_EN()
      message("English language loaded.")
      
    } else {
      load_en_EN()
      message("Not available. English language loaded (default).")
    }
  }
}

#load_language <- function(lang='auto') {
#  if (lang=='auto') {
#    if(grepl('=fr_',Sys.getlocale()) | grepl('French',Sys.getlocale())) {
#	    #import_dict("Français")
#	    load_fr_FR()
 #           print('[INFO] Version française chargée.')
 #   } else {
#	    #import_dict("English")
#	    load_en_EN()
#            print('[INFO] English language loaded (default).')
#    }
#  } else {
#	if (lang=='Français') {
#	    load_fr_FR()
#	    #import_dict("Français")
#           print('Version française chargée.')
#	} else if (lang=='English') {
#	    load_en_EN()
#	    #import_dict("English")
 #           print('English language loaded (default).')
#	} else {
#	    load_en_EN()
#	    #import_dict("English")
#	    print('Not available. English language loaded (default).')
#	}
# }
#}

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
