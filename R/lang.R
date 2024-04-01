load_language <- function(lang='auto') {
  if (lang=='auto') {
    if(grepl('=fr_',Sys.getlocale()) | grepl('French',Sys.getlocale())) {
	    load_fr_FR()
            print('[INFO] Version française chargée.')
    } else {
	    load_en_EN()
            print('[INFO] English language loaded (default).')
    }
  } else {
	if (lang=='Français') {
	    load_fr_FR()
            print('Version française chargée.')
	} else if (lang=='English') {
	    load_en_EN()
            print('English language loaded (default).')
	} else {
	    load_en_EN()
	   print('Not available. English language loaded (default).')
	}
  }
}

select_language <- function() {
	require(svDialogs)
    lang <- dlgList(c('English',
		      'Français'
		     ), preselect=NULL, multiple = FALSE, title=ask_what_is_your_choice)$res
    if (length(lang)!=0) { load_language(lang=lang) }
    return(easieR())
}
