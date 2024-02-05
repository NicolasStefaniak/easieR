voir <-
function(){
  require('svDialogs')
  data <- Filter( function(x) 'data.frame' %in% class( get(x) ), ls(envir=.GlobalEnv) )
  data<-dlgList(data, multiple = TRUE, title=voir.msg())$res
  if(length(data)==0) return(donnees())
  get(data)->data
  for(i in 1:ncol(data)) {
  	if(class(data[,i])!="factor"){
  	attributes(data[,i])<-NULL}}
  View(data)
}

voir.msg<-function(){
msg<-txt_dataframe_choice

return(msg)}
