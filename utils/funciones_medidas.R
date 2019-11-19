smin<-function(vector){
  min(vector[vector>min(vector,na.rm=T)],na.rm = T)
}

nnegas<-function(vector){
  length(unique(vector[vector<0 & !is.na(vector)]))
}

susnas<-function(vector){
  print(names(vector))
  x<-vector
  x[is.na(vector)]<-.00000000987654321*(-1)#.00000000987654321*(-1)
  return(x)
}

tvena<-function(vector){
  -.00000000987654321%in%vector
}

tna<-function(vector){
  if(sum(is.na(vector))>0){respuesta<-TRUE}else{respuesta<-FALSE}
  return(respuesta)
}

nunis<-function(vector){
  length(unique(vector))
}

nnas<-function(vector){
  sum(is.na(vector))
}

ninfs<-function(vector){
  sum(is.infinite(vector))
}

media_sinmin<-function(vector){
  # m<-min(vector[vector>min(vector,na.rm=T)],na.rm = T)
  mean(vector[vector>min(vector,na.rm=T)],na.rm = T)
}

media_sin2min<-function(vector){
  # m<-min(vector[vector>min(vector,na.rm=T)],na.rm = T)
  mean(vector[vector>smin(vector)],na.rm = T)
}

# if(!exists("df_varpcas")){df_varpcas<-readRDS("data/df_varpcas_84.rds")}
# pcareq<-function(propmin=.8,acumvarpcas=df_varpcas$acumvar){
#   
#   which.min(acumvarpcas<=propmin)
#   
# }