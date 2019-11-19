#Ajustes a las funciones de TR para que puedan incorporarse pesos diferenciados para cada
#registro (cliente/fila/producto) en caso de requerirse un cálculo de gini ponderado
library(dplyr)

MTR_yk<-function(vx,malos,denomsy=1,porcentaje=5){
  # malos: es la columna de 1's y 0's indicadora
  # denomsy: es la columna de lo que se considere como pesos diferenciados para cada registro ó, en su caso, un vector de 1's
  
  vy<-malos*denomsy #Éste sería ahora el vector de los pesos malos o de 1's y 0's indicando malos
  anti_y<-(1-malos)*denomsy #Éste sería ahora el vector de los saldos buenos o de 1's y 0's indicando buenos
  bam<-data.frame(x=vx, y=vy,z=denomsy,malos=malos,anti_y=anti_y,tm=1)
  
  #Quitando las filas donde vx sea NA
  TRows<-!is.na(bam$x)
  bam<-bam[TRows,]
  
  
  bam<-bam[order(bam$x, -bam$y),]
  
  obj<-bam$y
  denoms<-bam$z
  
  basebiv<-data.frame(x=0,tm=0,c=0,pobl=0,denomsy=-100000)
  
  acumcut<-0
  N.2<-length(obj)
  Npct<-max(c(ceiling(N.2*(porcentaje/100)),1)) #numero de registros minimos segun el porcentaje. Tamanio minimo del bucket
  x_woe<-rep(-1,N.2)
  cut_m1<-0
  
  while (acumcut<N.2) {
    N<-length(obj)
    acum<-cumsum(obj)
    temp<-acum/cumsum(denoms) #Para cuando denoms son 1's, se convierte en seq(1,N,1) 
    nnas<-sum(is.na(temp))
    mini<-min(temp[Npct:N])
    cut<-which.max(cumsum(temp[!is.na(temp)]==mini))+nnas #en caso de haber nas, forzosamente estarían al principio, 
    #por eso se permite sumar la cantidad de registros que se quitan
    
    #Se modifica temporalmente la columna de variable regresora para que no separe casos de un mismo valor
    if(acumcut+Npct<N.2){if (cut==Npct & bam$x[acumcut+Npct]==bam$x[acumcut+Npct+1]) {
      temp[Npct:(Npct+sum(bam$x[(acumcut+Npct):N.2]==bam$x[acumcut+Npct])-2)]<-Inf
      mini<-min(temp[Npct:N])
      cut<-which.max(cumsum(temp[!is.na(temp)]==mini))+nnas #en caso de haber nas, forzosamente estarían al principio, 
      #por eso se permite sumar la cantidad de registros que se quitan
      
    }
    }
    
    acumcut.0<-acumcut
    acumcut<-cut+acumcut
    
    #Ajuste para el caso en el que lo que resta de "x" sea de tamaño menor que el porcentaje mínimo requerido
    if (acumcut>(N.2-Npct)) {acumcut=N.2 ; mini=temp[N] ; cut=N}
    
    datatemp<-data.frame(x=bam$x[acumcut],tm=mini,c=acumcut,pobl=cut,denomsy=-100000)
    basebiv<-rbind(basebiv,datatemp)
    x_woe[(acumcut.0+1):acumcut]<-mini
    
    #Redefiniendo los vectores del numerador y denomindador de la var obj
    obj<-obj[(cut+1):N]
    denoms<-denoms[(cut+1):N]
    #Etiquetando con el "mini" a los registros de bam que corresponden al bucket que se está concluyendo
    bam$tm[(cut_m1+1):(cut_m1+cut)]<-mini
    #Guardando la suma de los saldos de los clientes de ese bucket
    basebiv$denomsy[nrow(basebiv)]<-sum(bam$z[(cut_m1+1):(cut_m1+cut)])
    #Guardando el cut de este bucket en cut_m1 para usarse en la siguiente iteración
    cut_m1<-cut_m1+cut
  }
  
  basebiv<-basebiv[-1,]
  bb2<-data.frame(pesos=bam$z, malos=bam$malos, tm=bam$tm)
  Lst<-list(vx_woe=x_woe,bb=basebiv,vy_woe=bam$y,bb2=bb2)
  return(Lst)
}

MTR2_yk2<-function(rayk_bb){
  
  agb<-(1-rayk_bb$tm)*rayk_bb$denomsy
  agm<-rayk_bb$tm*rayk_bb$denomsy
  agb<-c(0,cumsum(agb)/sum(agb))
  agm<-c(0,cumsum(agm)/sum(agm))
  
  sum((agb[-1]+agb[-length(agb)])*(agm[-1]-agm[-length(agm)]))-1
}

MTR2_yk2_4dplyr<-function(tm,denomsy){
  
  # agS<-rayk_bb%>%
  #   mutate(
  # agb=(1-tm)*denomsy#,
  # agm=tm*denomsy#)
  agb=(1-tm)*denomsy
  agm=tm*denomsy
  agb<-c(0,cumsum(agb)/sum(agb))
  agm<-c(0,cumsum(agm)/sum(agm))
  sum((agb[-1]+agb[-length(agb)])*(agm[-1]-agm[-length(agm)]))-1
}

MTR3_yk<-function(vx,malos,denomsy=1,porcentaje=5){
  
  ra<-MTR_yk(vx,malos,denomsy,porcentaje)
  rd<-MTR_yk(-vx,malos,denomsy,porcentaje)
  
  ga<-MTR2_yk2(ra$bb)#2)
  gd<-MTR2_yk2(rd$bb)#2)
  
  Lst<-list(tra=ra,trd=rd,ga=ga,gd=gd)
  return(Lst)
}

MTR4_yk<-function(fbase,vvect,vobj,denomsy=NULL,vesp=list(jkl=pi),vcat=NULL,porcentaje=5,verbose=FALSE){
  
  TRows<-!is.na(fbase[,vobj])
  fbase<-fbase[TRows,]
  if(is.null(denomsy)){
    denomsy<-rep(1,sum(!is.na(TRows)))  
  }else{
    denomsy<-denomsy[TRows]  
  }
  
  
  TDnms<-!is.na(denomsy)
  fbase<-fbase[TDnms,]
  denomsy<-denomsy[TDnms]
  
  
  ipm5<-data.frame(var="Variable",tndnc="tendencia",Gini=0)
  tb<-data.frame(x=0,tm=0,c=0,pobl=0,denomsy=-10000000,tndnc="tendencia",var="variable")
  
  for(a in vvect){ 
    if(verbose){print(a)}
    res<-NULL
    
    if(a%in%vcat){
      print(paste0("categorica:-> ",a))
      bbbase<-data.frame(x=as.numeric(fbase[,a]),y=as.numeric(fbase[,vobj]),denomsy=as.numeric(denomsy))  
      
      # baseesp<-bbbase
      
      bbbase$vy<-bbbase$y*bbbase$denomsy #  Éste sería ahora el vector de los pesos malos o de 1's y 0's indicando malos del subset

        #Sumando los pesos malos y luego los pesos de todos los registros;
        tbcat1_1<-aggregate(vy~x,bbbase, sum)
        tbcat1_2<-aggregate(denomsy~x,bbbase, sum)
        #Agregamos el vector de tasa de pesos malos
        tbcat1_2$tm<-tbcat1_1$vy/tbcat1_2$denomsy
        #cambiando los casos de tm=0 por .0001 y de tm==1 por .9999
        tbesp1_2$tm[tbesp1_2$tm==0]<-.0001
        tbesp1_2$tm[tbesp1_2$tm==1]<-.9999
        # Calculando la cantidad de información que hay en cada categoria
        tbcat2<-aggregate(vy~x,bbbase, length)
        names(tbcat2)[2] <- "pobl"
        
        tbcat<-merge(tbcat1_2,tbcat2)
        
        tbcat$var<-a
        tbcat$c<-0
        tbcat$tndnc<-"cat"
        tbcat$tndnc[tbcat$x%in%vesp[[a]]]<-"both" #Simplemente se sobreescribe con "both" a 
                              # los valores que corresponden a valores especiales del 
                              # campo "a" (en caso de que los tenga)
                              # La funcion mtr5 sera la que distinga entre ambos casos
        
        tb<-rbind(tb,tbcat)
      
    }else if (a%in%names(vesp)){
      print(paste0("con vesp:-> ",a))
      bbbase<-data.frame(x=as.numeric(fbase[,a]),y=as.numeric(fbase[,vobj]),denomsy=as.numeric(denomsy))  
      
      baseesp<-bbbase[bbbase$x%in%vesp[[a]],]  
      basenesp<-bbbase[!(bbbase$x%in%vesp[[a]]),]
      #print(aggregate(basenesp$y,basenesp["x"], length))
      if(nrow(baseesp)>0){
        baseesp$vy<-baseesp$y*baseesp$denomsy #  Éste sería ahora el vector de los pesos malos o de 1's y 0's indicando malos del subset
        ## de Valores Especiales para el campo <a>
        
        #Sumando los pesos malos y luego los pesos de todos los registros; ambos en el subset de Valores Especiales
        tbesp1_1<-aggregate(vy~x,baseesp, sum)
        tbesp1_2<-aggregate(denomsy~x,baseesp, sum)
        
        #Agregamos el vector de tasa de pesos malos
        tbesp1_2$tm<-tbesp1_1$vy/tbesp1_2$denomsy
        
        #cambiando los casos de tm=0 por .0001 y de tm==1 por .9999
        tbesp1_2$tm[tbesp1_2$tm==0]<-.0001
        tbesp1_2$tm[tbesp1_2$tm==1]<-.9999
        
        
        
        
        # Calculando la cantidad de información que hay en cada caso de Valor Especial
        tbesp2<-aggregate(vy~x,baseesp, length)
        names(tbesp2)[2] <- "pobl"
        
        tbesp<-merge(tbesp1_2,tbesp2)#tbesp1,tbesp2)
        #print(tbesp)
        
        tbesp$var<-a
        tbesp$c<-0
        tbesp$tndnc<-"both"
        
        tb<-rbind(tb,tbesp)
        # ipm5<-rbind(ipm5,data.frame(var=a,tndnc="both",Gini=0))
      }
      #Corremos la función MTR3_yk para el subset que NO contiene Valores Especiales para el campo <a>.
      if(nrow(basenesp)>1){
        res<-MTR3_yk(as.numeric(basenesp$x),as.numeric(basenesp$y),denomsy=as.numeric(basenesp$denomsy),porcentaje = porcentaje)
      }else{res<-NULL}
    }
    else{ 
      print(paste0("sin vesp:-> ",a))
      res<-MTR3_yk(vx = as.numeric(fbase[,a]),
                   malos = as.numeric(fbase[,vobj]),
                   denomsy=as.numeric(denomsy),
                   porcentaje = porcentaje)
    }
    if(!is.null(res)){
      ipm5<-rbind(ipm5,data.frame(var=a,tndnc="up",Gini=res$ga))
      ipm5<-rbind(ipm5,data.frame(var=a,tndnc="down",Gini=res$gd))
      res$tra$bb$tndnc<-"up"
      res$trd$bb$tndnc<-"down"
      
      res$trd$bb$x<- -res$trd$bb$x
      
      res$tra$bb$var<-a
      res$trd$bb$var<-a
      #cambiando los casos de tm=0 por .0001 y de tm==1 por .9999
      res$tra$bb$tm[res$tra$bb$tm==0]<-.0001
      res$tra$bb$tm[res$tra$bb$tm==1]<-.9999
      
      res$trd$bb$tm[res$trd$bb$tm==0]<-.0001
      res$trd$bb$tm[res$trd$bb$tm==1]<-.9999
      
      tb<-rbind(tb,res$tra$bb,res$trd$bb)

    }
  }
  
  return(list(tb=tb[-1,],ipm5=ipm5[-1,],fbase=fbase,denomsy=denomsy,vobj=vobj) )
}

MTR5_yk<-function(mtr4){
  #subset de tabla de woes a utilizar y cálculo de ln(BUENOS/MALOS) globales
  
  vobj<-mtr4$vobj
  fbase<-mtr4$fbase
  denomsy<-mtr4$denomsy
  M<-sum(fbase[,vobj]*denomsy)
  B<-sum((1-fbase[,vobj])*denomsy)
  lnbmg<-log(B/M)
  
  x2<-mtr4$ipm5
  b<-merge(merge(aggregate(Gini~var,x2,max),x2),mtr4$tb)
  b<-b[order(b$var, b$x),]
  
  listaesp<-NULL
  if(sum(mtr4$tb$tndnc=="both")!=0){
    x_esp<-mtr4$tb[mtr4$tb$tndnc=="both",]
    x_esp$Gini<-0
    listaesp<-list(woes_esp=x_esp)
  }
  if(sum(mtr4$tb$tndnc=="cat")!=0){
    x_cat<-mtr4$tb[mtr4$tb$tndnc=="cat",]
    x_cat$Gini<-0.5    #PARA QUE NO SE BORREN CASOS POR SER MENORES A 5% NI POR TENER DEMASIADO GINI(ej >80%)
    b<-rbind(b,x_cat)
  }
  listanesp<-list(woes_nesp=b)
    
  lista_resultados<-list("lista"=c(listanesp,listaesp),"lnbmg"=lnbmg)
  return(lista_resultados)
}

MTR6_yk<-function(vector_x,tbwnesp,tbwesp=NULL,lnbmg){
  #califica un campo con la tabla de woes de valores no especiales y con la tabla de los de valores especiales
  # vector_x<-as.numeric(as.character(vector_x))
  vector_x2<-vector_x
  ntramos<-length(unique(tbwnesp$x))
  
  if("cat"%in%tbwnesp$tndnc){
    ncat<-length(tbwnesp$x)
    for(i in 1:ncat){
      woe_i<-log((1-tbwnesp$tm[i])/tbwnesp$tm[i])-lnbmg
      vector_x2[vector_x==tbwnesp$x[i]]<-woe_i
    }
  }else if("down"%in%tbwnesp$tndnc){
    ## Si la tabla tiene tanto casos de tendencia creciente como decreciente, quiere decir que
    ## ambas tendencias tiene el mismo Gini, por lo que, bajo este criterio, es irrelevante cuál de las dos usar.
    ## Por practicidad, elegimos la decreciente.
    
    tbwnesp<-tbwnesp[tbwnesp$tndnc=="down",]
    for(i in 1:ntramos){
      woe_i<-log((1-tbwnesp$tm[ntramos-i+1])/tbwnesp$tm[ntramos-i+1])-lnbmg
      vector_x2[vector_x>=tbwnesp$x[ntramos-i+1]]<-woe_i
      
    }  
  }else{
    for(i in 1:ntramos){
      woe_i<-log((1-tbwnesp$tm[i])/tbwnesp$tm[i])-lnbmg
      vector_x2[vector_x<=tbwnesp$x[i]]<-woe_i
      
    }
  }
  if(!is.null(tbwesp)){
    if(nrow(tbwesp)!=0){
      nvesp<-length(tbwesp$x)
      for(i in 1:nvesp){
        woe_i<-log((1-tbwesp$tm[i])/tbwesp$tm[i])-lnbmg
        vector_x2[vector_x==tbwesp$x[i]]<-woe_i
      }
    }
  }
  
  names(vector_x2)[1]<-paste("woe_",names(vector_x2)[1],sep="")
  return(vector_x2)
}

MTR7_yk<-function(fbase,lmtr5,vobj="malos"){
  #Califica toda la base según la variables que han sido trameadas
  mtr5<-lmtr5$lista
  
  woes_nesp<-mtr5$woes_nesp
  
  woes_nesp$x<-as.numeric(as.character(woes_nesp$x))
  woes_nesp<-woes_nesp[order(as.numeric(as.character(woes_nesp$x)),decreasing = T),]
  
  
  if(length(mtr5)==2){
    woes_esp<-mtr5$woes_esp
    woes_esp$x<-as.numeric(as.character(woes_esp$x))
  }else{woes_esp<-NULL}
  
  vtram<-unique(as.character(woes_nesp$var))
  bd_calif<-fbase[names(fbase)%in%vtram] #fbase[,(names(fbase)%in%vtram)]
  # bd_calif<-as.data.frame(bd_calif)
  
  for(campo in vtram ){
    
    tb_nesp<-woes_nesp %>% filter(var==campo)
    if(!is.null(woes_esp)){
      tb_esp<-woes_esp %>% filter(var==campo)
    }else{tb_esp<-NULL}
    print(campo)
    bd_calif[campo]<-MTR6_yk(fbase[campo],tb_nesp,tb_esp,lmtr5$lnbmg)
    
  }
  
  names(bd_calif)<-paste("woe_",names(bd_calif),sep="")
  fbase<-cbind(fbase,bd_calif)
  return(fbase)
}

MTR8_yk<-function(fbase,vvect,vobj,denomsy=NULL,vesp=list(jkl=pi),vcat=NULL,porcentaje=5,verbose=F){
  #Genera todo el trameado y la calificacion de una base para las variables seleccionadas
  print("mtr4")
  mtr4<-MTR4_yk(fbase=fbase,
                vvect=vvect,
                vobj=vobj,
                denomsy=denomsy,
                vesp=vesp,
                vcat=vcat,
                porcentaje=porcentaje,verbose=verbose)
  print("mtr5")
  lmtr5<-MTR5_yk(mtr4)
  
  print("mtr7")
  mtr7<-MTR7_yk(fbase=mtr4$fbase,lmtr5=lmtr5,vobj=vobj)
  
  print("bdcalif")
  mtr7$id_woe<-seq(nrow(mtr7))
  mtr7<-mtr7[,c(ncol(mtr7),1:(ncol(mtr7)-1))]
  bdcalif<-data.frame(vobj=mtr7[vobj],denomsy=mtr4$denomsy,mtr7[,grep("woe",names(mtr7))])
  if("vobj"%in%names(bdcalif)){names(bdcalif)[match("vobj",names(bdcalif))]<-vobj}
  
  return(list("Ttram"=mtr4,"lmtr5"=lmtr5,"bdcompleta"=mtr7,"bdwoes"=bdcalif))
  
}