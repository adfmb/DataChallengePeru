gather_vartmp_ciclos<-function(valor_tmp,base=ciclos_castq03){
  
  # valor_tmp<-"ead"
  # base=ciclos_castq03
  prefijos_tmp<-get(paste0("prefijo_",valor_tmp))
  
  tmp_ciclos01<-base%>%
    # select(-one_of(remove))%>%
    select(CONTRATO,starts_with(prefijos_tmp))%>%
    select(CONTRATO,matches("\\d$"))%>%
    gather(key = "periodo_i",value = "vartmp",starts_with(prefijos_tmp))%>%
    rename_(.dots=setNames("vartmp",valor_tmp))%>%
    mutate(
      periodo_i=as.numeric(gsub("\\D","",gsub("\\|","",periodo_i)))
    )
  
  return(tmp_ciclos01)
  
}

