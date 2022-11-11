f_import<-function(){
  library(tidyr)
  library(magrittr)
  library(sp)
  library(dplyr)
  library(future)
  library(future.apply)
  library(foreach)
  library(doParallel)
  library(wrapr )
  library(stringr)
  
  Measure_Wheat_PO<-Measure_Wheat_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  Measure_Rice_PO<-Measure_Rice_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  Measure_Potato_PO<-Measure_Potato_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  Measure_Cassava_PO<-Measure_Cassava_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  Measure_Maize_PO<-Measure_Maize_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  Measure_Bean_PO<-Measure_Bean_PO %>% 
    mutate(plot = map(plot, ~ .x %>% 
                        mutate_all(as.character))) %>%
    unnest(cols = c(plot))%>% 
    mutate(`plot/PD`=as.list(`plot/PD`))%>%
    unnest(cols = c(`plot/PD`))%>% 
    type.convert(as.is = TRUE)
  
  
  #View(Measure_Maize_SG$`fertilizer2/plotFert2/urea2`[[68]])
  
  #NG
  
  # Measure_Maize_SG$`fertilizer2/plotFert2/urea2`[[68]] <-as.character(Measure_Maize_SG$`fertilizer2/plotFert2/urea2`[[68]] )
  # Measure_Maize_SG$plotFert3[[105]]$`fertilizer3/plotFert3/urea3` <-as.integer(Measure_Maize_SG$plotFert3[[68]]$`fertilizer3/plotFert3/urea3` )
  # Measure_Bean_PO$plot[[319]]$`plot/grainYield1_parameters/grainsFW` <-as.integer(Measure_Bean_PO$plot[[319]]$`plot/grainYield1_parameters/grainsFW` )
  
  #Measure_Maize_SG<-unnest(Measure_Maize_SG, `fertilizer2/plotFert2`)
  #Measure_Bean_PO<-unnest(Measure_Bean_PO, `plot/PD`, drop=F)
  #Measure_Maize_SG<-unnest(Measure_Maize_SG[c(1:67, 70:96, 103:132, 145:nrow(Measure_Maize_SG)), ], `fertilizer2/plotFert2`, drop=F)
  
  ##convert to csv and save
  Register_EN <- apply(Register_EN,2,as.character) %>%  as_tibble()
  write.csv(Register_EN, file = "./data/dpath1/Register_EN.csv")
  RegisterVerify_HH <- apply(RegisterVerify_HH,2,as.character) %>%  as_tibble()
  write.csv(RegisterVerify_HH, file = "./data/dpath1/RegisterVerify_HH.csv")
  RecordMgt_TL <- apply(RecordMgt_TL,2,as.character) %>%  as_tibble()
  write.csv(RecordMgt_TL, file = "./data/dpath1/RecordMgt_TL.csv")
  Process_PS <- apply(Process_PS,2,as.character) %>%  as_tibble()
  write.csv(Process_PS, file = "./data/dpath1/Process_PS.csv")
  Measure_Wheat_PO <- apply(Measure_Wheat_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Wheat_PO, file = "./data/dpath1/Measure_Wheat_PO.csv")
  Measure_Rice_PO <- apply(Measure_Rice_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Rice_PO, file = "./data/dpath1/Measure_Rice_PO.csv")
  Measure_Potato_PO <- apply(Measure_Potato_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Potato_PO, file = "./data/dpath1/Measure_Potato_PO.csv")
  Measure_Maize_PO <- apply(Measure_Maize_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Maize_PO, file = "./data/dpath1/Measure_Maize_PO.csv")
  Measure_Cassava_PO <- apply(Measure_Cassava_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Cassava_PO, file = "./data/dpath1/Measure_Cassava_PO.csv")
  Measure_Bean_PO <- apply(Measure_Bean_PO,2,as.character) %>%  as_tibble()
  write.csv(Measure_Bean_PO, file = "./data/dpath1/Measure_Bean_PO.csv")
  Describe_Household <- apply(Describe_Household,2,as.character) %>%  as_tibble()
  write.csv(Describe_Household, file = "./data/dpath1/Describe_Household.csv")
  Describe_FD <- apply(Describe_FD,2,as.character) %>%  as_tibble()
  write.csv(Describe_FD, file = "./data/dpath1/Describe_FD.csv")
  Collect_SS <- apply(Collect_SS,2,as.character) %>%  as_tibble()
  write.csv(Collect_SS, file = "./data/dpath1/Collect_SS.csv")
  Assign_FDTLPO <- apply(Assign_FDTLPO,2,as.character) %>%  as_tibble()
  write.csv(Assign_FDTLPO, file = "./data/dpath1/Assign_FDTLPO.csv")
  Assign_FDTLPO_SG <- apply(Assign_FDTLPO_SG,2,as.character) %>%  as_tibble()
  write.csv(Assign_FDTLPO_SG, file = "./data/dpath2/Assign_FDTLPO_SG.csv")
  Measure_Maize_SG <- apply(Measure_Maize_SG,2,as.character) %>%  as_tibble()
  write.csv(Measure_Maize_SG, file = "./data/dpath2/Measure_Maize_SG.csv")
}




