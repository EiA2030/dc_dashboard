#####This Script runs daily to  update and aggregate data collected
wd<-getwd()
wd2<-getwd()
#################################################################################################################
##source + downloaded files from ona.io
source('okapi.R')
# Load required libraries
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)
library(dplyr)
library(readr)
library(stringr)



#################################################################################################################
#ID DATA (Enumerators and households)
#merge enum +household registration data
Register_EN.Ids <- Register_EN%>%
  rename(
    ENID = `purpose/enumerator_ID`,
    ENSurname = `purpose/surname`,
    ENphoneNo = `purpose/phone_number`,
    ENfirstName= `purpose/first_name`,
    ENtoday = today
  ) %>%
  select(any_of(c("ENtoday","ENID","ENfirstName","ENSurname","ENphoneNo"))) %>%
  arrange(ENID, desc(ENtoday)) %>% #sort to Keep last entry by date in duplicated records
  distinct(ENID, .keep_all = TRUE) %>% # Keep last entry by date in duplicated records
  filter(ENID != "RSENRW000001")#leave out the enumerator registered for testing and monitoring the tool and is not expected to collect data



RegisterVerify_HH.Ids <- RegisterVerify_HH%>%
  rename(
    ENID = enumerator_ID_dataSCRIBEcode_a1e28af2b2a745b6bb29467aa015164c_ENDDS,
    HHID = `new_barcode_dataSCRIBEcode_02c9e5d2f2504f57ae636de562b9f837_ENDDS/household_ID_dataSCRIBEcode_85e11f6972e14bd0bfc5282a6d6b226f_ENDDS`,
    geopoint = `new_barcode_dataSCRIBEcode_02c9e5d2f2504f57ae636de562b9f837_ENDDS/household_geopoint_dataSCRIBEcode_46dd9da06bc541a0a2917f8b4fcf0bd8_ENDDS`,
    Country =country_ID_dataSCRIBEcode_95be8089f5c845e183a371095d44a55e_ENDDS
  )%>%
  separate(geopoint, into = c("LAT", "LON", "ALT", "ERR"), sep = " ")%>%
  dplyr::select(any_of(c("today","ENID","HHID","LAT", "LON","Country"))) %>%
  arrange(ENID, desc(today)) %>% # sorts to enable Keep last entry by date in duplicated records
  distinct(HHID, .keep_all = TRUE) # Keep last entry by date in duplicated records


# Join the data 
EN.HH_data <- Register_EN.Ids %>%
  left_join(RegisterVerify_HH.Ids, by = "ENID") %>% #join data household and enumerator data while keeping all enumerators
  mutate(
    DateId = coalesce(today, ENtoday), #date col for dash filter filter
    Stage = "Validation" ,   # for 'stage' filter purpose 
    `Site Selection` = today,
    `Site Selection` = ifelse(is.na(HHID), NA, `Site Selection`)
  )%>% select(-c(today,ENtoday)) %>% 
  suppressWarnings()




#################################################################################################################
#Validation data

data<-valTest #from ona api download (okapi2.R)

#------------------------------------------------------------------------------------------
#data cleaning
#------------------------------------------------------------------------------------------
#remove ystem variables
system_var<- c("_tags","_uuid","_notes" ,"_edited","_status" ,"_version","_duration" ,"_xform_id",
               "_attachments","_geolocation","_media_count","_total_media","formhub/uuid",
               "_id","_media_count","_total_media","_submitted_by","_date_modified",
               "meta/instanceID","_submission_time","intro/geopoint_household",
               "_xform_id_string","_bamboo_dataset_id","intro/in_the_field","_media_all_received")

data<- data %>% 
  select(-any_of(system_var))

#------------------------------------------------------------------------------------------
# plant stand data
Plant_stand_data<- data %>% 
  dplyr::select(start,today,`intro/country` ,`intro/event`,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/wrong_ENID`,`intro/wrong_ID`,crop,grep("planting.*", names(data), value = TRUE))

#Plot data
plot_data<- data %>% 
  dplyr::select(start,today,`intro/country` ,`intro/event`,`intro/latitude`,`intro/longitude`,`intro/altitude`,crop,`intro/wrong_ENID`,`intro/wrong_ID`,crop,grep("plotDescription.*", names(data), value = TRUE))

plot1<- plot_data %>% 
  gather(v, value, 12:33) %>% 
  mutate(treat=ifelse(v %in% grep("*.AEZ.*",v, value=T),"AEZ",
                      ifelse(v %in% grep("*.BR.*",v, value=T),"BR",
                             ifelse(v %in% grep("*.SSR.*",v, value=T),"SSR", NA)))) %>% 
  separate(v, c("details","var", "col"),"/") %>% 
  select(-details) %>% 
  mutate(col1=gsub("\\_aez|\\_BR|\\_ssr|\\_control", "", col)) %>% 
  mutate(col1=gsub("_SSR","",col1)) %>% select(-c(col,var))

# clean col to reshape wide
reshaped_data <- plot1 %>% 
  pivot_wider(
    id_cols = c( "start","today","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","intro/wrong_ENID", "intro/wrong_ID", "crop", "plotDescription/plotSizeDetails/row_number","treat"),
    names_from = col1,
    values_from = value
  )

#drop rows that are entirely missing
reshaped_data <- reshaped_data[rowSums(is.na(reshaped_data)) <= ncol(reshaped_data)-5-1, ]

# land preparation data
land_prep_data<- data %>% 
  dplyr::select(start,today,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/wrong_ENID`,`intro/wrong_ID`,crop,grep("LandPreparation*", names(data), value = TRUE))

# crop management data

crop_mgt_data<- data %>% 
  dplyr::select(start,today,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/wrong_ENID`,`intro/wrong_ID`,crop,grep("cropManagement*", names(data), value = TRUE))

# merge all the datasets
df_list<- list(reshaped_data,Plant_stand_data,land_prep_data,crop_mgt_data) 

full_data<-df_list %>% reduce(full_join, by=c("start","today","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","intro/wrong_ENID", "intro/wrong_ID","crop")) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/"), ""))

full_data <- full_data%>%
  rename(
    ENID = `intro/wrong_ENID`,
    HHID = `intro/wrong_ID`,
    todayVal = today,
    plantingDate = `planting/plantingDetails/planting_date`
  )#%>%mutate(todayVal2 = todayVal)

VAL_data <- full_data %>%
  dplyr::select(todayVal, ENID, HHID, crop, treat, `intro/event`) %>%
  distinct(ENID, HHID, crop, treat, `intro/event`, .keep_all = TRUE)%>%
  pivot_wider(names_from = `intro/event`, values_from = todayVal) %>%
  arrange(ENID, HHID, crop, treat) %>%
  left_join(
    full_data %>%
      distinct(ENID, HHID, crop, treat, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, crop, treat, plantingDate) %>%
      filter(!is.na(plantingDate)),
    by = c("ENID", "HHID", "crop", "treat")
  ) %>%
  left_join(
    full_data %>%
      distinct(ENID, HHID, crop, treat, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, crop, treat, todayVal) ,
    by = c("ENID", "HHID", "crop", "treat")
  ) %>% distinct(ENID, HHID, crop, treat, .keep_all = TRUE)%>% 
  mutate(event1 = plantingDate)%>% select(-(plantingDate))%>% suppressWarnings()




#################################################################################################################
# Join Identifiers+Validation Data
#################################################################################################################
#EN.HH_data  IDENTIFIERS (ENID HHID)
#VAL_data    val info

RWA.VAL_data <- EN.HH_data %>%
  left_join(VAL_data, by = c("ENID","HHID")) %>% #join identifiers and val data while keeping all enumerators/households
  mutate(Date = coalesce(todayVal, DateId))%>%select(-c(DateId,todayVal))%>%
  suppressWarnings()



dataev<-data%>%
  dplyr::select(today, `intro/wrong_ENID`,`intro/wrong_ID` ,crop, `intro/event`,  `planting/plantingDetails/planting_date`) 
dataev <- dataev%>%
  rename(
    ENID = `intro/wrong_ENID`,
    HHID = `intro/wrong_ID`,
    todayVal = today,
    plantingDate = `planting/plantingDetails/planting_date`
  )
dataev1 <- dataev%>%
  dplyr::select(todayVal, ENID, HHID, crop,  `intro/event`) %>%
  distinct(ENID, HHID, crop, `intro/event`, .keep_all = TRUE)%>%
  pivot_wider(names_from = `intro/event`, values_from = todayVal) %>%
  arrange(ENID, HHID, crop) %>%
  left_join(
    dataev %>%
      distinct(ENID, HHID, crop, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, crop, plantingDate) %>%
      filter(!is.na(plantingDate)),
    by = c("ENID", "HHID", "crop")
  ) %>%
  left_join(
    dataev %>%
      distinct(ENID, HHID, crop,  `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, crop, todayVal) ,
    by = c("ENID", "HHID", "crop")
  ) %>% distinct(ENID, HHID, crop, .keep_all = TRUE)%>% 
  mutate(event1 = plantingDate)%>% select(-(plantingDate))%>%
  suppressWarnings()

RWA.SUM_data <- EN.HH_data %>%
  left_join(dataev1, by = c("ENID","HHID")) %>% #join identifiers and val data while keeping all enumerators/households
  mutate(Date = coalesce(todayVal, DateId))%>%select(-c(DateId,todayVal))%>%
  suppressWarnings()




#################################################################################################################
#Validation Data
RWA.O_data<-valTest %>% 
  select(-any_of(system_var))%>% 
  select(-c(start,`intro/barcodehousehold_1`))%>% 
  rename_with(
  ~stringr::str_replace_all(.x, c("intro/"), ""))



ifelse(!dir.exists(file.path("./data/Usecases/SNS-Rwanda/")), dir.create(file.path("./data/Usecases/SNS-Rwanda/")), FALSE)

wdnew<-"./data/Usecases/SNS-Rwanda/"
setwd(wdnew)
#Save to be read into dc dashboard
write.csv(RWA.VAL_data,"./data/Usecases/SNS-Rwanda/SNS-Rwanda.VAL_data.csv")

#Save data for event submission summary purpose... not in long format (treatments)
write.csv(RWA.SUM_data,"./data/Usecases/SNS-Rwanda/SNS-Rwanda.SUM_data.csv")

write.csv(RWA.O_data,"./data/Usecases/SNS-Rwanda/SNS-Rwanda.O_data.csv")

setwd(wd)