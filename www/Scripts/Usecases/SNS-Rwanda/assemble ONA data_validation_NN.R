#####This Script runs daily to  update and aggregate data collected

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
    Date = coalesce(today, ENtoday), #date col for dash filter filter
    Stage = "Validation"    # for 'stage' filter purpose 
  )%>%suppressWarnings()

View(EN.HH_data)

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
colnames(data)
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
  )

VAL_data <- full_data %>%
  dplyr::select(todayVal, ENID, HHID, crop, treat, `intro/event`) %>%
  distinct(ENID, HHID, crop, treat, `intro/event`, .keep_all = TRUE)%>%
  pivot_wider(names_from = `intro/event`, values_from = todayVal) %>%
  arrange(ENID, HHID, crop, treat) %>%
  left_join(
    req_data %>%
      distinct(ENID, HHID, crop, treat, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, crop, treat, plantingDate) %>%
      filter(!is.na(plantingDate)),
    by = c("ENID", "HHID", "crop", "treat")
  ) %>%
  mutate(event1 = plantingDate)%>% select(-(plantingDate))




#################################################################################################################
# Join Identifiers+Validation Data
#################################################################################################################
#EN.HH_data  IDENTIFIERS (ENID HHID)
#VAL_data    val info

RWA.VAL_data <- EN.HH_data %>%
  left_join(VAL_data, by = c("ENID","HHID")) %>% #join identifiers and val data while keeping all enumerators/households
  suppressWarnings()

#Save to be read into dc dashboard
write.csv(RWA.VAL_data,"./data/Usecases/SNS-Rwanda/SNS-Rwanda.VAL_data.csv")

View(VAL_data)

# #setwd("D:/IITA")
# #wd<-getwd()
# 
# # Load required libraries
# library(httr)
# library(jsonlite)
# library(tidyr)
# library(purrr)
# library(dplyr)
# library(readr)
# library(stringr)
# 
# 
# source('okapi2.R')
# # Define the ONA server URL, username, password, and form ID
# url <- "https://api.ona.io/api/v1"
# 
# # creds <- scan(paste0("./pwd.txt"), what = "character")
# # username <- creds[1]
# # passwo   <- creds[2]
# 
# #form_id <- "752967" #cassava
# 
# #752966    register en
# #752965    register hh
# 
# 
# # # Create the authentication token
# # token <- paste0(username, ":", passwo)
# # auth_token <- base64enc::base64encode(charToRaw(token))
# # 
# # # Make the API request to retrieve data
# # response <- httr::GET(
# #   url = paste0(url, "/data/", form_id),
# #   httr::add_headers(Authorization = paste0("Basic ", auth_token)),752967
# #   query = list(format = "json")752552
# # )
# 
# 
# #################################################################################################################
# #merge enum +household registration data
# 
# Register_EN <- Register_EN%>%
#   rename(
#     ENID = `purpose/enumerator_ID`,
#     ENSurname = `purpose/surname`,
#     ENphoneNo = `purpose/phone_number`,
#     ENfirstName= `purpose/first_name`,
#     ENtoday = today
#   )
# 
# 
# RegisterVerify_HH <- RegisterVerify_HH%>%
#   rename(
#     ENID = enumerator_ID_dataSCRIBEcode_a1e28af2b2a745b6bb29467aa015164c_ENDDS,
#     HHID = `new_barcode_dataSCRIBEcode_02c9e5d2f2504f57ae636de562b9f837_ENDDS/household_ID_dataSCRIBEcode_85e11f6972e14bd0bfc5282a6d6b226f_ENDDS`,
#     geopoint = `new_barcode_dataSCRIBEcode_02c9e5d2f2504f57ae636de562b9f837_ENDDS/household_geopoint_dataSCRIBEcode_46dd9da06bc541a0a2917f8b4fcf0bd8_ENDDS`,
#     Country =country_ID_dataSCRIBEcode_95be8089f5c845e183a371095d44a55e_ENDDS
#   )
# 
# RegisterVerify_HH <- tidyr::separate(RegisterVerify_HH, col = geopoint,  c("LAT", "LON", "ALT", "ERR"), " ")
# 
# Register_EN.Ids<-Register_EN%>% 
#   dplyr::select(any_of(c("ENtoday","ENID","ENfirstName","ENSurname","ENphoneNo")))
# RegisterVerify_HH.Ids<-RegisterVerify_HH%>% 
#   dplyr::select(any_of(c("today","ENID","HHID","LAT", "LON","Country")))
# 
# RegisterVerify_HH.Ids <- RegisterVerify_HH.Ids[!duplicated(RegisterVerify_HH.Ids[c("HHID")], fromLast = TRUE), ] # Keep last entry by date in duplicated records
# Register_EN.Ids <- Register_EN.Ids[!duplicated(Register_EN.Ids[c("ENID")], fromLast = TRUE), ] # Keep last entry by date in duplicated records
# 
# joined_data<-left_join(Register_EN.Ids,RegisterVerify_HH.Ids, by="ENID")   #join data household and enumerator data while keeping all enumerators
# 
# joined_data <- joined_data[which(joined_data$ENID!="RSENRW000001"),]  #leave out the enumerator registered for testing and monitoring the tool and is not expected to collect data
# 
# joined_data$Date <- joined_data$today
# joined_data <- joined_data %>%
#   mutate(Date = ifelse(is.na(Date), ENtoday, Date)) %>% #date col for filter
#   tibble::add_column(Stage= "Validation") #%>%
# #   tibble::add_column(crops= NA)  #  for filter purpose....to update with validation data column once submissions available
# 
# 
# 
# ####validation data  752552
# 
# data<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 752967)
# 
# # Parse the JSON response into a data frame with the nesting in list
# # data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
# 
# #remove ystem variables
# system_var<- c("_tags","_uuid","_notes" ,"_edited","_status" ,"_version","_duration" ,"_xform_id",
#               "_attachments","_geolocation","_media_count","_total_media","formhub/uuid",
#                "_id","_media_count","_total_media","_submitted_by","_date_modified",
#               "meta/instanceID","_submission_time","intro/geopoint_household","start",
#                "_xform_id_string","_bamboo_dataset_id","intro/in_the_field","_media_all_received")
# 
# data<- data %>% 
#   select(-any_of(system_var))
# 
# data <- data%>%
#   rename(
#     ENID = `intro/wrong_ENID`,
#     HHID = `intro/wrong_ID`,
#     todayVal = today
#   )
# 
# #joined_data2<-left_join(joined_data,data, by=c("ENID","HHID"))
# 
# #------------------------------------------------------------------------------------------
#   #data cleaning
# 
# # plant stand data
# Plant_stand_data<- data %>% 
#   dplyr::select(any_of(c("todayVal","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","ENID","HHID","crop",grep("planting.*", names(data), value = TRUE))))
# 
# #Plot data
# plot_data<- data %>% 
#   dplyr::select(any_of(c("todayVal","intro/country" ,"intro/event","intro/latitude","intro/longitude","intro/altitude","ENID","HHID","crop",grep("plotDescription.*", names(data), value = TRUE))))
# 
# #data_processed <- if (ncol(data_processed) > 0) data_processed else data_processed[0,]
# 
# plot1<- plot_data %>% 
#   gather(v, value, 9:30) %>% 
#   mutate(treat=ifelse(v %in% grep("*.AEZ.*",v, value=T),"AEZ",
#                       ifelse(v %in% grep("*.BR.*",v, value=T),"BR",
#                              ifelse(v %in% grep("*.SSR.*",v, value=T),"SSR", NA)))) %>% 
#   separate(v, c("details","var", "col"),"/") %>% 
#   select(-details) %>% 
#   mutate(col1=gsub("\\_aez|\\_BR|\\_ssr|\\_control", "", col)) %>% 
#   mutate(col1=gsub("_SSR","",col1)) %>% select(-c(col,var))
# 
# # clean col to reshape wide
# 
# reshaped_data <- plot1 %>% 
#   pivot_wider(
#   id_cols = c( "todayVal","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","ENID", "HHID", "crop", "plotDescription/plotSizeDetails/row_number","treat"),
#   names_from = col1,
#   values_from = value
# )
# 
#  #drop rows that are entirely missing
# 
# reshaped_data <- reshaped_data[rowSums(is.na(reshaped_data)) <= ncol(reshaped_data)-5-1, ]
# 
# 
# # land preparation data
# land_prep_data<- data %>% 
#   dplyr::select(todayVal,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`ENID`,`HHID`,crop,grep("LandPreparation*", names(data), value = TRUE))
# 
# 
# # crop management data
# 
# crop_mgt_data<- data %>% 
#   dplyr::select(todayVal,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`ENID`,`HHID`,crop,grep("cropManagement*", names(data), value = TRUE))
# 
# # merge all the datasets
# 
# df_list<- list(reshaped_data,Plant_stand_data,land_prep_data,crop_mgt_data) 
# 
# full_data<-df_list %>% reduce(full_join, by=c("today","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","ENID", "HHID","crop")) %>% 
#   rename_with(
#     ~stringr::str_replace_all(.x, c("plot_plot/"), ""))
# datacrop_rwa<-full_data
# 
# 
# #########################################
#  #View(data)
# # unique_subset <- datacrop_rwa %>%
# #   distinct(start,today,`intro/enumerator_ID`,`intro/barcodehousehold`, .keep_all = TRUE)
# datacrop_rwa <- datacrop_rwa%>%
#   rename(
#     ENID = `intro/wrong_ENID`,
#     HHID = `intro/wrong_ID`,
#     todayVal = today
#   )
# 
# 
# datacropSUB <- datacrop_rwa %>%
#   distinct(start,todayVal,ENID,HHID, .keep_all = TRUE)
# 
# 
