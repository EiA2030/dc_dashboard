##########################################################################################
##########################SNS-RWANDA######################################################
##########################################################################################

#####This Script runs daily to  update and aggregate data collected

#print(wd)
#################################################################################################################
##source + downloaded files from ona.io
source('okapi.R')
# Load required libraries
suppressMessages(suppressWarnings(library(httr)))
suppressMessages(suppressWarnings(library(jsonlite)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(purrr)))
suppressMessages(suppressWarnings(library(dplyr)))
if(!'data.table' %in% installed.packages()[, 'Package']) {install.packages('data.table', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library("data.table",character.only = TRUE)))
if(!'readr' %in% installed.packages()[, 'Package']) {install.packages('readr', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library("readr",character.only = TRUE)))
suppressMessages(suppressWarnings(library(stringr)))
if(!'R.utils' %in% installed.packages()[, 'Package']) {install.packages('R.utils', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library(R.utils)))
if(!'aws.s3' %in% installed.packages()[, 'Package']) {install.packages('aws.s3', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library("aws.s3",character.only = TRUE)))


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
  distinct(ENID, HHID, .keep_all = TRUE) %>%# Keep last entry by date in duplicated records
  filter(!is.na(HHID) & ENID != "RSENRW000001")#leave out values with NA HHID the enumerator registered for testing and monitoring the tool and is not expected to collect data


# Join the data 
EN.HH_data <- Register_EN.Ids %>%
  full_join(RegisterVerify_HH.Ids, by = "ENID") %>% #join data household and enumerator data while keeping all enumerators
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

# Update HHID #scanned vs typed ids issue    ...merge vars: scanned - `intro/wrong_ID`, typed-`intro/barcodehousehold_1`...`intro/barcodehousehold`
data$`intro/barcodehousehold_1` <- sub("RSHHRW1", "RSHHRW0", data$`intro/barcodehousehold_1`)
data$`intro/wrong_ID` <- sub("LSHH", "RSHH", data$`intro/wrong_ID`)

data$`intro/wrong_ID`<- ifelse(is.na(data$`intro/wrong_ID`) & data$`intro/barcodehousehold_1` != "RSHHRWNaN",
                               data$`intro/barcodehousehold_1`,
                               data$`intro/wrong_ID`)
#control for `intro/barcodehousehold` variable too
data$`intro/wrong_ID`<- ifelse(is.na(data$`intro/wrong_ID`) & data$`intro/barcodehousehold` != "RSHHRWNaN",
                               data$`intro/barcodehousehold`,
                               data$`intro/wrong_ID`)

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
    Trial = crop,
    plantingDate = `planting/plantingDetails/planting_date`
  )#%>%mutate(todayVal2 = todayVal)

VAL_data <- full_data %>%
  dplyr::select(todayVal, ENID, HHID, Trial, treat, `intro/event`) %>%
  distinct(ENID, HHID, Trial, treat, `intro/event`, .keep_all = TRUE)%>%
  pivot_wider(names_from = `intro/event`, values_from = todayVal) %>%
  arrange(ENID, HHID, Trial, treat) %>%
  left_join(
    full_data %>%
      distinct(ENID, HHID, Trial, treat, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, Trial, treat, plantingDate) %>%
      filter(!is.na(plantingDate)),
    by = c("ENID", "HHID", "Trial", "treat")
  ) %>%
  left_join(
    full_data %>%
      distinct(ENID, HHID, Trial, treat, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, Trial, treat, todayVal) ,
    by = c("ENID", "HHID", "Trial", "treat")
  ) %>% distinct(ENID, HHID, Trial, treat, .keep_all = TRUE)%>% 
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
    Trial =crop,
    plantingDate = `planting/plantingDetails/planting_date`
  )
dataev1 <- dataev%>%
  dplyr::select(todayVal, ENID, HHID, Trial,  `intro/event`) %>%
  distinct(ENID, HHID, Trial, `intro/event`, .keep_all = TRUE)%>%
  pivot_wider(names_from = `intro/event`, values_from = todayVal) %>%
  arrange(ENID, HHID, Trial) %>%
  left_join(
    dataev %>%
      distinct(ENID, HHID, Trial, `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, Trial, plantingDate) %>%
      filter(!is.na(plantingDate)),
    by = c("ENID", "HHID", "Trial")
  ) %>%
  left_join(
    dataev %>%
      distinct(ENID, HHID, Trial,  `intro/event`, .keep_all = TRUE) %>%
      dplyr::select(ENID, HHID, Trial, todayVal) ,
    by = c("ENID", "HHID", "Trial")
  ) %>% distinct(ENID, HHID, Trial, .keep_all = TRUE)%>% 
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
  rename(Country = `intro/country`)%>% 
  rename_with(
  ~stringr::str_replace_all(.x, c("intro/"), ""))



#save to bucket 
temp_file <- tempfile()
write.csv(RWA.VAL_data, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaVALdata.csv"))
unlink(temp_file)

temp_file <- tempfile()
write.csv(RWA.SUM_data, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaSUMdata.csv"))
unlink(temp_file)

temp_file <- tempfile()
write.csv(RWA.O_data, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaOdata.csv"))
unlink(temp_file)



# zz <- rawConnection(raw(0), "r+")
# write.csv(RWA.VAL_data, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaVALdata.csv") )
# close(zz)
# 
# zz <- rawConnection(raw(0), "r+")
# write.csv(RWA.SUM_data, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaSUMdata.csv"))
# close(zz)
# 
# zz <- rawConnection(raw(0), "r+")
# write.csv(RWA.O_data, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaOdata.csv")) 
# close(zz)
# #setwd(wd)



##########################################################################################
##########################SOLIDARIDAD#####################################################
##########################################################################################
colsnot<-c("_id"                 ,                     "_tags"                      ,              "_uuid"       ,                            
           "event"                            ,        "_notes"                      ,             "_edited"          ,                       
           "_status"                          ,        "_version"                    ,             "_duration"          ,                     
           "_xform_id"                        ,        "_attachments"                ,             "_geolocation"       ,                     
           "_media_count"                     ,        "_total_media"                 ,            "formhub/uuid"         ,                   
           "_submitted_by"                     ,       "_date_modified"                 ,          "enumerator_id_1"      ,                   
           "meta/instanceID"                    ,      "_submission_time"               ,          "_xform_id_string"        ,                
           "meta/instanceName"                  ,      "_bamboo_dataset_id"                 ,      "_media_all_received"        ,             
           "projectDetails/project"              ,     "projectDetails/activity"             ,     "projectDetails/use_case"   ,              
           "projectDetails/countries"             ,    "projectDetails/plot_area"             ,    "projectDetails/project_ID"     ,          
           "projectDetails/trial_type"            ,    "projectDetails/use_case_ID"            ,   "planting_practice/crop_name"   ,          
           "projectDetails/plot_width_m"          ,    "projectDetails/sub_activity"            ,  "projectDetails/initiative_ID"     ,       
           "projectDetails/plot_length_m"          ,   "planting_practice/planting_date"         , "projectDetails/initiative_title"     ,    
           "projectDetails/number_of_blocks"       ,   "planting_practice/intercrop_name"      ,   "planting_practice/planting_labor"     ,   
           "planting_practice/row_spacing_cm"      ,   "planting_practice/seeding_method"        , "projectDetails/activity_end_date"       , 
           "projectDetails/experiment_design"      ,   "planting_practice/sowing_depth_cm"       , "projectDetails/rep_ID_or_number_1"      , 
           "planting_practice/plant_population"    ,   "planting_practice/plant_spacing_cm"      , "projectDetails/activity_start_date"      ,
           "projectDetails/plot_ID_or_number_1"    ,   "planting_practice/crop_variety_name"     , "planting_practice/planting_material"     ,
           "planting_practice/seedling_age_days"   ,   "planting_practice/planting_technique"    , "projectDetails/number_of_replications"   ,
           "planting_practice/number_of_intercrops" ,  "planting_practice/plant_population_unit"  ,"planting_practice/intercrop_variety_name",
           "planting_practice/planting_material_cost", "enumerator_ID"                          ,  "site_characterization/admin_1"           ,
           "site_characterization/admin_2"          ,  "site_characterization/admin_3"          ,  "site_characterization/country"           ,
           "site_characterization/currency"         ,  "projectDetails/rep_ID_or_number"        ,  "site_characterization/gps_field"         ,
           "projectDetails/plot_ID_or_number"       ,  "site_characterization/latitude_field"   ,  "site_characterization/longitude_field"   ,
           "site_characterization/altitude_field_m",    "site_characterization/admin_4"  ,         "site_characterization/site_name"      )


###############Solidaridad NOT trials######################################
NOTSol1<-NOTSol%>%
  select(-any_of(c( "_notes" , "_total_media", "_id", "_tags", "_uuid" ,"start", "_edited","_status" ,"_version" , "_duration"  ,"_xform_id" ,"_attachments", "_geolocation" ,"_media_count" ,"formhub/uuid"   ,                                      
                    "_submitted_by","consent/photo","_date_modified","meta/instanceID"  , "_xform_id_string" ,"_bamboo_dataset_id"  ,"meta/instanceName" ,
                    "_media_all_received"  ,  "consent/read_consent_form"    ,"consent/copy",  "consent/give_consent" ,"formhub/uuid"    )))%>%
  rename(
    ENID = enumerator_id_1,
    HHID = `projectDetails/rep_ID_or_number_1`,
    Country = `projectDetails/countries`,
    Event= event,
    latitude= `site_characterization/latitude_field`,
    longitude= `site_characterization/longitude_field`,
    Trial= `projectDetails/trial_type`,
    today = `_submission_time`,
  ) %>%
  mutate(today = as.IDate(today)) %>%
  mutate(HHID = coalesce(`start/barcodehousehold_solidaridad`, HHID) )%>%
  mutate(ENID = coalesce(`start/enumerator_ID`, ENID) )%>%
  mutate(Trial = coalesce(`start/trial`, Trial) )%>%
  mutate(Event = coalesce(`start/event`, Event) )%>%
  arrange(ENID,HHID, desc(today)) %>% #sort to Keep last entry by date in duplicated records
  distinct(ENID,HHID,today,Event, .keep_all = TRUE)  %>%
  mutate(Stage = "NOT Trials") %>%
  mutate(Country = coalesce(`start/country`, Country) )%>%
  mutate(Country = capitalize(Country))
  
NOTSol2<-NOTSol1%>%
  dplyr::select(any_of(c(  "today", "Event"  , "Trial", "ENID" , "HHID" 
  )  ))%>%
  arrange(Event) %>%
  mutate(Event = paste( "event",Event, sep = ""))%>%
  pivot_wider(names_from = Event, values_from = today, values_fn = last) %>%
  mutate(Stage = "NOT Trials") %>%
  arrange(Stage,Trial, 
          ENID, HHID )

# NOTSol2<-NOTSol%>%
#   rename(today =`_submission_time`,
#          Event =  event,
#          Trial= `projectDetails/trial_type`,
#          #Country = `projectDetails/countries`,
#          ENID= enumerator_id_1,
#          #PLID = `projectDetails/plot_ID_or_number_1`,
#          HHID = `projectDetails/rep_ID_or_number_1`
#   )%>%
#   mutate(today = as.IDate(today)) %>%
#   mutate(HHID = coalesce(`start/barcodehousehold_solidaridad`, HHID) )%>%
#   mutate(ENID = coalesce(`start/enumerator_ID`, ENID) )%>%
#   mutate(Trial = coalesce(`start/trial`, Trial) )%>%
#   mutate(Event = coalesce(`start/event`, Event) )%>%
#   mutate(Event = paste( "event",Event, sep = ""))%>%
#   pivot_wider(names_from = Event, values_from = today) %>%
#   mutate(Stage = "NOT Trials") %>%
#   #rename(`Site Selection` =event1)%>%
#   arrange(Stage,Trial, 
#           ENID, HHID )%>%
#   select(-any_of(c(colsnot)))


###############Solidaridad On-farm validations######################################
#farmer seg data 
#f.seg_malawi f.seg_zambia f.seg_mozambique
# f.seg_malawi1<-f.seg_malawi%>%
#   select(any_of(c( "_submission_time"
#                    ,"eia_addon_survey/eia_addon_use_case_main_crop"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1" 
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number" 
#                    )))%>%
#   rename(`Site Selection` =`_submission_time`,
#          crop=`eia_addon_survey/eia_addon_use_case_main_crop`,
#          Country = `eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country`,
#          ENfirstName=`eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name`,
#          ENID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,
#          HHfirstName=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1`,
#          HHID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
#          HHphoneNo=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number` 
#     
#   )%>%
#   mutate(`Site Selection` = as.Date(`Site Selection`)) %>%
#   distinct(ENID,HHID,Country,`Site Selection`, .keep_all = TRUE)  %>%
#   mutate(ENSurname =NA)%>%
#   mutate(ENphoneNo =NA)
# 
# f.seg_zambia1<-f.seg_zambia%>%
#   select(any_of(c( "_submission_time"
#                    ,"eia_addon_survey/eia_addon_use_case_main_crop"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1" 
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number" 
#   )))%>%
#   rename(`Site Selection` =`_submission_time`,
#          crop=`eia_addon_survey/eia_addon_use_case_main_crop`,
#          Country = `eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country`,
#          ENfirstName=`eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name`,
#          ENID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,
#          HHfirstName=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1`,
#          HHID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
#          HHphoneNo=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number` 
#          
#   )%>%
#   mutate(`Site Selection` = as.Date(`Site Selection`)) %>%
#   distinct(ENID,HHID,Country,`Site Selection`, .keep_all = TRUE)  %>%
#   mutate(ENSurname =NA)%>%
#   mutate(ENphoneNo =NA)
# 
# f.seg_mozambique1<-f.seg_mozambique%>%
#   select(any_of(c( "_submission_time"
#                    ,"eia_addon_survey/eia_addon_use_case_main_crop"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name"
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1" 
#                    ,"eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1"
#                    ,"eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number" 
#   )))%>%
#   rename(`Site Selection` =`_submission_time`,
#          crop=`eia_addon_survey/eia_addon_use_case_main_crop`,
#          Country = `eia_addon_survey/eia_addon_metadata_title/eia_addon_Location/eia_addon_country`,
#          ENfirstName=`eia_addon_survey/eia_addon_metadata_title/eia_addon_enumerator_name`,
#          ENID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodeenumerator_1`,
#          HHfirstName=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_part_1/eia_addon_hh_roster_title/eia_addon_hh_member1`,
#          HHID=`eia_addon_survey/eia_addon_metadata_title/eia_addon_barcodehousehold_1`,
#          HHphoneNo=`eia_addon_survey/eia_addon_surveywithland/eia_addon_survey_grp/eia_addon_closing_title/eia_addon_nearly_finished/eia_addon_phone_number` 
#          
#   )%>%
#   mutate(`Site Selection` = as.Date(`Site Selection`)) %>%
#   distinct(ENID,HHID,Country,`Site Selection`, .keep_all = TRUE)  %>%
#   mutate(ENSurname =NA)%>%
#   mutate(ENphoneNo =NA)
# 
#   #View(f.seg_malawi1)
# # Append all f_seg files rows using rbind() to get list of all IDs HHIDs and ENIDs
# f.seg_data <- rbind(f.seg_malawi1, f.seg_zambia1, f.seg_mozambique1)
# 

# #######Validation data
  valSol1<-valSol%>%
  rename(
    ENID = `intro/enumerator_id_1`,
    HHID = `intro/barcodehousehold_1`,
    Country = `location/country`,
    Event= `intro/event`,
    latitude= `location/latitude`,
    longitude= `location/longitude`,
    today = today
  ) %>%
  mutate(today = as.IDate(today)) %>%
  arrange(ENID,HHID, desc(today)) %>% #sort to Keep last entry by date in duplicated records
  distinct(ENID,HHID,today,Event, .keep_all = TRUE)  %>%
  mutate(Stage = "Validation") %>%
  mutate(
    Country = coalesce(`intro/country`, Country) )%>%
  mutate(Country = capitalize(Country))%>%
  select(-any_of(c( "_notes" , "_total_media", "_id", "_tags", "_uuid" ,"start", "_edited","_status" ,"_version" , "_duration"  ,"_xform_id" ,"_attachments", "_geolocation" ,"_media_count" ,"formhub/uuid"   ,
                                                            "_submitted_by","consent/photo","_date_modified","meta/instanceID"  ,"_submission_time", "_xform_id_string" ,"_bamboo_dataset_id"  ,
                                                            "_media_all_received"  ,  "consent/read_consent_form"    ,"consent/copy",  "consent/give_consent", "intro/country" )))
  



valSol2<-valSol1%>%
  dplyr::select(any_of(c(  "today", "Event"  ,  "ENID" , "HHID" 
                           )  ))%>%
  arrange(Event) %>%
  pivot_wider(names_from = Event, values_from = today, values_fn = last) %>%
  mutate(Stage = "Validation") %>%
  mutate(Trial = "Validation") %>%
  arrange(Stage,Trial, 
          ENID, HHID )

valSol1<-as.data.frame(valSol1)
# SOL.SUM_data <- f.seg_data %>%
#   left_join(valSol2, by = c("ENID","HHID")) %>% #join identifiers and val data while keeping all enumerators/households
#   suppressWarnings()


# 
# # Remove columns with all NA values
# clean_data <- n[, colSums(is.na(n)) != nrow(n)]

###########################################################################################################################
#rbind valSol2, NOTSol2 and save as SOL.SUM_data on aws
NOTValSol2 <-bind_rows(valSol2, NOTSol2)

NOTSolID<-NOTSol1%>%
  rename(
    HHfirstName=`site_characterization/first_name`,
    HHSurname = `site_characterization/surname`,
    HHphoneNo= `site_characterization/phone_number`
  )%>%
  dplyr::select(any_of(c( "ENID" , "HHID" ,"HHfirstName","HHSurname", "HHphoneNo"
  )  ))%>%
  filter(!is.na(HHfirstName)) %>%
  distinct(ENID, HHID, .keep_all = TRUE)
  


NOTValSol2 <-NOTValSol2 %>%
  left_join(NOTSolID, by = c("ENID","HHID")) %>% 
  filter(!is.na(ENID) & !is.na(HHID))

NOTSol1 <- lapply(NOTSol1, function(x) {
  if (is.list(x)) {
    sapply(x, paste, collapse = ',')
  } else {
    x
  }
})
NOTSol1 <- as.data.frame(NOTSol1)

NOTValSol2 <- lapply(NOTValSol2, function(x) {
  if (is.list(x)) {
    sapply(x, paste, collapse = ',')
  } else {
    x
  }
})

NOTValSol2 <- as.data.frame(NOTValSol2)


         
#save to bucket
temp_file <- tempfile()
write.csv(NOTValSol2, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadSUMdata.csv"))
unlink(temp_file)

temp_file <- tempfile()
write.csv(valSol1, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadOdata.csv"))
unlink(temp_file)

temp_file <- tempfile()
write.csv(NOTSol1, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadNOTdata.csv"))
unlink(temp_file)


# zz <- rawConnection(raw(0), "r+")
# write.csv(NOTValSol2, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadSUMdata.csv"))
# close(zz)
# 
# 
# zz <- rawConnection(raw(0), "r+")
# write.csv(valSol1, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadOdata.csv"))
# close(zz)
# 
# zz <- rawConnection(raw(0), "r+")
# write.csv(NOTSol1, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadNOTdata.csv"))
# close(zz)



##########################################################################################
##########################KALRO###########################################################
##########################################################################################

#ID DATA (Enumerators and households)
#merge enum +household registration data
KL.ENReg <- KL.Register_EN%>%
  rename(
    ENID = `register_enumerator/purpose/enumerator_id`,
    ENSurname = `register_enumerator/purpose/surname`,
    ENphoneNo = `register_enumerator/purpose/phone_number`,
    ENfirstName= `register_enumerator/purpose/first_name`,
    ENtoday = `register_enumerator/today`
  ) %>%
  select(any_of(c("ENtoday","ENID","ENfirstName","ENSurname","ENphoneNo"))) %>%
  arrange(ENID, desc(ENtoday)) %>% #sort to Keep last entry by date in duplicated records
  distinct(ENID, .keep_all = TRUE)# Keep last entry by date in duplicated records
  




KL.HHReg<-KL.RegisterVerify_HH%>%
  select(any_of(c( "register_hh/today"
                   ,"register_hh/country_ID"
                   ,"register_hh/enumerator_ID"
                   ,"register_hh/new_barcode/surname" 
                   ,"register_hh/new_barcode/first_name"
                   ,"register_hh/new_barcode/household_id"
                   ,"register_hh/new_barcode/phone_number" 
  )))%>%
  rename(`Site Selection` =`register_hh/today`,
         Country = `register_hh/country_ID`,
         ENID=`register_hh/enumerator_ID`,
         HHfirstName=`register_hh/new_barcode/first_name`,
         HHSurname = `register_hh/new_barcode/surname`,
         HHID=`register_hh/new_barcode/household_id`,
         HHphoneNo=`register_hh/new_barcode/phone_number` 
         
  )%>%
  mutate(`Site Selection` = as.Date(`Site Selection`)) %>%
  filter(!is.na(HHID)) %>%  # Filter out rows where HHID is NA
  distinct(ENID,HHID,Country,`Site Selection`,HHphoneNo, .keep_all = TRUE)  

KL.ENHHReg <- KL.ENReg %>%
  full_join(KL.HHReg, by = "ENID") %>%
  suppressWarnings()
 



#Validation data
KL.val1<-KL.valData%>%
  
  as.data.frame()%>%
  select(-any_of(c( "_notes" , "_total_media", "_id", "_tags", "_uuid" ,"start", "_edited","_status" ,"_version" , "_duration"  ,"_xform_id" ,"_attachments", "_geolocation" ,"_media_count" ,"formhub/uuid"   ,
                    "_submitted_by","consent/photo","_date_modified","meta/instanceID"  ,"_submission_time", "_xform_id_string" ,"_bamboo_dataset_id"  ,
                    "_media_all_received"  ,  "consent/read_consent_form"    ,"consent/copy",  "consent/give_consent")))%>%
  rename(
    ENID = `intro/enumerator_id`,
    HHID = `intro/household_id`,
    Country = `location/country_ID`,
    Event= `intro/event`,
    latitude= `location/latitude`,
    longitude= `location/longitude`,
    today = today
  ) %>%
  mutate(ENID = if_else(ENID == "KHENKE000028", "KLENKE000028", ENID)) %>%
  mutate(today = as.IDate(today)) %>%
  arrange(ENID,HHID, desc(today)) %>% #sort to Keep last entry by date in duplicated records
  distinct(ENID,HHID,today,Event, .keep_all = TRUE)  %>%
  mutate(Stage = "Validation") %>%
  mutate(Country = capitalize(Country))%>%
  filter(ENID != "KLENKE000000" ) %>%#leave out the enumerator registered for testing and monitoring the tool and is not expected to collect data
  filter(ENID != "KLENKE123456")



KL.val2 <- KL.val1 %>%
  dplyr::select(any_of(c("today", "Event", "ENID", "HHID"))) %>%
  mutate(ENID = if_else(ENID == "KHENKE000028", "KLENKE000028", ENID)) %>%
  arrange(Event) %>%
  pivot_wider(names_from = Event, values_from = today, values_fn = last) %>%
  mutate(across(starts_with("event"), as.Date, format = "%Y-%m-%d")) %>%
  arrange( ENID, HHID)%>%
  suppressWarnings()

 
#join to include all EN details... some not in the hh details. 

KL.ENHHReg2<-KL.ENHHReg %>%
  dplyr::select(-any_of(c("Country", "ENtoday", "ENfirstName","ENSurname","ENphoneNo" ))) 


#get hh details
KL.SUM_data <- KL.val2 %>%
  full_join(KL.ENHHReg2, by = c("ENID","HHID")) %>% #join identifiers and val data while keeping all enumerators/households
  left_join(KL.ENReg, by = "ENID")  %>%
  arrange(ENID,HHID, desc(`Site Selection`)) %>% 
  distinct(ENID,HHID, .keep_all = TRUE) %>% 
  filter(ENID != "KLENKE000000" ) %>%#leave out the enumerator registered for testing and monitoring the tool and is not expected to collect data
  filter(ENID != "KLENKE123456")%>%
  filter(!(duplicated(ENID) & is.na(HHID))) %>% # remove rows where ENID is not unique and HHID is NA
  mutate(Stage = "Validation") %>%
  mutate(Trial = "Validation") %>%
  suppressWarnings()

KL.val1 <- lapply(KL.val1, function(x) {
  if (is.list(x)) {
    sapply(x, paste, collapse = ',')
  } else {
    x
  }
})

KL.val1 <- as.data.frame(KL.val1)
##### KLENKE000000 KLHHKE000000 not duplicated... one househld id used in training with multiple people asigned with different details.  
###training data to be excluded later...
#View(KL.SUM_data)
#save to bucket
temp_file <- tempfile()
write.csv(KL.val1, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLOdata.csv"))
unlink(temp_file)

temp_file <- tempfile()
write.csv(KL.SUM_data, temp_file, row.names = FALSE)
aws.s3::put_object(file = temp_file,
                   bucket = "rtbglr", 
                   object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLSUMdata.csv"))
unlink(temp_file)

# zz <- rawConnection(raw(0), "r+")
# write.csv(KL.SUM_data, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLSUMdata.csv"))
# close(zz)
# 
# 
# zz <- rawConnection(raw(0), "r+")
# write.csv(KL.val1, zz, row.names = FALSE)
# aws.s3::put_object(file = rawConnectionValue(zz),
#                    bucket = "rtbglr", object = paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLOdata.csv"))
# close(zz)



