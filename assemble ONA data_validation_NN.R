
#setwd("D:/IITA")
#wd<-getwd()

# Load required libraries
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)
library(dplyr)
library(readr)
library(stringr)


source('okapi2.R')
# Define the ONA server URL, username, password, and form ID
url <- "https://api.ona.io/api/v1"

# creds <- scan(paste0("./pwd.txt"), what = "character")
# username <- creds[1]
# passwo   <- creds[2]

#form_id <- "752967" #cassava

#752966    register en
#752965    register hh


# # Create the authentication token
# token <- paste0(username, ":", passwo)
# auth_token <- base64enc::base64encode(charToRaw(token))
# 
# # Make the API request to retrieve data
# response <- httr::GET(
#   url = paste0(url, "/data/", form_id),
#   httr::add_headers(Authorization = paste0("Basic ", auth_token)),
#   query = list(format = "json")
# )

data<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 752967)

# Parse the JSON response into a data frame with the nesting in list
# data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

#remove ystem variables
system_var<- c("_tags","_uuid","_notes" ,"_edited","_status" ,"_version","_duration" ,"_xform_id",
              "_attachments","_geolocation","_media_count","_total_media","formhub/uuid",
               "_id","_media_count","_total_media","_submitted_by","_date_modified",
              "meta/instanceID","_submission_time","intro/geopoint_household",
               "_xform_id_string","_bamboo_dataset_id","intro/in_the_field","_media_all_received")

data<- data %>% 
  select(-any_of(system_var))

dataAll_RW<-data
dataAll_RW<-dataAll_RW %>% 
  select(-c("start"))
#------------------------------------------------------------------------------------------
  #data cleaning

# plant stand data
Plant_stand_data<- data %>% 
  dplyr::select(start,today,`intro/country` ,`intro/event`,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("planting.*", names(data), value = TRUE))

#Plot data
plot_data<- data %>% 
  dplyr::select(start,today,`intro/country` ,`intro/event`,`intro/latitude`,`intro/longitude`,`intro/altitude`,crop,`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("plotDescription.*", names(data), value = TRUE))

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
  id_cols = c( "start","today","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","intro/enumerator_ID", "intro/barcodehousehold", "crop", "plotDescription/plotSizeDetails/row_number","treat"),
  names_from = col1,
  values_from = value
)

 #drop rows that are entirely missing

reshaped_data <- reshaped_data[rowSums(is.na(reshaped_data)) <= ncol(reshaped_data)-5-1, ]


# land preparation data
land_prep_data<- data %>% 
  dplyr::select(start,today,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("LandPreparation*", names(data), value = TRUE))


# crop management data

crop_mgt_data<- data %>% 
  dplyr::select(start,today,`intro/country`,`intro/event` ,`intro/latitude`,`intro/longitude`,`intro/altitude`,`intro/enumerator_ID`,`intro/barcodehousehold`,crop,grep("cropManagement*", names(data), value = TRUE))

# merge all the datasets

df_list<- list(reshaped_data,Plant_stand_data,land_prep_data,crop_mgt_data) 

full_data<-df_list %>% reduce(full_join, by=c("start","today","intro/country","intro/event","intro/latitude","intro/longitude","intro/altitude","intro/enumerator_ID", "intro/barcodehousehold","crop")) %>% 
  rename_with(
    ~stringr::str_replace_all(.x, c("plot_plot/"), ""))
datacrop_rwa<-full_data


#########################################
 #View(data)
# unique_subset <- datacrop_rwa %>%
#   distinct(start,today,`intro/enumerator_ID`,`intro/barcodehousehold`, .keep_all = TRUE)
datacrop_rwa <- datacrop_rwa%>%
  rename(
    ENID = `intro/enumerator_ID`,
    HHID = `intro/barcodehousehold`
  )


datacropSUB <- datacrop_rwa %>%
  distinct(start,today,ENID,HHID, .keep_all = TRUE)



