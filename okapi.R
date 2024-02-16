#to install okapi [package
#remotes::install_github("rapidsurveys/odktools")
#ONA :: load data  R okapi package.
suppressMessages(suppressWarnings(library(okapi)))

##########################################################################################
##########################SNS-RWANDA######################################################
##########################################################################################
#Sys.setenv("TOKEN1" =''  )# set token in env file
ona_auth_token <- function(token) {
  Sys.setenv("ONA_TOKEN" = Sys.getenv("TOKEN1"))
}

ona_auth_token(token = Sys.getenv("ONA_TOKEN"))

# Define the ONA base URL, token and form ID 
Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 750671)
RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 750672)
#PotatoFertRT<-  ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 757128)
valTest<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =752552 )



##########################################################################################
##########################SOLIDARIDAD#####################################################
##########################################################################################

###############Solidaridad NOT trials######################################
NOTSol<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =780907 )


###############Solidaridad On-farm validations######################################
valSol<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =780906 )
f.seg_malawi<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =755562 )
f.seg_mozambique<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =756460 )
f.seg_zambia<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =755802 )


