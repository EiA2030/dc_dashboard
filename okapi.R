#to install okapi [package
#remotes::install_github("rapidsurveys/odktools")
#ONA :: load data  R okapi package.
suppressMessages(suppressWarnings(library(okapi)))



#Sys.setenv("TOKEN1" =''  )# set token in env file
ona_auth_token <- function(token) {
  Sys.setenv("ONA_TOKEN" = Sys.getenv("TOKEN1"))
}

ona_auth_token(token = Sys.getenv("ONA_TOKEN"))



##########################################################################################
##########################EiA_Demo_Validation#############################################
##########################################################################################

# Define the ONA base URL, token and form ID
DEMO.Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 808709)
DEMO.RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 808710)
#PotatoFertRT<-  ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 757128)
DEMO.valData<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =808706 )



##########################################################################################
##########################SNS-RWANDA######################################################
##########################################################################################

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


##########################################################################################
##########################KALRO###########################################################
##########################################################################################

# Define the ONA base URL, token and form ID 
KL.Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 789929)
KL.RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 789933)
#PotatoFertRT<-  ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 757128)
KL.valData<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =793461 )


##########################################################################################
##########################MercyCorpsSprot###########################################################
##########################################################################################

# Define the ONA base URL, token and form ID
MC.Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 805762)
MC.RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 805781)
#PotatoFertRT<-  ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 757128)
MC.valData<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =808517 )


