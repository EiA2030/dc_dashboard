#to install okapi [package
#remotes::install_github("rapidsurveys/odktools")
#ONA :: load data  R okapi package.
library(okapi)
#Sys.setenv("TOKEN1" =''  )
ona_auth_token <- function(token) {
  Sys.setenv("ONA_TOKEN" = Sys.getenv("TOKEN1"))
}

ona_auth_token(token = Sys.getenv("ONA_TOKEN"))

Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 750671)
RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 750672)
#PotatoFertRT<-  ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 757128)
valTest<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id =752552 )



