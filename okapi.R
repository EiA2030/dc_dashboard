#to install okapi [package
#remotes::install_github("rapidsurveys/odktools")
#ONA :: load data  R okapi package.
library(okapi)

###SG
ona_auth_token <- function(token) {
  Sys.setenv("ONA_TOKEN" = Sys.getenv("TOKEN1"))
}

ona_auth_token(token = Sys.getenv("ONA_TOKEN"))
Measure_Maize_SG<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 671033)
Assign_FDTLPO_SG<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 668967)
#View(Measure_Maize_SG)

###PO
ona_auth_token <- function(token) {
  Sys.setenv("ONA_TOKEN" = Sys.getenv("TOKEN"))
}
ona_auth_token(token = Sys.getenv("ONA_TOKEN"))

Register_PC<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 523974)
Register_EX<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 523971)
Register_EN<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 526753)
Assign_FDTLPO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 523975)
Describe_FD<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 526492)
Collect_SS<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 526510)
Process_PS<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 528337)
RecordMgt_TL<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 526549)
RegisterVerify_HH<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 527985)
Describe_Household<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 632119)
Measure_Bean_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 627363)
Measure_Cassava_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 523993)
Measure_Maize_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 627372)
Measure_Potato_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 526553)
Measure_Rice_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 625030)
Measure_Wheat_PO<-ona_data_get(base_url = "https://api.ona.io", auth_mode =  "token",form_id = 627364)

