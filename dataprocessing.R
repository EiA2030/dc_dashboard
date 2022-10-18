#scheduled daily run on server for processed data
## Script scheduled to run daily at 00:00 to avoid expensive computation within the app.
### 
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

cl <- makeCluster(6)
registerDoParallel(cl)
f_import()
 #setwd("C:/Users/dell/Documents/WORK/EiA/EiA Rwanda2/EiA_Rwanda_Dashboard")
 #getwd()

# 
# ## Read data (also updated daily)-json- consistent naming
# #RW
##NOT APPLICABLE

#Set paths of saved converted files
#list of paths additional projects to be added 
path1<-'./data/dpath1'  ##RW
path2<-'./data/dpath2'  ##NG

pathslist<-c(path1,path2)

#intoduce a loop through the paths 
# This function processes data for all crops
for (pl in pathslist){
  old_wd <- getwd()
  #Read crops names automatically; all named measure_crop
  list<-list.files(pl, pattern="Measure", full.names=TRUE)
  crops<-c()
  for (i in 1:length(list)){
    new_crop<-sub(".*Measure_", "", list[i])
    new_crop<-sub("_.*", "", new_crop)
    
    crops<- c(crops,new_crop)
    new_crop<-read.csv(list[i])
    
  }
  
  for (cr in crops){
    #print(cr)
    #read json data and convert to csv
    ###NOT THIS...TO CHANGE
    ##FIND A MORE DYNAMIC WAY *****read all files pattern== csv. save as (scrape name from lists)
    #*****or fix names (easier for datalist) for all forms ..read file with pattern similar to name
    if (pl==path1){
      path<-paste(path1)
      
      assign_ftp<-read.csv(file.path(path,"/Assign_FDTLPO.csv"))
      soilsample<-read.csv(file.path(path,"/Collect_SS.csv"))
      d_field<-read.csv(file.path(path,"/Describe_FD.csv"))
      d_HH<-read.csv(file.path(path,"/Describe_Household.csv"))
      plant_samples<-read.csv(file.path(path,"/Process_PS.csv"))
      trial_management<-read.csv(file.path(path,"/RecordMgt_TL.csv"))
      register_hh<-read.csv(file.path(path,"/RegisterVerify_HH.csv"))
      register_en<-read.csv(file.path(path,"/Register_EN.csv"))
      
      if ("Cassava" %in% cr){
        Cassava<-read.csv(file.path(path,"/Measure_Cassava_PO.csv"))
        cropdata<-Cassava
      }else if  ("Bean" %in% cr){
        Bean <- read.csv(file.path(path,"/Measure_Bean_PO.csv"))
        cropdata<-Bean
      }else if ("Wheat" %in% cr){
        Wheat<-read.csv(file.path(path,"/Measure_Wheat_PO.csv"))
        cropdata<-Wheat
      }else if  ("Potato" %in% cr) {
        Potato<-read.csv(file.path(path,"/Measure_Potato_PO.csv"))
        cropdata<-Potato
      }else if  ("Rice"  %in% cr){
        Rice<-read.csv(file.path(path,"/Measure_Rice_PO.csv"))
        cropdata<-Rice
      }else if  ("Maize" %in% cr){
        Maize<-read.csv(file.path(path,"/Measure_Maize_PO.csv"))
        cropdata<-Maize
      }
      
      # required to join to the df the HHID and expcode values
      vv2 <-as.data.frame( assign_ftp [c("HHID","FDID2", "FDID2_new", "expCode","season")])
      
      vv2<-vv2[which(vv2$FDID2_new !='n/a'), ] 
      
      #to sort mixup in the expcode given for FDID2 and FDID2_new within Assign_FDTLPO form
      #to avoid expcode  na's on join
      
      li<-c()
      si<-c()
      for (i in 1:nrow(vv2)){
        bbn<-assign_ftp[which(assign_ftp$FDID2==vv2$FDID2_new[i] ), ]
        #vv2$expCode[i]<-bbn$expCode
        #assign_ftp2$FDID2[i]
        #ifelse(is.na(vv2$expCode[i]),bbn$expCode[1], NA )
        li<-c(li,bbn$expCode[1])
        si<-c(si,bbn$season[1])
      }
      
      for (i in 1:nrow(vv2)){
        if (is.na(vv2$expCode[i])){
          vv2$expCode[i]<-li[i]
        }
        if (is.na(vv2$season[i])){
          vv2$season[i]<-si[i]
        }
        #ifelse(is.na(vv2$expCode[i]),li[i], NA )
      }
      vv2<-vv2
      
      
    }else if (pl==path2){
      path<-paste(path2)
      assign_ftp<-read.csv(file.path(path,"/Assign_FDTLPO_SG.csv"))
      vv2<-assign_ftp
      if (cr=="Maize"){
        Maize<-read.csv(file.path(path,"/Measure_Maize_SG.csv"))
        cropdata<-as.data.frame(Maize)
      }
      #datalist <- list(Maize)
    }
    
    #### add regions/district by intersecting points with GADM  shapefile.
    Lat <- as.numeric(cropdata$lat)
    Lon <- as.numeric(cropdata$lon)
    #make a data frame
    coords <- as.data.frame(cbind(Lon,Lat))
    #and into Spatial
    points <- SpatialPoints(coords)
    #SpatialPolygonDataFrame -
    path<-paste0(pl,"/Polygon")
    counties <- rgdal::readOGR(path, "Region")
    #assume same proj as shapefile!
    proj4string(points) <- proj4string(counties)
    #get GADM county polygon point is in
    #choosing NAME_2 as it standard for GADM data
    result <- as.character(over(points, counties)$NAME_2)
    cropdata <- cropdata %>%
      tibble:: add_column(District = ( result), .after = "today")
    
    measure_plot<-cropdata
    
    #join with assign plot to get missing variables
    v2 <- vv2 %>% 
      dplyr::select(any_of(c("HHID","FDID2","FDID2_new","expCode","season")))
    
    ###run expcode assignment in assignplot.... issues with FDID2 AND FDID2_new(repeated) n/a expcode
    
    cropdata <- left_join(cropdata, v2, by = c("FDID2" = "FDID2_new"))
    
    
    #a<-cropdata
    #select required columns for the interactive table 
    #a <- a%>% dplyr:: select("today","ENID","HHID","FDID2","TLID2","expCode", "District")
    
    #Add the required monitoring steps
  cropdata<-cropdata%>%
    tibble::add_column(Register.Household= NA) %>%
    tibble::add_column(Register.Field= NA) %>%
    tibble::add_column(Register.Trial.Plot= NA) %>%
    tibble::add_column(Field.Description= NA) %>%
    tibble::add_column(Soil.Sampling= NA) %>%
    tibble::add_column(Record.Crop.Variety= NA) %>%
    tibble::add_column(Germination.Count= NA) %>%
    tibble::add_column(Top.Dressing= NA) %>%
    tibble::add_column(PD.Scoring= NA) %>%
    tibble::add_column(Weeding= NA) %>%
    tibble::add_column(Household.Survey= NA) %>%
    tibble::add_column(Plant.Sampling= NA) %>%
    tibble::add_column(Harvest= NA)
  
  
  plist<-c("Register.Household", "Register.Field", "Register.Trial.Plot","Field.Description",
           "Soil.Sampling", "Record.Crop.Variety","Germination.Count","Top.Dressing",
           "PD.Scoring", "Weeding", "Household.Survey", "Plant.Sampling","Harvest")
  
  
  
  
  for (p in plist){
    
    clist <- foreach(
      i = 1:nrow(cropdata),
      .combine = 'c'
    ) %dopar%  {
      if (p == "Register.Household") {
        #ss<-register_hh[which(register_hh$ENID==cropdata$ENID[i] ), ]
        ss<-register_hh[which(register_hh$HHID==cropdata$HHID[i] ), ]
        #clist<-c(clist,(rev(ss$today)[1]))
      } else               if ("Household.Survey"  %in% p) {
        #ss<-d_HH[which(d_HH$startGroup.barcodeenumerator==cropdata$ENID[i] ), ]
        ss<-d_HH[which(d_HH$survey.barcodehousehold==cropdata$HHID[i] ), ]
        colnames(ss)[grepl('today',colnames(ss))] <- 'today'
        #clist<-c(clist,(rev(ss$today)[1]))
      }else if ("Register.Field"  %in% p || "Register.Trial.Plot"  %in% p)  {
        #ss<-assign_ftp[which(assign_ftp$ENID==cropdata$ENID[i] ), ]
        ss<-assign_ftp[which(assign_ftp$HHID==cropdata$HHID[i] ), ]
        ss<-ss[which(ss$FDID2_new==cropdata$FDID2[i] ), ]
        #clist<-c(clist,(rev(ss$today)[1]))
      }else               if ("Field.Description"  %in% p) {
        #ss<-d_field[which(d_field$ENID==cropdata$ENID[i] ), ]
        ss<-d_field[which(d_field$HHID==cropdata$HHID[i] ), ]
        ss<-ss[which(ss$FDID2==cropdata$FDID2[i] ), ]
        #clist<-c(clist,(rev(ss$today)[1]))
      }else               if ("Soil.Sampling"  %in% p) {
        ss<-soilsample[which(soilsample$ENID==cropdata$ENID[i] ), ]
        #clist<-c(clist,(rev(ss$today)[1]))
      }              else  if ("Record.Crop.Variety"  %in% p || "Top.Dressing"  %in% p || "Weeding"  %in% p) {
        #ss<-trial_management[which(trial_management$ENID==cropdata$ENID[i] ), ]
        #ss<-ss[which(ss$TLID2==cropdata$TLID2[i] ), ]
        ss<-trial_management[which(trial_management$FDID2==cropdata$FDID2[i] ), ]
        if ( "Record.Crop.Variety"  %in% p){
          #clist<-c(clist,(rev(ss$today)[1]))
        } else if ("Top.Dressing"  %in% p){
          ss<-ss[stringr::str_detect(ss$fieldbookSectionDetails, "fertilizer2"), ]
          
          #ss<-ss[which(ss$fieldbookSectionDetails.1..activities.fertilizer2 =="True" ), ]
          #clist<-c(clist,(rev(ss$today)[1]))
        }else if ("Weeding"  %in% p){
          ss<-ss[stringr::str_detect(ss$fieldbookSections, "weeding"), ]
          #ss<-ss[which(ss$fieldbookSections.weeding =="True" ), ]
          #clist<-c(clist,(rev(ss$today)[1]))
        }
      }else if ("Germination.Count"  %in% p || "PD.Scoring"  %in% p || "Plant.Sampling"  %in% p || "Harvest"  %in% p) {
        ss<-measure_plot[which(measure_plot$ENID==cropdata$ENID[i] ), ]
        ss<-ss[which(ss$TLID2==cropdata$TLID2[i] ), ]
        ss<-ss[which(ss$FDID2==cropdata$FDID2[i] ), ]
        if ("Germination.Count"  %in% p){
          ss<-ss[stringr::str_detect(ss$parameters, "plantStand"), ]
          #clist<-c(clist,(rev(ss$today)[1]))
        } else if ("PD.Scoring"  %in% p){
          ss<-ss[stringr::str_detect(ss$parameters, "PD"), ]
          #clist<-c(clist,(rev(ss$today)[1]))
        }else if ("Plant.Sampling"  %in% p){
          #ss<-ss[which(ss$parameters.plantSampling =="True" ), ]
          ss<-ss[stringr::str_detect(ss$parameters, "plantSampling"), ]
          #clist<-c(clist,(rev(ss$today)[1]))
        }else if ("Harvest"  %in% p){
          if ("Wheat" %in% cr ){
            ss<-ss[stringr::str_detect(ss$parameters, "plantHeight"), ]
            ss<-ss[stringr::str_detect(ss$parameters, "tillerNumber"), ]
            ss<-ss[stringr::str_detect(ss$parameters, "grainYield"), ]
            # ss<-ss[which(ss$parameters.plantHeight=="True" ), ]
            # ss<-ss[which(ss$parameters.tillerNumber=="True" ), ]
            # ss<-ss[which(ss$parameters.grainYield =="True" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          } else if ("Cassava" %in% cr){
            ss<-ss[stringr::str_detect(ss$parameters, "branching"), ]
            ss<-ss[stringr::str_detect(ss$parameters, "rootYield"), ]
            # ss<-ss[which(ss$parameters.branching =="True" ), ]
            # ss<-ss[which(ss$parameters.rootYield =="True" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          }else if ("Potato" %in% cr){
            ss<-ss[which(ss$tuberYield_parDetails.tuberQuality =="yes" ), ]
            ss<-ss[which(ss$tuberYield_parDetails.tuberSampling =="yes" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          }else if ("Beans" %in% cr){
            ss<-ss[stringr::str_detect(ss$parameters, "grainYield"), ]
            #ss<-ss[which(ss$parameters.grainYield =="True" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          }else if ("Rice" %in% cr){
            ss<-ss[stringr::str_detect(ss$parameters, "plantHeight"), ]
            ss<-ss[stringr::str_detect(ss$parameters, "tillerNumber"), ]
            ss<-ss[stringr::str_detect(ss$parameters, "grainYield"), ]
            
            # ss<-ss[which(ss$parameters.plantHeight=="True" ), ]
            # ss<-ss[which(ss$parameters.tillerNumber=="True" ), ]
            # ss<-ss[which(ss$parameters.grainYield =="True" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          }else if ("Maize" %in% cr){
            ss<-ss[stringr::str_detect(ss$parameters, "grainYield"), ]
            #ss<-ss[which(ss$parameters.grainYield =="True" ), ]
            #clist<-c(clist,(rev(ss$today)[1]))
          }
        }
      }
      (rev(ss$today)[1])
      
      #})
      
    }
    cropdata[p]<-as.data.frame(( clist))
    
    
    
  }
  setwd(file.path(pl,"/Processed"))
  
  write.csv(cropdata,paste(cr,".csv"))
  
  setwd(old_wd)

  }
  
}



stopCluster(cl)

