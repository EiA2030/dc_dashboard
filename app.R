# load packages
library(shiny)
library(shinyauthr)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(sf)
library(lubridate)
library(stringr)
library(plotly)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(shinyalert)
library(magrittr)
library(shinycssloaders)
library(ona)
library(magrittr)
library(reactable)
library(tippy)
library(shinyWidgets)
library(auth0)
library(data.table)
library(dplyr)


#source('ona.R')
#load script with data import and dataprep
#source('Sandbox.R')

## load functions
source('support_fun.R')

jscode <- "
shinyjs.hrefAuto = function(url) { window.location.href = url;};
"

# Define UI for application 
ui <- 
  bootstrapPage(
    
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = "hrefAuto"),
    
    # Sidebar to show user info after login
    #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    
    # login section
    #shinyauthr::loginUI(id = "login"),
    
    # to ensure display only after login
    uiOutput("sidebarpanel", padding = 0)
    
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Authentication credentials
  #getwd()
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE,
  #   cookie_getter = TRUE
  #   #log_out = reactive(logout_init())
  # )
  
  # logout_init <- shinyauthr::logoutServer(
  #                           id = "logout",
  #                           active = reactive(credentials()$user_auth))
  
  
  # # pulls out the user information returned from login module
  #user_data <- reactive({credentials()$info})
  ## Render UI ----------------------
  #Render UI: Require login
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    #req(credentials()$user_auth)
    ## 1. Header ------------------------------
    
    dashboardPage(
      #header bar
      
      dashboardHeader( 
        
        title =  list(
                      tags$img(src='Logo/EiA_logo.PNG', align='left', margin =10,height="50vh"),
                      
                      tags$h3("Data Collection Dashboard",align='right',style=" color: #fff;")),
        
        disable = FALSE, 
        titleWidth  = 550,
        dropdownMenuCustom( type = 'message',
                            customSentence = customSentence,
                            messageItem(
                              from = "",#'Feedback and suggestions',
                              message =  "",
                              icon = icon("envelope"),
                              href = "mailto:A@cgiar.org;B@cgiar.org;C@cgiar.org"
                            ),
                            icon = icon('comment')
        ),
        tags$li(class = "dropdown", actionButton("logoutt", "LOGOUT", style="color: #fff; background-color: #012514; border-color: #012514"),align='right')
        
        
      ),
      
      
      #sidebar with menu and selectinput options
      #sidebar<-
      
      ## 2. siderbar ------------------------------
      
      dashboardSidebar( 
        width = 200,
        sidebarMenu(
          
          id = 'sidebar',
          style = "position: relative; overflow: visible;",
          
          
          
          menuItem( "DATA", tabName = 'dashboard', icon = icon('dashboard'),startExpanded = T
                    
          ),
          #filter options
          uiOutput('experimentfinderr'), #experiment code
          #uiOutput('regionfinderr'),  #country
          uiOutput('seasonfinderr'),   #season
          
          uiOutput('enumeratorfinderr'), #enumerators
          
          uiOutput('householdfinderr'),  #households
          
          
          useShinyjs(),
          
          ## 'Glossary' tab 
          menuItem( "GLOSSARY", tabName = 'glossary', icon = icon('bell') 
                    
          )
          
        )
      ),
      
      #Body display
      #body<-
     
      ## 3. body --------------------------------
      dashboardBody( 
        
        ## 3.0. CSS styles in header 
        list(
          
          tags$script("document.title = 'Data Collection Dashboard'"),
          
          ### Styles
          tags$style(HTML('.alert {background-color: #202020;color:#FFFFFF;top: 300px; font-size:3px; !important;}')),
          tags$style(HTML(".small-box {height: 65px}")),
          tags$style(HTML(".fa { font-size: 35px; }")),
          tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
          tags$style(HTML(".fa-dashboard { font-size: 20px;color: white; }")),
          tags$style(HTML(".fa-comment { font-size: 20px;color: #02371d; }")),
          tags$style(HTML(".fa-envelope { font-size: 20px; }")),
          tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
          tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
          tags$style(HTML(".fa-bell { font-size: 17px;color: white; }")),
          tags$style(HTML(".small-box {height: 50px;font-size: 20%;}")),
          
          tags$style(HTML("
        
                       /* logo */
                       .logo {
                       background-color: #012514;
                       }
                       .main-header {
                       background-color: #012514;height:60px;
                       }
                       /* logo when hovered */
                       .logo:hover {
                       background-color: #012514;
                       }
                       /* navbar (rest of the header) */
                       .navbar {
                       background-color: #012514;
                       }
                       /* sidebar */
                       .sidebar {
                       background-color:#02371d ;
                       }
                       /* sidebar */
                       .main-sidebar {
                       background-color: #02371d;
                       }
                       .sidebar-menu a {
                       color:#ae755b;
                       }
                       a {
                       color:#ae755b;
                       }
                       .info-box-icon {
                       color:#ae755b;
                       }
                       .main-header .sidebar-toggle {
                       float: right;
                       color:white;
                       }
                       .fa-comment {
                       color:white;
                       }
                       .a {
                       color:white;
                       }
                       
                       /* active selected tab in the sidebarmenu */
                       .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #27674b;
                                 }
                       ")
          ),
          
          ## modify icon size in the sub side bar menu#222d32 012514
          tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } '
                          
          )) ,
          tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#00FFFFFF;
                    background:#00FFFFFF
                    }
                    
                    ")),
          
          tags$style(".downloadData { vertical-align: left; height: 40px; font-size: 10px;}"
          ),
         
          tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
          
          ## to not show error message in shiny
          tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
          tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
          
          ## dropdown menu size
          #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
          tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
          tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
          tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
        ),
        
        
        ## 3.1 Dashboard body 
        tabItems(
          ## 3.1 Main dashboard 
          
          
          tabItem( tabName = 'dashboard',
                   
                   
                   tabsetPanel(
                     id = "tabss", type = "tabs",
                     
                     tabPanel(tabName="Summary"  ,"SUMMARY",
                              
                              HTML('<h3>  Welcome! </h3>'),
                              
                              textOutput("selected_var"),
                              #HTML('<br>'),
                              HTML('<h5>  This data monitoring dashboards helps you track enumerator submissions for various trials and
                                check whether various quality checks are adhered to.</h5>'),
                              
                              HTML('<br>'),
                              
                             
                              fluidRow( column( width = 12,  align = 'left', infoBoxOutput("country") ,infoBoxOutput("project") , infoBoxOutput("Totsub_box")),
                                        #column( width = 6,  align = 'right', uiOutput("Totsub_box"))trials_map submission_trend 
                              ),
                              
                              
                              #point map and submissions_by_date
                              fluidRow( column( width = 6,h4("Trials by Location", align = 'center'), leafletOutput('trials_map',height = "50vh") ),
                                        column( width = 6,h4("Trend of Submissions", align = 'center'), plotlyOutput('submission_trend', height = "50vh") )
                              ),
                              
                              HTML('<br>'),
                              
                              fluidRow( column( width = 12,h4("Summary of Complete Submissions", align = 'center'), reactableOutput("ranking") )
                              ),
                              
                              
                     ),
                     
                     tabPanel(tabName="Enumerators","ENUMERATORS", 
                              HTML('<br>'),
                              HTML('<span  style="background-color: #BFffa590 ;align:right;" class="dot">Complete</span>&nbsp; <span  style="background-color: orange ;align:right;" class="dot">Missing Details</span>&nbsp;<span  style="background-color: #ffa590;align:right;" class="dot">Overdue</span>  &nbsp;<span  style="background-color: #BFA020F0;align:left;" class="dot">Future Event</span>'),
                              
                              HTML('<br>'),
                              
                              reactableOutput("tableR")
                              
                              
                     ),
                     
                     
                     tabPanel(tabName="quality" ,"QUALITY CHECKS",
                              reactableOutput("tableQ1")
                              
                     ),
                     
                     
                     tabPanel(tabName= "data","DATA PREVIEW",
                              # downloadButton("downloadData", "Download csv")
                              
                              column(12,
                                     dataTableOutput('tabledownload'),style = "height:75vh; overflow-y: scroll;overflow-x: scroll;"
                              ),
                              HTML('<br>'),
                              downloadButton("downloadData", "Download csv",style="color: green")
                     ),
                   )
                   
                   
                   
                   
          ),
          
          
          
          ## 3.5 metadata 
          tabItem( tabName = 'glossary',
                   
                   tags$div(
                     HTML('<h3>  Glossary </h3>'),
                     HTML('<br>'),
                     includeHTML("glossary.html")
                   )
                   
          )
          
          
        )
      )
    )
    
  })
  
  observeEvent(input$logoutt, {
    #session$reload()
    auth0:: logoutButton()
  })
  
  #gt a list of users for each project
  #sort from authorizr=ed  users by project name 
  PO_list<-c()
  SG_list<-c()
  for (i in 1:length(authlist)) {
    if (authlist$project == "PO"){
      i_list<-authlist$nickname
      PO_list<-c(PO_list,i_list)
    } else if (authlist$project == "SG"){
      i_list<-authlist$nickname
      SG_list<-c(SG_list,i_list)
    }
  }
  
  
  PO_list <<- list("reggkav", "annereina4")
  SG_list <<- list("reginakavive")
  observeEvent(session$userData$auth0_info$nickname, {
    
    #read processed data based on project:: user designated
  if (any(c(PO_list) %in%  session$userData$auth0_info$nickname )){
    
    path<-file.path("./data/dpath1/Processed")
    Bean <- read.csv(file.path(path,"/Bean .csv"))
    Cassava<-read.csv(file.path(path,"/Cassava .csv"))
    Maize<-read.csv(file.path(path,"/Maize .csv"))
    Potato<-read.csv(file.path(path,"/Potato .csv"))
    Rice<-read.csv(file.path(path,"/Rice .csv"))
    Wheat<-read.csv(file.path(path,"/Wheat .csv"))
    assign_ftp<-read.csv("./data/dpath1/Assign_FDTLPO.csv")
    register_en<-read.csv("./data/dpath1/Register_EN.csv")
    register_hh<-read.csv("./data/dpath1/RegisterVerify_HH.csv")
    
    myList<-list(Bean,Cassava,Maize,Potato,Rice,Wheat)
    
    datacrop<-rbindlist(myList, fill = TRUE)
    
    
    #datacrop <- rbind(Bean, Cassava,Maize,Potato,Rice,Wheat)
    
    
    datacrop<-datacrop[datacrop$projectCode=="RS",]
    register_en<-register_en[register_en$projectCode=="RS",]
    assign_ftp<-assign_ftp[assign_ftp$projectCode=="RS",]
      
      
      
    }else if (any(c(SG_list) %in% session$userData$auth0_info$nickname)) {
      path<-file.path('./data/dpath2/Processed')
      Maize <- read.csv(file.path(path,"/Maize .csv"))
      assign_ftp<-read.csv("./data/dpath2/Assign_FDTLPO.csv")
      register_en<-read.csv("./data/dpath1/Register_EN.csv")
      register_hh<-read.csv("./data/dpath1/RegisterVerify_HH.csv")

    #
      
      register_en<-register_en[register_en$projectCode=="SG",]
      assign_ftp<-assign_ftp[assign_ftp$projectCode=="SG",]
      register_hh<-register_hh[register_hh$projectCode=="SG",]
      
      datacrop<-Maize
      datacrop<-datacrop[datacrop$projectCode=="SG",]

    # #
    # #
    }else {
      #session$reload()
      output$selected_var <- renderText({
        "Wait to be confirmed by the administrator"
      })
     }
    
    #print(session$userData$auth0_info$name)
    #print(session$userData$auth0_info)
    
    output$experimentfinderr <- renderUI({
      selectInput(
        "experimentfinder",
        label = "Experiment",
        multiple=TRUE,
        #choices are the unique expCode 
        choices =c("All",sort(unique(assign_ftp$expCode))),
        selected= "All")
    })
    
    #observe user to update season list as per respective projects
    output$seasonfinderr <- renderUI({
      selectInput(
        "seasonfinder",
        label = "Season",
        multiple=FALSE,
        choices = list("2022A"="A","2022B"="B"),
        selected= "B")
    })
    
    
    
    
    output$enumeratorfinderr <- renderUI({
      selectInput(
        "enumeratorfinder",
        label = "Enumerator",
        multiple=T,
        choices = c("All",sort(unique(register_en$ENID))),
        selected= "All")
    })
    
    #observe user to update season list as per respective projects
    output$regionfinderr <- renderUI({
      
      selectInput(
        "regionfinder",
        label = "Country",
        multiple=FALSE,
        choices = c(sort(unique(datacrop$country)))
      )
     
      
    })
    
    output$householdfinderr <- renderUI({
      selectInput(
        "householdfinder",
        label = "Household",
        #label =  HTML('<p style="font-weight: bold;  color:white">Sector</p>'),
        multiple=T,
        choices = c("All",sort(unique(datacrop$HHID))),
        selected= "All")
    })
    
    
    
    output$trials_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    output$Totsub_box <- renderValueBox({
      
      infoBox(
        "Total submissions", nrow(datacrop), icon = icon("list"),
         color = "olive", width = "100%"
      )
    })
    
    output$country <- renderValueBox({
      infoBox(
        "Country", as.character(unique(datacrop$country)) , icon = icon("globe"),
        color = "olive",width = "100%"
      )
      
    })
    
    output$project <- renderValueBox({
      infoBox(
        "Project", as.character(unique(datacrop$projectCode)), icon = icon("barcode"),
        color = "olive",width = "100%"
        
      )
    })
    
    
    
    #Update enumerator ui (selectable) list by user and crop 
    ##to reconsider on datasource
    #, (credentials()$info)$user
    
    ###Update dashboard by user, experiment selected, enumerator , household and season   
    
    toListen2 <- reactive({
      list(input$experimentfinder,input$seasonfinder,input$enumeratorfinder, input$householdfinder )
    })
    
    observeEvent(toListen2(),{
      #if ((credentials()$info)$user == (credentials()$info)$user ){
      datacrop$today<-as.Date(datacrop$today)
      
      if ("All" %in% input$experimentfinder){
        datacrop<-datacrop
      }else {
        datacrop<-datacrop[which(datacrop$expCode==input$experimentfinder), ]
        
      }
      
      if ("B" %in% input$seasonfinder){
        if ("All" %in% input$enumeratorfinder ){
          datacrop<-datacrop
        }else{
          datacrop<-datacrop[which(datacrop$ENID==input$enumeratorfinder), ]
        }
        if ("All" %in% input$householdfinder){
          datacrop<-datacrop
        }else{
          datacrop<-datacrop[which(datacrop$HHID==input$householdfinder), ]
        }
        
        colnames(datacrop)[grepl('District',colnames(datacrop))] <- 'District'
        
        colnames(datacrop)[grepl('today',colnames(datacrop))] <- 'today'
        datacrop<-datacrop %>%
          filter(datacrop$today >= as.Date("2022-03-01") & datacrop$today < as.Date("2022-09-01"))####Filtering just for 22B
      }else if ("A" %in% input$seasonfinder){
        if ("All" %in% input$enumeratorfinder){
          datacrop<-datacrop
        }else{
          datacrop<-datacrop[which(datacrop$ENID==input$enumeratorfinder), ]
        }
        
        if ("All" %in% input$householdfinder){
          datacrop<-datacrop
        }else{
          datacrop<-datacrop[which(datacrop$HHID==input$householdfinder), ]
        }
        
        colnames(datacrop)[grepl('District',colnames(datacrop))] <- 'District'
        colnames(datacrop)[grepl('today',colnames(datacrop))] <- 'today'
        datacrop<-datacrop %>%
          filter(datacrop$today >= as.Date("2022-09-01")  & datacrop$today < as.Date("2023-03-01"))####Filtering just for 22B
      }
      
      
      rownames(datacrop) <- NULL
      #####SUMMARY    
      
      #Summary map
      
      output$trials_map <- renderLeaflet({
        # generate bins based on input$bins from ui.R
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)%>%
          addCircles(data = datacrop ,lng = datacrop$lon, lat = datacrop$lat,color = "orange")%>%
          fitBounds(max(datacrop$lon), max(datacrop$lat),min(datacrop$lon), min(datacrop$lat))
      })
     
      #today as date.
      colnames(datacrop)[grepl('today',colnames(datacrop))] <- 'today'
      
      ##Summary_submissions trend
      #group by date
      wgroup <- datacrop %>%
        mutate(date = as.Date(today)) %>%
        select(date) %>%
        group_by(date) %>%
        count() %>%
        #rename(total_freq = n) %>%
        mutate(date = as.Date(date))
      # 
      # 
      #plot of submissions trend
      Ir<-ggplot(wgroup, aes(x=date, y= n, group=1)) +
        geom_line(color="orange")+
        geom_point(color="orange")+
        #scale_x_discrete(labels= paste("Week", c(1:length(ff))))+
        theme_bw(base_size = 24)+
        labs(title="", x="Month", y="Submissions Count")+scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%m-%Y")+them2
      # 
      # #plotly -interactive ouput
      output$submission_trend<-renderPlotly({
        ggplotly(Ir, tooltip=c("x","y"))
      })
      # 
      
      ######DATA PREVIEW
      ##Data Preview/ download
      datable<-datacrop %>%
        select(-any_of(c("start","deviceid", "subscriberid","username","geopoint","lookup","barcode","latr","lonr","end","photo"   ,
                         "photoProfile","signature" ,"meta.instanceID","X_id","X_uuid","X_submission_time","X_date_modified","X_tags","X_notes","X_version",
                         "X_duration","X_submitted_by","X_total_media","X_media_count","X_media_all_received","X_xform_id",
                         "photoProfile" ,"firstName_qd","detailsHH_qd.surName_qd","detailsHH_qd.gender_qd" ,
                         "detailsHH_qd.birthYear_qd","detailsHH_qd.phoneNr_qd","detailsHH_qd.phoneNr2_qd"     ,
                         "detailsHH_qd.WhatsApp_qd" ,"detailsHH_qd.relationHH_qd", "detailsHH_qd.maritalStatus_qd",
                         "detailsHH_qd.education_qd" ,"detailsHH_qd.occupation_qd","detailsHH_qd.coopassoc_qd"    ,
                         "detailsHH_qd.correct" ,"X_status","X_edited","X_geolocation","X_attachments" ,
                         "X_bamboo_dataset_id","X_xform_id_string","formhub.uuid",contains("X_") ,
                         "Register.Household", "Register.Field", "Register.Trial.Plot","Field.Description",
                         "Soil.Sampling", "Record.Crop.Variety","Germination.Count","Top.Dressing",
                         "PD.Scoring", "Weeding", "Household.Survey", "Plant.Sampling","Harvest" ,"X","X.1"      )))
      
      # datable<- within(datacrop, rm("start","deviceid", "subscriberid","username","geopoint","lookup","barcode","lat","lon","latr","lonr","end","meta.instanceID","X_id","X_uuid","X_submission_time","X_date_modified","X_tags","X_notes","X_version",
      #                              "X_duration","X_submitted_by","X_total_media","X_media_count","X_media_all_received","X_xform_id"))
      
      # datable <- datable %>% dplyr:: select(contains("latitude"),contains("longitude"), contains("altitude"),
      #                                           contains("precision"),contains("country"),contains("today"),contains("District"),
      #                                           contains("ENID"),contains("FDID2"),contains("TLID2"),contains("expCode"),contains("HHID"),
      #                                           contains("POID2"),contains("POID2_label"))
      # 
      # 
      datable<-datable%>%
        dplyr::rename(any_of(c(Date = "today",Country = "country", ProjectCode = "projectCode")
                             
        ))
      
      colnames(datable)[grepl('lon',colnames(datable))] <- 'Longitude'
      colnames(datable)[grepl('lat',colnames(datable))] <- 'Latitude'
      colnames(datable)[grepl('altitude',colnames(datable))] <- 'Altitude'
      colnames(datable)[grepl('precision',colnames(datable))] <- 'Precision'
      
      datable <- datable %>% dplyr:: select("Date",  "Country","ProjectCode","District", "ENID", "HHID","FDID2", "TLID2", "expCode",everything() )
      output$tabledownload <- renderDataTable(datable)
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data_",gsub("-", "",Sys.Date()), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(datable, file, row.names = FALSE)
        }
      )
      
      
      ## ENUMERATOR----------------------
      a<-datacrop
      a <- a%>% dplyr:: select("today","ENID","HHID","FDID2","TLID2","expCode", "District",
                               "Register.Household", "Register.Field", "Register.Trial.Plot","Field.Description",
                               "Soil.Sampling", "Record.Crop.Variety","Germination.Count","Top.Dressing",
                               "PD.Scoring", "Weeding", "Household.Survey", "Plant.Sampling","Harvest")
      
      
      
      
      a<- as.data.frame(a)
      a<- within(a, rm("today"))
      
      a<-a[!duplicated(a), ]
      a<-unique(a)
      #print(ak)
      
      
      ak<-a
      #print(head(a))
      ak[is.na(ak)] <- "NA"
      
      #reorder vriables
      ak <- ak[, c(1, 6, 2:5,7:19)]
      #expCode TLID2_NEW
      
      ###tableenumeraror
      output$tableR <- renderReactable({
        reactable(ak,
                  pagination = FALSE,
                  showPagination = TRUE,
                  paginateSubRows = FALSE,
                  defaultExpanded = TRUE,
                  #groupBy = c("ENID","HHID"),
                  columnGroups = list(
                    colGroup(name = "", columns = c("ENID","District", "HHID","expCode","TLID2")),
                    colGroup(name = "Activity.1", columns = c("Register.Household", "Register.Field")),
                    colGroup(name = "Activity.2", columns = c("Register.Trial.Plot","Field.Description","Soil.Sampling", "Record.Crop.Variety")),
                    colGroup(name = "Activity.3", columns = c("Germination.Count")),
                    colGroup(name = "Activity.4", columns = c("Top.Dressing")),
                    colGroup(name = "Activity.5", columns = c("PD.Scoring","Weeding", "Household.Survey")),
                    colGroup(name = "Activity.6", columns = c("Plant.Sampling")),
                    colGroup(name = "Activity.7", columns = c("Harvest"))
                  ),
                  columns = list(
                    today = colDef(show = FALSE),
                    FDID2 = colDef(show = FALSE),
                    TLID2 = colDef(filterable = TRUE),
                    District = colDef(filterable = TRUE),
                    expCode = colDef(filterable = TRUE),
                    TLID2 = colDef(
                      cell =    function(value,index) {
                        s2<-register_en[which(register_en$ENID==ak$ENID[index] ), ]
                        tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                      },),
                    HHID = colDef(
                      cell =    function(value,index) {
                        s2<-register_en[which(register_en$ENID==ak$ENID[index] ), ]
                        tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                      },
                    ),
                    ENID = colDef(
                      html = TRUE,
                      #filterable = TRUE,
                      show = TRUE,
                      cell =    function(value,index) {
                        s2<-register_en[which(register_en$ENID==value ), ]
                        tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                      },
                      header = function(value) {tippy(value,tooltip = paste("NAME:", "<br>", "CONTACT:"))},
                      
                    ),
                    Register.Household = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Register.Field = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Register.Trial.Plot = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Soil.Sampling = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Record.Crop.Variety = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Germination.Count = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Top.Dressing = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    PD.Scoring = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Weeding = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Household.Survey = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Plant.Sampling = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#BE93D4","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Field.Description = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                        list(background =color)
                      }
                    ),
                    Harvest = colDef(
                      style  = function(value) {
                        color<-ifelse(value=="NA" ,"#BE93D4","#BFffa590")
                        list(background =color)
                      }
                    )
                    
                  ),
                  defaultColDef = colDef(
                    align = "center",
                    minWidth = 70,
                    headerStyle = list(
                      #`white-space` = "wrap",
                      # `transform-origin` = "70% 70%",
                      # transform = "rotate(-10deg)",
                      #`margin-top` = "0px",
                      # `margin-bottom` = "0px"#,,
                      #borderColor = "#ffffff"
                      
                    )
                  ),
                  bordered = TRUE
                  
                  # theme = reactableTheme(
                  #   color = "hsl(233, 9%, 87%)",
                  #   backgroundColor = "hsl(233, 9%, 19%)",
                  #   borderColor = "hsl(233, 9%, 22%)",
                  #   stripedColor = "hsl(233, 12%, 22%)",
                  #   highlightColor = "hsl(233, 12%, 24%)",
                  #   inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  #   selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  #   pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  #   pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
                  #   style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                  #   searchInputStyle = list(width = "100%")
                  # )
                  
                  
        )
        
      })
      
      #####QUALITY CHECKS
      
      a1 <- a %>% dplyr:: select("ENID","HHID")
      
      #a1<-tail(a1)
      a1 <- a1 %>%
        tibble:: add_column(Issues = NA)
      
      
      a1<-a1[!duplicated(a1[,c('HHID')]),]
      #a1<-as.data.frame(a1)
      #a1<-a1[which(unique(a1$HHID)), ]
      househh<-unique(a1$HHID)
      a1$Issues<-as.character(a1$Issues)
      issueslist<-c()
      for (i in househh) {
        enlist_df<-a[which(a$HHID==i), ]
        issueslist<-c(issueslist,paste(toString(names(which(colSums(is.na(enlist_df))>0)))))
        #a1$Issues[i]<-paste0((issueslist))
        
      }
      
      # print(length(a1$HHID))
      # print(length(a1$Issues))
      # print(length(issueslist))
      
      a1$Issues<-issueslist
      
      output$tableQ1 <- renderReactable({
        reactable(a1,
                  pagination = FALSE,
                  showPagination = TRUE,
                  #showPageSizeOptions = TRUE,
                  #pageSizeOptions = c(10, 15, 20),
                  #defaultPageSize = 10,
                  paginateSubRows = FALSE,
                  #groupBy = c("ENID","HHID","TLID2")
                  columns = list(
                    ENID = colDef(
                      html = TRUE,
                      show = TRUE,
                      cell =    function(value,index) {
                        s2<-register_en[which(register_en$ENID==value ), ]
                        tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                      },
                    )
                  )
        )
        
      })
      
      ranks<- a %>% 
        select(ENID,Register.Household, Register.Field, Register.Trial.Plot,Field.Description,
               Soil.Sampling, Record.Crop.Variety,Germination.Count,Top.Dressing,
               PD.Scoring, Weeding, Household.Survey, Plant.Sampling,Harvest) %>%
        group_by(ENID) %>%
        #summarise(n = n())
        summarise(across(.fns = ~sum(!is.na(.))))
      
      output$ranking <- renderReactable({
        reactable(ranks,
                  pagination = FALSE,
                  showPagination = TRUE,
                  paginateSubRows = FALSE,
                  columns = list(
                    ENID = colDef(
                      html = TRUE,
                      show = TRUE,
                      cell =    function(value,index) {
                        s2<-register_en[which(register_en$ENID==value ), ]
                        tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                      },
                    )
                  )
        )
      })
      
      
      
      outputOptions(output, "trials_map", suspendWhenHidden = FALSE)
      outputOptions(output, "submission_trend", suspendWhenHidden = FALSE)
      outputOptions(output, "tableR", suspendWhenHidden = FALSE)
      outputOptions(output, "tableQ1", suspendWhenHidden = FALSE)
      outputOptions(output, "ranking", suspendWhenHidden = FALSE)
      outputOptions(output, "tabledownload", suspendWhenHidden = FALSE) 
      
      
      
    })
    
    
    
    #
  })

  
  session$allowReconnect(TRUE)
  
}



# Run the application
#options(shiny.port = 8080)


#shinyApp(ui = ui, server = server)

##run on a browser if locaal
## if run locally- error, will require callback URL updated on Auth0 -
auth0::shinyAppAuth0(ui, server)

