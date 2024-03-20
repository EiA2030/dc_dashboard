

# load packages
suppressMessages(suppressWarnings(library("shiny",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinyauthr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinydashboard",character.only = TRUE)))
suppressMessages(suppressWarnings(library("tidyr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("ggplot2",character.only = TRUE)))
suppressMessages(suppressWarnings(library("sf",character.only = TRUE)))
suppressMessages(suppressWarnings(library("lubridate",character.only = TRUE)))
suppressMessages(suppressWarnings(library("stringr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("plotly",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinyBS",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinyjs",character.only = TRUE)))
suppressMessages(suppressWarnings(library("leaflet",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinyalert",character.only = TRUE)))
suppressMessages(suppressWarnings(library("magrittr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinycssloaders",character.only = TRUE)))
#library(ona)
suppressMessages(suppressWarnings(library("magrittr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("reactable",character.only = TRUE)))
suppressMessages(suppressWarnings(library("tippy",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinyWidgets",character.only = TRUE)))
suppressMessages(suppressWarnings(library("auth0",character.only = TRUE)))
suppressMessages(suppressWarnings(library("data.table",character.only = TRUE)))
suppressMessages(suppressWarnings(library("dplyr",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinydashboardPlus",character.only = TRUE)))
suppressMessages(suppressWarnings(library("shinythemes",character.only = TRUE)))
# if(!'tools' %in% installed.packages()[, 'Package']) {install.packages('tools', repos = 'http://cran.us.r-project.org')}
# if(!'rmarkdown' %in% installed.packages()[, 'Package']) {install.packages('rmarkdown', repos = 'http://cran.us.r-project.org')}
#if(!'memoise' %in% installed.packages()[, 'Package']) {install.packages('memoise', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library("tools",character.only = TRUE)))
suppressMessages(suppressWarnings(library("rmarkdown",character.only = TRUE)))
#suppressMessages(suppressWarnings(library("memoise",character.only = TRUE)))
if(!'aws.s3' %in% installed.packages()[, 'Package']) {install.packages('aws.s3', repos = 'http://cran.us.r-project.org')}
if(!'data.table' %in% installed.packages()[, 'Package']) {install.packages('data.table', repos = 'http://cran.us.r-project.org')}
suppressMessages(suppressWarnings(library("data.table",character.only = TRUE)))
suppressMessages(suppressWarnings(library("aws.s3",character.only = TRUE)))

#
#triger trigger on the CI/CD pipeline 
#tinytex::install_tinytex()
#source('dataprocessing.R')

## load functions+files
source('support_fun.R')






# Define UI for application 
ui <- 
  fluidPage(
    #fix refresh/reload error by removing the token from URL, (also sets timeout)
    tags$head(
      tags$script(HTML("setTimeout(function() { history.pushState({}, 'Page Title', '/'); }, 2000);"))
    ),
    # tags$head(
    #   tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');},2000);"))),
    #bootstrapPage(
    # tags$script(JS(
    #   "setTimeout(function(){history.replaceState(null, '', '/')}, 2000);"
    # ))
    
    
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = "hrefAuto"),
    
    # Sidebar to show user info after login
    #div( shinyauthr::logoutUI(id = "logout")),
    
    # login section ##toremove
    shinyauthr::loginUI(id = "login"),
    
    # to ensure display only after login
    uiOutput("sidebarpanel", padding = 0)
    
    
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  keep_alive <- shiny::reactiveTimer(intervalMs = 10000, session = shiny::getDefaultReactiveDomain())
  shiny::observe({keep_alive()})
  
  #session$setTimeout(1200)
  #extendShinyjsSession(session, timeout = 1200)
  #Authentication credentials  
  #getwd()
  #shinyjs::extendShinyjs()
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    cookie_getter = TRUE,
    log_out = reactive(logout_init())
  )##toremove
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)%>%
      bindCache(credentials()$user_auth))
  
  
  
  shinyjs::runjs("
    $(document).on('shiny:connected', function(event) {
      // When a tab is clicked, update the input$nav value
      $('.navbar-nav a').on('click', function() {
        var selectedTab = $(this).attr('data-value');
        Shiny.setInputValue('nav', selectedTab);
      });
    });
  ")
  
  
  
  # # pulls out the user information returned from login module
  #user_data <- reactive({credentials()$info})
  ## Render UI ----------------------
  #Render UI: Require login
  ##########################################################################################################################################
  #################################################### UI RENDER ####################################################################
  ##########################################################################################################################################
  
  output$sidebarpanel <- renderUI({
    # Show only when authenticated
    req(credentials()$user_auth)  ##toremove
    
    user_use_case_data <- (dplyr::filter(usersdata, grepl((credentials()$info)$user,usersdata$users)))$shortName
    
    
    ## 1. Header ------------------------------
    navbarPage(theme = shinytheme("flatly"), 
               title = tags$div(style="font-size:20px !important;margin-top: -22px !important;color: #ffc84f;",
                                img(src="Logo/EiA_logo.png", height = '50vh'),
                                HTML("&nbsp&nbsp&nbsp"),"Data Collection Dashboard", HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp")
               ),
               id = "nav",
               windowTitle = "DC Dashboard",
               collapsible = FALSE,
               
               tags$head(includeCSS("style.css")),
               
               # Use do.call to pass the list of tab panels to navbarMenu
               create_navbarMenu(c( user_use_case_data ))
               
               # dropdownBlock(
               #   id = "account",
               #   title = "Account",
               #   icon = NULL,
               #   actionButton("logoutt", "Logout", icon = icon("sign-out-alt", style = "color: #802c11"), style = "background-color:transparent;border: 0px;z")
               # )
               
    )
  })
  
  
  ##########################################################################################################################################
  #################################################### SERVER FUNCTIONS ####################################################################
  ##########################################################################################################################################
  
  observeEvent(input$dashboard, {
    shinyjs::toggle("dashboard") # Disable the menu item during animation
    shinyjs::toggle("collapsible-content", anim = TRUE) # Toggle the content with animation
    shinyjs::toggleState("dashboard") # Re-enable the menu item after animation
  })
  
  observeEvent(input$logoutt, {
    session$reload() ##toremove
    #auth0::logoutButton()
  })
  
  
  
  
  
  ##Define data for each usecase
  observeEvent(input$nav,{
    tryCatch( 
      if (input$nav== " SNS-Rwanda"){
        
        RWA.O_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaOdata.csv"),
                                  file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        RWA.SUM_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SNSRwandaSUMdata.csv"),
                                    file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        RWA.O_data <-RWA.O_data%>%
          rename(
            ENID = wrong_ENID,
            HHID = wrong_ID)%>%
          mutate(Stage = "Validation"  ) # for 'stage' filter purpose 
        
        
        datacrop <- RWA.SUM_data
        rawdata <- RWA.O_data
        columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                               "Site Selection", "event1", "event2", "event3", "event4", "event5", "event6","event7")
        patternissues<-"^RSENRW"
        patternissuesE<-"^RSHHRW"
        
        
      }else if (input$nav== " Solidaridad Soy Advisory"){
        #glossary<-'Solidaridadglossary.html'
        SOL.O_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadOdata.csv"),
                                  file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        SOL.SUM_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadSUMdata.csv"),
                                    file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        SOL.NOT_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "SolidaridadNOTdata.csv"),
                                    file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        datacrop <- SOL.SUM_data
        rawdata <- SOL.O_data
        patternissues<-"^SDENMW|SDENZM|SDENMZ"
        patternissuesE<-"^SDHHMW|SDHHZM|SDHHMZ|SDRP"
        columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                               "Site Selection", "event1", "event2", "event3", "event4", "event5", "event6","event7", "event8a", "event8b","event8c")
        
        
      }else if (input$nav== " KALRO"){
        KL.O_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLOdata.csv"),
                                  file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        KL.SUM_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLSUMdata.csv"),
                                    file = tempfile(fileext = ".csv")
        ) %>%
          fread()
        
        datacrop <- KL.SUM_data
        rawdata <- KL.O_data
        patternissues<-""
        patternissuesE<-""
        columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                               "Site Selection", "event1","event1", "event2", "event3", "event4", "event5", "event6", "event8", "eventS")
        
        
        
      }else{
        datacrop <- data.frame()
        rawdata <- data.frame()
        columns_to_append <- c()
        patternissues<-""
        patternissuesE<-""
        
      },
      error = function(e) NULL)
    
    
    
    
    selectInput_ids <- list()
    selectInput_values <- list()
    
    lapply(1:20, function(k) {
      i<- usecases.index[names(usecases.index[ k ])]
      
      selectInput_ids <- c(selectInput_ids,
                           list(
                             stage = paste0("stagefinder_", i),
                             experiment = paste0("experimentfinder_", i),
                             season = paste0("seasonfinder_", i),
                             date = paste0("datefinder_", i),
                             enumerator = paste0("enumeratorfinder_", i),
                             region = paste0("regionfinder_", i),
                             household = paste0("householdfinder_", i)
                           )
      )
      
      output[[paste0("stagefinderr_",i)]] <-renderUI({
        selectInput(
          paste0("stagefinder_",i),
          label = "Stage",
          multiple=FALSE,
          choices =c('NOT Trials','Validation','Piloting'),
          selected= "Validation")
      })
      
      output[[paste0("experimentfinderr_",i)]] <-renderUI({
        selectInput(
          paste0("experimentfinder_",i),
          label = "Experiment",
          multiple=TRUE,
          choices =c("All", sort(unique(datacrop$crop))),
          # choices =c("All", "Potato", "Rice"),
          selected= "All")
      })
      
      output[[paste0("datefinderr_",i)]] <-renderUI({
        dateRangeInput(paste0("datefinder_",i),
                       "DATE:",
                       start = min(na.omit(rawdata$today)),
                       end   =  Sys.time())
      })
      
      output[[paste0("enumeratorfinderr_",i)]] <-renderUI({
        selectInput(
          paste0("enumeratorfinder_",i),
          label = "Enumerator",
          multiple=T,
          choices = c("All", sort(unique(datacrop$ENID))),
          selected= "All")
      })
      
      output[[paste0("regionfinderr_",i)]] <-renderUI({
        selectInput(
          paste0("regionfinder_",i),
          label = "Country",
          multiple=FALSE,
          choices = c() )
      })
      
      output[[paste0("householdfinderr_",i)]] <-renderUI({
        selectInput(
          paste0("householdfinder_",i),
          label = "Household",
          multiple=T,
          choices = c("All", sort(unique(na.omit(datacrop$HHID)))
          ),
          selected= "All")
      })
      
      output[[paste0("Totsub_box_",i)]] <-renderUI({
        infoBox(
          "Total submissions",paste0(nrow(rawdata)), icon = icon("list"),
          color = "olive", width = "100%"       )
      })
      
      output[[paste0("country_",i)]] <-renderUI({
        infoBox(
          "Country", HTML(paste(unique(na.omit(rawdata$Country)), collapse = ", ")) , icon = icon("globe"),
          color = "olive",width = "100%"       )
        
      })
      
      output[[paste0("project_",i)]] <-renderUI({
        infoBox(
          "Usecase", as.character(input$nav), icon = icon("barcode"),
          color = "olive",width = "100%"       )
      })
      # output[[paste0("summaryevents_",i)]] <-renderUI({                       as.character(unique(datacrop$Country))[1]
      #   infoBox(
      #     "Usecase", as.character(input$nav), icon = icon("barcode"),
      #     color = "olive",width = "100%"       )
      # })
    })
    
    
    observe({
      input_nav <- input$nav
      
      
      
      # Create a reactive expression for all use cases
      lapply(1:20, function(k) {
        
        i <- usecases.index[names(usecases.index[ k ])]
        
        
        
        experimentUsecase <- input[[paste0("experimentfinder_", i)]]
        stageUsecase <- input[[paste0("stagefinder_", i)]]
        dateUsecase <- input[[paste0("datefinder_", i)]]
        enumeratorUsecase <- input[[paste0("enumeratorfinder_", i)]]
        householdUsecase <- input[[paste0("householdfinder_", i)]]
        applyfilter<-input[[paste0("apply_filters", i)]]
        
        
        
        
        reactive_expr <- reactive({
          req(input_nav,experimentUsecase, stageUsecase, dateUsecase, enumeratorUsecase, householdUsecase)
          
        })%>%
          bindCache(experimentUsecase, stageUsecase, dateUsecase, enumeratorUsecase, householdUsecase)
        # reactive_expr_filter <- reactive({
        #   req(applyfilter)
        #   
        # })
        # 
        # observeEvent(reactive_expr_filter()  , {
        
        observeEvent(reactive_expr(), {
          
          tryCatch(
            if (input$nav== " SNS-Rwanda"){
              datacrop <- RWA.SUM_data
              rawdata <- RWA.O_data
              columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                                     "Site Selection", "event1", "event2", "event3", "event4", "event5", "event6","event7")
              
            }else if (input$nav== " Solidaridad Soy Advisory"){
              if ("Validation" %in% stageUsecase ){
                datacrop <- SOL.SUM_data
                rawdata <- SOL.O_data
                columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                                       "Site Selection", "event1", "event2", "event3", "event4", "event5", "event6","event7", "event8a", "event8b","event8c")
                
              }else if ("NOT Trials" %in% stageUsecase ){
                datacrop <- SOL.SUM_data
                rawdata <- SOL.NOT_data
                columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                                       "Site Selection","event1",  "event2", "event3", "event4", "event5", "event6","event7", "event8", "event9", "event10","event11", "event12", "event13", "event14", "event15", "event16","event17", "event18", "event19", "event20","event21")
                
              }
            }else if (input$nav== " KALRO"){
              
              KL.O_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLOdata.csv"),
                                       file = tempfile(fileext = ".csv")
              ) %>%
                fread()
              
              KL.SUM_data <- save_object(paste0("s3://rtbglr/", Sys.getenv("bucket_path"), "KLSUMdata.csv"),
                                         file = tempfile(fileext = ".csv")
              ) %>%
                fread()
              
              datacrop <- KL.SUM_data
              rawdata <- KL.O_data
              patternissues<-""
              patternissuesE<-""
              columns_to_append <- c("ENID", "HHID", "crop",#"treat",
                                     "Site Selection", "event1","event1", "event2", "event3", "event4", "event5", "event6", "event8", "eventS")
              
              
              
            }else {
              datacrop <- data.frame()
              rawdata <- data.frame()
              columns_to_append <- c()
              
            }
            ,error = function(e) NULL)
          
          
          output[[paste0("Totsub_box_",i)]] <-renderUI({
            infoBox(
              "Total submissions",paste0(nrow(rawdata)), icon = icon("list"),
              color = "olive", width = "100%"       )
          })
          
          output[[paste0("country_",i)]] <-renderUI({
            infoBox(
              "Country", HTML(paste(unique(na.omit(rawdata$Country)), collapse = ", ")) , icon = icon("globe"),
              color = "olive",width = "100%"       )
            
          })
          
          experimentfinderr_new_choices <- c("All", sort(unique(datacrop$crop)))
          enumeratorfinderr_new_choices <- c("All", sort(unique(datacrop$ENID)))
          householdfinderr_new_choices <- c("All", sort(unique(na.omit(datacrop$HHID))))
          datefinderr_new_choices <- min(na.omit(rawdata$today))
          
          
          updateSelectInput(session, paste0("experimentfinderr_",i), choices =  experimentfinderr_new_choices  )
          updateSelectInput(session, paste0("enumeratorfinderr_",i), choices = enumeratorfinderr_new_choices)
          updateSelectInput(session, paste0("householdfinderr_",i), choices = householdfinderr_new_choices)
          updateDateRangeInput(session, paste0("datefinderr_",i), start = datefinderr_new_choices, end = Sys.time())
          
          
          #CORRECT TO-
          #subset_df <- df[df$category %in% selected_categories, ]
          tryCatch(
            if (stageUsecase %in% stageUsecase ){
              datacrop<-datacrop[datacrop$Stage %in% stageUsecase, ]
              datacropO<-rawdata[rawdata$Stage %in% stageUsecase, ]
              #datasum<-datasum[datasum$Stage %in% stageUsecase, ]
              
            }
            ,error = function(e) NULL)
          
       
          
          
          tryCatch(
            if ("All" %in% experimentUsecase){
              datacrop<-datacrop
              datacropO<-datacropO
              #datasum<-datasum
            }else {
              datacrop<-datacrop[datacrop$crop %in% experimentUsecase, ]
              datacropO<-datacropO[datacropO$crop %in% experimentUsecase, ]
              
              #datasum<-datasum[datasum$crop %in% experimentUsecase, ]
            }
            ,error = function(e) NULL)
          
          tryCatch(
            if ("All" %in% enumeratorUsecase ){
              datacrop<-datacrop
              datacropO<-datacropO
              #datasum<-datasum
            }else {
              #datasum<-datasum[datasum$ENID %in% enumeratorUsecase, ]
              datacrop<-datacrop[datacrop$ENID %in% enumeratorUsecase, ]
              datacropO<-datacropO[datacropO$ENID %in% enumeratorUsecase, ]
              
            }
            ,error = function(e) NULL)
          
          tryCatch(
            if ("All" %in% householdUsecase){
              datacrop<-datacrop
              datacropO<-datacropO
              #datasum<-datasum
            }else{
              datacrop<-datacrop[which(datacrop$HHID %in%  householdUsecase), ]
              datacropO<-datacropO[datacropO$HHID %in% householdUsecase, ]
              
              #datasum<-datasum[which(datasum$HHID %in%  householdUsecase), ]
            }
            ,error = function(e) NULL)
          
          
          
          tryCatch(
            datacropO <- datacropO[which(datacropO$today >= dateUsecase[1] & datacropO$today <= dateUsecase[2]), ]
            ,error = function(e) NULL)
          
          tryCatch(
            #datacrop <- datacrop[which(datacrop$Date >= dateUsecase[1] & datacrop$Date <= dateUsecase[2]), ]
            datacrop <- datacrop[datacrop$ENID %in% datacropO$ENID, ]
            ,error = function(e) NULL)
          tryCatch(
            #datacrop <- datacrop[which(datacrop$Date >= dateUsecase[1] & datacrop$Date <= dateUsecase[2]), ]
            datacrop <- datacrop[datacrop$HHID %in% datacropO$HHID, ]
            ,error = function(e) NULL)
          
          
          
          
          
          
          
          #Summary map
          output[[paste0("trials_map_",i)]] <-renderLeaflet({
            leaflet() %>%
              addProviderTiles(providers$CartoDB.Positron) %>%
              addCircles(data = datacropO ,lng = as.numeric(datacropO$longitude), lat = as.numeric(datacropO$latitude),color = "orange") %>%suppressWarnings()
            #fitBounds(max(as.numeric(datacrop$`intro/longitude`)), max(as.numeric(datacrop$`intro/latitude`)),min(as.numeric(datacrop$`intro/longitude`)), min(as.numeric(datacrop$`intro/latitude`)))
          })
          
          ##Summary_submissions trend
          #group by date
          
          wgroup <-tryCatch( 
            datacropO %>%
              mutate(date = as.Date(today)) %>%
              select(date) %>%
              group_by(date) %>%
              count() %>%
              #rename(total_freq = n) %>%
              mutate(date = as.Date(date))
            ,error = function(e) NULL)
          
          # #plot of submissions trend
          Ir<-ggplot(wgroup, aes(x=date, y= n, group=1)) +
            geom_line(color="orange")+
            geom_point(color="orange")+
            #scale_x_discrete(labels= paste("Week", c(1:length(ff))))+
            theme_bw(base_size = 24)+
            labs(title="", x="Month", y="Submissions Count")+scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%m-%Y")+them2
          
          # #plotly -interactive ouput
          output[[paste0("submission_trend_",i)]] <-renderPlotly({
            tryCatch(  ggplotly(Ir, tooltip=c("x","y"))  ,error = function(e) NULL)
          })
          
          
          
          
          ##Enumerator Ranking
          
          # if (ncol(datacrop) != 0) {
          # if (  ncol(datacrop)==0 ) {
          #   # Keep the data as-is
          #   column_names <- c("ENID", "HHID", "crop","treat","Site Selection", "event1", "event2", "event3", "event4", "event5", "event6","event7")
          #   datacroptable <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
          #   colnames(datacroptable) <- column_names
          #   datacroptablev <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
          #   colnames(datacroptablev) <- column_names
          #   
          #   # datacrop<-datacrop%>%
          #   #   tibble::add_column(ENID= NA) %>%
          #   #   tibble::add_column(HHID= NA) 
          #   # datacrop$ENID<-as.character(datacrop$ENID)
          #   # datacrop$HHID<-as.character(datacrop$HHID)
          #     #   
          # } else {
          datacroptable<-datacrop 
          #datacroptablev <- datasum 
          #}
          
          # Columns to append
          datacroptable<-as.data.frame(datacroptable)
          
          
          
          
          
          # # Check if columns exist in the dataframe
          missing_columns <- setdiff(columns_to_append, colnames(datacroptable))
          #missing_columns2 <- setdiff(columns_to_append, colnames(datacroptablev))
          
          # 
          # # Append missing columns only
          tryCatch(  if (length(missing_columns) > 0) {
            datacroptable[, missing_columns] <- NA
          } ,error = function(e) NULL)
          #  tryCatch(  if (length(missing_columns2) > 0) {
          #    datacroptablev[, missing_columns2] <- NA
          #  } ,error = function(e) NULL)
          # # 
          
          # datacroptable$ENID<-as.character(datacroptable$ENID)
          # datacroptable$HHID<-as.character(datacroptable$HHID)
          # 
          # 
          # datacroptable<-left_join(datacrop,datacroptable, by=c("ENID","HHID"), .keep_all = TRUE)
          #datacroptable$`Site Selection` <- datacroptable$today
          
          # if ("crop" %in% colnames(datacroptable) ){
          # datacroptable$crop<-datacroptable$crop}
          
          #datacroptable$`Site Selection` <- ifelse(is.na(datacroptable$HHID), NA, datacroptable$`Site Selection`)
          
          
          datacroptable<-  tryCatch(  datacroptable %>%
                                        select(any_of(columns_to_append ))
          )
          colnames(datacroptable) <- toTitleCase(colnames(datacroptable)) #Title case for table headers
          # datacroptablev<-left_join(datasum,datacroptable, by=c("ENID","HHID"))
          #colnames(datacroptablev) <- toTitleCase(colnames(datacroptablev)) #Title case for table headers
          
          ranks.events<-  tryCatch(  datacroptable %>%
                                       #select(any_of(columns_to_append)) %>%
                                       select(-any_of(c("ENID", "HHID", "Crop"))) %>%
                                       dplyr::summarise(across(.fns = ~sum(!is.na(.)))) %>%  suppressWarnings() ,error = function(e) NULL)  #total submissions for each event
          
          
          
          ranks<-  tryCatch(  datacroptable %>%
                                # select(any_of(columns_to_append)) %>%
                                select(-any_of(c( "HHID", "Crop")))%>%
                                group_by(ENID) %>%
                                dplyr::summarise(across(.fns = ~sum(!is.na(.))))%>%  suppressWarnings()
                              ,error = function(e) NULL)  #total submissions,  for each event per enumerator
          
          
          #datacroptable$Site.Selection <-as.Date(datacroptable$Site.Selection)
          
          output[[paste0("rankingevents_",i)]]  <- renderReactable({
            reactable(ranks.events,
                      pagination = FALSE,
                      showPagination = TRUE,
                      paginateSubRows = FALSE,
                      
            )
            
          }) %>%
            bindCache(ranks.events)
          
          output[[paste0("ranking_",i)]]  <- renderReactable({
            reactable(ranks,
                      pagination = FALSE,
                      showPagination = TRUE,
                      paginateSubRows = FALSE,
                      columns = list(
                        ENID = colDef(
                          html = TRUE,
                          show = TRUE,
                          cell =    function(value,index) {
                            s2<-datacrop[which(datacrop$ENID==value ), ]
                            tippy(value,tooltip = paste("NAME:", unique(s2$ENfirstName) , unique(s2$ENSurname), "<br>", "CONTACT:", unique(s2$ENphoneNo)))
                          },
                        )
                      )
            )
          })%>%
            bindCache(ranks)
          
          
          
          ################ISSUES TABLE ################################
          datacropI<-datacroptable 
          datacropissues<-datacropI %>%
            dplyr::select(any_of(c("ENID", "HHID")))
          
          event_conditions <- function(row) {
            for (i in 4:length(row)) { # Start from the "event1" column index
              if (is.na(row[i-1]) && !is.na(row[i])) {
                return(TRUE)
              } else if (!is.na(row[i-1]) && !is.na(row[i])) {
                break
              }
            }
            return(FALSE)
          }
          # Subset rows based on conditions
          datacropissuesA <-tryCatch(  datacropissues %>%
                                         filter(!grepl(patternissues, ENID)) %>%
                                         mutate(Issues = ifelse(!grepl(patternissues, ENID), "Check ENID", NA))%>%
                                         mutate(Issues = as.character(Issues)),error = function(e) NULL) 
          datacropissuesB <- tryCatch( datacropissues %>%
                                         filter(!grepl(patternissuesE, HHID)) %>%
                                         mutate(Issues = ifelse(!grepl(patternissuesE, HHID), "Check HHID", NA)) %>%
                                         mutate(Issues = as.character(Issues)),error = function(e) NULL) 
          datacropissuesC <- tryCatch( datacropI %>%
                                         filter(apply(datacropI[, 4:ncol(datacropI)], 1, event_conditions))%>% # Consider columns from "event1" to the end
                                         dplyr::select(any_of(c("ENID", "HHID")))%>%
                                         mutate(Issues = "Check submission events" ),error = function(e) NULL) 
          
          datacropissues <-bind_rows(datacropissuesA, datacropissuesB,datacropissuesC)
          datacropissues <- as.data.frame( datacropissues)
          
          output[[paste0("issues_",i)]]  <- renderReactable({
            reactable(datacropissues,
                      pagination = FALSE,
                      showPagination = TRUE,
                      paginateSubRows = FALSE
            )
          }) %>%
            bindCache(datacropissues)
          
          
          
          
          # Convert the date string to a Date object
          #datacroptable$`Site Selection` <- as.Date( datacroptable$`Site Selection` , format = "%Y-%m-%d")
          ##Enumerator Tracker Table
          output[[paste0("tableR_",i)]] <- renderReactable({
            #output$tableR <- renderReactable({
            reactable(datacroptable,
                      pagination = FALSE,
                      showPagination = TRUE,
                      paginateSubRows = FALSE,
                      defaultExpanded = TRUE,
                      #groupBy = c("ENID","HHID"),
                      # columnGroups = list(
                      #   colGroup(name = "", columns = c("ENID","District", "HHID","expCode")),
                      #   colGroup(name = "Activity.1", columns = c("Register.Household", "Register.Field")),
                      #   colGroup(name = "Activity.2", columns = c("Register.Trial.Plot","Field.Description","Soil.Sampling", "Record.Crop.Variety")),
                      #   colGroup(name = "Activity.3", columns = c("Germination.Count")),
                      #   colGroup(name = "Activity.4", columns = c("Top.Dressing")),
                      #   colGroup(name = "Activity.5", columns = c("PD.Scoring","Weeding", "Household.Survey")),
                      #   colGroup(name = "Activity.6", columns = c("Plant.Sampling")),
                      #   colGroup(name = "Activity.7", columns = c("Harvest"))
                      # ),
                      columns = list(
                        #   today = colDef(show = FALSE),
                        #   FDID2 = colDef(show = FALSE,
                        #                  style  = function(value) {
                        #                    list(background ="white")
                        #                  }),
                        # 
                        #   #expCode = colDef(show = FALSE),
                        #   # TLID2 = colDef(filterable = TRUE,
                        #   #                style  = function(value) {
                        #   #                  list(background ="white")
                        #   #                }),
                        #   District = colDef(filterable = TRUE,
                        #                     style  = function(value) {
                        #                       list(background ="white")
                        #                     }),
                        Crop= colDef(filterable = TRUE,
                                     style  = function(value) {
                                       list(background ="white")
                                     }),
                        # Treat = colDef(filterable = TRUE,
                        #               style  = function(value) {
                        #                 list(background ="white")
                        #               }),
                        HHID = colDef(
                          style  = function(value) {
                            list(background ="white")
                          }),
                        `Site Selection` = colDef(
                          #style  = function(value) {
                          
                          style = function(value) {
                            
                            # Check if the value is missing or not in the expected date format
                            if (is.na(value) ) {
                              list(background = "#ffa590")  # Set default background color for missing or invalid values
                            } else {
                              list(background = "#BFffa590")
                              # # Convert the date string to a Date object
                              # date_value <- as.Date(value, format ="%Y-%m-%d")
                              # 
                              # # Calculate the target date (16/08/2023 + 2 weeks)
                              # target_date <- as.Date("2023-08-16", format ="%Y-%m-%d") + 14
                              # 
                              # if (date_value > target_date) {
                              #   list(background = "#BFffa590") #WONT SET OVERDUE... NOT Necessary? 16/08/2023 -is just training date
                              # } else {
                              #   list(background = "#BFffa590")
                              # }
                            }
                          }
                          # 
                          # color<-ifelse(is.na(value) ,"orange","#BFffa590")
                          # 
                          # # if (value <= format((format('16/8/2023',"%d/%m/%Y"))+weeks(2), "%d/%m/%Y") ){
                          # #   color<-ifelse(is.na(value) ,"orange","#BFffa590")
                          # #   list(background =color)
                          # # } 
                          #ist(background =color)
                          
                          #}
                        ),
                        Event1 = colDef(
                          # style = function(value, index) {
                          #   # Get the current system date
                          #   current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")   
                          #   
                          #   if (is.na(value)) {
                          #     list(background = "orange")
                          #     # If Event1 is NA and current date is more than Site Selection + 4 weeks, color is red
                          #     # if (current_date > (as.Date(datacroptable$`Site Selection`, format = "%Y-%m-%d") + 28)) {
                          #     #   list(background = "red")
                          #     # } else {
                          #     #   # If Event1 is NA and current date is less than Site Selection + 4 weeks, color is orange
                          #     #   list(background = "orange")
                          #     # }
                          #   } 
                          # else {
                          #   # Convert Event1 value to Date
                          #   event1_date <- as.Date(value, format = "%Y-%m-%d")
                          #   
                          #   # Calculate target dates
                          #   target_date_2_weeks <- as.Date(datacroptable$`Site Selection`, format = "%Y-%m-%d") + 14
                          #   target_date_4_weeks <- as.Date(datacroptable$`Site Selection`, format = "%Y-%m-%d") + 28
                          #   
                          #   if (current_date < target_date_2_weeks) {
                          #     # If current date is less than Site Selection + 2 weeks, color is purple
                          #     list(background = "purple")
                          #   } else if (current_date < target_date_4_weeks) {
                          #     # If current date is less than Site Selection + 4 weeks but >= 2 weeks, color is green
                          #     list(background = "green")
                          #   } else {
                          #     # If current date is greater than or equal to Site Selection + 4 weeks, color is red
                          #     list(background = "red")
                          #   }
                          # }
                          #}
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            
                            target_dates <- as.Date(datacroptable$`Site Selection`[index], format = "%Y-%m-%d") + 14
                            
                            if (is.na(target_dates)&& is.na(value) ) {
                              color <-"#BE93D4"
                                list(background =color)
                            }else if (!is.na(target_dates)&& is.na(value) && current_date >= target_dates){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (!is.na(target_dates)&& is.na(value) && current_date <= target_dates){
                              color <-"orange"
                                list(background =color)
                            } else if (!is.na(target_dates)&& !is.na(value) ) {
                              color <-"#BFffa590"
                                list(background =color)
                            }
                            
                          }
                        ),
                        Event2 = colDef(
                          style  =    function(value,index) {
                            
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 29
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (!is.na(target_dates)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (!is.na(target_dates)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates){
                              color <-"#ffa590"
                                list(background =color)
                            } else if(!is.na(target_dates)&&  is.na(value) && current_date <= target_dates){
                              color <-"orange"
                                list(background =color)
                            }else if( !is.na(target_dates)&& is.na(value) && current_date >= target_dates){
                              color <-"#ffa590"
                                list(background =color)
                            } else if (is.na(target_dates)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                            
                            
                            
                            
                            #list(background =color)
                            
                            # if (current_date < target_date_4_weeks){
                            #   color<-ifelse(is.na(value) ,"orange","#BFffa590")
                            #   list(background =color)
                            # }
                            
                          }
                        ),
                        Event3 = colDef(
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 43
                            target_dates_prev <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 29
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (!is.na(target_dates)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (!is.na(target_dates)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates){
                              color <-"#ffa590"
                                list(background =color)
                            } else if(!is.na(target_dates)&&  is.na(value) && current_date <= target_dates && current_date >= target_dates_prev){
                              color <-"orange"
                                list(background =color)
                            } else if(!is.na(target_dates)&&  is.na(value) && current_date <= target_dates && current_date <= target_dates_prev){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if( !is.na(target_dates)&& is.na(value) && current_date >= target_dates){
                              color <-"#ffa590"
                                list(background =color)
                            } else if (is.na(target_dates)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                          }
                        ),
                        Event4 = colDef(
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 57
                            target_dates_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 64
                            target_dates_prev <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 43
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_potato) {
                              color <-"#BFffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_rice) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date >= target_dates_prev){
                              color <-"orange"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date >= target_dates_prev){
                              color <-"orange"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date <= target_dates_prev){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date <= target_dates_prev){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& is.na(value) && current_date >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& is.na(value) && current_date >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            } else if (is.na(target_dates_potato)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if (is.na(target_dates_rice)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                          }
                        ), 
                        Event5 = colDef(
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 71
                            target_dates_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 78
                            target_dates_prev_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 57
                            target_dates_prev_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 64
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_potato) {
                              color <-"#BFffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_rice) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date >= target_dates_prev_potato){
                              color <-"orange"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date >= target_dates_prev_rice){
                              color <-"orange"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date <= target_dates_prev_potato){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date <= target_dates_prev_rice){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& is.na(value) && current_date >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& is.na(value) && current_date >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (is.na(target_dates_potato)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if (is.na(target_dates_rice)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                          }
                        ), 
                        Event6 = colDef(
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 85
                            target_dates_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 92
                            target_dates_prev_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 71
                            target_dates_prev_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 78
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_potato) {
                              color <-"#BFffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_rice) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date >= target_dates_prev_potato){
                              color <-"orange"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date >= target_dates_prev_rice){
                              color <-"orange"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date <= target_dates_prev_potato){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date <= target_dates_prev_rice){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& is.na(value) && current_date >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& is.na(value) && current_date >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (is.na(target_dates_potato)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if (is.na(target_dates_rice)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                          }
                        ), 
                        Event7 = colDef(
                          style  = function(value,index) {
                            current_date <-as.Date(Sys.Date() , format = "%Y-%m-%d")  
                            #event2_date <- as.Date(value, format = "%Y-%m-%d")
                            target_dates_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 112
                            target_dates_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 112
                            target_dates_prev_potato <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 85
                            target_dates_prev_rice <- as.Date(datacroptable$Event1[index], format = "%Y-%m-%d") + 92
                            
                            #print(!is.na(value) && current_date > target_dates)
                            if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_potato) {
                              color <-"#BFffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) &&  as.Date(value , format = "%Y-%m-%d") <= target_dates_rice) {
                              color <-"#BFffa590"
                                list(background =color)
                            } else if (datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if (datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& !is.na(value) && as.Date(value , format = "%Y-%m-%d") >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date >= target_dates_prev_potato){
                              color <-"orange"
                                list(background =color)
                            }  else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date >= target_dates_prev_rice){
                              color <-"orange"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&&  is.na(value) && current_date <= target_dates_potato && current_date <= target_dates_prev_potato){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if(datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&&  is.na(value) && current_date <= target_dates_rice && current_date <= target_dates_prev_rice){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="potatoIrish" && !is.na(target_dates_potato)&& is.na(value) && current_date >= target_dates_potato){
                              color <-"#ffa590"
                                list(background =color)
                            }else if( datacroptable$Crop[index]=="rice" && !is.na(target_dates_rice)&& is.na(value) && current_date >= target_dates_rice){
                              color <-"#ffa590"
                                list(background =color)
                            } else if (is.na(target_dates_potato)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }else if (is.na(target_dates_rice)&& is.na(value) ){
                              color <-"#BE93D4"
                                list(background =color)
                            }
                          }
                        ),
                        # Event1 = colDef(
                        #   
                        #   style  = function(value, index) {
                        #     otherColumnValue <- value$`Site Selection`[index]
                        #     
                        #     if (){}
                        #     
                        #     color<-ifelse(is.na(value) ,"#BE93D4","#BFffa590")
                        #     list(background =color)
                        #   }
                        # ),
                        #   # TLID2 = colDef(
                        #   #   cell =    function(value,index) {
                        #   #     s2<-register_en[which(register_en$ENID==ak$ENID[index] ), ]
                        #   #     tippy(value,tooltip = paste("NAME:", s2$detailsEN.firstName , s2$detailsEN.surName, "<br>", "CONTACT:", s2$detailsEN.phoneNr))
                        #   #   },),
                        # HHID = colDef(
                        #     cell =    function(value,index) {
                        #      # s2<-RegisterVerify_HH[which(RegisterVerify_HH$HHID==ak$HHID[index] ), ]
                        #      # tippy(value,tooltip = paste("NAME:", s2$detailsHH.firstName , s2$detailsHH.surName))
                        #     },
                        #     style  = function(value) {
                        #       #color<-ifelse(value=="NA" ,"#BE93D4","#BFffa590")
                        #       list(background ="white")
                        #     }
                        #   ),
                        ENID = colDef(
                          html = TRUE,
                          #filterable = TRUE,
                          show = TRUE,
                          cell =    function(value,index) {
                            s2<-datacrop[which(datacrop$ENID==value ), ]
                            tippy(value,tooltip = paste("NAME:", unique(s2$ENfirstName) , unique(s2$ENSurname), "<br>", "CONTACT:", unique(s2$ENphoneNo)))
                          },
                          header = function(value) {tippy(value,tooltip = paste("NAME:", "<br>", "CONTACT:"))},
                          style  = function(value) {
                            #color<-ifelse(value=="NA" ,"#BE93D4","#BFffa590")
                            list(background ="white")
                          }
                        )
                        #   
                        #   
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
                          
                        ),
                        style  = function(value) {
                          # if (name=="Site Selection"){
                          #   color<-ifelse(is.na(value) ,"orange","#BFffa590")
                          # }
                          
                          color<-ifelse(is.na(value) ,"#BE93D4","#BFffa590")
                          # if (input[[paste0("seasonfinder_",i)]]== "2022B"){
                          #   color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                          # } else if (input[[paste0("seasonfinder_",i)]]== "2022A"){
                          #   color<-ifelse(value=="NA" ,"#BE93D4","#BFffa590")
                          # } else {
                          #   color<-ifelse(value=="NA" ,"#ffa590","#BFffa590")
                          # }
                          #list(background =color)
                        }
                        # 
                        
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
            
          })%>%
            bindCache(datacroptable)
          
          
          
          output[[paste0("downloadsummary_",i)]] <- downloadHandler(
            filename = function() {
              paste("summary_",gsub("-", "",Sys.Date()), ".pdf", sep = "")
              #paste("summary.pdf", sep = "")
            },
            content = function(file) {
              withProgress(message = "Downloading...", {
                # Create an R Markdown document
                rmarkdown::render(
                  "./www/Scripts/Summary.Rmd",
                  output_file = file,
                  params = list(df1 = ranks.events, df2 = ranks)
                )
              })
            }
            
          )
          
          ##Data Download
          datacropdown<-rawdata%>%
            dplyr::rename(any_of(c(Date = "today",Country = "intro/country",      Crop = "crop")
                                 
            ))
          
          output[[paste0("tabledownload_",i)]] <- renderDataTable(datacropdown)
          
          output[[paste0("downloadData_",i)]] <- downloadHandler(
            filename = function() {
              paste("data_",gsub("-", "",Sys.Date()), ".csv", sep = "")
            },
            content = function(file) {
              write.csv(rawdata, file, row.names = FALSE)
            }
          )
          
          
          outputOptions(output, paste0("trials_map_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("submission_trend_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("tabledownload_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("tableR_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("ranking_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("rankingevents_",i), suspendWhenHidden = FALSE)
          outputOptions(output, paste0("issues_",i), suspendWhenHidden = FALSE)
          
        })
        #})
        
      })
    })
    
    
    
  })
  
  session$allowReconnect(TRUE)
  
}

# Run the application
shinyApp(ui = ui, server = server,options = list(port = 8000))


