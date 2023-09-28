#load data(updated daily)

#SNS.Rwanda.VAL_data<-read.csv("./data/Usecases/SNS-Rwanda/SNS-Rwanda.VAL_data.csv") 
# SNS.Rwanda.SUM_data<-read.csv("./data/SNSRwandaSUMdata.csv") 
# SNS.Rwanda.O_data<-read.csv("./data/SNSRwandaOdata.csv") 
usersdata<-read.csv('./data/usecases_updated.csv')
userList<-as.data.frame(strsplit((paste0(usersdata$users, collapse=",")), ','))[,1]
passList<-as.data.frame(strsplit((paste0(usersdata$password, collapse=",")), ','))[,1]
permissionList<-as.data.frame(strsplit((paste0(usersdata$permissions, collapse=",")), ','))[,1]
#toremove ...replace with auth0 after deploy
user_base <- dplyr::tibble(
  #user = c("user1", "user2", "user3", "user4"),
  user = unique(userList),
  #password = sapply(c("pass1", "pass2", "pass3", "pass4"), sodium::password_store),
  password = sapply(unique(passList), sodium::password_store),
  #permissions = c("admin", "standard","admin", "standard"),
  permissions = unique(permissionList)#,
  #name = c("User RW1", "User RW2","User NG1", "User NG2")
)
# list1<-c("user1","user2")
# list2<-c("user3","user4")
 

#basemap for leaflet
basemap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) #%>%
 

#ggplot theme
them2<-theme(panel.background = element_rect(fill = "white"), # bg of the panel
             plot.background = element_rect(fill = "white", color = NA), # bg of the plot
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(size=12, face="bold",color = "#a9a9a9", hjust = 0.5 ),
             strip.text.x = element_text(size = 15, color = "#a9a9a9", face = "bold"),
             axis.text=element_text(color = "#a9a9a9",size=10),
             axis.text.x = element_text(angle = 60, hjust = 1),
             #axis.text.y = element_blank(),
             #axis.title=element_text(size=16,face="bold"),
             axis.title=element_text(color = "#a9a9a9",size=10),
             legend.title = element_text(color = "#a9a9a9",face="bold", size = 12),
             legend.text = element_text(color = "#a9a9a9", size = 10),                   
             legend.background = element_rect(fill = "black"),                   
             panel.border = element_blank(),
             #axis.line.x = element_line(color="black", size = 0.3),
             #scale_x_date(date_breaks = "months" , date_labels = "%b-%y"),
             #axis.line.y = element_line(color="black", size = 0.3))  
             axis.line.x = element_blank(),
             #hovertemplate = paste('%{x}', '<br>lifeExp: %{text:.2s}<br>'),
             axis.line.y = element_blank())




# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
  paste("Feedback & suggestions")
  
}

##
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence)
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}



# list<-list.files(p, pattern="Measure", full.names=TRUE)
# crops<-c()
# for (i in 1:length(list)){
#   new_crop<-sub(".*Measure_", "", list[i])
#   new_crop<-sub("_.*", "", new_crop)
#   
#   crops<- c(crops,new_crop)
#   
#   
# }






jscode <- "
shinyjs.hrefAuto = function(url) { window.location.href = url;};
"
# Function to create a tab panel
usecases.index<-c(  " SAA-Nigeria"  =1           , " DigGreen-Ethiopia"  =2  ," Fert-Ethiopia"     =3    , " SNS-Rwanda"  =4  ,  " ex-iSDA-Ghana"    =5      , " ex-Wcover-Ghana" =6     ,
                    " Planting-S-Asia" =7     ," DSRC-SE-Asia" =8       ,   " Govt-Egypt"     =9    ," Govt-LatAm"  =10     ,   " Cocoa Soils"  =11   ,   " Rainforest Alliance" =12  ,
                    " One Acre Fund"    =13     ,  " DRC Coffee OLAM"  =14       ,   " Solidaridad Soy Advisory" =15 , " DSR Extension Vietnam" =16  , " Morocco CA" =17          ,  " Mercy Corps SPROUT"  =18,
                    " BAYGAP (BAYER)" =19  )



create_tab_panel <- function(tab_name) {

  uc<-usecases.index[[tab_name]]
 
  tabPanel(tab_name,
           dashboardPage(
             dashboardHeader(),
             dashboardSidebar(
               width = 200,
               useShinyjs(),
               id = paste0("sidebar-", uc), 
               tags$head(tags$style(HTML("
                 .collapsible-content {
                   display: none;
                   padding: 5px;
                 }
                 .collapsed .collapsible-content {
                   display: none;
                 }
                 .expanded .collapsible-content {
                   display: block;
                 }
               "))),
               sidebarMenu(id = paste0("tabs-", uc), 
                           menuItem( "DATA", tabName = 'dashboard', icon = icon('dashboard'),startExpanded = T   ,

                                     #filter options
                                     uiOutput(paste0('stagefinderr_',uc)),
                                     uiOutput(paste0('experimentfinderr_',uc)), #experiment code
                                     uiOutput(paste0('datefinderr_',uc)),
                                     uiOutput(paste0('enumeratorfinderr_',uc)), #enumerators
                                     uiOutput(paste0('householdfinderr_',uc))#,  #households
                                     #actionButton(paste0('apply_filters_',uc), "Apply Filters")
                                     
                           )
                           ## 'Glossary' tab
                           #menuItem( "GLOSSARY", tabName = 'glossary', icon = icon('bell') )
               )


             ),
             dashboardBody(
               tabsetPanel(
                 id = paste0("tabss-", uc),  # Unique id for each tabPanel
                 type = "tabs",
                 tabPanel(tabName="Summary"  ,"SUMMARY",
                         # HTML('<h3>  Welcome! </h3>'),
                          #textOutput("selected_var"),
                          HTML('<h5>  This data monitoring dashboards helps you track enumerator submissions for various trials.</h5>'),
                          HTML('<br>'),
                          fluidRow( column( width = 12,  align = 'left', infoBoxOutput(  paste0("project_",uc) ) , infoBoxOutput(paste0("country_",uc) ),infoBoxOutput(paste0("Totsub_box_",uc) ))
                                    #column( width = 6,  align = 'right', uiOutput("Totsub_box"))trials_map submission_trend
                          ),
                         #fluidRow( column( width = 12,  align = 'left', infoBoxOutput(  paste0("summaryevents_",uc) ) )),
                          fluidRow( column( width = 6,h4("Trials by Location", align = 'center'), leafletOutput(paste0('trials_map_',uc),height = "50vh"),style = "background-color: #f2f2f2;"),
                                    column( width = 6,h4("Trend of Submissions", align = 'center'), plotlyOutput(paste0('submission_trend_',uc), height = "50vh"),style = "background-color: #f2f2f2;")
                          ),
                          HTML('<br>'),
                         fluidRow(
                           column(width = 9, h4("Summary of Complete Submissions", align = 'center')),
                           column(width = 1, downloadButton(paste0("downloadsummary_", uc), "Download Summary", style = "color: green")),
                           column(width = 2)
                         ),
                         fluidRow( column( width = 12,h4("", align = 'center'),  reactableOutput(paste0("rankingevents_", uc)) )
                         ),
                        
                          fluidRow( column( width = 12,h4("", align = 'center'), reactableOutput(paste0("ranking_",uc)) )
                          )
                         
                 ),

                 tabPanel(tabName="Enumerators","ENUMERATORS",
                          HTML('<br>'),
                          HTML('<span  style="background-color: #BFffa590 ;align:right;" class="dot">Complete</span>&nbsp; <span  style="background-color: orange ;align:right;" class="dot">Missing Details</span>&nbsp;<span  style="background-color: #ffa590;align:right;" class="dot">Overdue</span>  &nbsp;<span  style="background-color: #BE93D4;align:left;" class="dot">Future Event</span>'),
                          HTML('<br>'),
                          reactableOutput(paste0("tableR_",uc))
                 ),

                 # tabPanel(tabName="quality" ,"QUALITY CHECKS"
                 #          #reactableOutput("tableQ1")
                 # ),

                 tabPanel(tabName= "data","DATA PREVIEW",
                          column(12,
                                 dataTableOutput(paste0('tabledownload_',uc)),style = "height:75vh; overflow-y: scroll;overflow-x: scroll;"
                          ),

                          HTML('<br>'),
                          downloadButton(paste0("downloadData_",uc), "Download csv",style="color: green")
                 ),
                 tabPanel(tabName="glossary" ,"GLOSSARY",
                          includeHTML('glossary.html')
                 ),

               )

             )
           )
  )
  
  
}

# Function to create navbarMenu with tabPanel elements
create_navbarMenu <- function(tab_names) {
  # Create a list of tab panels using lapply
  tab_panels <- lapply(tab_names, create_tab_panel)
  
  # Use do.call to create the navbarMenu with the list of tab panels
  do.call(navbarMenu, c("Usecase", tab_panels))
}





blank2na = function(x,na.strings=c('','.','NA','na','N/A','n/a','<NA>','NaN','nan')) {
  if (is.factor(x)) {
    lab = attr(x, 'label', exact = T)
    labs1 <- attr(x, 'labels', exact = T)
    labs2 <- attr(x, 'value.labels', exact = T)
    
    # trimws will convert factor to character
    x = trimws(x,'both')
    if (! is.null(lab)) lab = trimws(lab,'both')
    if (! is.null(labs1)) labs1 = trimws(labs1,'both')
    if (! is.null(labs2)) labs2 = trimws(labs2,'both')
    
    if (!is.null(na.strings)) {
      # convert to NA
      x[x %in% na.strings] = NA
      # also remember to remove na.strings from value labels 
      labs1 = labs1[! labs1 %in% na.strings]
      labs2 = labs2[! labs2 %in% na.strings]
    }
    
    # the levels will be reset here
    x = factor(x)
    
    if (! is.null(lab)) attr(x, 'label') <- lab
    if (! is.null(labs1)) attr(x, 'labels') <- labs1
    if (! is.null(labs2)) attr(x, 'value.labels') <- labs2
  } else if (is.character(x)) {
    lab = attr(x, 'label', exact = T)
    labs1 <- attr(x, 'labels', exact = T)
    labs2 <- attr(x, 'value.labels', exact = T)
    
    # trimws will convert factor to character
    x = trimws(x,'both')
    if (! is.null(lab)) lab = trimws(lab,'both')
    if (! is.null(labs1)) labs1 = trimws(labs1,'both')
    if (! is.null(labs2)) labs2 = trimws(labs2,'both')
    
    if (!is.null(na.strings)) {
      # convert to NA
      x[x %in% na.strings] = NA
      # also remember to remove na.strings from value labels 
      labs1 = labs1[! labs1 %in% na.strings]
      labs2 = labs2[! labs2 %in% na.strings]
    }
    
    if (! is.null(lab)) attr(x, 'label') <- lab
    if (! is.null(labs1)) attr(x, 'labels') <- labs1
    if (! is.null(labs2)) attr(x, 'value.labels') <- labs2
  } else {
    x = x
  }
  return(x)
}
