#toremove ...replace with auth0 after deploy
user_base <- dplyr::tibble(
  user = c("user1", "user2", "user3", "user4"),
  password = sapply(c("pass1", "pass2", "pass3", "pass4"), sodium::password_store),
  permissions = c("admin", "standard","admin", "standard"),
  name = c("User RW1", "User RW2","User NG1", "User NG2")
)
list1<-c("user1","user2")
list2<-c("user3","user4")

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







