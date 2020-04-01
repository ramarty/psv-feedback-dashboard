# PSV Feedback Page

# RESOURCES:
# https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage

# setwd("/Users/robmarty/Documents/Github/psv-feedback-dashboard")

library(dplyr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(plotly)
library(lubridate)
library(shinydashboard)
library(tableHTML)





#mobileDetect <- function(inputId, value = 0) {
#  tagList(
#    singleton(tags$head(tags$script(src = "js/mobile.js"))),
#    tags$input(id = inputId,
#               class = "mobile-element",
#               type = "hidden")
#  )
#}

# Results ----------------------------------------------------------------------
ui <- fluidPage(
  navbarPage(
    theme = shinytheme("journal"), collapsible = TRUE,
    "PSV Feedback", id="nav",
    
    #the line of code places the logo on the left hand side before the tabs start. See image below.
    #title = div(img(src='psv_logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
    #title = "PSV Feedback",
    #theme = shinytheme("yeti"),
  #dashboardBody(
    #MAIN TAB~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                       
  #tags$style(HTML("
#
#
 #               .box.box-solid.box-primary>.box-header {
#
 #               }
  #              
   #             #final_text {
    #                text-align: center;
     #             }
      #            div.box-header {
       #             text-align: center;
      #            }
#
 #               .box.box-solid.box-primary{
#
#                background:#222d32
#                }
#
 #               ")),
#  tags$style(make_css(list('.box', 
#                           c('font-size', 'font-family', 'color'), 
#                           c('13px', 'helvetica', 'white')))),
#  fluidRow(
#    box(width = 12, title = "PSV Feedback Summary", status = "primary", solidHeader = TRUE #, height=35
#    )
#  ),  
  
tabPanel("t",
             #div(class="outer",
                 
                fluidRow(
                  column(3,
                         ""),
                  column(6, align="center",
                         "In select PSVs riders can rate the PSV along various categories.
                         This page shows results by PSV."
                  )
                ),
                fluidRow(
                  column(3,
                         ""),
                  column(6, align="center",
                                  selectInput(inputId = "question",
                                              label = h4("Select Question"),
                                              choice = c("Overall Safety",
                                                         "Driving Speed",
                                                         "Number of Passengers",
                                                         "Measures for COVID-19"))

                ),
                fluidRow(
                  column(12,
                         hr()
                         )
            
                ),
 
                fluidRow(
                  column(12,
                         align="center",
                  h4(textOutput("question_title")),
                    plotOutput(outputId = "quetion_fig",
                                 height = "700px")
                )
                ),
                hr(),
                fluidRow(
                  column(3,
                         ),
                  column(6, align="center",
                         "We're currently working with 5 PSVs, but will be growing! As we do, we'll add to this site -- showing ratings by sacco, PSVs within saccos, etc. Check back again soon!"
                  )
                ),
                fluidRow(
                  column(12, align="center",
                         div(img(src='logos.png',style="margin-top: 14px; padding-right:10px;padding-bottom:10px", height = 100)),
                         )
                ),
                fluidRow(
                  column(3,
                         ""),
                  column(6, align="center",
                         textOutput("last_updated")
                  )
                )
              )
            ),
tabPanel("By PSV",
         fluidRow(
           column(3,
                  ""),
           column(6, align="center",
                  h1("Page under development, come back soon!")
           )
         ),
         fluidRow(
           column(12, align="center",
                  div(img(src='logos.png',style="margin-top: 14px; padding-right:10px;padding-bottom:10px", height = 100)),
           )
         ),
         fluidRow(
           column(3,
                  ""),
           column(6, align="center",
                  textOutput("last_updated")
           )
         )
         
)
         
)
)

                      
# Server -----------------------------------------------------------------------
server <- function(input, output) {
  
  # ** Load and Prep Data ------------------------------------------------------
  qr_data <- readRDS("qr_code_data.Rds")
  
  output$last_updated <- renderText({
    t <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character()
    paste0("Last Updated: ", t)
  })
  
  output$question_title <- renderText({
    
    if(input$question %in% "Overall Safety"){
      qr_data$var <- qr_data$driver_safety_rating
      title <- "How would you rate your Matatu driver?"
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "springgreen4")
    } 
    
    if(input$question %in% "Driving Speed"){
      qr_data$var <- qr_data$driver_speed
      title <- "How would you describe your Matatu driver's speed?"
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "yellow3")
    } 
    
    if(input$question %in% "Number of Passengers"){
      qr_data$var <- qr_data$passenger_amount
      title <- "On the Matatu, are there..."
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "springgreen4")
    } 
    
    if(input$question %in% "Measures for COVID-19"){
      qr_data$var <- qr_data$covid19_measures
      title <- "Were measures taken to prevent the spread of COVID-19?"
      color_values <- c("firebrick3", "springgreen4", "springgreen4")
    } 
    
    title
    
  })
  

  output$quetion_fig <- renderPlot({
    
    
    if(input$question %in% "Overall Safety"){
      qr_data$var <- qr_data$driver_safety_rating
      title <- "How would you rate your Matatu driver?"
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "springgreen4")
      limit_max <- 1
    } 
    
    if(input$question %in% "Driving Speed"){
      qr_data$var <- qr_data$driver_speed
      title <- "How would you describe your Matatu driver's speed?"
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "yellow3")
      limit_max <- 1
    } 
    
    if(input$question %in% "Number of Passengers"){
      qr_data$var <- qr_data$passenger_amount
      title <- "On the Matatu, are there..."
      color_values <- c("firebrick3", "firebrick3", "springgreen4", "springgreen4")
      limit_max <- 2
    } 
    
    if(input$question %in% "Measures for COVID-19"){
      qr_data$var <- qr_data$covid19_measures
      title <- "Were measures taken to prevent the spread of COVID-19?"
      color_values <- c("firebrick3", "springgreen4", "springgreen4")
      limit_max <- 1
    } 
    
    strip_text_hjust <- 0.5
    
    #if(!is.null(input$isMobile)){
    #  if(input$isMobile) strip_text_hjust <- 0
    #}
    
    data_sub <- qr_data[!is.na(qr_data$var),] %>%
      
      # Summarise
      group_by(var, route_place_label) %>%
      summarise(N = n()) %>%
      ungroup() %>%

      # Complete
      complete(var, 
               route_place_label,
               fill = list(N = 0)) 
  
    order_df <- data_sub[as.numeric(data_sub$var) %in% 1,]
  
    data_sub$route_place_label <- data_sub$route_place_label %>%
      factor(levels = rev(order_df$route_place_label[order(order_df$N)]))
    
    data_sub <- data_sub %>%
      group_by(route_place_label) %>%
      mutate(N_total = sum(N)) %>%
      ungroup()
    
    #data_sub$prop <- data_sub$N 
    data_sub$prop <- data_sub$N / data_sub$N_total
    
    data_sub$zero <- .02
    
    p <- ggplot(data_sub) +
      geom_col(aes(x=var,
                   y=prop,
                   fill=var,
                   color=var), 
               alpha=.3) +
      geom_text(aes(x=var,
                    y=zero,
                    label = N),
                nudge_y = 0.02,
                fontface="bold") +
      coord_flip() +
      labs(x="",
           y="",
           title="") +
      scale_fill_manual(values = color_values) +
      scale_color_manual(values = color_values) +
      theme_minimal() + 
      scale_y_continuous(limits = c(0, limit_max)) +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, face="bold"),
            axis.text.y = element_text(face="bold", color="black", size=14),
            strip.text.x = element_text(hjust = strip_text_hjust, size=14, face="bold")) +
      facet_wrap( ~ route_place_label, ncol=1) 
    
p

  })
  
}

# App --------------------------------------------------------------------------
shinyApp(ui, server)


