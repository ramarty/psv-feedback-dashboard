# PSV Feedback Page

# RESOURCES:
# https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
# https://gargle.r-lib.org/articles/non-interactive-auth.html
# https://github.com/jennybc/googlesheets/tree/master/inst/shiny-examples

# setwd("/Users/robmarty/Documents/Github/psv-feedback-dashboard")

library(dplyr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(plotly)
library(lubridate)
library(shinydashboard)
library(tableHTML)
library(formattable)




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
  

    tabPanel("By Question",
             #div(class="outer",
                 
                fluidRow(
                  column(3,
                         ""),
                  column(6, align="center",
                         h5("**Data is currently made up and will be replaced with actual data once the project launches**"),
                        h4( "In select PSVs riders can rate the PSV along various categories.
                         We're currently working with 5 PSVs, but will be growing! As we do, we'll add to this site. Check back again soon!")
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
                  formattableOutput(outputId = "quetion_fig",
                                 height = "700px")
                )
                ),
                hr(),
                #fluidRow(
                #  column(3,
                #         ),
                #  column(6, align="center",
                #         "We're currently working with 5 PSVs, but will be growing! As we do, we'll add to this site -- showing ratings by sacco, PSVs within saccos, etc. Check back again soon!"
                #  )
                #),
                fluidRow(
                  column(12, align="center",
                         div(img(src='logos.png',style="margin-top: 14px; padding-right:10px;padding-bottom:10px", height = 100)),
                         )
                ),
                fluidRow(
                  column(3,
                         ""),
                  column(6, align="center",
                         textOutput("last_updated_1")
                  )
                )
              )
            #)
          ),


tabPanel("By PSV",
         div(class="outer",
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
                      textOutput("last_updated_2")
               )
             )
         )
        ),

tabPanel("By Sacco",
         div(class="outer",
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
                      textOutput("last_updated_3")
               )
             )
         )
),

tabPanel("About",
         div(class="outer",
             fluidRow(
               column(3,
                      ""),
               column(6, align="center",
                      h2("Let's reward safety!"),
                      h4("PSV Safety Awards is working to crowdsource information on PSVs. Why? To help everyone find safe PSVs.")
               )
             ),
             #fluidRow(
            #   column(3,
            #          ""),
            #   column(6, align="center",
            #          HTML(
            #            '<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSf2S3Vp8tlZeWwuPiT6E_KLkjt2O79A_wkPUpPauv7hbXc9EA/viewform?embedded=true" width="320" height="800" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>'
            #            
            #            #'<iframe src="https://docs.google.com/forms/d/e/1FAIpQLSf2S3Vp8tlZeWwuPiT6E_KLkjt2O79A_wkPUpPauv7hbXc9EA/viewform?embedded=true" width="640" height="450" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>'
            #          )
            #   )
            # ),
             fluidRow(
               column(12, align="center",
                      div(img(src='logos.png',style="margin-top: 14px; padding-right:10px;padding-bottom:10px", height = 100)),
               )
             ),
             fluidRow(
               column(3,
                      ""),
               column(6, align="center",
                      textOutput("last_updated_4")
               )
             )
         )
        )
)
)

                      
# Server -----------------------------------------------------------------------
server <- function(input, output) {
  
  # ** Load and Prep Data ------------------------------------------------------
  qr_data <- readRDS("qr_code_data.Rds")
  
  output$last_updated_1 <- renderText({
    t <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character()
    paste0("Last Updated: ", t)
  })
  
  output$last_updated_2 <- renderText({
    t <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character()
    paste0("Last Updated: ", t)
  })
  
  output$last_updated_3 <- renderText({
    t <- Sys.time() %>% with_tz(tzone = "Africa/Nairobi") %>% as.character()
    paste0("Last Updated: ", t)
  })
  
  output$last_updated_4 <- renderText({
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
  

  output$quetion_fig <- renderFormattable({
    
    
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
      group_by(var, route, licence_plate, route_place_label) %>%
      summarise(N = n()) %>%
      ungroup() %>%
      
      # Complete
      complete(var, 
               route_place_label,
               fill = list(N = 0)) %>%

      # Pivot
      pivot_wider(id_cols=c(route, licence_plate),
                  names_from = var,
                  values_from = N) %>%
      
      # Rename
      dplyr::rename(Route = route,
                    `Plate` = licence_plate)
    
    customGreen = "#71CA97"
    customGreen0 = "#DeF7E9"
    customRed = "#ff7f7f"
    customRed0 = "#FA614B66"
    customGreen0 = "#DeF7E9"
    customYellow = "#F6E655"
    
    calc_width <- function(x, max_value){
      x/max_value
    }
    
    fixedWidth = 150
    formatter_to_use <- function(color){
      formatter("span",
                style = x ~ style(
                  display = "inline-block",
                  direction = "lft",
                  font.weight = "bold",
                  "border-radius" = "4px",
                  "padding-left" = "2px",
                  "background-color" = csscolor(color),
                  #width = percent(proportion(x)),
                  width = paste(fixedWidth*calc_width(x, max_value),"px",sep=""),
                  #width = calc_width(x, max_value),
                  color = csscolor("black")
                ))
    } 
    
    # VAR SPECIFIC
    if(input$question %in% "Overall Safety"){
      
      data_sub <- data_sub %>%
        arrange(desc(`Very safe`))
      
      data_sub <- data_sub[,c("Route",
                              "Plate",
                              "Very safe",
                              "Safe",
                              "Unsafe",
                              "Very unsafe")] 
      
      max_value <- data_sub %>%
        select(-Route, -Plate) %>%
        as.matrix() %>%
        as.numeric() %>%
        max()
      
      table_out <- formattable(data_sub, align=c("l", "l", "l", "l", "l", "l"), list(
        `Route` = formatter("span", style = ~ style(color = "black",                                                   
                                                    font.size = "15px")),
        `Plate` = formatter("span", style = ~ style(color = "black")),
        `Very safe`   = formatter_to_use(customGreen),
        `Safe`        = formatter_to_use(customGreen),
        `Unsafe`      = formatter_to_use(customRed0),
        `Very unsafe` = formatter_to_use(customRed0)
      ))

    }
    
  
    if(input$question %in% "Driving Speed"){
      
      data_sub <- data_sub %>%
        arrange(desc(`Okay`))
      
      data_sub <- data_sub[,c("Route",
                              "Plate",
                              "Too slow",
                              "Okay",
                              "Fast",
                              "Dangerously Fast")]
      
      max_value <- data_sub %>%
        select(-Route, -Plate) %>%
        as.matrix() %>%
        as.numeric() %>%
        max()
      
      table_out <- formattable(data_sub, align=c("l", "l", "l", "l", "l", "l"), list(
        `Route` = formatter("span", style = ~ style(color = "black",                                                   
                                                    font.size = "15px")),
        `Plate` = formatter("span", style = ~ style(color = "black")),
        `Too slow`         = formatter_to_use(customYellow),
        `Okay`             = formatter_to_use(customGreen),
        `Fast`             = formatter_to_use(customRed0),
        `Dangerously Fast` = formatter_to_use(customRed0)
      ))
    }
    
    if(input$question %in% "Number of Passengers"){
      

      
      data_sub <- data_sub %>%
        arrange(desc(`Less people than seats`))
      
      data_sub <- data_sub[,c("Route",
                              "Plate",
                              "Less people than seats",
                              "Same number of people as seats",
                              "More people than seats",
                              "More people than can fit")]
      
      max_value <- data_sub %>%
        select(-Route, -Plate) %>%
        as.matrix() %>%
        as.numeric() %>%
        max()
      
      table_out <- formattable(data_sub, align=c("l", "l", "l", "l", "l", "l"), list(
        `Route` = formatter("span", style = ~ style(color = "black",                                                   
                                                    font.size = "15px")),
        `Plate` = formatter("span", style = ~ style(color = "black")),
        `Less people than seats`         = formatter_to_use(customGreen),
        `Same number of people as seats` = formatter_to_use(customGreen),
        `More people than seats`         = formatter_to_use(customRed0),
        `More people than can fit`       = formatter_to_use(customRed0)
      ))
    }
    
    
    if(input$question %in% "Measures for COVID-19"){

      data_sub <- data_sub %>%
        arrange(desc(`Yes, effective`))
      
      data_sub <- data_sub[,c("Route",
                              "Plate",
                              "Yes, effective",
                              "Yes, but seemed limited",
                              "No")]
      
      max_value <- data_sub %>%
        select(-Route, -Plate) %>%
        as.matrix() %>%
        as.numeric() %>%
        max()
      
      table_out <- formattable(data_sub, align=c("l", "l", "l", "l", "l", "l"), list(
        `Route` = formatter("span", style = ~ style(color = "black",                                                   
                                                    font.size = "15px")),
        `Plate` = formatter("span", style = ~ style(color = "black")),
        `Yes, effective`         = formatter_to_use(customGreen),
        `Yes, but seemed limited` = formatter_to_use(customGreen),
        `No`       = formatter_to_use(customRed0)
      ))
    }
    
    
    table_out
    

    



  })
  
}

# App --------------------------------------------------------------------------
shinyApp(ui, server)


