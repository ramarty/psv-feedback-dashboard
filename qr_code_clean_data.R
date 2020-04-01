# Clean Data from QR Codes

library(googlesheets4)
library(forcats)
library(dplyr)

PRETEND_DATA <- TRUE

#sheets_auth(
#  email = gargle::gargle_oauth_email(),
#  path = NULL,
#  scopes = "https://www.googleapis.com/auth/spreadsheets",
#  cache = gargle::gargle_oauth_cache(),
#  use_oob = gargle::gargle_oob_default(),
#  token = NULL
#)

#token <- gargle::token_fetch()

# Load and Append Data ---------------------------------------------------------
# Matatu characteristics. Liscence plate, psv sticker number, route, etc.
matatu_details <- read_sheet("https://docs.google.com/spreadsheets/d/1IrOZ--nsNDYMiipUOeQjdiGY8oyF4MXaU39hXRA-444")

if(PRETEND_DATA){
  
  # General data
  feedback_df <- bind_rows(
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCL 287P"),
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCN 592S"),
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCX 025N"),
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCX 025N"),
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCX 259D"),
    read_sheet("https://docs.google.com/spreadsheets/d/1SIaFHrrUOXqsVtqzApN-u5wqd_7eOE7ZzswRf7mcH_s") %>%
      mutate(licence_plate = "KCP 863M")) %>%
    mutate(source = "qr_code")

  
} else{
  
  # Data
  feedback_df <- bind_rows(
    read_sheet("https://docs.google.com/spreadsheets/d/1zdT9UQGBfw4GO0sgh_tmX7ODj7IfPBPDDM15ytxtM3k/edit#gid=225571172") %>%
      mutate(licence_plate = "KCL 287P"),
    read_sheet("https://docs.google.com/spreadsheets/d/1s4ogLRQmex-EDokR08lkGMmG8U8ZaydD3uZzH1m3-i4/edit#gid=54770904") %>%
      mutate(licence_plate = "KCN 592S"),
    read_sheet("https://docs.google.com/spreadsheets/d/1s9NlSZALlXZgDoX6tny0YQWlj5PEeKNWy47WfWMIjq8/edit#gid=1991415175") %>%
      mutate(licence_plate = "KCX 025N"),
    read_sheet("https://docs.google.com/spreadsheets/d/1etKUdclDKurADv58lkNyXAwTsTgv2aJGLKPC4-HE4Q8/edit#gid=990515000") %>%
      mutate(licence_plate = "KCX 259D"),
    read_sheet("https://docs.google.com/spreadsheets/d/1_OZk-CNTtv5s4KIglNw7-aVzs5KpCsAPem1x1ahdmZE/edit#gid=575564100") %>%
      mutate(licence_plate = "KCP 863M")

  ) %>%
    mutate(source = "qr_code")
  
}

feedback_df <- merge(feedback_df, matatu_details, by = "licence_plate")

# Restrict to after pilot began; anything before is testing survey
#feedback_df <- feedback_df[feedback_df$Timestamp >= "2020-03-31",]

# Clean variables --------------------------------------------------------------

#### Add attribute of original variable name
for(var in names(feedback_df)){
  attr(feedback_df[[var]], "name_full") <- var
}

#### Rename
feedback_df <- feedback_df %>%
  dplyr::rename("driver_safety_rating" = "How would you rate your Matatu driver?",
                "driver_speed" = "How would you describe your Matatu driver's speed?",
                "passenger_amount" = "On the Matatu, are there",
                "covid19_measures" = "Were measures taken to prevent the spread of COVID-19? (e.g, limiting passengers or providing sanitary wipes?)",
                "amenity_importance" = "How important is it for a Matatu to have amenities like Music and WIFI?",
                "comments" = "Leave any comments about the matatu or ride. Report unsafe driving or compliment the driver.")

#### Factor variables
feedback_df$driver_safety_rating <- feedback_df$driver_safety_rating %>% 
  factor(levels = c("Very safe",
                    "Safe",
                    "Unsafe",
                    "Very unsafe")) %>%
  fct_rev() # reverse order for plotting later

feedback_df$driver_speed[feedback_df$driver_speed %in% "Too slow (could have gone faster)"] <- "Too slow"
feedback_df$driver_speed <- feedback_df$driver_speed %>% 
  factor(levels = c("Too slow",
                    "Okay",
                    "Fast",
                    "Dangerously Fast")) %>%
  fct_rev() # reverse order for plotting later

feedback_df$passenger_amount <- feedback_df$passenger_amount %>% 
  factor(levels = c("Less people than seats",
                    "Same number of people as seats",
                    "More people than seats",
                    "More people than can fit")) %>%
  fct_rev() # reverse order for plotting later

feedback_df$covid19_measures <- feedback_df$covid19_measures %>% 
  factor(levels = c("Yes, effective",
                    "Yes, but seemed limited",
                    "No")) %>%
  fct_rev() # reverse order for plotting later

# Create additional variables --------------------------------------------------
feedback_df$route <- paste0(feedback_df$location_1, " <--> ",
                            feedback_df$location_2)

feedback_df$route_place_label <- paste0(feedback_df$route, "\n",
                                        feedback_df$licence_plate)

# Export -----------------------------------------------------------------------
saveRDS(feedback_df, "/Users/robmarty/Documents/Github/psv-feedback-dashboard/qr_code_data.Rds")





