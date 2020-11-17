library(shiny)
library(shinyWidgets)
library(htmltools)
library(glue)
library(httr)
library(jsonlite)
library(xgboost)
library(tidymodels)
library(tidyverse)

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("n_beds",
                    "# of bedrooms",
                    value = 2,
                    min = NULL,
                    max = NULL,
                    step = 1),
        sliderInput("n_baths",
                    "# of bathrooms",
                    value = 2,
                    min = NULL,
                    max = NULL,
                    step = 1),
        textInput("address",
                  label = "Street address",
                  placeholder = "Eg. 27 King's College Circle"),
        pickerInput("home_type",
                    "Home type",
                    options = pickerOptions(
                      maxOptions = 1
                    ),
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        pickerInput("town",
                    "Town/City",
                    options = pickerOptions(
                      liveSearch = TRUE,
                      maxOptions = 1
                    ),
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        actionButton("get_geocode",
                     label = "Get postal code/longitude/latitude)"),
        pickerInput("postal_code",
                    "Postal Code",
                    options = pickerOptions(
                      liveSearch = TRUE,
                      maxOptions = 1
                    ),
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        numericInput("longitude", 
                     "Longitude",
                     value = NULL),
        numericInput("latitude", 
                     "Latitude",
                     value = NULL),
        actionButton("get_prediction",
                     label = "Predict Price")
      ),
      mainPanel(
        h1("The predicted house price is: ",
           textOutput("predicted_price"))
      )
    )
  )
}

server <- function(input, output, session) {
  
  model <- readRDS("model_fit.rds")
  recipe <- readRDS("preprocessing_recipe.rds")
  data_values <- readRDS("data_values.rds")
  
  # Update input value ranges based on training data
  observe({
    updateSliderInput(
      session,
      inputId = "n_beds",
      min = data_values$n_beds[1],
      max = data_values$n_beds[2],
      value = 2
    )
    
    updateSliderInput(
      session,
      inputId = "n_baths",
      min = data_values$n_baths[1],
      max = data_values$n_baths[2],
      value = 2
    )
    
    updatePickerInput(
      session,
      inputId = "town",
      choices = sort(data_values$locality),
      selected = NULL
    )
    
    updatePickerInput(
      session,
      inputId = "home_type",
      selected = NULL,
      choices = sort(data_values$listing_type)
    )
    
    updatePickerInput(
      session,
      inputId = "postal_code",
      selected = NULL,
      choices = sort(data_values$postal_code)
    )
    
    updateNumericInput(
      session,
      inputId = "longitude",
      min = data_values$longitude[1],
      max = data_values$longitude[2]
    )
    
    updateNumericInput(
      session,
      inputId = "latitude",
      min = data_values$latitude[1],
      max = data_values$latitude[2]
    )
    
  })
  
  
  
  # Generate prediction
  observeEvent(input$get_geocode,{
    # browser()
    # Get geocoding data from Geocoder API based on inputted address
    if (!is.null(input$address)){
      # Format address string
      address <- str_replace_all(input$address, " ", "%20")
      
      town <- input$town
      
      # Fetch response from API
      res <- GET(url = glue(
        "https://geocoder.ca/?locate={address}%20{town}&json=1"
      ))
      
      content <- content(res)
      
      # Update postal code
      updatePickerInput(
        session,
        inputId = "postal_code",
        selected = substr(content$postal, 1, 3)
      )
      
      # Update longitude and latitude
      updateNumericInput(
        session,
        inputId = "longitude",
        value = as.numeric(content$longt)
      )
      
      updateNumericInput(
        session,
        inputId = "latitude",
        value = as.numeric(content$latt)
      )
    }
    
  })
  
  observeEvent(input$get_prediction, {
    
    output$predicted_price <- renderText(
      {
        need(input$longitude, "Need longitude")
        need(input$latitude, "Need latitude")
        
        input_data <- isolate(
          data.frame(
            n_beds = input$n_beds,
            n_baths = input$n_baths,
            listing_type = input$home_type,
            locality = input$town,
            latitude = input$latitude,
            longitude = input$longitude,
            postal_code_abb = input$postal_code
          ))
        
        input_vector <- prep(recipe, new_data = input_data) %>%
          bake(new_data = input_data) %>%
          data.matrix()

        predict(model, newdata = input_vector)
        
      }
    )
    
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")