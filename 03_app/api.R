library(shiny)
library(shinyWidgets)
library(htmltools)
library(glue)
library(httr)
library(jsonlite)
library(xgboost)
library(tidymodels)
library(tidyverse)

# Load model object, preprocessing steps, and data ranges
model <- readRDS("model.rds")
recipe <- readRDS("preprocessing_recipe.rds")
data_values <- readRDS("data_values.rds")

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("n_beds",
                    "# of bedrooms",
                    value = 2,
                    min = 0,
                    max = 10,
                    step = 1),
        sliderInput("n_baths",
                    "# of bathrooms",
                    value = 2,
                    min = 0,
                    max = 16,
                    step = 1),
        pickerInput("home_type",
                    "Home type",
                    options = pickerOptions(
                      maxOptions = 1
                    ),
                    choices = data_values$listing_type,
                    selected = "condo",
                    multiple = TRUE),
        pickerInput("town",
                    "Town/City",
                    options = pickerOptions(
                      liveSearch = TRUE,
                      maxOptions = 1
                    ),
                    choices = data_values$locality,
                    selected = "Toronto",
                    multiple = TRUE),
        pickerInput("postal_code",
                    "Postal Code",
                    options = pickerOptions(
                      liveSearch = TRUE,
                      maxOptions = 1
                    ),
                    choices = data_values$postal_code,
                    selected = "M5S",
                    multiple = TRUE),
        numericInput("longitude", 
                     "Longitude",
                     value = -79.396090,
                     min = data_values$longitude[1],
                     max = data_values$longitude[2]),
        numericInput("latitude", 
                     "Latitude",
                     value = 43.660945,
                     min = data_values$latitude[1],
                     max = data_values$latitude[2])
      ),
      mainPanel(
        h1("The predicted house price is: ",
           textOutput("predicted_price"))
      )
    )
  )
}

server <- function(input, output, session) {
  
  input_vector <- reactive(
    {
      need(input$home_type, "Need input data")
      need(input$town, "Need input data")
      need(input$latitude, "Need input data")
      need(input$longitude, "Need input data")
      need(input$postal_code, "Need input data")
      input_data <- data.frame(
        n_beds = input$n_beds,
        n_baths = input$n_baths,
        listing_type = input$home_type,
        locality = input$town,
        latitude = input$latitude,
        longitude = input$longitude,
        postal_code_abb = input$postal_code
      )
      
      prep(recipe, new_data = input_data) %>%
        bake(new_data = input_data) %>%
        data.matrix()
    }
  )
  
  # Generate prediction
  
  output$predicted_price <- renderText(
    {
      need(input$home_type, "Need input data")
      need(input$town, "Need input data")
      need(input$latitude, "Need input data")
      need(input$longitude, "Need input data")
      need(input$postal_code, "Need input data")
      predict(model, newdata = input_vector())
    }
  )
  
  # Automatically bookmark every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
  
}

shinyApp(ui, server, enableBookmarking = "url")