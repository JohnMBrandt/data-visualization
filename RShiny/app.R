library(shiny)
library(ggvis)

data <- mtcars
colnames(data) <- c("MPG", "Number_cylinders", "Displacement", "Horsepower", "Rear_axle_ratio",
                    "Weight_1000_lb", "Quarter_m_time", "VS", "Transmission", "Forward_gears", "Carburetors")

data$Number_cylinders <- factor(data$Number_cylinders)
data$Transmission <- factor(data$Transmission)
data$Forward_gears <- factor(data$Forward_gears)
data$Carburetors <- factor(data$Forward_gears)

axis_vars <- c("Displacement", "Horsepower", "Rear_axle_ratio", "Weight_1000_lb",
               "Quarter_m_time", "VS")

factor_vars <- c("Number_cylinders", "Transmission", "Forward_gears", "Carburetors")

data <- tibble::rownames_to_column(data, var="Model")

# Define UI ----
ui <- fluidPage(
  titlePanel("Yale NUS"),
  
  fluidRow(
    column(3,
           helpText("Explore relationships between fuel efficiency and vehicle properties"),
           #selectInput("xvar", "X-axis variable", axis_vars, selected = "MPG"),
           selectInput("yvar", "Y-axis variable", axis_vars, selected = "Horsepower"),
           selectInput("loess", "Linear model variable", factor_vars, selected = "Number_cylinders"),
           selectInput("size", "Sizing variable", axis_vars, selected = "Weight_1000_lb"),
           sliderInput("range", label= "Horsepower:",
                       min = 0, max = 350, value = c(0,350))
    ),
    column(9,
           ggvisOutput("plot1")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  car_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    # Pick out the car with this model
    column <- yvar$value
    if(is.null(x[[column]])) return(NULL)
    car <- data[data[[column]] == x[[column]] & data$MPG == x$MPG,]
    paste0("<b>", as.character(car$Model), "</b><br>")
  }
  
  vis <- reactive({
    # Labels for axes
    #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    sizevar <- prop("size", as.symbol(input$size))
    #xvar <<- prop("x", as.symbol(data$MPG))
    yvar <<- prop("y", as.symbol(input$yvar))
    factorvar <<- prop("factor", as.symbol(input$loess))
    
    data %>%
      group_by(data[[factorvar$value]]) %>%
      ggvis(~MPG, yvar) %>%
      layer_model_predictions(stroke= factorvar$value, model="lm") %>%
      layer_points(size = sizevar, fill = factorvar$value) %>%
      add_tooltip(car_tooltip, on="hover") %>%
      add_axis("x", title = "MPG") %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 1000, height = 700) %>%
      hide_legend("size") %>%
      add_legend("stroke")
  })
  
  vis %>% bind_shiny("plot1")
}


# Run the app ----
shinyApp(ui = ui, server = server, options = list(height= 700))