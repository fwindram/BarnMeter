library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# https://rstudio.github.io/shinydashboard for a possible new approach

source("funcs/interpolation.R")
source("funcs/graph_and_predict.R")
source("funcs/data_generation.R")
source("funcs/calc_change_cols.R")

version <- "0.1.2"
sterling <- scales::dollar_format(prefix = "£")    # Define formatter for Sterling

generated_readings <- generate_data('2019/01/01', '2020/12/31', 27505, 30)
generated_readings <- interpolate(generated_readings)
generated_readings$change <- calc_change(generated_readings$measurement)
generated_readings$change_residuals <- calc_change_residiuals(generated_readings$change)
generated_readings_cal_heatmap <- generate_cal_heatmap_df(generated_readings)

measured_readings <- read.csv('../Data/readings.csv')
measured_readings$date <- as.Date(measured_readings$date, format = "%d/%m/%Y")
measured_readings <- interpolate(measured_readings)
measured_readings$change <- calc_change(measured_readings$measurement)
measured_readings$change_residuals <- calc_change_residiuals(measured_readings$change)
measured_readings_cal_heatmap <- generate_cal_heatmap_df(measured_readings)

ui <- fluidPage(
    titlePanel(paste0("BarnMeter v", version)),
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            # sliderInput(inputId = "bins",
            #             label = "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            radioButtons(inputId = "data_source",
                         label = "Data source:",
                         choices = c("Measured" = "measured",
                                     "Generated" = "generated"),
                         inline = FALSE),
            
            sliderInput("predict_n", "Predicted days",
                        value = as.integer(nrow(measured_readings)/2),
                        min = 0,
                        max = 365
                        )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Usage", plotOutput("usage_plot")),
                        tabPanel("Cost", plotOutput("cost_plot")),
                        tabPanel("Usage Residuals", plotOutput("usage_resids_plot")),
                        tabPanel("Calendar Heatmap", plotOutput("cal_heatmap"))
            )
            # Output: Histogram ----
            
        )
    )
)

server <- function(input, output) {
    readings <- reactive({
        switch(input$data_source,
               "measured" = measured_readings,
               "generated" = generated_readings)
        })
    
    readings_cal_heatmap <- reactive({
        switch(input$data_source,
               "measured" = measured_readings_cal_heatmap,
               "generated" = generated_readings_cal_heatmap)
    })
    
    output$usage_plot <- renderPlot({
        plot_usage(readings(), input$predict_n)
    })
    output$cost_plot <- renderPlot({
        plot_usage(readings(), input$predict_n, as.cost = TRUE, ppu = 0.14)
    })
    output$usage_resids_plot <- renderPlot({
        plot_residual_change(readings())
    })
    output$cal_heatmap <- renderPlot({
        plot_cal_heatmap(readings_cal_heatmap())
    })
}

shinyApp(ui = ui, server = server)
