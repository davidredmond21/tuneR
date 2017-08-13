library(shiny)
library(tuner1)
# Define UI
ui <- shinyUI(fluidPage(
   
   titlePanel(" Vibration analysis of NASA Datasets " ),
   
   sliderInput("s_size",
                  "Number of measurement samples",
                  min=3,
                  max = 100,
                  value = 10
                  ),
   selectInput("n_set",
               "Datasets selection",
               choices = c("1st_test", "2nd_test"),
               selected = "2nd_test"
               ),
   selectInput("n_chart",
               "Feature to plot",
               choices = c("RMS","Kurtosis","Skewness","Crest", "Entropy","mahalanobis"),
               selected = "Kurtosis"
               ),
   # Show a plot of the generated distribution
   mainPanel(
   plotOutput(outputId = "main_plot")
   )
))
# Define server
server <- shinyServer(function(input, output,session) {
   output$main_plot <- renderPlot({
      Result      = load_tunr(input$n_set,input$s_size) 
      out_tbl  = eda_tunr(Result)
      plot_tunr(input$n_chart,out_tbl) 
   })
})
# Run the application
shinyApp(ui = ui, server = server)


