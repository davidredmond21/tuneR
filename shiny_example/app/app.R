library(shiny)
library(tuner2)
# Define UI
ui <- shinyUI(fluidPage(

  titlePanel(" Vibration analysis of NASA Datasets " ),
  checkboxInput(inputId = "regen",
                label = strong("regenerate summary statistics"),
                value = FALSE),
  selectInput("n_set",
              "Datasets selection",
              choices = c("1st_test", "2nd_test"),
              selected = "1st_test"),
  selectInput("n_chart",
              "Feature to plot",
               choices = c("RMS","Kurtosis","Skewness","Crest", "Entropy","Mahalanobis_ts","Mahalanobis_iid"),
              selected = "Kurtosis"),

  # Display this only if the summary regen is needed

  # Show a plot
  mainPanel(
    plotOutput(outputId = "main_plot", width="auto", height="400px")),

conditionalPanel(condition = "input.regen == true",
                   sliderInput("s_size",
                               "Number of measurement samples",
                               min=3,max = 200,
                               value = 10))
))
# Define server
server <- shinyServer(function(input, output,session) {
  output$main_plot <- renderPlot({
    if(!input$regen) {
      Result      = load_tunr(input$n_set,input$s_size)
      out_tbl     = eda_tunr(Result)}
    plot_tunr(input$n_chart,input$n_set)
  })
})
# Run the application
shinyApp(ui = ui, server = server)


