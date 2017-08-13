library(shiny)
library(tuner1)
# Define UI
ui <- shinyUI(fluidPage(
         headerPanel('Vibration analysis of NASA Datasets'),
         sidebarPanel(
            selectInput(inputId = "n_set",
                        label = "Datasets selection",
                        choices = c("1st_test", "2nd_test"),
                        selected = "1st_test"),    
            selectInput(inputId = "n_chart",
                        label = "Feature to plot",
                        choices = c("rms","kurtosis","skewness","crest", "entropy","mahalanobis"),
                        selected = "kurtosis"),          
            numericInput(inputId = "s_size",
                  label = "Number of measurement samples",
                  choices = c(15, 30, 50, 100, -1),
                  selected = 20)
         ),
          mainPanel(
             plotOutput("main_plot", height = "300px"))
   )
)
# Define server
server <- shinyServer(function(input, output,session) {
   dataset = as.character(input$n_set) 
   feature = as.character(input$n_chart) 
   sample_size = input$s_size
   obj      = reactive({ load_tunr(type=dataset,sample_size)  })
   out_tbl  = reactive({ eda_tunr(obj=obj)   })
   output$main_plot <- renderPlot({   
      plot_tunr(x_type=feature,out_tbl=out_tbl) })
})

# Run the application
shinyApp(ui = ui, server = server)


