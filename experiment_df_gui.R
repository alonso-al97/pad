getwd()

#install.packages('shiny')
#install.packages("shiny",dependencies = TRUE)
library(shiny)

df = read.csv('measures-2000-2005.csv')
df
vars <- setdiff(names(df), "SeasonTarget")
vars <- setdiff(vars, "X")
print(vars)


ui <- pageWithSidebar(
  headerPanel('KNN para medidas de Aerosoles'),
  sidebarPanel(
    selectInput('xcol', 'Variable X', vars, selected = vars[[1]]),
    selectInput('ycol', 'Variable Y', vars, selected = vars[[2]]),
    numericInput('clusters', 'Centroides (K)', 4, min = 1, max = 10)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    df[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                       "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
                       
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
