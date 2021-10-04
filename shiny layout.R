library(shiny)

fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "Observations:",
                  min = 0,
                  max = 1000,
                  value = 500),
      mainPanel(
        plotOutput("displot")
      )
    )
  )
)


ui <- fluidPage(
  titlePanel("Central limit theorem"),
  sidebarLayout(
    sidebarPanel(
        numericInput("m","Number of samples:",2,min=1,max=100)
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input,output,session){
  output$hist <- renderPlot({
    means <- replicate(1e4,
                       mean(runif(input$m)))
    hist(means,breaks=20)
  },
  res = 96)
}

shinyApp(ui,server)


###
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Import data",
             fileInput("file","Data",buttonLabel = "Unload..."),
             textInput("delim","Delimiter (leave blank to guess)",""),
             numericInput("skip","Rows to skip",0,min = 0),
             numericInput("rows","Rows to preview",10,min = 1)
             ),
    tabPanel("Set parameters"),
    tabPanel("Visualize results")
  )
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textOutput("panel")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("panel 1", "one"),
        tabPanel("panel 2", "two"),
        tabPanel("panel 3", "three")
      )
    )
  )
)
server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
}
