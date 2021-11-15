


ui <- fluidPage(
  titlePanel("Pimco Closed End Fund Analyzer"),

  sidebarLayout(
    sidebarPanel(width=2,
      actionButton("submit", label = "Submit"),
      selectInput("graphChosen", "Graph", graphChoices),
      checkboxInput("useFacet", "Facet plot", FALSE),
      prettyCheckboxGroup("chosenTickers", 
                         label="Choose Funds.", 
                          choices=cefTickers,
                          inline=TRUE,
                         plain=T
                         )
    ),
    
    mainPanel(width = 7,
      plotOutput("distPlot")
    )
  )
)
