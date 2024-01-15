


ui <- fluidPage(
  titlePanel("Pimco Closed End Fund Analyzer"),

  sidebarLayout(
    sidebarPanel(width=2,
      actionButton("submit", label = "Submit"),
      selectInput("graphChosen", "Graph", graphChoices),
      checkboxInput("useFacet", "Facet plot", FALSE),
      # prettyCheckboxGroup("chosenTickers", 
      #                    label="Choose Funds.", 
      #                     choices=cefTickers,
      #                     inline=TRUE,
      #                    plain=T
      #                    )
      prettyCheckboxGroup("muniT", 
                          label="Municipal", 
                          choices=cefMuni,
                          inline=TRUE,
                          plain=T
      ),
      prettyCheckboxGroup("muni2T", 
                          label="CA NY Muni", 
                          choices=cefMuni2,
                          inline=TRUE,
                          plain=T
      ),
      prettyCheckboxGroup("corpT", 
                          label="Corporate", 
                          choices=cefCorp,
                          inline=TRUE,
                          plain=T
      ),
      prettyCheckboxGroup("mortT", 
                          label="Mortgage", 
                          choices=cefMort,
                          selected = (c("PDI","PDO")),
                          inline=TRUE,
                          plain=T
      ),
      prettyCheckboxGroup("miscT", 
                          label="Misc", 
                          choices=cefMisc,
                          selected = (c("PGP")),
                          inline=TRUE,
                          plain=T
      )
    ),
    
    mainPanel(width=10, ncol=3,
      plotOutput("distPlot")
    )
  )
)
