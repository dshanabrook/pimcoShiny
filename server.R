server <- function(input, output, session) {
  
  dataP <- eventReactive(input$submit, {getPimcoData(input$chosenTickers) })
  reactive(print(chosenTickers()))

  UNII <- reactive({
    ggplot(dataP()) + 
      geom_line(aes(x=date, 
                    y=UNII/MonthlyDistribution, 
                    group=(Ticker),
                    colour=dataP()$Ticker)) +
      {if(input$useFacet) facet_wrap(~Ticker) }+
      labs(title="UNII extra months") 
    
  })
  
  #rolling 1 month from NII div coverage
  roll1 <- reactive({
    ggplot(dataP()) + 
      geom_line(aes(x=date, 
                    y=Rolling1Mon, 
                    group=(Ticker),
                    colour=dataP()$Ticker)) +  
      {if(input$useFacet) facet_wrap(~Ticker) }+
      labs(title="Div Coverage based on NII")
  })
  
  
  roll3 <- reactive({
    ggplot(dataP()) +
      geom_line(aes(x=date, 
                    y=Rolling3Mon, 
                    group=Ticker,
                colour=dataP()$Ticker))+
      {if(input$useFacet) facet_wrap(~Ticker) } +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ 
      ggtitle("Rolling3Mon")
  })
  
  roll6 <- reactive({
    ggplot(dataP()) +
      geom_line(aes(x=date, 
                    y=Rolling6Mon, 
                    group=Ticker,
                colour=dataP()$Ticker))+
      {if(input$useFacet) facet_wrap(~Ticker) } +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ 
      ggtitle("Rolling6Mon")
  })
  
  #NII
  NII <- reactive({
    ggplot(dataP()) +
      geom_line(aes(x=date, 
                    y=NII, 
                    group=Ticker, 
                colour=dataP()$Ticker)) + 
      {if(input$useFacet) facet_wrap(~Ticker) } +
      ggtitle("NII")
  })
  #yeild based on NII
  yieldNII <- reactive({
    ggplot(dataP())+
      geom_line(aes(x=date, 
                    y=yield1Month, 
                    group=Ticker, 
                    colour=dataP()$Ticker)) +
      {if(input$useFacet) facet_wrap(~Ticker) } +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("1 Month Yield using NII change")
  })
  #Yield based on dividend
  yield <- reactive({
    ggplot(dataP())+
      geom_line(aes(x=date, 
                    y=yield, 
                    group=Ticker, 
                    colour=dataP()$Ticker)) +
      {if(input$useFacet) facet_wrap(~Ticker) } +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%')) + 
      ggtitle("1 Month Yield using NII change")
  })
  discount <- reactive({
    ggplot(dataP())+
      geom_line(aes(x=date, 
                    y=discount, 
                    group=Ticker, 
                    colour=dataP()$Ticker)) +
      {if(input$useFacet) facet_wrap(~Ticker) } +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%')) + 
      ggtitle("Yield Actual")
  })
  
    graphInput <- reactive({
      switch(input$graphChosen,
             "yield" = yield(),
             "yieldNII" = yieldNII(),
             "NII" = NII(),
             "roll6" = roll6(),
             "roll3" = roll3(),
             "roll1" = roll1(),
             "UNII" = UNII(),
              "discount" = discount())
      })
    
    output$distPlot <- renderPlot({ graphInput() })
}