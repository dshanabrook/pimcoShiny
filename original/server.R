server <- function(input, output, session) {
  
  chosenT <- eventReactive(input$submit, c(input$muniT,input$muni2T,input$corpT,input$miscT,input$mortT))
  dataP <- reactive(getPimcoData(chosenT()))
  #mostRecentDataP <- reactive(dataP()[dataP()$date==max(dataP()$date),])
  mostRecentDataP <- reactive(getMostRecent(dataP()))
  reactive(print(chosenTickers()))
  
  UNII <- reactive({
    ggplot(dataP()) + 
      geom_line(aes(x=date, 
                    y=UNII/MonthlyDistribution, 
                    group=(Ticker),
                    colour=dataP()$Ticker)) +
      geom_hline(yintercept=0) +
      {if(input$useFacet) facet_grid(~Ticker)} +
      {if(input$useFacet) guides(colour=F)} +
      labs(title="UNII extra months") 
    
  })
  
  #rolling 1 month from NII div coverage
  roll1 <- reactive({
    ggplot(dataP(),aes(x=date, 
                     y=Rolling1Mon, 
                     group=(Ticker),
                     colour=dataP()$Ticker)) + 
      geom_line() + 
      geom_point() +
      geom_hline(yintercept=1) +
      {if(input$useFacet) facet_grid(~Ticker)} +       
      {if(input$useFacet) guides(colour=F)} +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ 
      labs(title="Div Coverage based on NII")
  })
  
  
  roll3 <- reactive({
    ggplot(dataP()) +
      geom_line(aes(x=date, 
                    y=Rolling3Mon, 
                    group=Ticker,
                    colour=dataP()$Ticker))+
      geom_hline(yintercept=1) +
      {if(input$useFacet) facet_grid(~Ticker)} +      
      {if(input$useFacet) guides(colour=F)} +      
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ 
      ggtitle("Rolling3Mon")
  })
  
  roll6 <- reactive({
    ggplot(dataP()) +
      geom_line(aes(x=date, 
                    y=Rolling6Mon, 
                    group=Ticker,
                    colour=dataP()$Ticker))+
      geom_hline(yintercept=1) +
      {if(input$useFacet) facet_grid(~Ticker)} +      
      {if(input$useFacet) guides(colour=F)} +
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
      {if(input$useFacet) facet_grid(~Ticker)} +      
      {if(input$useFacet) guides(colour=F)} +
      ggtitle("NII")
  })
  #yeild based on NII
  yieldNII <- reactive({
    ggplot(dataP())+
      geom_line(aes(x=date, 
                    y=yieldNII, 
                    group=Ticker, 
                    colour=dataP()$Ticker)) +
      {if(input$useFacet) facet_grid(~Ticker)} +      
      {if(input$useFacet) guides(colour=F)} +
      #limits= c(-.1,0.25),
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ ggtitle("1 Month Yield using NII change")
  })
  #Yield based on dividend
  yield <- reactive({
    ggplot(dataP())+
      geom_line(aes(x=date, 
                    y=yield, 
                    group=Ticker, 
                    colour=Ticker)) +
      {if(input$useFacet) facet_grid(~Ticker)} +      
      {if(input$useFacet) guides(colour=F)} +
      scale_y_continuous(labels = function(x) paste0(x * 100, '%')) + 
      ggtitle("Yield based on distribution and current price")
  })
  discount <- reactive({
 #   mostRecentDataP <- dataP()[dataP()$date==max(dataP()$date),]
    ggplot(mostRecentDataP(),
           aes(y=yieldNII, 
               x=discount, label=Ticker))+
      geom_point() +
      geom_label_repel() +
      scale_y_continuous(labels = function(x) paste0(x*100, '%')) + 
      scale_x_continuous(labels = function(x) paste0(x, '%')) + 
      ggtitle("Yield based on monthly NII increase")
  })
  discountReported <- reactive({
    #   mostRecentDataP <- dataP()[dataP()$date==max(dataP()$date),]
    ggplot(mostRecentDataP(),
           aes(y=distributionNAV, 
               x=discount, label=Ticker))+
      geom_point() +
      geom_label_repel() +
      geom_hline(yintercept=1) +
      scale_y_continuous(labels = function(x) paste0(x, '%')) + 
      scale_x_continuous(labels = function(x) paste0(x, '%')) + 
      ggtitle("How much overpaying to get yield")
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
           "discount" = discount(),
           "reported" =discountReported()
    )
  })
  
  output$distPlot <- renderPlot({ graphInput() })
}