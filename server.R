server <- function(input, output, session) {
  
  chosenT <- eventReactive(input$submit, c(input$muniT,input$muni2T,input$corpT,input$miscT,input$mortT))
  dataP <- reactive(getPimcoData(chosenT()))
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
      labs(title="UNII extra months") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    
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
      labs(title="Div Coverage based on NII")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
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
      ggtitle("Rolling3Mon")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
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
      ggtitle("Rolling6Mon")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
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
      ggtitle("NII")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
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
      scale_y_continuous(labels = function(x) paste0(x * 100, '%'))+ 
      ggtitle("1 Month Yield using NII change")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
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
      ggtitle("Yield based on distribution and current price")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  })
#compare the discount and yield as calculated from actual income
  discount <- reactive({
    ggplot(mostRecentDataP(),
           aes(y=yieldNII, 
               x=discount, label=Ticker))+
      geom_point() +
      geom_label_repel() +
      scale_y_continuous(labels = function(x) paste0(x*100, '%')) + 
      scale_x_continuous(labels = function(x) paste0(x, '%')) + 
      ggtitle("How much overpaying to get yield calculated using NII")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  })
  discountReported <- reactive({
    ggplot(mostRecentDataP(),
           aes(y=NAVdistribution, 
               x=discount, label=Ticker))+
      geom_point() +
      geom_label_repel() +
      geom_hline(yintercept=1) +
      scale_y_continuous(labels = function(x) paste0(x, '%')) + 
      scale_x_continuous(labels = function(x) paste0(x, '%')) + 
      ggtitle("How much overpaying to get yield, calculated using dividend")  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  })  

  # discount_NII <- reactive({
  #   ggplot(dataP(),
  #          aes(y=yieldNII,
  #              x=discount, 
  #              label=paste(Ticker,date),
  #              colour= date))+
  #     geom_point() +
  #     geom_label_repel() +
  #     geom_hline(yintercept=0) +
  #     scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  #     scale_x_continuous(labels = function(x) paste0(x, '%')) +
  #     scale_colour_gradientn(colors = c("red","blue")) +
  #     ggtitle("Discount vs Yield")
  # })  
  # discount_yield <- reactive({
  #   ggplot(dataP(),
  #          aes(y=yield,
  #              x=discount, 
  #              label=paste(Ticker,date),
  #              colour= date))+
  #     geom_point() +
  #     geom_label_repel() +
  #     geom_hline(yintercept=0) +
  #     scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  #     scale_x_continuous(labels = function(x) paste0(x, '%')) +
  #     scale_colour_gradientn(colors = c("red","blue")) +
  #     ggtitle("Discount vs Yield")
  # })
  graphInput <- reactive({
    switch(input$graphChosen,
           "Yield" = yield(),
           "NII yield" = yieldNII(),
           "NII" = NII(),
           "roll6" = roll6(),
           "roll3" = roll3(),
           "roll1" = roll1(),
           "UNII" = UNII(),
           "discount/NII" = discount(),
           "discount/yield" =discountReported()
    )
  })
  
  output$distPlot <- renderPlot({ graphInput() })
}