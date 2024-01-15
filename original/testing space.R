dataP <- getPimcoData("PDO")

#check yieldNII
ggplot(dataP)+
geom_line(aes(x=date, 
              y=yieldNII, 
              group=Ticker, 
              colour=dataP$Ticker)) +
 # {if(input$useFacet) facet_grid(~Ticker)} +      
 # {if(input$useFacet) guides(colour=F)} +
  scale_y_continuous(limits= c(0,0.3),labels = function(x) paste0(x * 100, '%'))+ ggtitle("1 Month Yield using NII change")