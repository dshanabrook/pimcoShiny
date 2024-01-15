chosenT <- c( "PAXS")
dataP <- getPimcoData(chosenT)
#check yieldNII
write.csv(dataP,"dataP.csv")
ggplot(dataP)+
geom_line(aes(x=date, 
              y=yieldNII, 
              group=Ticker, 
              colour=dataP$Ticker)) +
 # {if(input$useFacet) facet_grid(~Ticker)} +      
 # {if(input$useFacet) guides(colour=F)} +
  scale_y_continuous(limits= c(0,0.3),labels = function(x) paste0(x * 100, '%'))+ ggtitle("1 Month Yield using NII change")

ggplot(dataP,
       aes(y=yieldNII,
           x=discount, 
           label=paste(Ticker,date),
           colour= date))+
  geom_point() +
  geom_label_repel() +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = function(x) paste0(x*100, '%')) +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_gradientn(colors = c("red","green","blue","violet")) +
  ggtitle("Discount vs Yield")


  ggplot(dataP,
         aes(y=yieldNII, 
             x=discount, label=Ticker))+
    geom_point() +
    geom_label_repel() +
    geom_hline(yintercept=1) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) + 
    scale_x_continuous(labels = function(x) paste0(x, '%')) + 
    ggtitle("Discount vs Yield")


ggplot(dataP,
       aes(y=NAVdistribution, 
           x=discount, label=Ticker))
