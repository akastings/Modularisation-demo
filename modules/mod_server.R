
boxOutServer <- function(id,data, xaxis, yaxis, fill,title){
  moduleServer(
    id,
    function(input, output, session){
      output$boxplot <- renderPlot({
        ggplot(data,aes_string(x=as.name(xaxis),y= as.name(yaxis),fill = as.name(fill)))+
          geom_bar(position="dodge",stat = "identity")+
          ggtitle(label = title)+
          theme(plot.title = element_text(hjust = 0.5))+
          theme(plot.subtitle = element_text(hjust = 0.5))
      })
    }
  )
}