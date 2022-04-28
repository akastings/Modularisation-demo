
library(shiny)
library(ggplot2)
library(DBI)
library(dplyr)
library(RSQLite)
library(plotly)

mycon <- dbConnect(SQLite(),"../education_assign/data/primary_enrollment_sctypesex.db")

cropprod.data <- dbConnect(SQLite(),"../education_assign/data/crop.prod_db.db")

#dbListTables(cropprod.data)


#dbWriteTable(conn = cropprod.data,"hf.crops.combined", hf.combined)

hortcrop <- dbGetQuery(cropprod.data, statement = "select * from 'hf.crops.combined';")

primsumaries <- dbGetQuery(conn = mycon, statement = "select*from primary_long;")

# Define UI for application that draws a histogram
#ui <- fluidPage(

    # titlePanel("Primary School Data"),
    #    sidebarLayout(
    #       sidebarPanel(
    # 
    # 
    #         sliderInput(inputId = "year", "Per Year", min = 2017, max = 2019, value = 2018)
    #         ),
    #       mainPanel(
    #         plotOutput("lineplot"),
    #         plotOutput("barplot"),
    #         plotlyOutput("school_type")
    # 
    #       )
    #     )
  ui <- navbarPage(
    
    
    "Makueni Dashboard",
                   tabPanel("Education",
                            sliderInput(inputId = "year", "Per Year", min = 2017, max = 2019, value = 2018),
                            plotOutput("lineplot"),
                            plotOutput("barplot"),
                            ),
                   tabPanel("Agriculture", 
                            plotOutput("hort.crops"),
                            
                            tags$head(
                              
                              tags$style(HTML(
                                "
      body {
        background-color: brown;
        #color: white;
           }"))
                            ),
                            ),
    
                   tabPanel("Component 3")
  )

 #   )
server <- function(input,output){
  
  output$lineplot <- renderPlot({
    primsumaries <- primsumaries %>% group_by(year,gender)%>% 
      summarise(total_enrollment=sum(enrolment))
    
    ggplot(primsumaries,aes(x=year,y=total_enrollment,color=gender,group=gender))+
      geom_line()+
      geom_point(size=4)+
    
      ggtitle(label = "Primary School Enrolment",subtitle = paste("For the year ",input$year))+
      labs(x="Year",y="Enrolment")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.subtitle = element_text(hjust = 0.5))
    
  }) 
  
  output$barplot <- renderPlot({
    primsummary2 <- primsumaries %>% group_by(subcounty,year,gender)%>% 
      filter(year==input$year)%>%
      summarise(total_enrollment=sum(enrolment))
    
    ggplot(primsummary2,aes(x=subcounty,y=total_enrollment,fill = gender))+
      geom_bar(position="dodge",stat = "identity")+
      ggtitle(label = "Primary School Enrolment",subtitle = paste("For the year ",input$year))+
      labs(x="Sub County",y="Enrolment")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(plot.subtitle = element_text(hjust = 0.5))
    
  }) 
  
  output$school_type <- renderPlotly({
    primsummary3 <- primsumaries %>% 
      group_by(schooltype,gender)%>% 
      summarise(total_enrollment=sum(enrolment))
    
    #plotly(data = primsummary3, x=~schooltype,y=~total_enrollment, type = 'bar', color =~gender)
    plot_ly(data=primsummary3, x=~schooltype, y=~total_enrollment, name=~gender, type = "bar", mode="line")%>%
      layout(title="Gender Enrolment by School Type",xaxis = list(title="School Type"),yaxis = list(title="Enrolment"))
  })
  
  output$hort.crops <- renderPlot({
    
    
    cropbar<- hortcrop
    
    ggplot(data = cropbar,aes(x=subcounty,y=value.Kshs, fill=crop_type))+
      geom_col(position = position_dodge()) + 
      ggtitle(label = "Crop Types", subtitle = paste("Per Sub County ",input$years))+
      labs(x="Sub County",y="Value in Kshs", fill= "Crop Type",) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    
  })
  
}

shinyApp(ui,server)