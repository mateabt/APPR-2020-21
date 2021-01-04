library(shiny)



server <- function(input, output, session) {
 
  
  
  output$plot <- renderUI({
    if(input$rd=="stolpičen"){
      output$plot1<-renderPlot({
        ptlist<-list(stolpicni_izvoz,stolpicni_uvoz)
        grid.arrange(grobs=ptlist)
      })
      plotOutput("plot1", height = 650)
    }
    
    
    else if(input$rd=="tortni"){
      output$plot2<-renderPlot({
        ptlist<-list(pie1,pie2)
        grid.arrange(grobs=ptlist)
      })
      plotOutput("plot2", height = 650)
    }
    
    
    else if(input$rd=="razpredelnica"){
      
      output$tbl =renderDataTable(razdelitve1)
      
      dataTableOutput("tbl")
    }
    
  })
  
}