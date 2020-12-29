library(shiny)
library(gridExtra)


server <- function(input, output, session) {
  output$filter_degree<-renderUI({
    radioButtons("rd","Select Option",choices = c("tortni","stolpične",'razpredelnice'),
                 selected = "tortni")
  })
  
  
  output$plot <- renderUI({
    if(input$rd=="stolpične"){
      output$plot1<-renderPlot({
        ptlist<-list(stolpicni_izvoz,stolpicni_uvoz)
        grid.arrange(grobs=ptlist)
      })
      plotOutput("plot1", height = 650)
    }
    
    
    else if(input$rd=="tortni"){
      output$plot2<-renderUI({
        
      })
      plotlyOutput("plot2")
    }
    
    
    else if(input$rd=="razpredelnice"){
      
      output$tbl =renderDataTable(razdelitve1)
      
      dataTableOutput("tbl")
    }
    
  })
  
}