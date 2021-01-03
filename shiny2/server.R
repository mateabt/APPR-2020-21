library(shiny)



server <- function(input, output, session) {
    output$filter_degree<-renderUI({
        selectInput("rd","Izberi zeleno geografsko podrocje ",
                    choices = c("Svet","Evropa","Afrika","Južna Amerika" ),
                     selected = NULL)
    })
    
    
    output$plot <- renderUI({
        if(input$rd=="Svet"){
            output$plot1<-renderPlot({
                ptlist<-list(zemljevid_svet_izvoz,zemljevid_svet_uvoz)
                grid.arrange(grobs=ptlist)
            })
            plotOutput("plot1", height = 650)
        }
        
        else if(input$rd=="Evropa"){
            
            output$plot2<-renderPlot({
                ptlist<-list(zemljevid_evropa_izvoz,zemljevid_evropa_uvoz)
                grid.arrange(grobs=ptlist)
            })
            plotOutput("plot2", height = 650)
            
        }
        
        else if(input$rd=="Afrika"){
            
            output$plot3<-renderPlot({
                ptlist<-list(zemljevid_afrika_izvoz,zemljevid_afrika_uvoz)
                grid.arrange(grobs=ptlist)
            })
            plotOutput("plot3", height = 650)
            
        }
        
        else if(input$rd=="Južna Amerika"){
            
            output$plot4<-renderPlot({
                ptlist<-list(zemljevid_juznaamerika_izvoz,zemljevid_juznaamerika_uvoz)
                grid.arrange(grobs=ptlist)
            })
            plotOutput("plot4", height = 650)
            
        }
        
    })
    
}