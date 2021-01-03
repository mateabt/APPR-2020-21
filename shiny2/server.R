library(shiny)



server <- function(input, output, session) {
    output$filter_degree<-renderUI({
        selectInput("rd","Izberi zeleno geografsko podrocje ",choices = c("Svet","Evropa"),
                     selected = NULL)
    })
    
    
    output$plot <- renderUI({
        if(input$rd=="Svet"){
            output$plot1<-renderPlot({
                ptlist<-list(zemljevid_evropa_izvoz,zemljevid_evropa_uvoz)
                grid.arrange(grobs=ptlist)
            })
            plotOutput("plot1", height = 650)
        }
    })
    
}