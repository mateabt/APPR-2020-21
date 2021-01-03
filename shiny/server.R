library(shiny)
library(gridExtra)


server <- function(input, output, session) {
  output$filter_degree<-renderUI({
    radioButtons("rd","Izberi ",choices = c("tortni","stolpične",'razpredelnice'),
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
      output$plot2<-renderPlot({
        # pie izvoz
        slices <- c(razdelitve1$izvoz)
        lbls <- c(razdelitve1$`opis blaga`)
        pct <- round(slices/sum(slices)*100)
        pct1 <- paste(pct,"%",sep="")
        lbls <- paste(lbls, pct) # dodaj odstotke na labels 
        lbls <- paste(lbls,"%",sep="") # doda znak % 
        par(mai = c(0,0,1,3))
        pie(slices, col=rainbow(length(lbls)),
            main="izvoz po razdelitve",clockwise=TRUE,cex=0.5,labels=pct1)
            #legend("right", inset=c(-0.95,0),cex=0.5,legend =unique(lbls), bty="n",fill=rainbow(length(lbls)))
      })
      
    }
    
    
    else if(input$rd=="razpredelnice"){
      
      output$tbl =renderDataTable(razdelitve1)
      
      dataTableOutput("tbl")
    }
    
  })
  
}