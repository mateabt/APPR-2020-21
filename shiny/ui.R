

ui <- pageWithSidebar(
  headerPanel("Nemška trgovina klasificirana po panoge"),
  sidebarPanel(
    width = 2,
    radioButtons("rd","Izberi ",choices = c("tortni","stolpičen",'razpredelnica'),
                 selected = "tortni")
    
  ),
  mainPanel(
    uiOutput('plot')
    
  )
)