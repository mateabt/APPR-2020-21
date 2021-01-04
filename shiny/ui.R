

ui <- pageWithSidebar(
  headerPanel("Nemška trgovina klasificirana po panoge"),
  sidebarPanel(
    width = 2,
    radioButtons("rd","Izberi ",choices = c("stolpičen","tortni",'razpredelnica'),
                 selected = "stolpičen")
    
  ),
  mainPanel(
    uiOutput('plot')
    
  )
)