

ui <- pageWithSidebar(
  headerPanel("Nemška trgovina klasificirana po panoge"),
  sidebarPanel(
    uiOutput("filter_degree")
    
  ),
  mainPanel(
    uiOutput('plot')
    
  )
)