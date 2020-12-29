

ui <- pageWithSidebar(
  headerPanel("Nemška trgovina klasificirana po panoge"),
  sidebarPanel(
    width = 2,
    uiOutput("filter_degree")
    
  ),
  mainPanel(
    uiOutput('plot')
    
  )
)