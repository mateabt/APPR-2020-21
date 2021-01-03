ui <- pageWithSidebar(
    headerPanel("Uvoz in izvoz"),
    sidebarPanel(
        width = 2,
        uiOutput("filter_degree")
        
    ),
    mainPanel(
        uiOutput('plot')
        
    )
)