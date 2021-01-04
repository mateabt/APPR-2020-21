ui <- pageWithSidebar(
    headerPanel("Uvoz in izvoz"),
    sidebarPanel(
        width = 2,
        selectInput("rd","Izberi želeno geografsko podrocje ",
                    choices = c("Svet","Evropa","Afrika","Južna Amerika","Severna Amerika",
                                "Avstralija in Oceanija","Azija","Antarktika" ),
                    selected = NULL)
        
    ),
    mainPanel(
        uiOutput('plot')
        
    )
)