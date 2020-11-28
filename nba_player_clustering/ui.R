shinyUI(fluidPage(
    includeCSS("theme.css"),
    
    # Application title
    titlePanel("NBA Player Clustering"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
            sliderInput("num_cluster", 
                        "Select Number of Clusters", 
                        min = 1,
                        max = 15,
                        value = 5),
            
            textInput("years_select",
                      "Enter Years to Cluster:",
                      value = "1980"),
            
            actionButton("refresh_clusters","Refresh")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("player_clusters"),
            tableOutput("dataTable")
        )
    )
    )
    )
