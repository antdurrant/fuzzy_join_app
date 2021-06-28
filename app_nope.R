library(shiny)
library(tidyverse)
library(fuzzyjoin)


ui <- fluidPage(
    
    # Application title
    titlePanel("Join messy data with user approval"),
    
    # Sidebar with file inputs
    sidebarLayout(
        sidebarPanel(
            fileInput("user_info",
                      "your file for base-users goes here", 
                      # accept = ".csv"
            ),
            fileInput("join_info",
                      "the data you want to connect to your base-users goes here",
                      # accept = ".csv"
            ),
            checkboxGroupInput("try",
                               "joins to try",
                               choices = c("exact",
                                           "id_exact&name_by_1",
                                           "id_by_1&name_exact")
            )
        ),
        
        # matched and not matched data
        mainPanel(
            h5("Perfectly matched data"),
            tableOutput("good"),
            h5("Data that has not been matched yet"),
            tableOutput("not_yet")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    user_info <- reactive(read_csv(input$user_info$datapath))
    join_info <- reactive(read_csv(input$join_info$datapath))
    
    
    main_effort <- reactive({
        req(input$user_info)
        req(input$join_info)
        
        
        base <- list()
        
        if("exact" %in% input$try){
            
            
            base$good <- inner_join(user_info(), join_info() %>% mutate(match_type = "exact"))
            base$not_yet <- anti_join(join_info(), user_info())
            
        }
        if("id_exact&name_by_1" %in% input$try){
            base$good <- bind_rows(base$good, 
                                   stringdist_inner_join(a,
                                                         b %>% 
                                                             select(-id) %>%
                                                             mutate(match_type = "id_exact&name_by_1"), 
                                                         max_dist = 1, 
                                                         method = "lv") %>%
                                       select(-name.y) %>%
                                       rename(name = name.x ) %>%
                                       inner_join(b %>% rename(name_join = name), by = c("id" = "id", "score" = "score")) %>%
                                       distinct()
            )
          
        }
        
        base
    })
    
    output$good <- renderTable({
        main_effort()$good
    })
    
    output$not_yet <- renderTable({
        main_effort()$not_yet
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



    
               
               