server <- function(input, output, session) {
  avg_batter_stats <- read_delim("https://raw.githubusercontent.com/noahknob/Baseball_App/master/data/avg_batter_stats.txt",delim = "\t")
  avg_batter_stats <- avg_batter_stats %>%
    mutate_if(is.double,round,digits = 3)
  
  df <- reactive({
    data <- avg_batter_stats %>%
      filter(Name == input$Catcher | Name == input$FirstBase | Name == input$SecondBase | Name == input$ThirdBase | 
               Name == input$Shortstop | Name == input$UTL | Name %in% input$Outfield )
    return(data)
  })
  output$values <- renderPrint({
    summed_df <- df()
    list("Runs" = sum(summed_df$R), "RBI" = sum(summed_df$RBI), "HR" = sum(summed_df$HR), 
         "SB" = sum(summed_df$SB),"AVG" = round(mean(summed_df$BA), digits = 3))
  })
  output$table <- DT::renderDataTable(DT::datatable({
    df()
    
  }))
  
}
