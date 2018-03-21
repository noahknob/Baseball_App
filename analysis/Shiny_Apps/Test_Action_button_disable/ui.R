
library(shiny)


shinyUI(basicPage(
  tags$head(tags$script(HTML('
                             Shiny.addCustomMessageHandler("jsCode",
                             function(message) {
                             console.log(message)
                             eval(message.code);
                             }
                             );
                             ')))
  ,actionButton("btn1","Disable the other button")  
  ,actionButton("btn2","Button")
  )
  )