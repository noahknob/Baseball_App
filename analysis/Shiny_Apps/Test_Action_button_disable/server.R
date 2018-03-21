library(shiny)

disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

shinyServer(function(input, output,session) {
  
  observe({
    if(input$btn1 == 0) return()
    disableActionButton("btn2",session)
  })
})

