# BNP places.R automation
library(shiny)

shinyServer(function(input, output,session) {
observeEvent(input$apikey,{
  shinyalert("APIKEY received",text = input$apikey)
})
  
  observeEvent(input$keywords,{
    shinyalert("keywords received",text = paste(input$keywords,collapse = ","))
  })
 

})
