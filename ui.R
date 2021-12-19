# check apikey
library(shiny)
library(shinyjs)
library(data.table)
library(shinydashboard)
library(shinyalert)
source("places.R")
ui <- dashboardPage(
    header = dashboardHeader(title = "BNP Analytics"),
    sidebar = dashboardSidebar(menuItem(text = "APIKey",tabName = "APIKEY"),
                               menuItem("Select parameters",tabName = "PARAM")
                               ),
    body = dashboardBody(useShinyjs(),useShinyalert(),
        tabItems(
                 tabItem(tabName = "APIKEY",textAreaInput(inputId = "apikey",label = "Google API KEY",placeholder = "Copy Paste API key",width = "500px")),
                 tabItem(tabName = "PARAM",selectizeInput("keywords",label = "Select Keywords",choices = list_of_keywords,multiple = T))
        )
    ),
    title = "Nearby residential places search",skin = "green"
)