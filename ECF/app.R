library(shiny)
library(ggplot2)
library(shinythemes)
library(bslib)
library(wordcloud2)
library(tidyverse)
library(DT)

# data$`ECF-Dec24_1m.csv` = NULL
library(magrittr)
Rcpp::sourceCpp("Cpp_lib.cpp")
library(progress)

source("R_library.R")
dict = readRDS("final_dict.RDS")
bt = readRDS("backtest.RDS")
ta = readRDS("TA.RDS")

init_file = "app_data.RDS"

tmp = read_rds(init_file)

# UI definition (Modernized with bslib and shinythemes)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"), # Apply a modern theme 
  navbarPage(
    title = "ECF Wiz",
    tabPanel(
      "Live Trading",
      sidebarLayout(
        sidebarPanel(align="center",width=2,
                     textInput("wd", "Change production file location here (do not touch):", value = init_file),
                     br(),
                     actionButton("update","Update"),
                     br(),
                     helpText('Actual backend runs in different process, manually update visualization upon need!'),
                     br(),
                     br()
        ),
        mainPanel(
          br(),
          h5("Current situation Expectations (Long on top, short on bottom):"),
          plotOutput("current",width = "100%",height = "700px"),
          br(),
          h5("AI decision:"),
          br(),
          dataTableOutput("ai"),
          br(),
          h5("Current sitaton details:"),
          br(),
          dataTableOutput("current_dt")# Display DataTable
        )
      )
    ),
    tabPanel(
      "Backtest",
      mainPanel(align = "center",width = 10,
        br(),
        br(),
        plotOutput("bt",width = "100%",height = "800px")
      )
    ),
    tabPanel(
      "Manual Execution",
      p("Execution part.")
    )
  )
)
# Server logic (unchanged)
server <- function(input, output) {

  filtered_data <- reactiveVal(tmp)
  
  
  index <- reactiveVal(tmp$index)
  
  observeEvent(input$update, {
    new_data <- readRDS(input$wd)  # Assuming input$wd is the file path or object to read
    filtered_data(new_data)
    index(new_data$index)
  })
  
  output$current_dt <- renderDataTable({
    data <- filtered_data()$index
    tt = dict$table[data,,drop=F] %>% t
    colnames(tt)= ""
    datatable(tt,options = list(pageLength = 20))
  })
  
  output$ai <- renderDataTable({
    long = c(filtered_data()$long[[1]],round(filtered_data()$min_long_ev*100,2))
    short = c(filtered_data()$short[[1]],round(filtered_data()$min_short_ev*100,2))
    tmp = data.frame(Long=long,Short=short)
    rownames(tmp) = c("Execute","Min Required EV (%)")
    tmp = tmp %>% t %>% as.data.frame()
    tmp[,1] = as.logical(tmp[,1])
    datatable(tmp )
  })
  
  output$current <- renderPlot({
    plot_expectations(dict,index())
  })
  
  output$bt <- renderPlot({
    visualize_eval(x = bt,data=ta)
  })
  

  
}
shinyApp(ui,server)
# Run the application
# runApp(shinyApp(ui=ui,server = server,options=list(port=91231,host="ip")))
