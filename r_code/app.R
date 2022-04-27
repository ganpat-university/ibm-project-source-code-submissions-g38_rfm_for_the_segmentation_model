library(shiny)
library(shinythemes)
library(caTools)
library(ROCR)
library(GGally)
library(plotly)
library(ggplot2)
ui<-navbarPage("rfm model",
               tags$head(
                 tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Play&display=swap');
        @import url('https://fonts.googleapis.com/css2?family=Monda&display=swap');
        
      body{
        font-family:Monda;
        font-size:16px;
      }
      .tab-pane{
        margin-right:2%;
        margin-left:2%;
      }
      h1, h2, h3{
        color: #FF5E13;
        text-align:center;
        font-family:Play;
      }
      p{
        color: #FF5E13;
        font-family:Play;
      }
      h2{
        font-family:Play;
        font-size: 50px;
        text-align: left;
        padding-left:24px;
      }
      h4{
      color: #FF5E13;
        font-family:Play;
        font-size: 30px;
        text-align: left;
        padding-top:24px;
      }
      h5{
        color: #FF5E13;
        font-family:Play;
        font-size: 20px;
        text-align: left;
        padding-top:24px;
      }
      h2::first-letter {
      
        font-size: 70px;
      }
      h3{
        font-size: 35px;
      }
      table, th, td {
        color: #FF5E13;
        }
      .col-sm-8{
        border-radius: 5px;
        border: 1px solid black;
        border-color:#b0b0b0;
      }
      li{
        color: #FF5E13;
      }
      .well{
        border-radius: 10px;
      }
    "))
               ),
               sidebarLayout(
                 sidebarPanel(
                   fileInput(
                     "file","Upload your CSV",multiple = FALSE
                   ),
                   tags$hr(),
                   checkboxInput(inputId = 'header', label = 'Header', value = TRUE),radioButtons(inputId = 'sep', label = 'Separator',
                                                                                                  choices = c(Comma=','), selected = ','),
                   uiOutput("var1_select"),
                   uiOutput("var2_select"),
                   uiOutput("var1_slider"),
                   uiOutput("var2_slider"),
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Data Set",
                              tags$h3("Data Set"),
                              uiOutput("tb1")
                     ),
                     tabPanel("Structure",
                              tags$h3("Structure"),
                              verbatimTextOutput("otherone_val_show")
                     ),
                     tabPanel("Histogram",
                              tags$h3("Histogram"),
                              plotOutput("Hist") 
                     ),
                     tabPanel("Line Chart",
                              tags$h3("Line Chart"),
                              plotOutput("lineChart") 
                     ),
                     tabPanel("Bar plot",
                              tags$h3("Bar plot"),
                              plotOutput("barPlot") 
                     )
                   )
                 )
               )
)
server<-function(input,output) { 
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    tt1=read.table(file=file1$datapath, sep=input$sep, header = input$header)
  })  
  output$table <-  DT::renderDataTable({
    DT::datatable(data(), filter = 'top', options = list( autoWidth = TRUE))
  })
  output$tb1 <- renderUI({
    DT::dataTableOutput("table")
  })
  output$var1_select<-renderUI({
    selectInput("ind_var_select","Select X", choices =names(data()),multiple = FALSE)
  })
  output$var1_slider<-renderUI({
    f<-data()
    temp <- f[[input$ind_var_select]]
    min_val <- min(temp)
    max_val <- max(temp)
    sliderInput("slider1", "Slider for X", min = min_val , max = max_val, value = c(40, 60))
  })
  output$var2_select<-renderUI({
    selectInput("depnd_var_select","Select Y", choices =names(data()),multiple = FALSE)
  })
  output$var2_slider<-renderUI({
    f<-data()
    temp <- f[[input$depnd_var_select]]
    min_val <- min(temp)
    max_val <- max(temp)
    sliderInput("slider2", "Slider for y", min = min_val , max = max_val, value = c(40, 60))
  })
  output$otherone_val_show<-renderPrint({
    f<-data()
    str(f)
    summary(f)
  })
  output$Hist <- renderPlot({
    f<-data()
    hist(f[[input$ind_var_select]] , xlim=c(input$slider1[1],input$slider1[2]), ylim=c(input$slider2[1],input$slider2[2]), col = c("red", "green", "yellow", "blue"))
  })
  output$lineChart <- renderPlot({
    f<-data()
    plot(f[[input$ind_var_select]],type = "o" , xlim=c(input$slider1[1],input$slider1[2]), ylim=c(input$slider2[1],input$slider2[2]), col = c("red", "green", "yellow", "blue"))
  })
  
  
  output$barPlot <- renderPlot({
    f<-data()
    bar2 <- tapply(f[,input$ind_var_select], f[,input$depnd_var_select])
    barplot(bar2,beside = input$sidebar , xlim=c(input$slider1[1],input$slider1[2]), ylim=c(input$slider2[1],input$slider2[2]), col = c("red", "green", "yellow", "blue"))
  })
  
}
shinyApp(ui=ui,server=server)