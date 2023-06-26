library(shiny)
library(ggplot2)
library(tidyverse)
library(drc)
library(multcomp)
library(shinyjs)
library(shinycssloaders)

#############################################################################################################################################################################################################

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 500px;
              position:fixed;
              top: calc(50%);
              left: calc(50% - 250px);
              color: #009E73;
              font-size: 16px;
              font-style: italic;
              background-color: #c6c6c6;
            }
            .progress-bar {
              background-color: #999999;
              value: #eee;
            }
            progress::-webkit-progress-value { 
  background-color: #999999; 
            } 
progress::-moz-progress-bar { 
  background-color: #999999; 
} 
           ",
           lang = "en"
      )
    )),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "Logo.png"), tags$title("Logo"))
  ),
  br(),
  
  navbarPage(title = "", 
             tabPanel(img(src = "Logo.png", title = "Logo", width = 150),
                      
                      fluidRow(
                        br(), br(), 
                        HTML("<center><font face=verdana size=6 color=#009E73>Welcome to the Drc-Shiny application</font></center>"),
                        br(), br(),
                        fixedRow(column(10, offset = 1,
                                        tags$blockquote("Drc-Shiny is a freely available tool for dose-response (or concentration-response) characterization from standardized ecotoxicity data.", br(), 
                                                        "This application consists of three parts: upload toxicity data and select analysis methods (Part I), ", br(), 
                                                         br(), br(),
                                                        style="text-align:justify;")
                                        
                        )),
                        fixedRow(column(10, offset = 1,
                                        br(),
                                        p(strong("Contact")),
                                        p("If you have any feedback on the Shiny application, feel free to email us: ", strong("hiki.kyoshiro@nies.go.jp"), ".")
                        )),
                      )
             ),
             
             ####################################################################################
             ####### STEP 1: Data upload ########################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#009E73>Data upload</font>"),
                      br(), HTML("<font face=verdana size=5 color=#009E73><b>Upload ecotoxicity data</b></font>"), br(), br(), br(),
                      fixedRow(
                        ###### Select type of data                                
                        sidebarPanel(
                          style = "background-color: #009E73;",
                          width = 3,
                          radioButtons('test_type', 
                                       "Select test type",
                                       choices = c("Algae: TG201" = "TG201",
                                                   "Daphnia: TG202" = "TG202",
                                                   "Fish: TG203" = "TG203",
                                                   "Chironomus: TG218,219" = "TG218",
                                                   "Chironomus: TG235" = "TG235"),
                                       selected = "Algae: TG201"),
                          selectInput("conc_unit", "Concentration Unit", 
                                      choices = c("g/L", "mg/L", "µg/L", "ng/L")),
                          ###### For algae (TG201)
                          conditionalPanel(
                            condition = "input.test_type == 'TG201'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG201',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", a("here", href = "AlgaeData_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'AlgaeData_sample.csv') ),
                            br(),
                            radioButtons('model_TG201',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2')
                            ),
                          
                          ###### For daphnia (TG202)
                          conditionalPanel(
                            condition = "input.test_type == 'TG202'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG202',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", a("here", href = "DaphniaTG202Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'DaphniaTG202Data_sample.csv') ),
                            br(),
                            radioButtons('model_TG202',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2')
                            ),
                          
                          ###### For fish (TG203)
                          conditionalPanel(
                            condition = "input.test_type == 'TG203'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG203',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "FishData_sample.csv", TARGET = "_blank", style="text-decoration:underline;",
                                 download = 'FishData_sample.csv') ),
                            br(),
                            radioButtons('model_TG203',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2')
                            ),
                          
                          ###### For chironomus (TG218,219)
                          conditionalPanel(
                            condition = "input.test_type == 'TG218'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG218',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "ChironomusTG218Data_sample.csv", TARGET = "_blank",style="text-decoration:underline;",
                                 download = 'ChironomusTG218Data_sample.csv') ),
                            br(),
                            radioButtons('model_TG218',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2')
                            ),
                          
                          ###### For chironomus (TG235)
                          conditionalPanel(
                            condition = "input.test_type == 'TG235'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG235',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "ChironomusTG235Data_sample.csv", TARGET = "_blank",style="text-decoration:underline;",
                                 download = 'ChironomusTG235Data_sample.csv') ),
                            br(),
                            radioButtons('model_TG235',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2')
                            ),
                          
                          fixedRow(
                            column(12, align="center",
                                   actionButton("buttonRunStep1", "Run", icon = icon("file-import"), style='font-size:200%')
                            )
                          )
                        ),
                        
                        mainPanel(
                          width = 9,
                          DT::dataTableOutput('rawdata'),
                          br(),
                          h5("Effect concentrations"),
                          tableOutput('drc_result'),
                          withSpinner(plotOutput('drc_plot', width = "100%", height = "850px"), type = 4, color = '#009E73'),
                          br(),
                          br()
                        )
                      )
             ),
             
             
             ####################################################################################
             ####### STEP 2: hypothesis testing #################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#009E73>Hypothesis testing</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#009E73><b>Selection of hypothesis test methods</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #009E73;",
                                   width = 3,
                                   ###### For algae (TG201)
                                   conditionalPanel(
                                     condition = "input.test_type == 'TG201'",
                                     tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                                     radioButtons('test_method_TG201',
                                         'Select hypothesis testing method',
                                         choices = c('Dunnett test' = 'Dunnett'),
                                         selected = 'Dunnett'),
                                     fixedRow(
                                       column(12, align="center",
                                              actionButton("buttonRunStep2", "Run", icon = icon("file-import"), style='font-size:200%')
                                              )
                                       )
                                     ),
                                   ###### For daphnia (TG202)
                                   conditionalPanel(
                                     condition = "input.test_type == 'TG202'",
                                     tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                                     h5('No need to perform a hypothesis testing')
                                     ),
                                    ###### For fish (TG203)
                                   conditionalPanel(
                                     condition = "input.test_type == 'TG203'",
                                     tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                                     radioButtons('test_method_TG203',
                                         'Select hypothesis testing method',
                                         choices = c('Dunnett test' = 'Dunnett'),
                                         selected = 'Dunnett'),
                                     fixedRow(
                                       column(12, align="center",
                                              actionButton("buttonRunStep2", "Run", icon = icon("file-import"), style='font-size:200%')
                                              )
                                       )
                                     ),
                                   ###### For chironomus (TG235)
                                   conditionalPanel(
                                     condition = "input.test_type == 'TG235'",
                                     tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                                     radioButtons('test_method_TG235',
                                         'Select hypothesis testing method',
                                         choices = c("Dunnett's test" = 'Dunnett',
                                                     "Fisher's exact test" = 'Fisher'),
                                         selected = 'Dunnett'),
                                     fixedRow(
                                       column(12, align="center",
                                              actionButton("buttonRunStep2", "Run", icon = icon("file-import"), style='font-size:200%')
                                              )
                                       )
                                     ),
                                   
                            ),
                            mainPanel(
                              width = 9,
                              withSpinner(verbatimTextOutput('test_result'), type = 4, color = '#009E73')
                              )
                            )
                            )
                        )),

             ####################################################################################
             ####### Info   #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#009E73>R code </font>"),
                      fixedRow(
                        column(8, 
                               br(), HTML("<font face=verdana size=5 color=#009E73><b>R code for the analysis</b></font>"), br(), br(), br(),
                               downloadButton("buttonDownRCode", "Download R Code", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCode'), br(), br(),
                               br()
                        ))
             )
  )
)


