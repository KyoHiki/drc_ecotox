library(shiny)
library(ggplot2)
library(tidyverse)
library(drc)
library(multcomp)
library(here)
library(rmarkdown)
library(shinyjs)
library(shinycssloaders)

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
                        HTML("<center><font face=verdana size=6 color=#009E73>Welcome to the drc-ecotox application</font></center>"),
                        br(), br(),
                        fixedRow(column(8, offset = 1,
                                        tags$blockquote("'drc-ecotox' is a freely available tool for dose-response (or concentration-response) characterization from standardized ecotoxicity data.",
                                                        br(), 
                                                        "This application helps the users to make a plot of dose-response curve, estimate effect concentrations (ECx), and estimate no observed effect concentrations (NOEC).", br(), 
                                                         br(), 
                                                        style="text-align:justify;")
                                        
                        )),
                        fixedRow(column(10, offset = 1,
                                        br(),
                                        p(strong("Note")),
                                        p("Please be aware that this application is currently in beta testing. As such, modifications and updates may occur without prior notification."),
                                        br(),
                                        p(strong("Contact")),
                                        p("We welcome your feedback and suggestions as we continue to improve and refine this application: ", strong("referencelab.risk@nies.go.jp"), ".")
                        )),
                      )
             ),
             
             ####################################################################################
             ####### Data upload & analysis #####################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=4 color=#009E73>Analysis</font>"),
                      br(), HTML("<font face=verdana size=5 color=#009E73><b>Upload and analyze ecotoxicity data</b></font>"), br(), br(), br(),
                      fixedRow(
                        ###### Select type of data                                
                        sidebarPanel(
                          style = "background-color: #009E73;",
                          width = 3,
                          selectInput('test_type', 
                                       "Select test type",
                                       choices = c("Algae: TG201" = "TG201",
                                                   "Daphnia: TG202" = "TG202",
                                                   "Fish: TG203" = "TG203",
                                                   "Chironomus: TG218,219" = "TG218",
                                                   "Chironomus: TG235" = "TG235",
                                                   "Fish embryo: TG236" = "TG236",
                                                   "Fish cell: TG249" = "TG249"),
                                       selected = "TG201"),
                          textInput(inputId = "chemical", "Input the name of test chemical",value="Chemical"),
                          selectInput("conc_unit", "Concentration Unit", 
                                      choices = c("g/L", "mg/L", "µg/L", "ng/L")),
                          ###### For algae (TG201)
                          conditionalPanel(
                            condition = "input.test_type == 'TG201'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG201',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", a("here", href = "AlgaeTG201Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'AlgaeTG201Data_sample.csv') ),
                            br(),br(),
                            radioButtons('model_TG201',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG201",label="Determine effect concentration X%",value=50,min=0,max=100),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            radioButtons('test_method_TG201',
                                         'Select hypothesis testing method',
                                         choices = c('Dunnett test' = 'Dunnett'),
                                         selected = 'Dunnett')
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
                            br(),
                            radioButtons('model_TG202',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG202",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),
                            HTML("<b><size=2>Select hypothesis testing method</font></b>"),
                            h5('No need to perform a hypothesis testing for TG202')
                            ),
                          
                          ###### For fish (TG203)
                          conditionalPanel(
                            condition = "input.test_type == 'TG203'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG203',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "FishTG203Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;",
                                 download = 'FishTG203Data_sample.csv') ),
                            br(),
                            br(),
                            radioButtons('model_TG203',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG203",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            radioButtons('test_method_TG203',
                                         'Select hypothesis testing method',
                                         choices = c("Fisher's exact test with BH correction" = 'Fisher'),
                                         selected = 'Fisher')
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
                            br(),
                            radioButtons('model_TG218_mortality',
                                         'Select fitting model for mortality',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            radioButtons('model_TG218_emergence',
                                         'Select fitting model for emergence',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            radioButtons('model_TG218_development',
                                         'Select fitting model for development rate',
                                         choices = c('log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll3'),
                            br(),
                            numericInput(inputId="ecx_TG218",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            radioButtons('test_method_TG218_mortality',
                                         'Select hypothesis testing method for mortality',
                                         choices = c("Cochran-Armitage test" = 'CA',
                                           "Fisher's exact test with BH correction" = 'Fisher'),
                                         selected = 'CA'),
                            radioButtons('test_method_TG218_emergence',
                                         'Select hypothesis testing method for emergence ratio',
                                         choices = c("Cochran-Armitage test" = 'CA',
                                           "Fisher's exact test with BH correction" = 'Fisher'),
                                         selected = 'CA'),
                            radioButtons('test_method_TG218_development',
                                         'Select hypothesis testing method for development rate',
                                         choices = c("Dunnett's test" = 'Dunnett',
                                           "Steel's test" = 'Steel'),
                                         selected = 'Dunnett'),
                           h5("You can see the bartlett's test result for homogenity of variance of development rate, and then select testing method.")
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
                            br(),
                            radioButtons('model_TG235',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG235",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            radioButtons('test_method_TG235',
                                         'Select hypothesis testing method',
                                         choices = c("Fisher's exact test with BH correction" = 'Fisher'),
                                         selected = 'Fisher'),
                       #    h5("You can see the bartlett's test result for homogenity of variance, and then select testing method.")
                            ),

                          ###### For fish embryo (TG236)
                          conditionalPanel(
                            condition = "input.test_type == 'TG236'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG236',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "FishTG236Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;",
                                 download = 'FishTG236Data_sample.csv') ),
                            br(),
                            br(),
                            radioButtons('model_TG236',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG236",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}'))
                            ),

                          ###### For fish cell line (TG249)
                          conditionalPanel(
                            condition = "input.test_type == 'TG249'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG249',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", 
                               a("here", href = "FishCellTG249Data_sample.csv", TARGET = "_blank",style="text-decoration:underline;",
                                 download = 'FishCellTG249Data_sample.csv') ),
                            br(),
                            br(),
                            radioButtons('model_TG249',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG249",label="Determine effect concentration X%",value=50,min=0,max=100),
                            br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            radioButtons('test_method_TG249',
                                         'Select hypothesis testing method',
                                         choices = c("Dunnett's test" = 'Dunnett'),
                                         selected = 'Dunnett'),
                       #    h5("You can see the bartlett's test result for homogenity of variance, and then select testing method.")
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
                          tableOutput('drc_result'),
                          withSpinner(plotOutput('drc_plot', width = "100%", height = "850px"), type = 4, color = '#009E73'),
                          br(),
                          withSpinner(verbatimTextOutput("test_result")),
                          br()
                        )
                      )
             ),
             
             ####################################################################################
             ####### Report   ###################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=4 color=#009E73>Download report </font>"),
                      fixedRow(
                        column(8, 
                               br(), HTML("<font face=verdana size=5 color=#009E73><b>Download the analysis report</b></font>"),
                               br(), br(), br(),
                               radioButtons("format","Select report format", c('Word'), inline = TRUE),
                               downloadButton("DownloadReport", "Download report", icon = icon("fas fa-download"),
                                              style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'),
                               br(),
                               br(),
                               br()
                        ))
             )
  )
)
