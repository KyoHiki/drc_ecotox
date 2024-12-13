library(shiny)
library(ggplot2)
library(scales)
library(tidyverse)
library(drc)
library(multcomp)
library(here)
library(rmarkdown)
library(shinyjs)
library(shinycssloaders)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "inst/i18n/translation.json")
i18n$set_translation_language("en")


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 80px;
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
            .navbar {
              background-color: #b3b3b3;
              border-color: #b3b3b3;
              min-height: 40px; 
              margin-bottom: 0; 
            }
             .navbar-brand {
             height: 50px;
             padding-top: 5px;
             padding-bottom: 5px;
             }
             .navbar-nav > li > a {
             line-height: 50px;
             height: 50px;
             padding-top: 0;
             padding-bottom: 0;
             }
            progress::-webkit-progress-value { 
            background-color: #999999; 
            } 
            progress::-moz-progress-bar { 
            background-color: #999999; 
            } 
           "
      )
    )),
  
  tags$ul(class = "nav navbar-nav navbar-right",
          tags$li(
            style = "margin-right:7px; margin-top:7px;",
            actionButton("lang_en", "English", class = "btn-xs")
          ),
          tags$li(
            style = "margin-right:7px; margin-top:7px;",
            actionButton("lang_ja", "Japanese", class = "btn-xs")
          )
  ),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "Logo_2.png"), tags$title("Logo"))
  ),
  br(),
  

  
  navbarPage(title = "", 
             tabPanel(img(src = "Logo_2.png", title = "Logo", width = 40, height=40),
                      
                      fluidRow(
                        br(), br(), 
                        div(
                          style = "display: flex; align-items: center; justify-content: center;",
                          img(src = "Logo_2.png", title = "Logo", width = 100, style = "margin-right: 15px;"), # Logo
                          HTML("<font face=verdana size=6 color=#009E73><b>  Welcome to ShinyEcotox  </b></font>"," ") # title
                        ),
                        br(), br(),
                        fixedRow(column(8, offset = 1,
                                        tags$blockquote("'ShinyEcotox' is an online tool for the analysis of standardized ecotoxicity data.",
                                                        br(), 
                                                        "This application helps the users to make a plot of dose-response curve, estimate effect concentrations (ECx), and estimate no observed effect concentrations (NOEC).", br(), 
                                                         br(), 
                                                        "「ShinyEcotox」はOECDテストガイドラインなどの標準化された生態毒性試験のデータを解析するためのオンラインツールです。",
                                                        br(), 
                                                        "主に濃度-応答関係の解析やECxの算出、無影響濃度（NOEC）の算出などが可能です。",
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
             tabPanel(HTML("<font face=verdana size=5 color=#009E73>Analysis</font>"),
                      br(), uiOutput("analysis_title"),
                      br(), br(), 
                      fixedRow(
                        ###### Select type of data                                
                        sidebarPanel(
                          style = "background-color: #009E73",
                          width = 3,
                          uiOutput("select_test_type_ui"),
                          uiOutput("chemical_name_ui"),
                          uiOutput("conc_unit_ui"),
                          ###### For algae (TG201)
                          conditionalPanel(
                            condition = "input.test_type == 'TG201'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg201_example_file_ui"),br(),
                            uiOutput("tg201_input_file_ui"),
                            br(), 
                            uiOutput("tg201_select_model_ui"),
                            br(),
                            uiOutput("tg201_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg201_select_hypothesis_ui"),
                            uiOutput("tg201_bartlett_note_ui")
                          ),
                          
                          ###### For daphnia (TG202)
                          conditionalPanel(
                            condition = "input.test_type == 'TG202'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg202_example_file_ui"),br(),
                            uiOutput("tg202_input_file_ui"),
                            br(), 
                            uiOutput("tg202_select_model_ui"),
                            br(),
                            uiOutput("tg202_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg202_select_hypothesis_ui"),
                            uiOutput("tg202_no_need_ui")
                          ),
                          
                          ###### For fish (TG203)
                          conditionalPanel(
                            condition = "input.test_type == 'TG203'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg203_example_file_ui"),br(),
                            uiOutput("tg203_input_file_ui"),
                            br(), 
                            uiOutput("tg203_select_model_ui"),
                            br(),
                            uiOutput("tg203_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg203_select_hypothesis_ui"),
                            ),
                          
                          ###### For chironomus (TG218,219)
                          conditionalPanel(
                            condition = "input.test_type == 'TG218'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            uiOutput("tg218_example_file_ui"),br(),
                            uiOutput("tg218_input_file_ui"),
                            br(), br(),
                            uiOutput("tg218_select_model_mortality_ui"),
                            uiOutput("tg218_select_model_emergence_ui"),
                            uiOutput("tg218_select_model_development_ui"),
                            br(),
                            uiOutput("tg218_determine_ecx_ui"),
                            br(), br(),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            uiOutput("tg218_select_hypothesis_mortality_ui"),
                            uiOutput("tg218_select_hypothesis_emergence_ui"),
                            uiOutput("tg218_select_hypothesis_development_ui"),
                            uiOutput("tg218_bartlett_note_ui")
                          ),
                          
                          ###### For Lemna (TG221)
                          conditionalPanel(
                            condition = "input.test_type == 'TG221'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_TG221',
                                      'Select an input file',
                                      accept = c('.csv')),
                            h5("You can download an example file: ", a("here", href = "LemnaTG221Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'LemnaTG221Data_sample.csv'),
                               style = "font-size:23px;"),
                            br(),br(),
                            radioButtons('endpoint_TG221',
                                         'Select endpoints',
                                         choices = c('Frond number' = 'Number',
                                                     'Total frond area' = 'Area'),
                                         selected = 'Number'),
                            br(),
                            radioButtons('model_TG221',
                                         'Select fitting model',
                                         choices = c('log-logistic 2 parameters' = 'll2',
                                                     'log-logistic 3 parameters' = 'll3',
                                                     'log-logistic 4 parameters' = 'll4'),
                                         selected = 'll2'),
                            br(),
                            numericInput(inputId="ecx_TG221",label="Determine effect concentration X%",value=50,min=0,max=100),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            radioButtons('test_method_TG221',
                                         'Select hypothesis testing method',
                                         choices = c("Dunnett's test" = 'Dunnett',
                                                     "Steel's test" = 'Steel'),
                                         selected = 'Dunnett'),
                            h5("You can see the bartlett's test result for homogenity of variance of growth rate, and then select testing method.")
                          ),
                          
                          ###### For chironomus (TG235)
                          conditionalPanel(
                            condition = "input.test_type == 'TG235'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg235_example_file_ui"),br(),
                            uiOutput("tg235_input_file_ui"),
                            br(), 
                            uiOutput("tg235_select_model_ui"),
                            br(),
                            uiOutput("tg235_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg235_select_hypothesis_ui"),
                          ),

                          ###### For fish embryo (TG236)
                          conditionalPanel(
                            condition = "input.test_type == 'TG236'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg236_example_file_ui"),br(),
                            uiOutput("tg236_input_file_ui"),
                            br(), 
                            uiOutput("tg236_select_model_ui"),
                            br(),
                            uiOutput("tg236_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg236_select_hypothesis_ui"),
                            uiOutput("tg236_no_need_ui")
                          ),

                          ###### For fish cell line (TG249)
                          conditionalPanel(
                            condition = "input.test_type == 'TG249'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg249_example_file_ui"),br(),
                            uiOutput("tg249_input_file_ui"),
                            br(), 
                            uiOutput("tg249_select_model_ui"),
                            br(),
                            uiOutput("tg249_determine_ecx_ui"),
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            br(),
                            uiOutput("tg249_select_hypothesis_ui"),
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
             tabPanel(HTML("<font face=verdana size=5 color=#009E73>Download report </font>"),
                      fixedRow(
                        column(8, 
                               br(), uiOutput("download_title"),
                               br(), br(), 
                               uiOutput("report_format_ui"),
                               downloadButton("DownloadReport", "Download report", icon = icon("fas fa-download"),
                                              style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'),
                               br(),
                               br(),
                               br()
                        ))
             )
  )
)
