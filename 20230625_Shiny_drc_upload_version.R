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




#############################################################################################################################################################################################################
server <- function(input, output, session) {
  
  
  ####################################################################################
  ####### Data upload ################################################################
  ####################################################################################
  
  intest_type <- reactive({input$test_type})
  inmodel_201 <- reactive({input$model_TG201})
  inmodel_202 <- reactive({input$model_TG202})
  inmodel_203 <- reactive({input$model_TG203})
  inmodel_218 <- reactive({input$model_TG218})
  inmodel_235 <- reactive({input$model_TG235})

  
  validateFile <- function(filename){
    extFile <- tools::file_ext(filename)
    validate(
      need(extFile == "csv", "Only csv files are allowed.")
    )
  }
  
  ## Input: file data
  filedata <- eventReactive(input$buttonRunStep1, {
      if(intest_type() == 'TG201') {
        req(input$datafile_TG201)
        validateFile(input$datafile_TG201)
        ff <- input$datafile_TG201
        read.csv(file=ff$datapath, header=TRUE)
      } else if(intest_type() == 'TG202') {
        req(input$datafile_TG202)
        validateFile(input$datafile_TG202)
        ff <- input$datafile_TG202
        read.csv(file=ff$datapath, header=TRUE)
      } else if(intest_type() == 'TG203') {
        req(input$datafile_TG203)
        validateFile(input$datafile_TG203)
        ff <- input$datafile_TG203
        read.csv(file=ff$datapath, header=TRUE)
      }  else if(intest_type() == 'TG235') {
        req(input$datafile_TG235)
        validateFile(input$datafile_TG235)
        ff <- input$datafile_TG235
        read.csv(file=ff$datapath, header=TRUE)
      }
  })
  
  ## Output : print and plot toxicity data
  output$rawdata <- DT::renderDataTable({filedata()})
  
  fitmodel <- eventReactive(input$buttonRunStep1,{
    if(intest_type() == 'TG201') {
        if(inmodel_201() == 'll2') {
          fit <- drm( H72 ~ CONC, data = filedata() , fct = LL.2())
        }
        else if(inmodel_201() == 'll4') {
          fit <- drm( H72 ~ CONC, data = filedata(), fct = LL.4())
        }
      return(fit)
      }
    else if(intest_type() == 'TG202') {
        if(inmodel_202() == 'll2') {
          fit1 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")        
        }
        else if(inmodel_202() == 'll4') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial") 
        }
      fit <- list(fit1 = fit1, fit2 = fit2)
      return(fit)
      }
    else if(intest_type() == 'TG203') {
        if(inmodel_203() == 'll2') {
          fit <- drm( DEAD/TOTAL ~ CONC, data = filedata(), fct = LL.2(), type="binomial")
        }
        else if(inmodel_203() == 'll4') {
          fit <- drm( DEAD/TOTAL ~ CONC, data = filedata(), fct = LL.4(), type="binomial")
        }
      return(fit)
      }
    else if(intest_type() == 'TG235') {
        if(inmodel_235() == 'll2') {
          fit1 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")        
        }
        else if(inmodel_235() == 'll4') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial") 
        }
      fit <- list(fit1 = fit1, fit2 = fit2)
      return(fit)
      }
  })
  
  
  # output of model summary
  ECx <- eventReactive(input$buttonRunStep1,{
    if(intest_type() == 'TG201') {
      drc_df <- data.frame(ED(fitmodel(), c(50),interval = "delta",display=FALSE))
      colnames(drc_df) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      }
    else if(intest_type() == 'TG202') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      drc_df1 <- data.frame(ED(fit1, c(50),interval = "delta",display=FALSE)) 
      drc_df2 <- data.frame(ED(fit2, c(50),interval = "delta",display=FALSE))
      colnames(drc_df1) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df2) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      drc_df <- rbind(drc_df1,drc_df2)
      rownames(drc_df) <- c('24 h','48 h')
      }
    else if(intest_type() == 'TG203') {
      drc_df <- data.frame(ED(fitmodel(), c(50),interval = "delta",display=FALSE))
      colnames(drc_df) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      }
    else if(intest_type() == 'TG235') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      drc_df1 <- data.frame(ED(fit1, c(50),interval = "delta",display=FALSE)) 
      drc_df2 <- data.frame(ED(fit2, c(50),interval = "delta",display=FALSE))
      colnames(drc_df1) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df2) <- c('EC50', 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      drc_df <- rbind(drc_df1,drc_df2)
      rownames(drc_df) <- c('24 h','48 h')
    }
    drc_df
    }
    )

  output$drc_result <- renderTable({
    ECx()
    }, rownames = TRUE
    )  
    

  output$drc_plot <- renderPlot({
    if(intest_type() == 'TG201') {
      par(mar=c(5,9,2,2))
      plot(fitmodel(), log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="",
             cex=2,cex.axis =2, cex.lab=2)
      }
    else if(intest_type() == 'TG202') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      par(mar=c(5,5,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Mortality", 
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#D55E00",cex=2,cex.axis =2, cex.lab=2)
      legend("topleft",inset=0.05, legend = c("24 h","48 h"), col = c("black","#D55E00"), lty = c("dotted","solid"),cex=2)
    }
    else if(intest_type() == 'TG203') {
      par(mar=c(5,5,2,2))
      plot(fitmodel(), log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Mortality",
           cex=2,cex.axis =2, cex.lab=2)
    }
    else if(intest_type() == 'TG235') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      par(mar=c(5,5,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Immobility",
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#D55E00",cex=2,cex.axis =2, cex.lab=2)
      legend("topleft",inset=0.05, legend = c("24 h","48 h"), col = c("black","#D55E00"), lty = c("dotted","solid"),cex=2)
    }
    })

  
  
  
  
  ####################################################################################
  ####### STEP 2: hypothesis tets ####################################################
  ####################################################################################
  
   inmethod_201 <- reactive({input$test_method_TG201})
   inmethod_203 <- reactive({input$test_method_TG203})
#  inmethod_218 <- reactive({input$test_method_TG218})
   inmethod_235 <- reactive({input$test_method_TG235})

    TestResult <- eventReactive(input$buttonRunStep2,{ 
      if(intest_type() == 'TG201') {
        data=filedata()
        data$CONC <- as.factor(data$CONC)
        fit <- lm( H72 ~ CONC, data = data )
        Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
        Res
      }
      else if(intest_type() == 'TG203') {
        data=filedata()
        data$CONC <- as.factor(data$CONC)
        fit <- glm( cbind(DEAD,TOTAL-DEAD) ~ CONC, data = data, weights=TOTAL, family=binomial(link="logit")  )
        Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
        Res
        }
      else if(intest_type() == 'TG235'){
        data=filedata()
        data$CONC <- as.factor(data$CONC)
        if( inmethod_235() =="Dunnett"){
          fit1 <- glm( cbind(IMMOBILIZED,TOTAL-IMMOBILIZED) ~ CONC, data = data %>% dplyr::filter(TIME=="24"),
                       weights=TOTAL, family=binomial(link="logit")  )
          Res1 <- summary (glht (fit1, linfct=mcp (CONC="Dunnett"), alternative="less")) 
          fit2 <- glm( cbind(IMMOBILIZED,TOTAL-IMMOBILIZED) ~ CONC, data = data %>% dplyr::filter(TIME=="48"),
                       weights=TOTAL, family=binomial(link="logit")  )
          Res2 <- summary (glht (fit2, linfct=mcp (CONC="Dunnett"), alternative="less"))
#         list(Res1,Res2)
          fit1
          }
        else if ( inmethod_235() =="Fisher"){
          fit1 <- lm( IMMOBILIZED ~ CONC, data = data  )
          fit1
        }
        }
      })
    
    output$test_result <- renderPrint({
      TestResult()
    })
 
 
  
  
  ####################################################################################
  ####### R CODE #####################################################################
  ####################################################################################
  
  output$printRCode <- renderText({
    
    if(intest_type() == 'TG201') {
      req(input$datafile_TG201)
    } else if(intest_type() == 'TG202') {
      req(input$datafile_TG202)
    } else if(intest_type() == 'TG203') {
      req(input$datafile_TG203)
    } else if(intest_type() == 'TG218') {
      req(input$datafile_TG218)
    } else if(intest_type() == 'TG235') {
      req(input$datafile_TG235)
    } 
    text <- c("library(drc)",
              "",
              "# Step 1",
              paste0("o <- ", ifelse(input$test_type == 'TG201', 
                                     paste0("TG201('", input$datafile_TG201$name, "', backgrounddose = ", input$bgdose_microarray, ", check = TRUE, norm.method = '", input$normMethod_microarray, "')"), 
                                     ifelse(input$test_type == 'TG202', 
                                            paste0("TG202('", input$datafile_TG202$name, "', backgrounddose = ", input$bgdose_rnaseq, ", check = TRUE, transfo.method = '", input$transfoMethod_rnaseq, "', round.counts = TRUE)"), 
                                            ifelse(input$test_type == 'TG203', 
                                                   paste0("TG203('", input$datafile_TG203$name, "', backgrounddose = ", input$bgdose_metabolomic, ", check = TRUE)")
                                                   )))),
              "print(o)",
              "plot(o)",
              "",
              "# Step 2",
              paste0("s <- itemselect(o, select.method = '", inSelectMethod(), "', FDR = ", inFDR(), ")"),
              "print(s)",
              "",
    )
    output$buttonDownRCode <- downloadHandler(
      filename = function(){
        paste0("Rcode-", Sys.Date(), ".pdf")
      },
      content = function(file) {
        writeLines(paste(text, collapse = "\n"), file)
      },
      contentType = {"text/plain"}
    )
    return(paste(text, collapse = "\n"))
  })
  
}


shinyApp(ui = ui, server = server)
