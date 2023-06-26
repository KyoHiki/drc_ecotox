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


