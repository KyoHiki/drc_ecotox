server <- function(input, output, session) {
  
  
  ####################################################################################
  ####### Data upload & analysis #####################################################
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
      }  else if(intest_type() == 'TG218') {
        req(input$datafile_TG218)
        validateFile(input$datafile_TG218)
        ff <- input$datafile_TG218
        read.csv(file=ff$datapath, header=TRUE)
      }  else if(intest_type() == 'TG235') {
        req(input$datafile_TG235)
        validateFile(input$datafile_TG235)
        ff <- input$datafile_TG235
        read.csv(file=ff$datapath, header=TRUE)
      }
  })
  
  ## Output : print raw data
  output$rawdata <- DT::renderDataTable({filedata()})
  
  ## model fitting
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
    else if(intest_type() == 'TG218') {
        if(inmodel_218() == 'll2') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, data = filedata(), fct = LL.2(), type="binomial")
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.2(), type="continuous")
        }
        else if(inmodel_218() == 'll4') {
          fit1 <- drm( DEAD/TOTAL~CONC, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
      fit <- list(fit1 = fit1, fit2 = fit2, fit3 = fit3)
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
  
  
  # output of ECx estimate
  ECx <- eventReactive(input$buttonRunStep1,{
    if(intest_type() == 'TG201') {
      XX <- input$ecx_TG201
      drc_df <- data.frame(ED(fitmodel(), c(XX),interval = "delta",display=FALSE))
      colnames(drc_df) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      }
    else if(intest_type() == 'TG202') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      XX <- input$ecx_TG202
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE)) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE))
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      drc_df <- rbind(drc_df1,drc_df2)
      rownames(drc_df) <- c('24 h','48 h')
      }
    else if(intest_type() == 'TG203') {
      XX <- input$ecx_TG203
      drc_df <- data.frame(ED(fitmodel(), c(XX),interval = "delta",display=FALSE))
      colnames(drc_df) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
    }
    else if(intest_type() == 'TG218') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      XX <- input$ecx_TG218
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE)) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE))
      drc_df3 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE))
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df3) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      drc_df <- rbind(drc_df1,drc_df2,drc_df3)
      rownames(drc_df) <- c('Mortality','Emergence ratio',"Development rate")
    }
    else if(intest_type() == 'TG235') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      XX <- input$ecx_TG235
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE)) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE))
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI')
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
  #######  hypothesis testing ########################################################
  ####################################################################################
  
   inmethod_201 <- reactive({input$test_method_TG201})
   inmethod_203 <- reactive({input$test_method_TG203})
   inmethod_218 <- reactive({input$test_method_TG218})
   inmethod_235 <- reactive({input$test_method_TG235})

    TestResult <- eventReactive(input$buttonRunStep1,{
      if(intest_type() == 'TG201') {
        data=filedata()
        data$CONC <- as.factor(data$CONC)
        fit <- lm( H72 ~ CONC, data = data )
        Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
        Res
        } 
      else if(intest_type() == "TG203") {
        data=filedata()
        data$CONC <- as.factor(data$CONC)
        fit <- glm( cbind(DEAD,TOTAL-DEAD) ~ CONC, data = data, weights=TOTAL, family=binomial(link="logit")  )
        Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="greater")) 
        Res
        } 
      else if(intest_type() == 'TG235'){
          data=filedata()
          data$CONC <- as.factor(data$CONC)
          if( inmethod_235() =="Dunnett"){
            fit1 <- glm( cbind(IMMOBILIZED,TOTAL-IMMOBILIZED) ~ CONC, data = data %>% dplyr::filter(TIME=="24"),
                       weights=TOTAL, family=binomial(link="logit")  )
            Res1 <- summary (glht (fit1, linfct=mcp (CONC="Dunnett"), alternative="greater")) 
            fit2 <- glm( cbind(IMMOBILIZED,TOTAL-IMMOBILIZED) ~ CONC, data = data %>% dplyr::filter(TIME=="48"),
                       weights=TOTAL, family=binomial(link="logit")  )
            Res2 <- summary (glht (fit2, linfct=mcp (CONC="Dunnett"), alternative="greater"))
            list("24 h" = Res1,"48 h" = Res2)
            } 
          else if ( inmethod_235() =="Fisher"){
            data=filedata() %>% group_by(CONC,TIME) %>%
              summarize(TOTAL=sum(TOTAL),IMMOBILIZED=sum(IMMOBILIZED)) %>% ungroup
            TOTAL_24 <- data %>% dplyr::filter(TIME=="24" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            TOTAL_48 <- data %>% dplyr::filter(TIME=="48" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            IM_24 <- data %>% dplyr::filter(TIME=="24" & CONC=="0") %>% dplyr::select(IMMOBILIZED) %>% as.numeric()
            IM_48 <- data %>% dplyr::filter(TIME=="48" & CONC=="0") %>% dplyr::select(IMMOBILIZED) %>% as.numeric()
            data_24 <- data %>% mutate(TOTAL_ctrl = TOTAL_24, IMMOBILIZED_ctrl =IM_24) %>%
              dplyr::filter(CONC!="0" & TIME=="24")
            data_48 <- data %>% mutate(TOTAL_ctrl = TOTAL_48, IMMOBILIZED_ctrl =IM_48) %>%
              dplyr::filter(CONC!="0" & TIME=="48")
            fisher <- function(a,b,c,d){
              dt <- matrix(c(a,b,c,d),ncol=2)
              c(pvalue = fisher.test(dt)$p.value) 
            }
            Res1 <- data_24 %>%
              rowwise()%>%
              mutate(pvalue = fisher(IMMOBILIZED,TOTAL-IMMOBILIZED, IMMOBILIZED_ctrl,TOTAL_ctrl-IMMOBILIZED_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))
            Res2 <- data_48 %>%
              rowwise()%>%
              mutate(pvalue = fisher(IMMOBILIZED,TOTAL-IMMOBILIZED, IMMOBILIZED_ctrl,TOTAL_ctrl-IMMOBILIZED_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))    
            list("24 h" = Res1,"48 h" = Res2)
            }
          }
      })
    
    output$test_result <- renderPrint({
      TestResult()
    })
 
 
  
  
  ####################################################################################
  ####### Report  ####################################################################
  ####################################################################################
  output$DownloadReport <- downloadHandler(
    filename = function() {
      paste('report', sep = '.', 
            switch(input$format,
                   #PDF = 'pdf',
                   Word = 'docx') )
    },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    if(intest_type() == 'TG201') {
      src <- normalizePath('report_TG201.Rmd')
      file.copy(src, 'report_TG201.Rmd', overwrite = TRUE)
      out <- render('report_TG201.Rmd', switch(
        input$format,
      #PDF = pdf_document(), 
        Word = word_document()
      ))
      file.rename(out, file)
    } else if(intest_type() == 'TG235'){
      src <- normalizePath('report_TG235.Rmd')
      file.copy(src, 'report_TG235.Rmd', overwrite = TRUE)
      out <- render('report_TG235.Rmd', switch(
        input$format,
      #PDF = pdf_document(), 
        Word = word_document()
      ))
      file.rename(out, file)    
    }
    }
    )


    
}
                                
