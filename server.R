i18n <- Translator$new(translation_json_path = "inst/i18n/translation.json")
i18n$set_translation_language("en")


server <- function(input, output, session) {
  lang <- reactiveVal("en")
  
  observeEvent(input$lang_en, {
    lang("en")
  })
  observeEvent(input$lang_ja, {
    lang("ja")
  })
  
  ####################################################################################
  ####### Data upload & analysis #####################################################
  ####################################################################################
  
  intest_type <- reactive({input$test_type})
  inmodel_201 <- reactive({input$model_TG201})
  inmodel_202 <- reactive({input$model_TG202})
  inmodel_203 <- reactive({input$model_TG203})
  inmodel_218_mortality <- reactive({input$model_TG218_mortality})
  inmodel_218_emergence <- reactive({input$model_TG218_emergence})
  inmodel_218_development <- reactive({input$model_TG218_development})
  inmodel_221 <- reactive({input$model_TG221})
  inendpoint_201 <- reactive({input$endpoint_TG221})
  inmodel_235 <- reactive({input$model_TG235})
  inmodel_236 <- reactive({input$model_TG236})
  inmodel_249 <- reactive({input$model_TG249})
  
  validateFile <- function(filename){
    extFile <- tools::file_ext(filename)
    validate(
      need(extFile == "csv", i18n$translate("Only csv files are allowed."))
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
      }  else if(intest_type() == 'TG221') {
          req(input$datafile_TG221)
          validateFile(input$datafile_TG221)
          ff <- input$datafile_TG221
          read.csv(file=ff$datapath, header=TRUE)
      }  else if(intest_type() == 'TG235') {
        req(input$datafile_TG235)
        validateFile(input$datafile_TG235)
        ff <- input$datafile_TG235
        read.csv(file=ff$datapath, header=TRUE)
      }  else if(intest_type() == 'TG236') {
        req(input$datafile_TG236)
        validateFile(input$datafile_TG236)
        ff <- input$datafile_TG236
        read.csv(file=ff$datapath, header=TRUE)
      }  else if(intest_type() == 'TG249') {
        req(input$datafile_TG249)
        validateFile(input$datafile_TG249)
        ff <- input$datafile_TG249
        read.csv(file=ff$datapath, header=TRUE)
      }
  })
  
  ## Side bar
  output$analysis_title <- renderUI({
    i18n$set_translation_language(lang())
    HTML(paste0("<font face=verdana size=5 color=#009E73><b>",
                i18n$translate("Upload and analyze ecotoxicity data"),
                "</b></font>"))
  })
  
  output$select_test_type_ui <- renderUI({
    i18n$set_translation_language(lang())
    selectInput('test_type', 
                i18n$translate("Select test type"),
                choices = c("Algae: TG201" = "TG201",
                            "Daphnia: TG202" = "TG202",
                            "Fish: TG203" = "TG203",
                            "Chironomus: TG218,219" = "TG218",
                            "Lemna: TG221" = "TG221",
                            "Chironomus: TG235" = "TG235",
                            "Fish embryo: TG236" = "TG236",
                            "Fish cell: TG249" = "TG249"),
                selected = "TG201")
  })
  
  output$chemical_name_ui <- renderUI({
    i18n$set_translation_language(lang())
    textInput("chemical", i18n$translate("Chemical name"), value = "Chemical")
  })
  output$conc_unit_ui <- renderUI({
    i18n$set_translation_language(lang())
    selectInput("conc_unit", i18n$translate("Concentration unit"), 
                choices = c("g/L", "mg/L", "µg/L", "ng/L"))
  })
    
  
  output$tg201_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG201',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })
  output$tg201_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "AlgaeTG201Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'AlgaeTG201Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg201_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG201',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg201_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG201",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg201_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG201',
                 i18n$translate("Select hypothesis testing method"),
                 choices = c("Dunnett's test" = 'Dunnett',
                             "Steel's test" = 'Steel'),
                 selected = 'Dunnett')
  })
  output$tg201_bartlett_note_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(i18n$translate("You can see the bartlett's test result for homogenity of variance, and then select testing method."))
  })
  
  output$tg202_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG202',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })
  output$tg202_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "DaphniaTG202Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'DaphniaTG202Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg202_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG202',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg202_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG202",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg202_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    HTML(paste0("<size=2><b>",
                i18n$translate("Select hypothesis testing method"),"</b></font>"))
  })
  output$tg202_no_need_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(i18n$translate("No need to perform a hypothesis testing."))
  })
  
  
  output$tg203_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "FishTG203Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'FishTG203Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg203_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG203',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })  
  output$tg203_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG203',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg203_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG203",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg203_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG203',
                 i18n$translate("Select hypothesis testing method"),
                 choices = c("Fisher's exact test with BH correction" = 'Fisher'),
                 selected = 'Fisher')
  })
  
  output$tg218_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG218',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })
  output$tg218_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "ChironomusTG218Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'ChironomusTG218Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg218_select_model_mortality_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG218_mortality',
                 i18n$translate("Select fitting model for mortality"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg218_select_model_emergence_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG218_emergence',
                 i18n$translate("Select fitting model for emergence"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg218_select_model_development_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG218_development',
                 i18n$translate("Select fitting model for development rate"),
                 choices = c('log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll3')
  })
  output$tg218_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG218",
                 i18n$translate("Determine effect concentration X%"),
                 value = 50, min=0, max=100)
  })
  output$tg218_select_hypothesis_mortality_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG218_mortality',
                 i18n$translate("Select hypothesis testing method for mortality"),
                 choices = c("Cochran-Armitage test" = 'CA',
                             "Fisher's exact test with BH correction" = 'Fisher'),
                 selected = 'CA')
  })
  output$tg218_select_hypothesis_emergence_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG218_emergence',
                 i18n$translate("Select hypothesis testing method for emergence"),
                 choices = c("Cochran-Armitage test" = 'CA',
                             "Fisher's exact test with BH correction" = 'Fisher'),
                 selected = 'CA')
  })
  output$tg218_select_hypothesis_development_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG218_development',
                 i18n$translate("Select hypothesis testing method for development rate"),
                 choices = c("Dunnett's test" = 'Dunnett',
                             "Steel's test" = 'Steel'),
                 selected = 'Dunnett')
  })
  output$tg218_bartlett_note_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(i18n$translate("You can see the bartlett's test result for homogenity of variance, and then select testing method."))
  })
  
  
  output$tg235_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "ChironomusTG235Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'ChironomusTG235Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg235_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG235',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })  
  output$tg235_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG235',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg235_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG235",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg235_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG235',
                 i18n$translate("Select hypothesis testing method"),
                 choices = c("Fisher's exact test with BH correction" = 'Fisher'),
                 selected = 'Fisher')
  })
  
  
  output$tg236_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG236',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })
  output$tg236_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "FishEmbryoTG236Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'FishEmbryoTG236Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg236_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG236',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg236_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG236",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg236_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    HTML(paste0("<size=2><b>",
                i18n$translate("Select hypothesis testing method"),"</b></font>"))
  })
  output$tg236_no_need_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(i18n$translate("No need to perform a hypothesis testing."))
  })

  
  output$tg249_example_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    h5(
      i18n$translate("You can download a template file: "),
      a("here", href = "FishCellTG249Data_sample.csv", TARGET = "_blank", style="text-decoration:underline;", download = 'FishCellTG249Data_sample.csv'),
      style = "font-size:23px;"
    )
  })
  output$tg249_input_file_ui <- renderUI({
    i18n$set_translation_language(lang())
    fileInput('datafile_TG249',
              i18n$translate("Select an input file"),
              accept = c('.csv'))
  })  
  output$tg249_select_model_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('model_TG249',
                 i18n$translate("Select fitting model"),
                 choices = c('log-logistic 2 parameters' = 'll2',
                             'log-logistic 3 parameters' = 'll3',
                             'log-logistic 4 parameters' = 'll4'),
                 selected = 'll2')
  })
  output$tg249_determine_ecx_ui <- renderUI({
    i18n$set_translation_language(lang())
    numericInput("ecx_TG249",
                 i18n$translate("Determine effect concentration X%"),
                 value=50, min=0, max=100)
  })
  output$tg249_select_hypothesis_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons('test_method_TG249',
                 i18n$translate("Select hypothesis testing method"),
                 choices = c("Dunnett's test" = 'Dunnett'),
                 selected = 'Dunnett')
  })
  
  
  output$download_title <- renderUI({
    i18n$set_translation_language(lang())
    HTML(paste0("<font face=verdana size=5 color=#009E73><b>",
                i18n$translate("Download the analysis report"),
                "</b></font>"))
  })
  
  output$report_format_ui <- renderUI({
    i18n$set_translation_language(lang())
    radioButtons("format",i18n$translate("Select report format"), c('Word'), inline = TRUE)
    })
  
  
  
  ## Output : print raw data
  output$rawdata <- DT::renderDataTable({filedata()})
  
  ## model fitting
  fitmodel <- eventReactive(input$buttonRunStep1,{
    if(intest_type() == 'TG201') {
      growth_df <- filedata() %>% mutate( gr0to24 = log(H24 / H0),
                                          gr24to48 = log(H48 / H24),
                                          gr48to72 = log(H72 / H48),
                                          gr0to72 = log(H72 / H0)/(3),
                                          gr24to72 = log(H72 / H24)/(2) )
      growth_ctrl <- growth_df %>% dplyr::filter(CONC=="0") %>% summarize(Mean=mean(gr0to72)) %>% as.vector
        if(inmodel_201() == 'll2') {
          # from cell density to growth
          fit <- drm( gr0to72 ~ CONC, data = growth_df , fct = LL.2(upper=growth_ctrl[[1]]), type="continuous")
        }
      else if(inmodel_201() == 'll3') {
        fit <- drm( gr0to72 ~ CONC, data = growth_df, fct = LL.3(), type="continuous")
      }
      else if(inmodel_201() == 'll4') {
        fit <- drm( gr0to72 ~ CONC, data = growth_df, fct = LL.4(),  type="continuous")
        }
      return(fit)
      }
    else if(intest_type() == 'TG202') {
        if(inmodel_202() == 'll2') {
          fit1 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")        
        }
        else if(inmodel_202() == 'll3') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.3u(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.3u(), type="binomial") 
        }
        else if(inmodel_202() == 'll4') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial") 
        }
      fit <- list(fit1 = fit1, fit2 = fit2)
      return(fit)
      }
    else if(intest_type() == 'TG203') {
        if(inmodel_203() == 'll2') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.2(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.2(), type="binomial")
        }
      else if(inmodel_203() == 'll3') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.3u(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.3u(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.3u(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.3u(), type="binomial")
        }
          else if(inmodel_203() == 'll4') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.4(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.4(), type="binomial")
        }
      fit <- list(fit1 = fit1, fit2 = fit2, fit3=fit3, fit4=fit4)
      return(fit)
      }
    else if(intest_type() == 'TG218') {
        if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL,data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
         else if(inmodel_218_mortality() == 'll2' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC,  data = filedata(), fct=LL.4(), type="continuous")
        }
          
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll3' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.3u(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }

        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll2' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.2(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll3' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.3(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll3' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.3(), type="continuous")
        }
        else if(inmodel_218_mortality() == 'll4' & inmodel_218_emergence() == 'll4' & inmodel_218_development() == 'll4' ) {
          fit1 <- drm( DEAD/TOTAL~CONC, weights=TOTAL, data = filedata(), fct = LL.4(), type="binomial")
          fit2 <- drm( EMERGED/TOTAL ~ CONC, weights=TOTAL,data = filedata(), fct = LL.4(), type="binomial") 
          fit3 <- drm( DEVELOPMENT ~ CONC, data = filedata(), fct=LL.4(), type="continuous")
        }
      fit <- list(fit1 = fit1, fit2 = fit2, fit3 = fit3)
      return(fit)
    }
    else if(intest_type() == 'TG221') {
      # from frond number to growth
      growth_df <- filedata() %>% mutate( gr0to3 = log(D3 / D0)/(3),
                                          gr3to5 = log(D5 / D3)/(2),
                                          gr5to7 = log(D7 / D5)/(2),
                                          gr0to7 = log(D7 / D0)/(7),
                                          gr3to7 = log(D7 / D3)/(4) )
      growth_ctrl <- growth_df %>% dplyr::filter(CONC=="0") %>% summarize(Mean=mean(gr0to7)) %>% as.vector
      if(inmodel_221() == 'll2') {
        fit <- drm( gr0to7 ~ CONC, data = growth_df , fct = LL.2(upper=growth_ctrl[[1]]), type="continuous")
      }
      else if(inmodel_221() == 'll3') {
        fit <- drm( gr0to7 ~ CONC, data = growth_df, fct = LL.3(), type="continuous")
      }
      else if(inmodel_221() == 'll4') {
        fit <- drm( gr0to7 ~ CONC, data = growth_df, fct = LL.4(),  type="continuous")
      }
      return(fit)
    }
    else if(intest_type() == 'TG235') {
        if(inmodel_235() == 'll2') {
          fit1 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")        
        }
      else if(inmodel_235() == 'll3') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.3u(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.3u(), type="binomial") 
        }
      else if(inmodel_235() == 'll4') {
          fit1 <- drm( IMMOBILIZED/TOTAL~CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( IMMOBILIZED/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial") 
        }
      fit <- list(fit1 = fit1, fit2 = fit2)
      return(fit)
      }
    else if(intest_type() == 'TG236') {
        if(inmodel_236() == 'll2') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.2(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.2(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.2(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.2(), type="binomial")
        }
      else if(inmodel_236() == 'll3') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.3u(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.3u(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.3u(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.3u(), type="binomial")
        }
          else if(inmodel_236() == 'll4') {
          fit1 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="24"), fct = LL.4(), type="binomial")
          fit2 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="48"), fct = LL.4(), type="binomial")
          fit3 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="72"), fct = LL.4(), type="binomial")
          fit4 <- drm( DEAD/TOTAL ~ CONC, weights=TOTAL, data = filedata() %>% dplyr::filter(TIME=="96"), fct = LL.4(), type="binomial")
        }
      fit <- list(fit1 = fit1, fit2 = fit2, fit3=fit3, fit4=fit4)
      return(fit)
      }
    else if(intest_type() == 'TG249') {
        if(inmodel_249() == 'll2') {
          alamarBlue_bg <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          CDFAAM_bg <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          NeutralRed_bg <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          alamarBlue_ctrl <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          CDFAAM_ctrl <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          NeutralRed_ctrl <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector 
          alamarBlue <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC!="CellFree") %>% mutate(FLUORESCENCE = (FLUORESCENCE- alamarBlue_bg[[1]])/alamarBlue_ctrl[[1]])
          CDFAAM <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC!="CellFree")  %>% mutate(FLUORESCENCE = (FLUORESCENCE- CDFAAM_bg[[1]])/CDFAAM_ctrl[[1]])
          NeutralRed <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC!="CellFree")  %>% mutate(FLUORESCENCE = (FLUORESCENCE- NeutralRed_bg[[1]])/NeutralRed_ctrl[[1]])
          alamarBlue$CONC <- as.numeric(alamarBlue$CONC)
          CDFAAM$CONC <- as.numeric(CDFAAM$CONC)
          NeutralRed$CONC <- as.numeric(NeutralRed$CONC)
          fit1 <- drm( FLUORESCENCE ~ CONC, data = alamarBlue , fct = LL.2(), type="continuous")
          fit2 <- drm( FLUORESCENCE ~ CONC, data = CDFAAM, fct = LL.2(), type="continuous")
          fit3 <- drm( FLUORESCENCE ~ CONC, data = NeutralRed, fct = LL.2(), type="continuous")
        }
      fit <- list(fit1 = fit1, fit2 = fit2, fit3=fit3)
      return(fit)
    }
  })
  
  
  
  
  # output of ECx estimate
  ECx <- eventReactive(input$buttonRunStep1,{
    if(intest_type() == 'TG201') {
      XX <- input$ecx_TG201
      fit <- fitmodel()
      drc_df <- data.frame(ED(fit, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit)[[1]] )
      colnames(drc_df) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      }
    else if(intest_type() == 'TG202') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      XX <- input$ecx_TG202
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit2)[[1]])
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2)
      rownames(drc_df) <- c('24 h','48 h')
      }
    else if(intest_type() == 'TG203') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      fit4 <- fit$fit4
      XX <- input$ecx_TG203
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit2)[[1]])
      drc_df3 <- data.frame(ED(fit3, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit3)[[1]]) 
      drc_df4 <- data.frame(ED(fit4, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit4)[[1]])
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df3) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df4) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2, drc_df3, drc_df4)
      rownames(drc_df) <- c('24 h','48 h','72 h','96 h')
    }
    else if(intest_type() == 'TG218') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      XX <- input$ecx_TG218
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit2)[[1]])
      drc_df3 <- data.frame(ED(fit3, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit3)[[1]])
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df3) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2,drc_df3)
      rownames(drc_df) <- c('Mortality','Emergence ratio',"Development rate")
    }
    if(intest_type() == 'TG221') {
      XX <- input$ecx_TG221
      fit <- fitmodel()
      drc_df <- data.frame(ED(fit, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit)[[1]] )
      colnames(drc_df) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
    }
    else if(intest_type() == 'TG235') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      XX <- input$ecx_TG235
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE), "Slope"=coefficients(fit2)[[1]])
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2)
      rownames(drc_df) <- c('24 h','48 h')
    }
    else if(intest_type() == 'TG236') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      fit4 <- fit$fit4
      XX <- input$ecx_TG236
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit2)[[1]])
      drc_df3 <- data.frame(ED(fit3, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit3)[[1]]) 
      drc_df4 <- data.frame(ED(fit4, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit4)[[1]])
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df3) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df4) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2, drc_df3, drc_df4)
      rownames(drc_df) <- c('24 h','48 h','72 h','96 h')
    }
    else if(intest_type() == 'TG249') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      XX <- input$ecx_TG249
      drc_df1 <- data.frame(ED(fit1, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit1)[[1]]) 
      drc_df2 <- data.frame(ED(fit2, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit2)[[1]])
      drc_df3 <- data.frame(ED(fit3, c(XX),interval = "delta",display=FALSE),"Slope"=coefficients(fit3)[[1]]) 
      colnames(drc_df1) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df2) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      colnames(drc_df3) <- c(paste0('EC',XX), 'Standard Error', 'Lower 95%CI', 'Upper 95%CI','Slope')
      drc_df <- rbind(drc_df1,drc_df2, drc_df3)
      rownames(drc_df) <- c('alamarBlue','CFDA-AM','Neutral Red')
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
      par(mar=c(5,8,2,2))
      plot(fitmodel(), log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Growth rate (/d)",
             cex=2,cex.axis =2, cex.lab=2)
      }
    else if(intest_type() == 'TG202') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      par(mar=c(5,8,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Immobility", 
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#D55E00",cex=2,cex.axis =2, cex.lab=2)
      legend("topleft",inset=0.05, legend = c("24 h","48 h"), col = c("black","#D55E00"), lty = c("dotted","solid"),cex=2)
    }
    else if(intest_type() == 'TG203') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      fit4 <- fit$fit4
      par(mar=c(5,8,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Mortality", 
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#0072B2",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit3, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), lty="dotted", col="tomato",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit4, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="black",cex=2,cex.axis =2, cex.lab=2,lwd=1.3)
      legend("topleft",inset=0.05, legend = c("24 h","48 h","72 h","96 h"), col = c("black","#0072B2","tomato","black"), lty = c("dotted","solid","dotted","solid"),cex=2)
    }
      else if(intest_type() == 'TG218') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      par(mar=c(5,5,2,2), mfrow=c(3,1),mgp=c(2.5, 0.7, 0))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Mortality",
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      plot(fit2, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Emergence ratio",
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      plot(fit3, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Development rate",
           lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      }
    else if(intest_type() == 'TG221') {
      par(mar=c(5,8,2,2))
      plot(fitmodel(), log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Growth rate (/d)",
           cex=2,cex.axis =2, cex.lab=2)
    }
      else if(intest_type() == 'TG235') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      par(mar=c(5,8,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Immobility",
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#D55E00",cex=2,cex.axis =2, cex.lab=2)
      legend("topleft",inset=0.05, legend = c("24 h","48 h"), col = c("black","#D55E00"), lty = c("dotted","solid"),cex=2)
    }
      else if(intest_type() == 'TG236') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      fit4 <- fit$fit4
      par(mar=c(5,8,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Mortality", 
           ylim=c(0,1),lty="dotted",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="#0072B2",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit3, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), lty="dotted", col="tomato",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit4, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), col="black",cex=2,cex.axis =2, cex.lab=2,lwd=1.3)
      legend("topleft",inset=0.05, legend = c("24 h","48 h","72 h","96 h"), col = c("black","#0072B2","tomato","black"), lty = c("dotted","solid","dotted","solid"),cex=2)
    }
    else if(intest_type() == 'TG249') {
      fit <- fitmodel()
      fit1 <- fit$fit1
      fit2 <- fit$fit2
      fit3 <- fit$fit3
      par(mar=c(5,8,2,2))
      plot(fit1, log="x", broken=TRUE, xlab=paste0("Concentration (", input$conc_unit, ")"), ylab="Cell viability", 
           ylim=c(0,1),lty="solid",col="#0072B2",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit2, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), lty="dotted",col="#009E73",cex=2,cex.axis =2, cex.lab=2)
      par(new=TRUE)
      plot(fit3, log="x", broken=TRUE, xlab="", ylab="", main="",
           ylim=c(0,1), lty="solid", col="#D55E00",cex=2,cex.axis =2, cex.lab=2)
      legend("bottomleft",inset=0.05, legend = c("alamarBlue","CFDA-AM","Neutral Red"), col = c("#0072B2","#009E73","#D55E00"), lty = c("solid","dotted","solid"),cex=2)
    }
    })

  
  
  
  ####################################################################################
  #######  hypothesis testing ########################################################
  ####################################################################################
  
   inmethod_201 <- reactive({input$test_method_TG201})
   inmethod_203 <- reactive({input$test_method_TG203})
   inmethod_218_mortality <- reactive({input$test_method_TG218_mortality})
   inmethod_218_emergence <- reactive({input$test_method_TG218_emergence})
   inmethod_218_development <- reactive({input$test_method_TG218_development})
   inmethod_221 <- reactive({input$test_method_TG221})
   inmethod_235 <- reactive({input$test_method_TG235})
   inmethod_236 <- reactive({input$test_method_TG236})
   inmethod_249 <- reactive({input$test_method_TG249})
    
   # function of Steel's test, taken from https://www.trifields.jp/introducing-steel-in-r-1637
   steel.test <- function(x, ...) UseMethod("steel.test")
    
    steel.test.default <- function(x, g, control = NULL, alternative = c("two.sided", "less", "greater"), ...){
      alternative <- match.arg(alternative)
      if (is.list(x)) {
        if (length(x) < 2L)
          stop("'x' must be a list with at least 2 elements")
        if (!missing(g))
          warning("'x' is a list, so ignoring argument 'g'")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
                    if (!all(sapply(x, is.numeric)))
                      warning("some elements of 'x' are not numeric and will be coerced to numeric")
                    k <- length(x)
                    l <- sapply(x, "length")
      if (any(l == 0L))
        stop("all groups must contain data")
      g <- factor(rep.int(seq_len(k), l))
      x <- unlist(x)
    }
    else {
      if (length(x) != length(g))
        stop("'x' and 'g' must have the same length")
      DNAME <- paste(deparse(substitute(x)), "and",
                     deparse(substitute(g)))
      OK <- complete.cases(x, g)
      x <- x[OK]
      g <- g[OK]
      if (!all(is.finite(g)))
        stop("all group levels must be finite")
      g <- factor(g)
      k <- nlevels(g)
      if (k < 2L)
        stop("all observations are in the same group")
    }
    
    if (is.null(control)) {
      control <- levels(g)[1]
    }
    if (!any(levels(g) == control)) {
      stop("The dataset doesn't contain this control group!")
    }
    
    # calculate ρ
    get.rho <- function(ni)
    {
      l <- length(ni)
      rho <- outer(ni, ni, function(x, y) { sqrt(x/(x+ni[1])*y/(y+ni[1])) })
      diag(rho) <- 0
      return(sum(rho[-1, -1]) / (l - 2) / (l - 1))
    }
    ## number of data in each group
    ni <- table(g)
    ## number of group
    a <- length(ni)
    ## data of control group
    xc <- x[g == control]
    ## number of data in control group
    n1 <- length(xc)
    ## decide ρ
    rho <- ifelse(sum(n1 == ni) == a, 0.5, get.rho(ni))
    
    vc <- c()
    vt <- c()
    vp <- c()
    
    for (i in levels(g)) {
      if(control == i) {
        next
      }
      ## ranking group i,j
      r <- rank(c(xc, x[g == i]))
      ## test statistic
      R <- sum(r[1 : n1])
      ## total number of the 2 group data
      N <- n1 + ni[i]
      ## expectation of test statistic
      E <- n1 * (N + 1) / 2
      ## variance of test statistic
      V <- n1 * ni[i] / N / (N - 1) * (sum(r^2) - N * (N + 1)^2 / 4)
      ## t.value
      t <- (R - E) / sqrt(V)
      
      # calculate p.value
      corr <- diag(a - 1)
      corr[lower.tri(corr)] <- rho
      pmvt.lower <- -Inf
      pmvt.upper <- Inf
      if (alternative == "less") {
        pmvt.lower <- -t
        pmvt.upper <- Inf
      }
      else if (alternative == "greater") {
        pmvt.lower <- t
        pmvt.upper <- Inf
      }
      else {
        t <- abs(t)
        pmvt.lower <- -t
        pmvt.upper <- t
      }
      p <- 1 - pmvt(lower = pmvt.lower, upper = pmvt.upper, delta = numeric(a - 1), df = 0, corr = corr, abseps = 0.0001)
      
      vc <- c(vc, paste(i, control, sep = ':'))
      vt <- c(vt, t)
      vp <- c(vp, p)
    }
    df <- data.frame(comparison = vc,
                     t.value = vt,
                     rho = rho,
                     p.value = vp)
    rownames(df) <- NULL
    return(df)
  }
steel.test.formula <-
  function(formula, data, subset, na.action, ...)   {
    if(missing(formula) || (length(formula) != 3L))
      stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame()) 
    if(length(mf) > 2L)
      stop("'formula' should be of the form response ~ group")
    names(mf) <- NULL
    y <- do.call("steel.test", append(as.list(mf), list(...)))
    y
  }
                    

    

    
    TestResult <- eventReactive(input$buttonRunStep1,{
      if(intest_type() == 'TG201') {
        growth_df <- filedata() %>% mutate( gr0to24 = log(H24 / H0),
                                            gr24to48 = log(H48 / H24),
                                            gr48to72 = log(H72 / H48),
                                            gr0to72 = log(H72 / H0)/(3),
                                            gr24to72 = log(H72 / H24)/(2) )
        growth_df$CONC <- as.factor(growth_df$CONC)
        Res_variance <- bartlett.test(gr0to72~CONC, data=growth_df)
        Note<- c("If p < 0.05, the assumption that variances are equal across groups is rejected. Steel's test is recommended")
        if ( inmethod_201() =="Dunnett"){
          fit <- aov( gr0to72~CONC, data = growth_df )
          Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
          list("Bartlett's test for growth rate" = Res_variance, "Note for Bartlett's test" = Note,
               "Dunnett's test for growth rate" = Res)
        } else if ( inmethod_201() =="Steel"){
          Res <- steel.test(gr0to72 ~ CONC, data = growth_df, control = "0", alternative="less") %>%
            mutate(Asterisk = ifelse(p.value<0.05,ifelse(p.value>0.01,"*","**"),"" ))        
          list("Bartlett's test for growth rate" = Res_variance, "Note for Bartlett's test" = Note,
               "Steel's test for growth rate" = Res)
        }
        } 
      else if(intest_type() == "TG203") {
          if ( inmethod_203() =="Fisher"){
            data=filedata() %>% group_by(CONC,TIME) %>%
              summarize(TOTAL=sum(TOTAL),DEAD=sum(DEAD)) %>% ungroup
            TOTAL_24 <- data %>% dplyr::filter(TIME=="24" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            TOTAL_48 <- data %>% dplyr::filter(TIME=="48" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            TOTAL_72 <- data %>% dplyr::filter(TIME=="72" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            TOTAL_96 <- data %>% dplyr::filter(TIME=="96" & CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
            DEAD_24 <- data %>% dplyr::filter(TIME=="24" & CONC=="0") %>% dplyr::select(DEAD) %>% as.numeric()
            DEAD_48 <- data %>% dplyr::filter(TIME=="48" & CONC=="0") %>% dplyr::select(DEAD) %>% as.numeric()
            DEAD_72 <- data %>% dplyr::filter(TIME=="72" & CONC=="0") %>% dplyr::select(DEAD) %>% as.numeric()
            DEAD_96 <- data %>% dplyr::filter(TIME=="96" & CONC=="0") %>% dplyr::select(DEAD) %>% as.numeric()
            data_24 <- data %>% mutate(TOTAL_ctrl = TOTAL_24, DEAD_ctrl =DEAD_24) %>%
              dplyr::filter(CONC!="0" & TIME=="24")
            data_48 <- data %>% mutate(TOTAL_ctrl = TOTAL_48, DEAD_ctrl =DEAD_48) %>%
              dplyr::filter(CONC!="0" & TIME=="48")
            data_72 <- data %>% mutate(TOTAL_ctrl = TOTAL_72, DEAD_ctrl =DEAD_72) %>%
              dplyr::filter(CONC!="0" & TIME=="72")
            data_96 <- data %>% mutate(TOTAL_ctrl = TOTAL_96, DEAD_ctrl =DEAD_96) %>%
              dplyr::filter(CONC!="0" & TIME=="96")
            fisher <- function(a,b,c,d){
              dt <- matrix(c(a,b,c,d),ncol=2)
              c(pvalue = fisher.test(dt)$p.value) 
            }
            Res1 <- data_24 %>%
              rowwise()%>%
              mutate(pvalue = fisher(DEAD,TOTAL-DEAD, DEAD_ctrl,TOTAL_ctrl-DEAD_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))
            Res2 <- data_48 %>%
              rowwise()%>%
              mutate(pvalue = fisher(DEAD,TOTAL-DEAD, DEAD_ctrl,TOTAL_ctrl-DEAD_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))    
            Res3 <- data_72 %>%
              rowwise()%>%
              mutate(pvalue = fisher(DEAD,TOTAL-DEAD, DEAD_ctrl,TOTAL_ctrl-DEAD_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))    
            Res4 <- data_96 %>%
              rowwise()%>%
              mutate(pvalue = fisher(DEAD,TOTAL-DEAD, DEAD_ctrl,TOTAL_ctrl-DEAD_ctrl)) %>% ungroup() %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))    
            list("24 h" = knitr::kable(Res1), "48 h" = knitr::kable(Res2),"72 h" = knitr::kable(Res3), "96 h" = knitr::kable(Res4) )
            }
        }
      else if(intest_type() == 'TG218'){
          data=filedata()
          data$CONC <- as.factor(data$CONC)
          Res_variance <- bartlett.test(DEVELOPMENT~CONC, data=data)
          Note<- c("If p < 0.05, the assumption that variances are equal across groups is rejected. Steel's test is recommended")
          if( inmethod_218_mortality() =="CA"){
            data_CA <- data %>% group_by(CONC) %>% summarize(DEAD= sum(DEAD),TOTAL=sum(TOTAL))  
            Res1 <- data.frame(CONC = data_CA$CONC, TOTAL=data_CA$TOTAL, DEAD=data_CA$DEAD)
            LENGTH <- nrow(data_CA)
          　for (i in 3:LENGTH){
　　　　　　　 Res1[i,4]<-  prop.trend.test(data_CA[1:i,]$DEAD, data_CA[1:i,]$TOTAL)$statistic
               Res1[i,5]<-  prop.trend.test(data_CA[1:i,]$DEAD, data_CA[1:i,]$TOTAL)$p.value
               }
            colnames(Res1) <-c("CONC","TOTAL","DEAD","Chi_squared","pvalue")
            Res1 <- Res1 %>%
                  mutate(Asterisk=  ifelse(pvalue<0.05,ifelse(pvalue>0.01,"*","**"),"" )) 
            } else if ( inmethod_218_mortality() =="Fisher"){
              data_F =filedata() %>% group_by(CONC) %>%
              summarize(TOTAL=sum(TOTAL),DEAD=sum(DEAD)) %>% ungroup
              TOTAL <- data_F  %>% dplyr::filter(CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
              DEAD_ctrl <- data_F %>% dplyr::filter(CONC=="0") %>% dplyr::select(DEAD) %>% as.numeric()
              data2 <- data_F %>% mutate(TOTAL_ctrl = TOTAL, DEAD_ctrl =DEAD_ctrl) %>%  dplyr::filter(CONC!="0")
              ## Fisher's exact test                  
              fisher <- function(a,b,c,d){
                dt <- matrix(c(a,b,c,d),ncol=2)
                c(pvalue = fisher.test(dt)$p.value) 
              }
              Res1 <- data2 %>%
                rowwise()%>%
                mutate(pvalue = fisher(DEAD,TOTAL-DEAD, DEAD_ctrl,TOTAL_ctrl-DEAD_ctrl)) %>% ungroup() %>%
                mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
                mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))
          }
          if ( inmethod_218_emergence() =="CA"){
                data_CA <- data %>% group_by(CONC) %>% summarize(EMERGED= sum(EMERGED),TOTAL=sum(TOTAL))  
                Res2 <- data.frame(CONC = data_CA$CONC, TOTAL=data_CA$TOTAL, EMERGED=data_CA$EMERGED)
                LENGTH <- nrow(data_CA)
          　　  for (i in 3:LENGTH){ 
　　　　　　　　　　 Res2[i,4]<-  prop.trend.test(data_CA[1:i,]$EMERGED, data_CA[1:i,]$TOTAL)$statistic
                   Res2[i,5]<-  prop.trend.test(data_CA[1:i,]$EMERGED, data_CA[1:i,]$TOTAL)$p.value
                  　}
                colnames(Res2) <-c("CONC","TOTAL","EMERGED","Chi_squared","pvalue")
                Res2 <- Res2 %>%
                  mutate(Asterisk=  ifelse(pvalue<0.05,ifelse(pvalue>0.01,"*","**"),"" )) 
                } else if ( inmethod_218_emergence() =="Fisher"){
              data_F_emer =filedata() %>% group_by(CONC) %>%
                summarize(TOTAL=sum(TOTAL),EMERGED=sum(EMERGED)) %>% ungroup
              TOTAL <- data_F_emer %>% dplyr::filter(CONC=="0") %>% dplyr::select(TOTAL) %>% as.numeric()
              EMERGED_ctrl <- data_F_emer %>% dplyr::filter(CONC=="0") %>% dplyr::select(EMERGED) %>% as.numeric()
              data2_emer <- data_F_emer %>% mutate(TOTAL_ctrl = TOTAL, EMER_ctrl =EMERGED_ctrl) %>%  dplyr::filter(CONC!="0")
              ## Fisher's exact test                  
              fisher <- function(a,b,c,d){
                dt <- matrix(c(a,b,c,d),ncol=2)
                c(pvalue = fisher.test(dt)$p.value) 
              }
              Res2 <- data2_emer %>%
                rowwise()%>%
                mutate(pvalue = fisher(EMERGED,TOTAL-EMERGED, EMER_ctrl,TOTAL_ctrl-EMER_ctrl)) %>% ungroup() %>%
                mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
                mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))
              }
         if ( inmethod_218_development() =="Dunnett"){
            data_raw=filedata()
            data_raw$CONC <- as.factor(data_raw$CONC)
            fit <- aov( DEVELOPMENT ~ CONC, data = data_raw  )
            Res3 <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
            } else if ( inmethod_218_development() =="Steel"){
            data_raw=filedata()
            data_raw$CONC <- as.factor(data_raw$CONC)
            Res3 <- steel.test(DEVELOPMENT ~ CONC, data = data_raw, control = "0",alternative="less") %>%
              mutate(Asterisk = ifelse(p.value<0.05,ifelse(p.value>0.01,"*","**"),"" ))        
            }
          list("Mortality" = knitr::kable(Res1), "Emergence ratio" = knitr::kable(Res2), 
               "Bartlett's test for development rate (DR)" = Res_variance, "Note for Bartlett's test"=Note,"Development rate" = Res3 )
          }
      else if(intest_type() == 'TG221') {
            growth_df <- filedata() %>% mutate( gr0to3 = log(D3 / D0)/(3),
                                            gr3to5 = log(D5 / D3)/(2),
                                            gr5to7 = log(D7 / D5)/(2),
                                            gr0to7 = log(D7 / D0)/(7),
                                            gr3to7 = log(D7 / D3)/(4) )
            growth_df$CONC <- as.factor(growth_df$CONC)
            Res_variance <- bartlett.test(gr0to7~CONC, data=growth_df)
            Note<- c("If p < 0.05, the assumption that variances are equal across groups is rejected. Steel's test is recommended")
            if ( inmethod_221() =="Dunnett"){
              fit <- aov( gr0to7~CONC, data = growth_df )
              Res <- summary (glht (fit, linfct=mcp (CONC="Dunnett"), alternative="less")) 
              list("Bartlett's test for growth rate" = Res_variance, "Note for Bartlett's test" = Note,
               "Dunnett's test for growth rate" = Res)
              } else if ( inmethod_221() =="Steel"){
                Res <- steel.test(gr0to7 ~ CONC, data = growth_df, control = "0", alternative="less") %>%
                  mutate(Asterisk = ifelse(p.value<0.05,ifelse(p.value>0.01,"*","**"),"" ))        
                list("Bartlett's test for growth rate" = Res_variance, "Note for Bartlett's test" = Note,
               "Steel's test for growth rate" = Res)
        }
      } 
      else if(intest_type() == 'TG235'){
          data=filedata()
          data$CONC <- as.factor(data$CONC)
          if ( inmethod_235() =="Fisher"){
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
            ## Fisher's exact test                  
            fisher <- function(a,b,c,d){
              dt <- matrix(c(a,b,c,d),ncol=2)
              c(pvalue = fisher.test(dt)$p.value) 
            }
            Res1 <- data_24 %>%
              rowwise()%>%
              mutate(pvalue = fisher(IMMOBILIZED,TOTAL-IMMOBILIZED, IMMOBILIZED_ctrl,TOTAL_ctrl-IMMOBILIZED_ctrl)) %>% ungroup() %>%
              dplyr::rename(IMMOB =IMMOBILIZED, IMMOB_ctrl = IMMOBILIZED_ctrl) %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))
            Res2 <- data_48 %>%
              rowwise()%>%
              mutate(pvalue = fisher(IMMOBILIZED,TOTAL-IMMOBILIZED, IMMOBILIZED_ctrl,TOTAL_ctrl-IMMOBILIZED_ctrl)) %>% ungroup() %>%
              dplyr::rename(IMMOB =IMMOBILIZED, IMMOB_ctrl = IMMOBILIZED_ctrl) %>%
              mutate(p_adjusted = p.adjust(pvalue,"holm")) %>%
              mutate(Asterisk = ifelse(p_adjusted<0.05,ifelse(p_adjusted>0.01,"*","**"),"" ))    
            list("Fisher's exact test for 24 h" = knitr::kable(Res1),"Fisher's exact test for 48 h" = knitr::kable(Res2))
            }
          }
      else if(intest_type() == 'TG249'){
        if ( inmethod_249() =="Dunnett"){
          alamarBlue_bg <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          CDFAAM_bg <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          NeutralRed_bg <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC=="CellFree") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          alamarBlue_ctrl <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          CDFAAM_ctrl <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector
          NeutralRed_ctrl <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC=="0") %>% summarize(Mean=mean(FLUORESCENCE)) %>% as.vector 
          alamarBlue <- filedata() %>% dplyr::filter(DYE=="alamarBlue" & CONC!="CellFree") %>% mutate(FLUORESCENCE = (FLUORESCENCE- alamarBlue_bg[[1]])/alamarBlue_ctrl[[1]])
          CDFAAM <- filedata() %>% dplyr::filter(DYE=="CDFAAM" & CONC!="CellFree")  %>% mutate(FLUORESCENCE = (FLUORESCENCE- CDFAAM_bg[[1]])/CDFAAM_ctrl[[1]])
          NeutralRed <- filedata() %>% dplyr::filter(DYE=="NeutralRed" & CONC!="CellFree")  %>% mutate(FLUORESCENCE = (FLUORESCENCE- NeutralRed_bg[[1]])/NeutralRed_ctrl[[1]])
          alamarBlue$CONC <- as.factor(as.numeric(alamarBlue$CONC) )
          CDFAAM$CONC <- as.factor(as.numeric(CDFAAM$CONC))
          NeutralRed$CONC <- as.factor(as.numeric(NeutralRed$CONC))
            fit1 <- aov( FLUORESCENCE ~ CONC, data = alamarBlue  )
            fit2 <- aov( FLUORESCENCE ~ CONC, data = CDFAAM )
            fit3 <- aov( FLUORESCENCE ~ CONC, data = NeutralRed  )
            Res1 <- summary (glht (fit1, linfct=mcp (CONC="Dunnett"), alternative="less"))  
            Res2 <- summary (glht (fit2, linfct=mcp (CONC="Dunnett"), alternative="less"))  
            Res3 <- summary (glht (fit3, linfct=mcp (CONC="Dunnett"), alternative="less"))  
            list("Dunnett's test for alamarBlue" = Res1,"Dunnett's test for CFDA-FA" = Res2,
                 "Dunnett's test for Neutral Red" = Res3 )
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
      src <- base::normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      out <- rmarkdown::render('report.Rmd',  switch(
        input$format,
      #PDF = pdf_document(), 
        Word = word_document()
      ))
      file.rename(out, file)
    }
    )



    
}
                                
