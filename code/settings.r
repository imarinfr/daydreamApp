settingsUI <- function(id) {
  # init gamma file choices and selection
  choices  <- dir(dayParams$confPath, pattern = "*.csv")
  selected <- ""
  if(length(choices) == 0) choices  <- "-"
  else if(dayParams$gammaFile %in% choices) selected <- dayParams$gammaFile
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, textInput(ns("serverIP"), "Daydream IP", dayParams$serverIP)),
      column(4, textInput(ns("serverPort"), "Daydream port", dayParams$serverPort)),
      column(4, textInput(ns("testPort"), "Test port", dayParams$testPort))
    ),
    fluidRow(
      column(4, textInput(ns("dbPath"), "Database Path", dayParams$dbPath)),
      column(4, textInput(ns("resPath"), "Results path", dayParams$resPath)),
      column(4, textInput(ns("confPath"), "Configuration path", dayParams$confPath)),
    ),
    fluidRow(
      br(), br(),
      column(3, numericInput(ns("fovy"), "Field of view Y:", dayParams$fovy)),
      column(3, selectInput(ns("color"), "Stimulus color:", choices = c("white", "green", "red", "blue"), selected = dayParams$color)),
      column(3, numericInput(ns("bg"), "Background (cd/m2)", dayParams$bg)),
      column(3, selectInput(ns("gammaFile"), "Gamma file:", choices = choices, selected = selected))
    ),
    fluidRow(
      column(3, numericInput(ns("minlum"), "Min lum (cd/m2)",  dayParams$minlum)),
      column(3, numericInput(ns("maxlum"), "Max lum (cd/m2)",  dayParams$maxlum)),
      column(3, numericInput(ns("minarea"), "Min area (deg2)", dayParams$minarea)),
      column(3, numericInput(ns("maxarea"), "Max area (deg2)", dayParams$maxarea))
    ),
    fluidRow(
      br(), br(),
      column(3, numericInput(ns("presTime"), "Pres time (ms):", dayParams$presTime)),
      column(3, numericInput(ns("respWindow"), "Resp window (ms):", dayParams$respWindow)),
      column(3, numericInput(ns("respWinPed"), "Resp pedestal (ms):", dayParams$respWinPed)),
      column(3, numericInput(ns("respTimesLength"), "Resp list length", dayParams$respTimesLength)),
    ),
    fluidRow(
      column(3, numericInput(ns("minISI"), "min ISI (ms):", dayParams$minISI)),
      column(3, numericInput(ns("fprate"), "FP catch rate", dayParams$fprate)),
      column(3, numericInput(ns("fnrate"), "FN catch rate", dayParams$fnrate)),
      column(3, numericInput(ns("refresh"), "Refresh rate (Hz)", dayParams$refresh))
    ),
    fluidRow(
      column(12, radioButtons(ns("runType"), "Run type", c("Luminance perimetry", "Size perimetry", "Simulation"), dayParams$runType, inline = TRUE))
    ),
    fluidRow(
      br(), br(),
      column(3, actionButton(ns("saveSettings"), "Save settings"), offset=3),
      column(3, actionButton(ns("loadSettings"), "Load settings"))
    )
  )
}

settings <- function(input, output, session) {
  ####################
  # EVENTS
  ####################
  # if any input has changed, then update dayParams
  lapply(names(dayParams), function(par) {
    observeEvent(input[[par]], {
      dayParams[[par]] <<- input[[par]]
      settingsChanged(FALSE)
      settingsChanged(TRUE)
    }, ignoreInit = TRUE)
  })
  # check if input OK or not
  observeEvent(settingsChanged(), {
    if(!file.exists(paste0(dayParams$dbPath, "patientdb.rda")) |
       !file.exists(paste0(dayParams$confPath, "default.csv")) |
       !file.exists(paste0(dayParams$confPath, "grids.rda"))   |
       !dir.exists(dayParams$resPath))
      errorMessage("Make sure the three folders exist and that there is a file called 'patientdb.rda' in the db path and two called 'default.csv' and 'grids.rda' in the configuration path")
  })
  # update gamma files for selection
  observeEvent(input$confPath, {
    choices  <- dir(dayParams$confPath, pattern = "*.csv")
    selected <- ""
    if(length(choices) == 0) choices  <- "-"
    else if(dayParams$gammaFile %in% choices) selected <- dayParams$gammaFile
    updateSelectInput(session, "gammaFile", choices = choices, selected = selected)
  })
  # save default values
  observeEvent(input$saveSettings, {
    save(dayParams, file = "dayParams.rda")
  }, ignoreInit = TRUE)
  # load default values
  observeEvent(input$loadSettings, {
    load("dayParams.rda", envir = globalenv())
    populateDefaults(session)
  }, ignoreInit = TRUE)
}
####################
# ROUTINES
####################
# error message
errorMessage <- function(txt) {
  showModal(modalDialog(
    title = HTML("<span style = 'color:red'>Error Message</span>"),
    HTML(paste0("<span style = 'color:red'>", txt, "</span>")),
    easyClose = TRUE))
}
# populate screen fields with saved parameter values
populateDefaults <- function(session) {
  updateTextInput(session,    "dbPath",          value    = dayParams$dbPath)
  updateTextInput(session,    "resPath",         value    = dayParams$resPath)
  updateTextInput(session,    "confPath",        value    = dayParams$confPath)
  updateTextInput(session,    "gammaFile",       value    = dayParams$gammaFile)
  updateTextInput(session,    "serverIP",        value    = dayParams$serverIP)
  updateTextInput(session,    "serverPort",      value    = dayParams$serverPort)
  updateTextInput(session,    "testPort",        value    = dayParams$testPort)
  updateNumericInput(session, "fovy",            value    = dayParams$fovy)
  updateSelectInput(session,  "color",           selected = dayParams$color)
  updateRadioButtons(session, "runType",         selected = dayParams$runType)
  updateNumericInput(session, "bg",              value    = dayParams$bg)
  updateNumericInput(session, "minlum",          value    = dayParams$minlum)
  updateNumericInput(session, "maxlum",          value    = dayParams$maxlum)
  updateNumericInput(session, "minarea",         value    = dayParams$minarea)
  updateNumericInput(session, "maxarea",         value    = dayParams$maxarea)
  updateNumericInput(session, "maxPres",         value    = dayParams$maxPres)
  updateSelectInput(session,  "stopType",        selected = dayParams$stopType)
  updateNumericInput(session, "stopValue",       value    = dayParams$stopValue)
  updateNumericInput(session, "presTime",        value    = dayParams$presTime)
  updateNumericInput(session, "respWindow",      value    = dayParams$respWindow)
  updateNumericInput(session, "respWinPed",      value    = dayParams$respWinPed)
  updateNumericInput(session, "respTimesLength", value    = dayParams$respTimesLength)
  updateNumericInput(session, "minISI",          value    = dayParams$minISI)
  updateNumericInput(session, "fprate",          value    = dayParams$fprate)
  updateNumericInput(session, "fnrate",          value    = dayParams$fnrate)
  updateNumericInput(session, "refresh",         value    = dayParams$refresh)
}