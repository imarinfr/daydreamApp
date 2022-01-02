settingsUI <- function(id) {
  # init gamma file choices and selection
  choices  <- dir("../config/", pattern = "*.csv")
  selected <- ""
  if(length(choices) == 0) choices  <- "-"
  else if(dayParams$gammaFile %in% choices) selected <- dayParams$gammaFile
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Perimetry type"),
      column(4, selectInput(ns("machine"),  "OPI implementation", choices = c("Daydream", "Display", "SimHenson", "SimYes", "SimNo"), selected = dayParams$machine)),
      column(4, radioButtons(ns("runType"), "Perimetry type", c("luminance", "size"), dayParams$runType, inline = TRUE)),
      column(4, textInput(ns("resPath"),    "Results path", dayParams$resPath)),
    ),
    fluidRow(
      column(6,
        h3("Daydream parameters"),
        column(6, textInput(ns("serverIP"), "Daydream IP",      dayParams$serverIP)),
        column(6, numericInput(ns("fovy"),  "Field of view Y:", dayParams$fovy))
      ),
      column(6,
        h3("Display parameters"),
        column(4, numericInput(ns("dimx"),    "num pix x", "1024")),
        column(4, numericInput(ns("dimy"),    "num pix y", "800")),
        column(4, numericInput(ns("pixsize"), "pix size",  "1"))
      )
    ),
    fluidRow(
      h3("Stimulus parameters"),
      column(3, numericInput(ns("bg"),       "Background", dayParams$bg)),
      column(3, selectInput(ns("color"),     "Stimulus color:", choices = c("white", "green", "red", "blue"), selected = dayParams$color)),
      column(6, selectInput(ns("gammaFile"), "Gamma file:", choices = choices, selected = selected)),
    ),
    fluidRow(
      column(3, numericInput(ns("minlum"),  "Min luminance",  dayParams$minlum)),
      column(3, numericInput(ns("maxlum"),  "Max luminance",  dayParams$maxlum)),
      column(3, numericInput(ns("minarea"), "Min area",       dayParams$minarea)),
      column(3, numericInput(ns("maxarea"), "Max area",       dayParams$maxarea))
    ),
    h3("ZEST parameters"),
    fluidRow(
      column(3, numericInput(ns("presTime"),        "Pres time (ms):",     dayParams$presTime)),
      column(3, numericInput(ns("respWindow"),      "Resp window (ms):",   dayParams$respWindow)),
      column(3, numericInput(ns("respWinPed"),      "Resp pedestal (ms):", dayParams$respWinPed)),
      column(3, numericInput(ns("respTimesLength"), "Resp list length",    dayParams$respTimesLength)),
    ),
    fluidRow(
      column(3, numericInput(ns("minISI"), "min ISI (ms):", dayParams$minISI)),
      column(3, numericInput(ns("fprate"), "FP catch rate", dayParams$fprate)),
      column(3, numericInput(ns("fnrate"), "FN catch rate", dayParams$fnrate))
    ),
    fluidRow(
      column(3, actionButton(ns("saveSettings"), "Save settings"), offset = 3),
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
      if(substr(input[["machine"]], 1, 3) == "Sim")
        updateRadioButtons(session, "runType", selected = "luminance")
      dayParams[[par]] <<- input[[par]]
      settingsChanged(FALSE)
      settingsChanged(TRUE)
    }, ignoreInit = TRUE)
  })
  # check if input OK or not
  observeEvent(settingsChanged(), {
    if(!dir.exists(dayParams$resPath))
      errorMessage("Make sure the folder for the results exists")
    if(substr(dayParams$machine, 1, 3) == "Sim")
      dayParams$machine <- "Simulation"
  })
  # save default values
  observeEvent(input$saveSettings, {
    save(dayParams, file = "../config/dayParams.rda")
  }, ignoreInit = TRUE)
  # load default values
  observeEvent(input$loadSettings, {
    load("../config/dayParams.rda", envir = environment(server))
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
  updateTextInput(session,    "resPath",         value    = dayParams$resPath)
  updateTextInput(session,    "gammaFile",       value    = dayParams$gammaFile)
  updateTextInput(session,    "serverIP",        value    = dayParams$serverIP)
  updateNumericInput(session, "fovy",            value    = dayParams$fovy)
  updateSelectInput(session,  "color",           selected = dayParams$color)
  updateSelectInput(session,  "machine",         selected = dayParams$machine)
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
}