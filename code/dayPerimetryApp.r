library(OPI)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(rhandsontable)
library(DT)
library(plotrix)
library(txtq)
library(future)
library(promises)
# prepare to run processes in parallel
plan(multisession)
# remove all threads at the end of all things
onStop(function() plan(sequential))

source("settings.r",  local = TRUE)
source("gamma.r",     local = TRUE)
source("patients.r",  local = TRUE)
source("test.r",      local = TRUE)
source("report.r",    local = TRUE)
source("pdfReport.r", local = TRUE)

# global variables
settingsChanged  <- reactiveVal(FALSE)
patientChanged   <- reactiveVal(FALSE)
patientdbChanged <- reactiveVal(FALSE)
opiInitialized   <- reactiveVal(FALSE)
newReports       <- reactiveVal(FALSE)
patient          <- list(id = NA, name = NA, surname = NA, age = NA, gender = NA, type = NA)
ShinySender      <- txtq(tempfile())  # Messages from GUI to test
ShinyReceiver    <- txtq(tempfile())  # Messages from test to GUI

# load global parameters dayParams, then the patient db, then the grids
if(!file.exists("../config/dayParams.rda"))
  stop("please create file dayParams.rda to start using the application")
load("../config/dayParams.rda")
if(!file.exists("../config/grids.rda"))
  stop("please create file grids.rda to start using the application")
load("../config/grids.rda")
if(!file.exists("../db/patientdb.rda"))
  stop("please create file patientdb.rda to start using the application")
load("../db/patientdb.rda")

ui <- dashboardPage(
  dashboardHeader(title = "Daydream perimetry"),
  dashboardSidebar(
    useShinyjs(),
    actionButton("settingsbtn", label = "Settings",       icon = icon("cog"),        width = "90%"),
    actionButton("gammabtn",    label = "Gamma function", icon = icon("chart-line"), width = "90%"),
    actionButton("patientsbtn", label = "Patients",       icon = icon("user"),       width = "90%", disabled = TRUE),
    actionButton("testbtn",     label = "Test",           icon = icon("sad-cry"),    width = "90%"),
    actionButton("reportbtn",   label = "Reports",        icon = icon("file-alt"),   width = "90%")
  ),
  dashboardBody(
    uiOutput("tab")
  )
)

server <- function(input, output, session) {
  invisible(chooseOpi("Daydream")) # choose the Daydream as the OPI implementation
  # render all pages
  settingsPage <- renderUI({settingsUI("settings")})
  gammaPage    <- renderUI({gammaUI("gamma")})
  patientsPage <- renderUI({patientsUI("patients")})
  testPage     <- renderUI({testUI("test")})
  reportPage   <- renderUI({reportUI("report")})
  # start the modules
  callModule(settings, "settings")
  callModule(gamma,    "gamma")
  callModule(patients, "patients")
  callModule(test,     "test")
  callModule(report,   "report")
  browsePage <- "patientsPage"
  output$tab <- patientsPage
  ####################
  # EVENTS
  ####################
  # check setting parameters have changed
  observeEvent(settingsChanged(), {
    if(!file.exists("../db/patientdb.rda") |
       !file.exists("../config/default.csv") |
       !file.exists("../config/grids.rda")   |
       !dir.exists(dayParams$resPath)) {
      browsePage <<- "settingsPage"
      output$tab <<- settingsPage
      disableAll()
    } else {
      enableAll()
      if(browsePage == "settingsPage") disable("settingsbtn")
      if(browsePage == "patientsPage") disable("patientsbtn")
      load("../db/patientdb.rda")
      load("../config/grids.rda")
      patientdbChanged(TRUE)
      newReports(TRUE)
    }
  })
  # if OPI is initialized, do not allow to browse anywhere
  observeEvent(opiInitialized(), {
    if(opiInitialized()) disableAll()
    else {
      enableAll()
      if(browsePage == "gammaPage") disable("gammabtn")
      if(browsePage == "testPage")  disable("testbtn")
    }
  }, ignoreInit = TRUE)
  # go to settings page
  observeEvent(input$settingsbtn, {
    browsePage <<- "settingsPage"
    output$tab <<- settingsPage
    disable("settingsbtn")
    lapply(c("gammabtn", "patientsbtn", "testbtn", "reportbtn"), enable)
  })
  # go to gamma function page
  observeEvent(input$gammabtn, {
    browsePage <<- "gammaPage"
    output$tab <<- gammaPage
    disable("gammabtn")
    lapply(c("settingsbtn", "patientsbtn", "testbtn", "reportbtn"), enable)
  })
  # go to patients page
  observeEvent(input$patientsbtn, {
    browsePage <<- "patientsPage"
    output$tab <<- patientsPage
    disable("patientsbtn")
    lapply(c("settingsbtn", "gammabtn", "testbtn", "reportbtn"), enable)
  })
  # go to test page
  observeEvent(input$testbtn, {
    browsePage <<- "testPage"
    output$tab <<- testPage
    disable("testbtn")
    lapply(c("settingsbtn", "gammabtn", "patientsbtn", "reportbtn"), enable)
  })
  # go to reports page
  observeEvent(input$reportbtn, {
    browsePage <<- "reportPage"
    output$tab <<- reportPage
    disable("reportbtn")
    lapply(c("settingsbtn", "gammabtn", "patientsbtn", "testbtn"), enable)
  })
  # close OPI server
  onSessionEnded(function() ShinySender$push(title = "CMD", message = "opiClose"))
}
####################
# ROUTINES
####################
# disable all buttons
disableAll <- function()
  lapply(c("settingsbtn", "gammabtn", "patientsbtn", "testbtn", "reportbtn"), disable)
# enable all buttons
enableAll <- function()
  lapply(c("settingsbtn", "gammabtn", "patientsbtn", "testbtn", "reportbtn"), enable)
# run app
shinyApp(ui, server)