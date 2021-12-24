library(OPI)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(rhandsontable)
library(DT)
library(plotrix)
library(deldir)
library(sp)
library(rgeos)
library(txtq)
library(callr)

source("settings.r",  local = TRUE)
source("gamma.r",     local = TRUE)
source("patients.r",  local = TRUE)
source("test.r",      local = TRUE)
source("report.r",    local = TRUE)
source("zesttests.r", local = TRUE)
source("pdfReport.r", local = TRUE)

ShinySender   <- txtq(tempfile())  # Messages from GUI to test
ShinyReceiver <- txtq(tempfile())  # Messages from test to GUI

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
  envir <- environment()
  # global variables
  assign("settingsChanged",  reactiveVal(FALSE))
  assign("patientChanged",   reactiveVal(FALSE))
  assign("patientdbChanged", reactiveVal(FALSE))
  assign("opiInitialized",   reactiveVal(FALSE))
  assign("newReports",       reactiveVal(FALSE))
  # load global parameters dayParams, then the patient db, then the grids
  if(!file.exists("dayParams.rda"))
    stop("please create file dayParams.rda to start using the application")
  load("dayParams.rda")
  if(!file.exists(paste0(dayParams$dbPath, "patientdb.rda")))
    stop("please create file patientdb.rda to start using the application")
  load(paste0(dayParams$dbPath, "patientdb.rda"))
  if(!file.exists(paste0(dayParams$confPath, "grids.rda")))
    stop("please create file grids.rda to start using the application")
  load(paste0(dayParams$confPath, "grids.rda"))
  # record structure where to store patient id, name, surname, and eye to test
  patient <- list(id = NA, name = NA, surname = NA, age = NA, gender = NA, type = NA)
  invisible(chooseOpi("Daydream")) # choose the Daydream as the OPI implementation
  # render all pages
  settingsPage <- renderUI({settingsUI("settings", envir = envir)})
  gammaPage    <- renderUI({gammaUI("gamma", envir = envir)})
  patientsPage <- renderUI({patientsUI("patients")})
  testPage     <- renderUI({testUI("test", envir = envir)})
  reportPage   <- renderUI({reportUI("report")})
  # start the modules
  callModule(settings, "settings", envir = envir)
  callModule(gamma,    "gamma",    envir = envir)
  callModule(patients, "patients", envir = envir)
  callModule(test,     "test",     envir = envir)
  callModule(report,   "report",   envir = envir)
  browsePage <- "patientsPage"
  output$tab <- patientsPage
  ####################
  # EVENTS
  ####################
  # check setting parameters have changed
  observeEvent(settingsChanged(), {
    if(!file.exists(paste0(dayParams$dbPath, "patientdb.rda")) |
       !file.exists(paste0(dayParams$confPath, "default.csv")) |
       !file.exists(paste0(dayParams$confPath, "grids.rda"))   |
       !dir.exists(dayParams$resPath)) {
      browsePage <<- "settingsPage"
      output$tab <<- settingsPage
      disableAll()
    } else {
      enableAll()
      if(browsePage == "settingsPage") disable("settingsbtn")
      if(browsePage == "patientsPage") disable("patientsbtn")
      load(paste0(dayParams$dbPath, "patientdb.rda"))
      load(paste0(dayParams$confPath, "grids.rda"))
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