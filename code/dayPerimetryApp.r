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
library(callr)

source("settings.r")
source("gamma.r")
source("patients.r")
source("test.r")
source("report.r")
source("zesttests.r")
source("pdfReport.r")

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
  # global variables
  assign("settingsChanged",  reactiveVal(FALSE), envir = globalenv())
  assign("patientChanged",   reactiveVal(FALSE), envir = globalenv())
  assign("patientdbChanged", reactiveVal(FALSE), envir = globalenv())
  assign("opiInitialized",   reactiveVal(FALSE), envir = globalenv())
  assign("newReports",       reactiveVal(FALSE), envir = globalenv())
  # check if global paramerter for daydream exist
  if(!file.exists("dayParams.rda"))
    stop("please create file dayParams.rda to start using the application")
  # load global parameters dayParams
  load("dayParams.rda", envir = globalenv())
  # record structure where to store patient id, name, surname, and eye to test
  assign("patient", list(id = NA, name = NA, surname = NA, age = NA, gender = NA, type = NA), envir = globalenv())
  invisible(chooseOpi("Daydream")) # choose the Daydream as the OPI implementation
  # render all pages
  settingsPage <- renderUI({settingsUI("settings")})
  gammaPage    <- renderUI({gammaUI("gamma")})
  patientsPage <- renderUI({patientsUI("patients")})
  testPage     <- renderUI({testUI("test")})
  reportPage   <- renderUI({reportUI("report")})
  # start the modules
  callModule(settings, "settings", session = session)
  callModule(gamma,    "gamma",    session = session)
  callModule(patients, "patients", session = session)
  callModule(test,     "test",     session = session)
  callModule(report,   "report",   session = session)
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
      load(paste0(dayParams$dbPath, "patientdb.rda"), envir = globalenv())
      load(paste0(dayParams$confPath, "grids.rda"), envir = globalenv())
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