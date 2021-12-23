testUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, htmlOutput(ns("selected"))),
      column(8,
        fluidRow(
          column(6, textInput(ns("serverIP"), "Server IP", dayParams$serverIP)),
          if(dayParams$runType == "Luminance perimetry")
            column(6, selectInput(ns("size"), "Stimulus Size", choices = c("Size I", "Size II", "Size III", "Size IV", "Size V", "Size VI"), selected = "Size V"))
          else if(dayParams$runType == "Size perimetry")
            column(6, numericInput(ns("lum"), "Stimulus Luminance (cd/m2)", value = 100))
        ),
        fluidRow(
          column(6, selectInput(ns("eye"), "Eye", choices = c("OD", "OS"), selected = "OD")),
          column(6, selectInput(ns("grid"), "Grid", choices = gridNames, selected = gridNames[1]))
        )
      )
    ),
    fluidRow(column(12, htmlOutput(ns("msgconn")))),
    fluidRow(
      column(3, actionButton(ns("init"),  label = "Initialize OPI", width = "100%")),
      column(2, actionButton(ns("fovea"), label = "Test fovea", width = "100%", disabled = TRUE)),
      column(3, actionButton(ns("close"), label = "Close OPI", width = "100%",  disabled = TRUE)),
      column(4, align = "center",
        actionButton(ns("run"),   label = "Run", disabled = TRUE),
        actionButton(ns("pause"), label = "Pause", disabled = TRUE),
        actionButton(ns("stop"),  label = "Stop", disabled = TRUE)
      )
    ),
    p(),
    fluidRow(br(),
      column(8, plotOutput(ns("plotres"))),
      column(4, htmlOutput(ns("textres")))
    ),
    fluidRow(br(),
      column(8, textInput(ns("comments"), label = "Comments", width = "100%"))
    ),
    fluidRow(br(),
      column(4, disabled(actionButton(ns("save"), label = "Save", width = "100%"))),
      column(4, disabled(actionButton(ns("cancel"), label = "Cancel", width = "100%")))
    )
  )
}

test <- function(input, output, session) {
  debugrun   <- FALSE
  ns         <- session$ns
  rs         <- NA    # R session
  con        <- NA    # socket connection
  opiParams  <- NULL  # parameters to pass to OPI implementation
  size       <- 1.72  # stimulus size for Luminance perimetry
  lum        <- 400   # stimulus luminance for Size perimetry
  begin      <- TRUE  # whether the test is to start from beginning or after pause
  go         <- FALSE # keep going
  locs       <- NULL  # test locations
  foveadb    <- NULL  # sensitivity value obtained at the fovea
  listen     <- reactiveTimer(1000 / dayParams$refresh) # from Hz to ms
  msg        <- reactiveVal("Press 'Initialize OPI' to start") # message where to display status of connection, etc
  refreshout <- reactiveVal(TRUE) # refresh output graphs?
  # define routine to initialized all run control variables
  initRunVariables <- function() {
    res     <<- NULL        # keep results of run
    runlog  <<- NULL        # keep log of run
    tp0 <<- tt <<- tp <<- 0 # to control test time and pause
    tt0 <<- Sys.time()      # start test time is current system time
  }
  # then init all those variables
  initRunVariables()
  # outputs plot and results
  output$msgconn <- renderText(msg())
  ####################
  # OBSERVE
  ####################
  # if selected patient changed
  observe(
    if(patientChanged()) {
      patientChanged(FALSE)
      output$selected <- renderUI(parsePatientOutput())
    }
  )
  # if output data changed
  observe(
    if(refreshout()) {
      output$plotres <- renderPlot(showPlot(locs, input$eye, foveadb))
      output$textres <- renderUI(renderResult(locs, res, runlog))
      refreshout(FALSE)
    }
  )
  ####################
  # EVENTS
  ####################
  # if grid or eye changes
  observeEvent(input$grid, {
    locs <<- testLocations()
    initRunVariables()
    refreshout(TRUE)
  })
  observeEvent(input$eye, {
    locs    <<- testLocations()
    if(opiInitialized()) {
      eye <- ifelse(input$eye == "OD", "R", "L")
      rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg)))
    }
    foveadb <<- NA
    initRunVariables()
  })
  # get stimulus size in degrees of visual angle
  observeEvent(input$size, {
    stimArea  <- c(0.25, 1, 4, 16, 64, 256)
    stimAngle <- round(180 / pi * atan(2 * sqrt(stimArea / pi) / 300), 2)
    size <<- switch(input$size,
                    "Size I"   = stimAngle[1],
                    "Size II"  = stimAngle[2],
                    "Size III" = stimAngle[3],
                    "Size IV"  = stimAngle[4],
                    "Size V"   = stimAngle[5],
                    "Size VI"  = stimAngle[6])
  })
  # change stored luminance
  observeEvent(input$lum, lum <<- input$lum)
  # initialize OPI
  observeEvent(input$init, {
    if(is.na(patient$id)) errorMessage("Select a patient before proceeding")
    else {
      disableElements(c("init", "serverIP"))
      opiParams <<- fillOpiParams(input$serverIP)
      tryCatch({
        # start a new R session and change working directory to the same as this one
        if(!debugrun) {
          rs <<- r_session$new()
          invisible({
            # set working directory
            rs$run(setwd, args = list(dir = getwd()))
            # load the OPI library in background R session along with other required packages
            rs$run(library, args = list(package = "OPI"))
            rs$run(library, args = list(package = "deldir"))
            rs$run(library, args = list(package = "sp"))
            rs$run(library, args = list(package = "rgeos"))
            # choose OPI in background R session
            rs$run(chooseOpi, args = list(opiImplementation = chooseOpi()[.OpiEnv$chooser]))
          })
          # initialize OPI
          rs$run(do.call, args = list(what = opiInitialize, args = opiParams))
          eye <- ifelse(input$eye == "OD", "R", "L")
          rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg)))
          # assign 
          rs$run(assign, args = list(x = "makePMF",     value = makePMF))
          rs$run(assign, args = list(x = "openNewLocs", value = openNewLocs))
          rs$run(assign, args = list(x = "findNeighbors", value = findNeighbors))
        }
        msg("OPI connection opened")
        opiInitialized(TRUE)
        showModal(modalDialog(title = "OPI connection", "OPI has been initialized", easyClose = TRUE))
        enableElements(c("fovea", "close", "run"))
      }, error = function(e) {
        msg(errortxt(e))
        opiInitialized(FALSE)
        enableElements(c("init", "serverIP"))
      })
      locs    <<- initLocations(locs)
      foveadb <<- NA
      initRunVariables()
      refreshout(TRUE)
    }
  }, ignoreInit = TRUE)
  # close OPI connection
  observeEvent(input$close, {
    if(!debugrun) {
      invisible(rs$run(opiClose))
      rs$close()
      rs <<- NA
    }
    disableElements(c("close", "fovea", "run"))
    enableElements(c("init", "serverIP"))
    msg("OPI connection closed")
    opiInitialized(FALSE)
    locs    <<- testLocations()
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # test fovea
  observeEvent(input$fovea, {
    if(!debugrun) {
      eye <- ifelse(input$eye == "OD", "R", "L")
      rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg, fixation = "Circle", fix_sx = 3, fix_sy = 3)))
    }
    showModal(modalDialog(
      title = "Start testing the fovea?",
      "Press Yes when ready or Cancel",
      footer = tagList(actionButton(ns("okfovea"), "Yes"), actionButton(ns("cancelfovea"), "Cancel"))
    ))
  }, ignoreInit = TRUE)
  # if OK to test fovea
  observeEvent(input$okfovea, {
    removeModal()
    showModal(modalDialog(title = "Testing the fovea", "Please wait while the fovea is being tested", footer = NULL))
    # run foveal test
    if(!debugrun) {
      eye <- ifelse(input$eye == "OD", "R", "L")
      rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg, fixation = "Circle", fix_sx = 3, fix_sy = 3)))
      foveadb <<- round(rs$run(fovealTest, args = list(eye = eye, bg = dayParams$bg, minlum = dayParams$minlum, maxlum = dayParams$maxlum, 
                      color = dayParams$color, presTime = dayParams$presTime, respWin = dayParams$respWindow)))
      rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg)))
    }
    removeModal()
    showModal(modalDialog(title = "Testing the fovea", "Foveal test has finished", easyClose = TRUE))
    msg(paste("Foveal threshold is", foveadb, "dB"))
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # if Cancel test fovea
  observeEvent(input$cancelfovea, {
    if(!debugrun) {
      eye <- ifelse(input$eye == "OD", "R", "L")
      rs$run(do.call, args = list(what = opiSetBackground, args = list(eye = eye, lum = dayParams$bg)))
    }
    removeModal()
  })
  # start or continue test
  observeEvent(input$run, {
    if(begin) {
      locs <<- initLocations(locs)
      initRunVariables()
      disableElements(c("eye", "size", "grid"))
    }
    runTest()
  }, ignoreInit = TRUE)
  # pause test halfway through
  observeEvent(input$pause, {
    if(!begin) { # if test has not started or it has finished, do not do this
      tp0 <<- Sys.time()
      go  <<- FALSE
      disableElements("pause")
      enableElements(c("run", "stop"))
      msg("ZEST test paused. Pres 'continue' or 'stop' to terminate it")
    }
  }, ignoreInit = TRUE, priority = 10)
  # stop test halfway through
  observeEvent(input$stop, { # stop test
    showModal(modalDialog(
      title = "Are you sure you want to terminate the test?",
      "Press TERMINATE to cancel",
      footer = tagList(actionButton(ns("okstop"), "TERMINATE"), modalButton("Continue"))
    ))
  }, ignoreInit = TRUE)
  # OK to stop halfway through?
  observeEvent(input$okstop, { # stop test
    removeModal()
    stopTest()
    msg(errortxt(paste(dayParams$runType, "ZEST test terminated")))
    enableElements(c("close", "run", "fovea", "eye", "size", "grid"))
    disableElements(c("pause", "stop", "save"))
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  # save test once finished
  observeEvent(input$save, { # save and allow to run a fresh test
    saveResults()
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableElements(c("eye", "size", "grid", "fovea", "close", "run"))
    msg("Results have been saved")
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
    newReports(TRUE)
  }, ignoreInit = TRUE)
  # cancel save once the test has finished
  observeEvent(input$cancel, { # do not save and allow to run a fresh test
    updateTextInput(session, "comments", value = "")
    disableElements(c("save", "cancel"))
    enableElements(c("eye", "size", "grid", "fovea", "close", "run"))
    msg(errortxt("Results have not been saved"))
    begin   <<- TRUE
    locs    <<- initLocations(locs)
    foveadb <<- NA
    initRunVariables()
    refreshout(TRUE)
  }, ignoreInit = TRUE)
  ####################################################################
  # main loop: listening to what remote perimetry test code has to say
  ####################################################################
  observe({ # refresh at a fast rate (similar to a loop)
    listen() # timer for the listener
    if(go) {
      npress <- ifelse(is.null(runlog), 1, nrow(runlog))
      writeBin("NEXT", con) # send NEXT command
      res <<- readFromSocket()
      # update locations
      idx <- which(locs$x == res$x & locs$y == res$y)
      locs$th[idx] <<- res$th
      locs$done[idx]  <<- res$done
      runlog <<- rbind(runlog, updateLog(res))
      # check and show if a catch trial needs to be presented
      if(npress %% dayParams$fprate == 0) { # false positive trial
        idx <- sample(1:nrow(locs), 1) # get random location
        writeBin("FALS", con) # send FALS command
        if(dayParams$runType != "Size perimetry") {
          size <- 0  # ignore size
          db   <- 50 # very dim stimulus, corresponds to light differential of 11 * 1e-4 cd/m2 if max luminance is 110
        } else {
          size <- 0.01 # very small stimulus size
          db   <- 0    # ignore db
        }
        # send catch trial and retrieve results
        sendCatchTrialData(locs$x[idx], locs$y[idx], size, db)
        res <- readFromSocket()
        # update log
        runlog <<- rbind(runlog, updateLog(res))
      } else if(npress %% dayParams$fnrate == 0) { # false negative trial
        # get all locations that for which at least a stimulus double the background luminance has been seen
        logfn <- runlog[runlog$level >= log10(dayParams$maxlum / dayParams$bg) & runlog$seen == TRUE,]
        if(nrow(logfn) > 0) {
          writeBin("FALS", con) # send FALS command
          idx <- sample(logfn$level, 1) # get one at random
          # send catch trial and retrieve results: brightest or largest stimulus,
          # ignore size if luminance perimetry or luminance is size perimetry
          sendCatchTrialData(logfn$x[idx], logfn$y[idx], dayParams$maxarea, 0)
          res <- readFromSocket()
          res$level <- -50
          # update log
          runlog <<- rbind(runlog, updateLog(res))
        }
      }
      # update results
      refreshout(TRUE)
      # if all points are done, stop perimetry test
      if(all(locs$done)) {
        msg(paste(dayParams$runType, "ZEST test finished"))
        stopTest()
        begin <<- TRUE
        enableElements(c("fovea", "save", "cancel"))
        disableElements(c("run", "pause", "stop", "close"))
        showModal(modalDialog(
          title = "ZEST test finished",
          "The test has finished. You can 'Save' it or 'Cancel' all without saving",
          easyClose = TRUE))
      }
    }
  }, priority = 1)
  ####################
  # SUBROUTINES
  ####################
  # get test locations depending on the grid we are interested on
  testLocations <- function() {
    if(is.null(grids)) return(NULL)
    locs <- grids[[which(gridNames %in% input$grid)]]
    if(input$eye == "OS") locs$x <- -locs$x
    return(initLocations(locs))
  }
  # run Test
  runTest <- function() {
    go    <<- TRUE
    begin <<- FALSE
    # if connection has not been established yet, do it
    if(is.na(con)) {
      if(!debugrun) {
        if(dayParams$runType == "Size perimetry") type <- "size"
        else type <- "luminance"
        opts <- list(size = size, lum = lum, color = dayParams$color, bg = dayParams$bg,
                     minlum = dayParams$minlum, maxlum = dayParams$maxlum,
                     minarea = dayParams$minarea, maxarea = dayParams$maxarea,
                     presTime = dayParams$presTime, respWindow = dayParams$respWindow, respWinPed = dayParams$respWinPed, 
                     respTimesLength = dayParams$respTimesLength, minISI = dayParams$minISI)
        eye <- ifelse(input$eye == "OD", "R", "L")
        rs$call(zestTest, args = list(type = type, eye = eye, locs = locs[, c("x", "y", "wave")], opts = opts, port = dayParams$testPort))
        if(!is.null(rsread <- rs$read())) print(rsread$stdout)
      }
      # start client
      con <<- socketConnection(port = dayParams$testPort, server = FALSE, open = "")
      # loop until can open the connection. Connection is open so that execution
      # is blocked when waiting for perimetry test server to respond
      while(tryCatch({open(con, open = "w+b"); FALSE},
                     error   = function(cond) TRUE,
                     warning = function(cond) TRUE)) {}
    }
    disableElements(c("close", "run", "fovea", "stop", "save"))
    enableElements(c("pause"))
    msg(paste(dayParams$runType, "ZEST test running"))
  }
  # stop test
  stopTest <- function() {
    go <<- FALSE
    if(!is.na(con)) {
      writeBin("STOP", con) # send STOP command
      if(!debugrun) while(is.null(rs$read())) {}
      close(con) # close client connection
      con <<- NA
    }
  }
  readFromSocket <- function() {
    # read results returned by perimetry test process through socket
    x     <- readBin(con, "numeric") # stimulus x coordinate
    y     <- readBin(con, "numeric") # stimulus y coordinate
    level <- readBin(con, "numeric") # stimulus level
    th    <- readBin(con, "numeric") # estimated threshold
    seen  <- readBin(con, "logical") # seen or not seen
    time  <- readBin(con, "numeric") # response time
    done  <- readBin(con, "logical") # whether we are done testing the location
    return(data.frame(x = x, y = y, level = level, th = th, seen = seen, time = time, done = done))
  }
  sendCatchTrialData <- function(x, y, size, db) {
    writeBin(as.numeric(x),    con)
    writeBin(as.numeric(y),    con)
    writeBin(as.numeric(size), con)
    writeBin(as.numeric(db),   con)
  }
  # update log
  updateLog <- function(res) {
    # update test and pause times
    if(tp0 != 0) {
      tp <<- tp + as.numeric(difftime(Sys.time(), tp0, units = "secs"))
      tp0 <<- 0
      tt0 <<- Sys.time()
    }
    tt <<- tt + as.numeric(difftime(Sys.time(), tt0, units = "secs"))
    tt0 <<- Sys.time()
    return(data.frame(
      x      = res$x,
      y      = res$y,
      level  = res$level,
      th     = res$th,
      seen   = res$seen,
      time   = res$time,
      done   = res$done,
      tt     = tt,
      tp     = tp))
  }
  # save results at the end of the test
  saveResults <- function() {
    systime  <- Sys.time()
    tdate    <- format(systime, "%Y-%m-%d")
    ttime    <- format(systime, "%H:%M:%S")
    # if patient folder does not exist, then create
    patientDir <- paste0(dayParams$resPath, patient$id, "/")
    dir.create(patientDir, showWarnings = FALSE) # create directories if they do not exist
    dir.create(paste0(patientDir, "raw/"), showWarnings = FALSE)
    if(dayParams$runType == "Luminance perimetry") runType <- "luminance"
    if(dayParams$runType == "Size perimetry")      runType <- "size"
    if(dayParams$runType == "Simulation")          runType <- "simulation"
    # save raw log first
    fnamelog <- paste0(patientDir, "raw/", trimws(as.character(patient$id)), "_", runType, "_", input$eye, format(systime, "_%Y%m%d_%H%M%S"), ".csv")
    write.csv(runlog, file = fnamelog, row.names = FALSE)
    # then save the processed test
    fname <- paste0(patientDir, trimws(as.character(patient$id)), "_", runType, "_", names(grids)[gridNames %in% input$grid], ".csv")
    res <- prepareToSave(patient$id, input$eye, tdate, ttime, patient$age, patient$type, input$comments, locs, runlog, foveadb)
    # if file exist, append result
    if(file.exists(fname)) res <- rbind(read.csv(file = fname, colClasses = "character"), res)
    # save test for the patient
    write.csv(res, file = fname, row.names = FALSE)
  }
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
# error texts
errortxt <- function(txt) return(paste("<span style=\"color:#FF0000\">", txt, "</span>"))
# enable and disable elements
enableElements <- function(ids) lapply(ids, enable)
disableElements <- function(ids) lapply(ids, disable)
# fill out parameters to initialize the OPI, but also as side effect, it sets the OPI implementation
fillOpiParams <- function(serverIP) {
  # choose correct OPI implementation
  if(dayParams$runType != "Simulation") {
    chooseOpi("Daydream")
    # get parameters for the Daydream implementation
    opiParams <- opiGetParams("opiInitialize")
    # and use the values in DayParams settings for initialization parameters
    opiParams$ip   <- serverIP
    opiParams$port <- dayParams$serverPort
    opiParams$lut  <- read.csv(paste0(dayParams$confPath, dayParams$gammaFile), header = FALSE)$V1
    opiParams$fovy <- dayParams$fovy
  } else {
    #chooseOpi("SimHenson")
    #chooseOpi("SimYes")
    chooseOpi("SimNo")
    opiParams <- opiGetParams("opiInitialize")
  }
  return(opiParams)
}
# patient's information to show: id, name, surname, age, Gender
parsePatientOutput <- function() {
  if(is.na(patient$id)) {
    txt <- errortxt("Please select a patient to continue")
  } else {
    txt <- paste0("<strong>Patient ID:</strong> ", patient$id, "</br>")
    txt <- paste0(txt, " <strong>Name:</strong> ",  patient$name, "</br>")
    txt <- paste0(txt, " <strong>Surname:</strong> ", patient$surname, "</br>")
    txt <- paste0(txt, " <strong>Age:</strong> ", patient$age, "</br>")
    txt <- paste0(txt, " <strong>Gender:</strong> ", patient$gender, "</br>")
    txt <- paste0(txt, " <strong>Type:</strong> ", patient$type, "</br>")
  }
  return(HTML(txt))
}
# initialize locations
initLocations <- function(locs) {
  locs$th <- NA
  locs$done  <- FALSE
  return(locs)
}
# template of the plot to show
templatePlot <- function(eye) {
  if(eye == "OD") x <- 15
  else            x <- -15
  par(mar = c(0, 0, 0, 0))
  lty <- 1
  lwd <- 1
  linColor     <- "lightgray"
  ellipseColor <- "gray92"
  pchColor     <- "black"
  plot(0, 0, typ = "n", xlim = c(-31, 31), ylim = c(-31, 31), asp = 1,
       axes = FALSE, ann = FALSE, bty = "n")
  draw.ellipse(x, -1.5, 2.75, 3.75, col = ellipseColor, border = ellipseColor)
  lines(c(-30, 30), c(0, 0), col = linColor, lty = lty, lwd = lwd)
  lines(c(0, 0), c(-30, 30), col = linColor, lty = lty, lwd = lwd)
  text( 31,  0, "+30", adj = c(0, 0.5))
  text(-31,  0, "-30", adj = c(1, 0.5))
  text( 0,  31, "+30", adj = c(0.5, 0))
  text( 0, -31, "-30", adj = c(0.5, 1))
  draw.circle(0, 0, 10, border = linColor, lty = lty, lwd = lwd)
  draw.circle(0, 0, 20, border = linColor, lty = lty, lwd = lwd)
  draw.circle(0, 0, 30, border = linColor, lty = lty, lwd = lwd)
}
# show plot with updated data
showPlot <- function(locs, eye, foveadb) {
  templatePlot(eye)
  # unfinished symbols are presented in gray, finished symbols in black
  cols <- rep("black", length(locs$done))
  cols[locs$done & locs$th >= 5] <- "darkgreen"
  cols[locs$done & locs$th < 5]  <- "red"
  fovcol <- ifelse(foveadb < 5, "red", "darkgreen")
  text(0, 0, foveadb, col = fovcol, font = 2)
  isna <- is.na(locs$th)
  if(any(isna))  text(locs$x[isna], locs$y[isna], ".", cex = 2, col = cols[isna])
  if(any(!isna)) text(locs$x[!isna], locs$y[!isna], locs$th[!isna], col = cols[!isna], font = 2)
}
# prepare test results to show next to the plot
renderResult <- function(locs, res, runlog) {
  if(is.null(res)) {
    level <- x <- y <- time <- ""
    seentxt <- ""
  } else {
    level <- res$level
    x     <- paste(res$x, "degrees")
    y     <- paste(res$y, "degrees")
    time  <- res$time
    if(res$seen) seentxt <- "Stimulus <strong>seen</strong>"
    else         seentxt <- "Stimulus <strong>not seen</strong>"
    seentxt <- paste(seentxt, "after", round(time), "ms")
  }
  npoints   <- length(locs$done) # total number of locations
  nfinished <- sum(locs$done)    # locations finished
  if(is.null(runlog)) {
    fp <- fpt <- fpp <- fn <- fnt <- fnp <- 0
    tttxt <- tptxt <- "00:00"
    rtsd <- rtm <- ""
  } else {
    # compute false positives and negatives
    fp  <- sum(runlog$level == 50 & runlog$seen)
    fpt <- sum(runlog$level == 50)
    fpp <- ifelse(fpt == 0, 0, round(100 * fp / fpt))
    fn  <- sum(runlog$level == -50 & !runlog$seen)
    fnt <- sum(runlog$level == -50)
    fnp <- ifelse(fnt == 0, 0, round(100 * fn / fnt))
    # compute response time SD and mean
    rt <- runlog$time[which(runlog$seen == TRUE)]
    if (length(rt) > 1) { #can only calculate SD if there are more than 2 response times available
      rtsd <- round(sd(rt))
      rtm <- round(mean(rt))
    } else if (length(rt) == 1) {
      rtm <- round(mean(rt))
    } else {
      rtsd <- rtm <- ""
    }
    # calculate test time and pause time
    secs <- runlog$tt[length(runlog$tt)]
    mm <- as.character(secs %/% 60)
    ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
    if(nchar(mm) == 1) mm <- paste0("0", mm)
    if(nchar(ss) == 1) ss <- paste0("0", ss)
    tttxt <- paste(mm, ss, sep = ":")
    secs <- runlog$tp[length(runlog$tp)]
    mm <- as.character(secs %/% 60)
    ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
    if(nchar(mm) == 1) mm <- paste0("0", mm)
    if(nchar(ss) == 1) ss <- paste0("0", ss)
    tptxt <- paste(mm, ss, sep = ":")
  }
  if(level != "") level <- paste(level, "dB")
  if(rtm != "") rtm <- paste(rtm, "ms")
  if(rtsd != "") rtsd <- paste(rtsd, "ms")
  
  # get state text
  txt <- paste("<strong>Stimulus x:</strong>", x, "<br/>")
  txt <- paste(txt, "<strong>Stimulus y:</strong>", y, "<br/><br/>")
  txt <- paste(txt, "<strong>Level:</strong>", level, "<br/>")
  txt <- paste0(txt, seentxt)
  txt <- paste0(txt, "<br/><br/>")
  # False positives and negatives
  txt <- paste(txt, "<strong>False Positives:</strong>", fp, "of", fpt)
  txt <- paste0(txt, " (", fpp, "%)<br/>")
  txt <- paste(txt, "<strong>False Negatives:</strong>", fn, "of", fnt)
  txt <- paste0(txt, " (", fnp, "%)<br/><br/>")
  # Response Times
  txt <- paste(txt, "<strong>Responses < 150 ms:</strong>", sum(runlog$time < 150), "<br/>")
  txt <- paste(txt, "<strong>Responses > 600 ms:</strong>", sum(runlog$time > 600 & runlog$seen == TRUE), "<br/>")
  txt <- paste(txt, "<strong>Mean Response Time:</strong>", rtm, "<br/>")
  txt <- paste(txt, "<strong>SD of Response Time:</strong>", rtsd, "<br/><br/>")
  # Progress
  txt <- paste(txt, "<strong>Finished:</strong>", nfinished, "of", npoints)
  txt <- paste0(txt, " (", round(100 * nfinished / npoints), "%)<br/>")
  txt <- paste(txt, "<strong>Presentations:</strong>", nrow(runlog) - sum(runlog$level < 0), "<br/><br/>")
  # test time and pause time
  txt <- paste(txt, "<strong>Test Time (mm:ss):</strong>",  tttxt, "<br/>")
  txt <- paste(txt, "<strong>Pause Time (mm:ss):</strong>", tptxt, "<br/>")
  txt <- paste0(txt, "<br/>")
  return(HTML(txt))
}

# prepare results to save
prepareToSave <- function(id, eye, tdate, ttime, age, type, comments, locs, runlog, foveadb) {
  res <- data.frame(id = id, eye = eye, date = tdate, time = ttime, age = age, type = type, fpr = NA, fnr = NA,
                    rt150 = sum(runlog$time < 150), rt600 = sum(runlog$time > 600 & runlog$seen == TRUE), rtsd = NA, rtm = NA,
                    duration = NA, pause = NA, npres = nrow(runlog) - sum(runlog$level < 0), comments = comments,
                    fovea = ifelse(is.na(foveadb), "", foveadb)) # do not count catch trials
  
  # test and pause time
  secs <- runlog$tt[length(runlog$tt)]
  mm <- as.character(secs %/% 60)
  ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
  if(nchar(mm) == 1) mm <- paste0("0", mm)
  if(nchar(ss) == 1) ss <- paste0("0", ss)
  res$duration <- paste(mm, ss, sep = ":")
  secs <- runlog$tp[length(runlog$tp)]
  mm <- as.character(secs %/% 60)
  ss <- as.character(floor((secs / 60 - secs %/% 60) * 60))
  if(nchar(mm) == 1) mm <- paste0("0", mm)
  if(nchar(ss) == 1) ss <- paste0("0", ss)
  res$pause <- paste(mm, ss, sep = ":")
  # false positive and false negatives
  fp  <- sum(runlog$level == 50 & runlog$seen)
  fpt <- sum(runlog$level == 50)
  res$fpr <- ifelse(fpt == 0, 0, fp / fpt)
  fn  <- sum(runlog$level == -50 & !runlog$seen)
  fnt <- sum(runlog$level == -50)
  res$fnr <- ifelse(fnt == 0, 0, fn / fnt)
  # compute response time SD and mean
  rt <- runlog$time[which(runlog$seen == TRUE)]
  res$rtsd <- round(sd(rt))
  res$rtm <- round(mean(rt))
  # get levels at each location to compute stats and scores and save
  runlog <- runlog[runlog$done == TRUE,]
  final_threshold <- runlog$th
  # results for each location
  res[,paste0("l", 1:nrow(locs))] <- final_threshold
  return(res)
}