###############
# ZEST routines 
###############
# set up probability mass function (PMF)
makePMF <- function (domain, start_guess, weight = 4, floor = 0.001) {
  glaucomaPMF <- rep(0.001,length(domain))
  # bimodal prior
  glaucomaPMF[1:10] <- c(rep(0.001, 4), 0.2, 0.3, 0.2, 0.15, 0.1, 0.02)
  healthyPMF <- function(normalModePoint) {
    temp <- c(rep(0.001, 100), 0.001, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01, rep(0.001, 100))
    mode <- which.max(temp)
    return(temp[(mode - normalModePoint + domain[1]):(mode - normalModePoint + domain[length(domain)])])
  }
  makeBiModalPMF <- function(normalModePoint, weight, pdf.floor) {
    npdf <- healthyPMF(normalModePoint)
    cpdf <- npdf * weight + glaucomaPMF
    cpdf[which(cpdf < pdf.floor)] = pdf.floor 
    return (cpdf)
  }
  # define prior PMF, minimum stimulus and maximum stimulus for luminance test
  # bimodal Prior PMF
  prior_pmf <- makeBiModalPMF(start_guess, weight, floor)
  # normalise PMF
  prior_pmf <- prior_pmf / sum(prior_pmf)
  return(prior_pmf)
}
# create table of neighboring locations
findNeighbors <- function(locs) {
  # Function which creates Voronoi tessellation tiles from grid locations
  voroni_to_polys <- function(dd) {
    # tile.list extracts the polygon data from the deldir computation
    vor_desc <- tile.list(dd)
    # gets us the points for the polygons but we still have to close them, hence the need for the rbind
    vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
      tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
      tmp <- rbind(tmp, tmp[1,])
      Polygons(list(Polygon(tmp)), ID = i) # now we can make the Polygons
    })
    # create location IDs
    xdf <- data.frame(id=sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID'))
    rownames(xdf) <- xdf$id
    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=xdf)
  }
  #create lookup table of neighbors
  vpoly <- voroni_to_polys(deldir(locs$x,locs$y))
  neighbors <- gTouches(vpoly, byid = TRUE)
  return(neighbors)
}
# Open up new locations to test in the growth pattern
openNewLocs <- function (neighbors, loc, locs, unfinished, finished) {
  # check locations to open up for the growth algorithm
  wave <- locs$wave[loc] # find wave of reference location
  # find neighbors that are in the next wave
  new_locations <- intersect(which(neighbors[loc,] == TRUE),
                             which(locs$wave == wave + 1))
  # check that new locations aren't already in unfinished queue as they could have opened up earlier
  # also check that locations have not already terminated
  if (length(new_locations) > 0)
    new_locations <- setdiff(new_locations, c(unfinished, finished))
  return(new_locations)
}
# helper to make stimulus
prepareZestRun <- function(params, eye, size, lum) {
  if(params$runType == "Luminance perimetry") { # Luminance perimetry
    makeStimHelper <- function(x, y) { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = params$bg + params$maxlum * 10^(-db / 10),
                  size  = size,
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
    # domain
    domain <- -5:(round(10 * log10(params$maxlum / params$minlum)) + 5)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + 5
    maxStim <- tail(domain,1) - 5
    # stimulus maker for false positive trials
    makeStimHelperFP <- function(x, y) { # returns a function of (db)
      ff <- function(db) db
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = params$bg, size = 0,
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
  } else if(params$runType == "Size perimetry") { # size
    makeStimHelper <- function(x, y) { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = lum,
                  size  = 2 * sqrt(params$maxarea * 10^(-db / 10) / pi),
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
    # domain
    domain <- -5:(round(10 * log10(params$maxarea / params$minarea)) + 5)
    prior_pmf <- rep(1/length(domain), length(domain)) #uniform prior PMF
    minStim <- head(domain, 1) + 5
    maxStim <- tail(domain, 1) - 5
    # stimulus maker for false positive trials
    makeStimHelperFP <- function(x, y) { # returns a function of (db)
      ff <- function(db) db
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = params$bg, size = 0,
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
  } else if(params$runType == "Simulation") { # Simulation same as luminance
    makeStimHelper <- function(x, y) { # returns a function of (db, n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = params$maxlum * 10^(-db / 10),
                  size  = size,
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
    # domain
    domain <- -5:(round(10 * log10(params$maxlum / params$minlum)) + 5)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + 5
    maxStim <- tail(domain,1) - 5
    # stimulus maker for false positive trials
    makeStimHelperFP <- function(x, y) { # returns a function of (db)
      ff <- function(db) db
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = dbTocd(50), size = 0,
                  color = params$color, duration = params$presTime,
                  responseWindow = params$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x, y = y))
      return(ff)
    }
  } else stop("wrong test type")
  return(list(domain = domain, prior_pmf = prior_pmf,
              minStim = minStim, maxStim = maxStim,
              makeStimHelper   = makeStimHelper,
              makeStimHelperFP = makeStimHelperFP))
}
# read command from the client
readCommand <- function(q) {
  # get all messages received
  msg <- q$pop()
  if(msg$title != "CMD")
    stop("Expecting a command. There is an error in logic")
  return(msg$message)
}
# send results to client
writeResults <- function(q, res) {
  q$push(title   = rep("VAL", 9),
         message = c(res$type, res$x, res$y, res$level, res$th, as.character(res$seen),
                     res$time, res$respWin, as.character(res$done)))
}
# ZEST server for foveal test, zest test, etc.
zestServer <- future({
  ##########################################################################################################
  # Define help functions
  ##########################################################################################################
  # foveal ZEST test
  fovealTest <- function() {
    if(dayParams$runType != "Simulation")
      makeStimHelper <- function() { # returns a function of (db, n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = x, eye = eye,
                    level = dayParams$bg + dayParams$maxlum * 10^(-db / 10),
                    size  = 0.43,
                    color = dayParams$color, duration = dayParams$presTime,
                    responseWindow = dayParams$respWindow)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = 0, y = 0))
        return(ff)
      }
    else
      makeStimHelper <- function() { # returns a function of (db, n)
        ff <- function(db, n) db + n
        body(ff) <- substitute({
          s <- list(x = x, y = x, eye = eye,
                    level = dayParams$maxlum * 10^(-db / 10),
                    size  = 0.43,
                    color = dayParams$color, duration = dayParams$presTime,
                    responseWindow = dayParams$respWindow)
          class(s) <- "opiStaticStimulus"
          return(s)
        }, list(x = 0, y = 0))
        return(ff)
      }
    # domain
    domain <- -5:(round(10 * log10(dayParams$maxlum / dayParams$minlum)) + 5)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + 5
    maxStim <- tail(domain,1) - 5
    # ZEST algorithm
    state <- ZEST.start(domain = domain, prior = prior_pmf, minStimulus = minStim, maxStimulus = maxStim, makeStim = makeStimHelper())
    while(!ZEST.stop(state)) {
      Sys.sleep(runif(1, min = 300, max = 600) / 1000)
      state <- ZEST.step(state)$state
    }
    return(max(-1, ZEST.final(state)))
  }
  # ZEST test
  zestTest <- function() {
    # weight stimulus choice by growth pattern wave
    weight <- 1 / (locs$wave[unfinished]^2)
    # choose next location to test
    if (length(unfinished) == 1) loc <- unfinished[1]
    else loc <- sample(unfinished, 1, prob = weight)
    sr <- ZEST.step(states[[loc]])  # present stimulus and obtain response
    states[[loc]] <<- sr$state      # update state
    if(ZEST.stop(states[[loc]])) { # remove terminated location from queue
      ii <- which(unfinished == loc)
      unfinished <<- unfinished[-ii]
      finished <<- c(loc, finished)
      new_locs <- openNewLocs(findNeighbors(locs), loc, locs, unfinished, finished) # open up new locations
      unfinished <<- sort(c(unfinished, new_locs)) # add new locations to queue
      # for newly opened locations, create PMF based on previous terminated location's final threshold
      if (length(new_locs) > 0) {
        for (i in new_locs) {
          # find index of neighboring locations that have terminated
          nb <- intersect(which(findNeighbors(locs)[i, ] == TRUE), finished)
          # calculate mean threshold of neighboring locations that have terminated
          avg_th <- mean(sapply(nb, function (x) ZEST.final(states[[x]])))
          states[[i]]$pdf <<- makePMF(zestopt$domain, avg_th)
        }
      }
    }
    # wait if necessary
    if(dayParams$runType != "Simulation" && sr$resp$seen) {
      if(sr$resp$time > 150) {
        respWin <<- c(sr$resp$time, respWin[-tail(respWin,1)]) # update recent response times vector
        dayParams$respWindow <<- round(dayParams$respWinPed + mean(respWin)) # update response window
      }
      Sys.sleep(runif(1, min = dayParams$minISI, max = max(dayParams$minISI, mean(respWin))) / 1000)
    }
    return(list(type = "Z", x = locs$x[loc], y = locs$y[loc],
                level   = tail(sr$state$stimuli, 1),
                th      = ZEST.final(sr$state),
                seen    = sr$resp$seen,
                time    = sr$resp$time,
                respWin = dayParams$respWindow,
                done    = ZEST.stop(states[[loc]])))
    return(sr)
  }
  zestTestCatchTrial <- function(type) {
    if(type == "P") {
      idx <- sample(1:nrow(locs), 1) # get random location
      res <- opiPresent(zestopt$makeStimHelperFP(locs$x[idx], locs$y[idx])(0))
      level <- 0
    }
    if(type == "N") {
      if(dayParams$runType == "Luminance perimetry")
        thr <- 2 * dayParams$bg
      else if(dayParams$runType == "Size perimetry")
        thr <- 2 * dayParams$minarea
      else if(dayParams$runType == "Simulation")
        thr <- 2 * dayParams$bg
      # check seen responses
      respseen <- lapply(states, function(ss) return(ss$stimuli[ss$responses]))
      # that are a greater value than the threshold
      idx <- which(sapply(respseen, function(rr) return(any(rr >= thr))))
      # if no point found, no N catch trial and ignore completely
      if(length(idx) == 0) return(list(type = "F", x = NA, y = NA, level = NA, th = NA,
                                       seen = NA, time = NA, respWin = NA,done = NA))
      idx <- sample(idx, 1) # get one at random
      level <- 0 # maximum luminance and maximum size for false negatives
      res   <- opiPresent(zestopt$makeStimHelper(locs$x[idx], locs$y[idx])(level))
    }
    return(list(type = type, x = locs$x[idx], y = locs$y[idx],
                level   = level,
                th      = level,
                seen    = res$seen,
                time    = res$time,
                respWin = 0,
                done    = FALSE))
  }
  ##########################################################################################################
  # Main algorithm
  ##########################################################################################################
  repeat{
    # if command is idle, then wait for instructions
    while(ShinySender$empty()) Sys.sleep(0.01)
    cmd <- readCommand(ShinySender)
    if(cmd == "opiInit") {
      chooseOpi(machine)
      do.call(what = opiInitialise, args = opiParams)
      # get eye from message
      eye <- ShinySender$pop()$message
      if(machine == "Daydream")
        do.call(what = opiSetBackground,
                args = list(eye = eye, lum = dayParams$bg, color = dayParams$color))
    }
    if(cmd == "opiClose") {
      opiClose()
      break
    }
    if(cmd == "opiBg") {
      # get eye from message
      eye <- ShinySender$pop()$message
      if(machine == "Daydream")
        do.call(what = opiSetBackground,
                args = list(eye = eye, lum = dayParams$bg, color = dayParams$color))
    }
    if(cmd == "zestInit") {
      # get eye, size, and luminance from messages
      grid <- ShinySender$pop()$message
      eye  <- ShinySender$pop()$message
      size <- ShinySender$pop()$message
      lum  <- ShinySender$pop()$message
      locs <- grids[[which(gridNames %in% grid)]]
      if(eye == "L") locs$x <- -locs$x
      zestopt <- prepareZestRun(dayParams, eye, size, lum)
      # Init states
      states <- lapply(1:nrow(locs), function(i)
        ZEST.start(domain = zestopt$domain, prior = zestopt$prior_pmf,
                   stopType  = "S", stopValue = 1.5,
                   minStimulus = zestopt$minStim, maxStimulus = zestopt$maxStim,
                   maxPresentations = 100,
                   makeStim  = zestopt$makeStimHelper(locs$x[i], locs$y[i])))
      # vector of locations with unfinished ZEST business
      unfinished <- as.numeric(row.names(locs[which(locs$wave == 1),])) #start with wave 1 primary locations
      finished <- NULL
      # set up loop variables and source external functions
      respWin <- rep(dayParams$respWindow, dayParams$respTimesLength)  # set up adaptive response window
      npress <- 0 # reset number of presentations to zero
    }
    if(cmd == "zestStep") {
      npress <- npress + 1 # get number of presentations so far
      # check if it is time for a positive catch trial
      # FP = false positive trials
      # FN = false negative trials
      # N  = normal trial
      if(npress %% dayParams$fprate == 0)
        writeResults(ShinyReceiver, zestTestCatchTrial("P")) 
      else if(npress %% dayParams$fnrate == 0)
        writeResults(ShinyReceiver, zestTestCatchTrial("N"))
      else
        writeResults(ShinyReceiver, zestTest())
    }
    if(cmd == "zestPause") Sys.sleep(0.25)
    if(cmd == "zestFovea") {
      # get eye from message
      eye <- ShinySender$pop()$message
      # change background for Foveal test and wait for 1 seconds
      if(machine == "Daydream") {
        do.call(what = opiSetBackground,
                args = list(eye = eye, lum = dayParams$bg, color = dayParams$color,
                            fixation = "Circle", fix_sx = 3, fix_sy = 3))
        Sys.sleep(1)
      }
      ShinyReceiver$push(title = "VAL", message = as.character(fovealTest()))
      # return back to regular background
      if(machine == "Daydream")
        do.call(what = opiSetBackground,
                args = list(eye = eye, lum = dayParams$bg, color = dayParams$color))
    }
  }
},
seed = TRUE,
globals = list(machine        = machine,
               dayParams      = dayParams,
               opiParams      = opiParams,
               grids          = grids,
               gridNames      = gridNames,
               makePMF        = makePMF,            # ZEST help functions
               findNeighbors  = findNeighbors,
               openNewLocs    = openNewLocs,
               prepareZestRun = prepareZestRun,
               ShinySender    = ShinySender,        # communication files
               ShinyReceiver  = ShinyReceiver,
               readCommand    = readCommand,        # communication help functions
               writeResults   = writeResults),
packages = c("OPI", "deldir", "sp", "rgeos"))