###############
# ZEST routines 
###############
# Set up probability mass function (PMF)
makePMF <- function (domain, start_guess, weight = 4, floor = 0.001) {
  glaucomaPMF <- rep(0.001,length(domain))
  glaucomaPMF[1:10] <- c(rep(0.001,4),0.2, 0.3, 0.2, 0.15, 0.1, 0.02) #Bimodal prior
  
  healthyPMF  <- function (normalModePoint) {
    temp <- c(rep(0.001,100), 0.001, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01,rep(0.001,100))
    mode <- which.max(temp)
    return(temp[(mode-normalModePoint+domain[1]):(mode-normalModePoint+domain[length(domain)])])
  }
  makeBiModalPMF <- function(normalModePoint, weight, pdf.floor) {
    npdf <- healthyPMF(normalModePoint)
    cpdf <- npdf * weight + glaucomaPMF
    cpdf[which(cpdf < pdf.floor)] = pdf.floor 
    return (cpdf)
  }
  #define prior PMF, minimum stimulus and maximum stimulus for luminance test
  prior_pmf <- makeBiModalPMF(start_guess, weight, floor) #Bimodal Prior PMF
  prior_pmf <- prior_pmf/sum(prior_pmf) #normalise PMF
  return(prior_pmf)
}

#Create table of neighboring locations
findNeighbors <- function (coord) {
  # Function which creates Voronoi tessellation tiles from grid locations
  voroni_to_polys <- function(dd) {
    vor_desc <- tile.list(dd) # tile.list extracts the polygon data from the deldir computation
    # gets us the points for the polygons but we still have to close them, hence the need for the rbind
    vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
      tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
      tmp <- rbind(tmp, tmp[1,])
      Polygons(list(Polygon(tmp)), ID=i) # now we can make the Polygons
    })
    # create location IDs
    xdf <- data.frame(id=sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID'))
    rownames(xdf) <- xdf$id
    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=xdf)
  }
  #create lookup table of neighbors
  vpoly <- voroni_to_polys(deldir(coord$x,coord$y))
  neighbors <- gTouches(vpoly, byid = TRUE)
} 

# Open up new locations to test in the growth pattern
openNewLocs <- function (neighborTab, loc, coord, unfinished, finished) {
  # check locations to open up for the growth algorithm
  wave <- coord$wave[loc] # find wave of reference location
  neighbors <- which(neighborTab[loc,] == TRUE)
  new_locations <- intersect(neighbors, which(coord$wave == wave + 1)) #find neighbors that are in the next wave
  
  #check that new locations aren't already in unfinished queue as they could have opened up earlier
  #also check that locations have not already terminated
  if (length(new_locations) > 0) {
    new_locations <- setdiff(new_locations, c(unfinished, finished)) 
  }
  return(new_locations)
}

# foveal test
fovealTest <- function(eye, bg, minlum, maxlum, color, presTime, respWin) {
  # helper to make stimulus
  makeStimHelper <- function(x, y) { # returns a function of (db,n)
    ff <- function(db, n) db + n
    body(ff) <- substitute({
      s <- list(x = x, y = y, level = bg + maxlum * 10^(-db / 10), eye = eye,
                color = color, size = 0.43, duration = presTime, responseWindow = respWin)
      class(s) <- "opiStaticStimulus"
      return(s)
    }, list(x = x,y = y))
    return(ff)
  }
  # ZEST algorithm
  domain <- -5:(round(10 * log10(maxlum / minlum)) + 5)
  prior_pmf <- makePMF(domain, start_guess = 30)
  minStim <- head(domain,1) + 5
  maxStim <- tail(domain,1) - 5
  
  state <- ZEST.start(domain = domain, pdf = prior_pmf, minStimulus = minStim, maxStimulus = maxStim, makeStim = makeStimHelper(0, 0))
  while(!ZEST.stop(state)) {
    Sys.sleep(runif(1, min = 300, max = 600) / 1000)
    state <- ZEST.step(state)$state
  }
  return(max(-1, ZEST.final(state)))
}

# luminance and size test
zestTest <- function(type, eye, locs, opts, port = 6011) {
  # send results to socket
  sendToSocket <- function(x, y, level, th, seen, time, respWin, done) {
    # stimulus location
    writeBin(as.numeric(x), con)
    writeBin(as.numeric(y), con)
    # stimulus level
    writeBin(as.numeric(level), con)
    # estimated threshold
    writeBin(as.numeric(th), con)
    # Time it was seen or response window if not seen
    writeBin(as.logical(seen), con)
    if(seen) writeBin(as.numeric(round(time)), con)
    else writeBin(as.numeric(respWin), con)
    # we done or need to keep going?
    writeBin(as.logical(done), con)
  }
  if(type == "luminance") {
    # helper to make stimulus
    makeStimHelper <- function(x, y) { # returns a function of (db,n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = opts$bg + opts$maxlum * 10^(-db / 10),
                  size  = opts$size,
                  color = opts$color, duration = opts$presTime,
                  responseWindow = opts$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x,y = y))
      return(ff)
    }
    # domain
    domain <- -5:(round(10 * log10(opts$maxlum / opts$minlum)) + 5)
    prior_pmf <- makePMF(domain, 30)
    minStim <- head(domain,1) + 5
    maxStim <- tail(domain,1) - 5
  } else {
    # helper to make stimulus
    makeStimHelper <- function(x, y) { # returns a function of (db,n)
      ff <- function(db, n) db + n
      body(ff) <- substitute({
        s <- list(x = x, y = y, eye = eye,
                  level = opts$lum,
                  size  = 2 * sqrt(opts$maxarea * 10^(-db / 10) / pi),
                  color = opts$color, duration = opts$presTime,
                  responseWindow = opts$respWindow)
        class(s) <- "opiStaticStimulus"
        return(s)
      }, list(x = x,y = y))
      return(ff)
    }
    # domain
    domain <- -5:(round(10 * log10(opts$maxarea / opts$minarea)) + 5)
    prior_pmf <- rep(1/length(domain), length(domain)) #uniform prior PMF
    minStim <- head(domain, 1) + 5
    maxStim <- tail(domain, 1) - 5
  }
  
  # create socket connection and attempt to open it until we connect to the server
  con <- socketConnection(port = port, server = TRUE, open = "w+b")
  # vector with states
  states <- lapply(1:nrow(locs), function(i)
  ZEST.start(domain = domain, prior = prior_pmf,
               stopType  = "S", stopValue = 1.5,
               minStimulus = minStim, maxStimulus = maxStim,
               maxPresentations = 100,
               makeStim  = makeStimHelper(locs$x[i], locs$y[i])))
  # vector of locations with unfinished ZEST business
  unfinished <- as.numeric(row.names(locs[which(locs$wave == 1),])) #start with wave 1 primary locations
  finished <- NULL

  #Set up loop variables and source external functions
  respWin <- rep(opts$respWindow, opts$respTimesLength)  ## set up adaptive response window
  
  ##########################################################################################################
  # Main Loop
  ##########################################################################################################
  
  # start listening to the server
  while(TRUE) {
    command <- readBin(con, "character")
    if(command == "STOP") break
    # deal with FALSE trials
    if(command == "FALS") {
      x     <- readBin(con, "numeric")
      y     <- readBin(con, "numeric")
      size  <- readBin(con, "numeric") # ignore size for luminance perimetry
      level <- readBin(con, "numeric")
      res   <- opiPresent(makeStimHelper(x,y)(level, 0))
      sendToSocket(x, y, level, NA, res$seen, res$time, respWin, FALSE)
      next
    }
    if(command != "NEXT") stop("invalid command") # if at this stage, command is not NEXT, something's wrong
    # if command is RUN, then we continue running the ZEST test
    # send results to GUI
    
    if (length(unfinished) > 0) {
      
      # weight stimulus choice by growth pattern wave
      weight <- 1/(locs$wave[unfinished]^2)
      
      #choose next location to test
      if (length(unfinished) == 1) {
        loc <- unfinished[1]
      } else {
        loc <- sample(unfinished, 1, prob=weight)
        if (exists("previous_loc")) {while (loc == previous_loc) {loc <- sample(unfinished, 1, prob = weight)}} #ensure that same location doesn't get sampled twice in a row
        previous_loc <- loc
      }
      
      sr <- ZEST.step(states[[loc]]) #present stimulus and obtain response
      states[[loc]] <- sr$state #update state
      if (ZEST.stop(states[[loc]])) {
        #remove terminated location from queue
        ii <- which(unfinished == loc)
        unfinished <- unfinished[-ii]
        finished <- c(loc, finished)
        
        new_locs <- openNewLocs(findNeighbors(locs), loc, locs, unfinished, finished) #open up new locations
        unfinished <- sort(c(unfinished, new_locs)) #add new locations to queue

        #For newly opened locations, create PMF based on previous terminated location's final threshold
        if (length(new_locs) > 0) {
          for (i in new_locs) {
            nb <- intersect(which(findNeighbors(locs)[i, ] == TRUE), finished)  #find index of neighboring locations that have terminated
            avg_th <- mean(sapply(nb, function (x) ZEST.final(states[[x]]))) #calculate mean threshold of neighboring locations that have terminated
            states[[i]]$pdf <- makePMF(domain, avg_th)
          }
        }
      }
      sendToSocket(locs$x[loc], locs$y[loc], tail(states[[loc]]$stimuli, 1), max(-1, round(ZEST.final(states[[loc]]))), sr$resp$seen, sr$resp$time, respWin, ZEST.stop(states[[loc]]))
      # wait if necessary
      if(sr$resp$seen) {
        if (sr$resp$time > 150) {
          respWin <- c(sr$resp$time,respWin[-tail(respWin,1)]) #update recent response times vector
          opts$respWindow <- round(opts$respWinPed + mean(respWin)) #update response window
        }
        Sys.sleep(runif(1, min = opts$minISI, max = max(opts$minISI,mean(respWin)))/1000)
      }
    }
  }
  close(con)
}

