#' First local maximum
#'
#' @param x numeric vector
#'
#' @return position
#' @export
#'
#' @examples
#' flm(1:3)
flm <- function(x){
  if(length(x) == 1){
    return(1)
  }else{
    return(which.max(diff(x) <= 0))
  }
}

#' New kml3d
#'
#' Modified to turn off saving.  I am quite sure it needs kml3d libraried.
#'
#' @param object C
#' @param nbClusters h
#' @param nbRedrawing r
#' @param toPlot i
#' @param parAlgo s
#'
#' @return t
#' @export
#'
#' @examples
#' # No thanks.
dk.kml3d <- function (object, nbClusters = 2:6, nbRedrawing = 20,
                      toPlot = "none", parAlgo = parKml3d()){
    if (inherits(object, "ClusterLongData")) {
      stop("[kml3d]: kml3d is for joint longitudinal data (object 'ClusterLongData3d').\nFor classic longitudinal data (object of class 'ClusterLongData'), use kml")
    }
    else {
    }
    nameObject <- deparse(substitute(object))
    on.exit(if (toPlot != "none") {
      close.screen(listScreen)
    } else {
    })
    if (parAlgo["scale"]) {
      scale(object)
    }
    else {
    }
    nbIdFewNA <- object["nbIdFewNA"]
    convergenceTime <- 0
    traj <- object["traj"]
    nbTime <- length(object["time"])
    saveCld <- 0
    listScreen <- cutScreen(toPlot)
    if (toPlot %in% c("both", "criterion")) {
      screen(listScreen[2])
      plotCriterion(as(object, "ListPartition"), nbCriterion = parAlgo["nbCriterion"])
    }
    else {
    }
    startingCond <- expandStartingCond(parAlgo["startingCond"],
                                       nbRedrawing, object["initializationMethod"])
    object["initializationMethod"] <- unique(c(object["initializationMethod"],
                                               startingCond))
    fast <- kml3d:::fastOrSlow3d(toPlot, parAlgo["distanceName"])
    for (iRedraw in 1:nbRedrawing) {
      for (iNbClusters in nbClusters) {
        saveCld <- saveCld + 1
        clustersInit <- initializePartition(nbClusters = iNbClusters,
                                            lengthPart = nbIdFewNA, method = startingCond[iRedraw],
                                            data = traj)
        clust <- rep(NA, nbIdFewNA)
        if (fast) {
          resultKml <- kml3d:::kml3dFast(traj = traj, clusterAffectation = clustersInit)
        }
        else {
          if (toPlot %in% c("both", "traj")) {
            screen(listScreen[1])
          }
          else {
          }
          resultKml <- kml3dSlow(traj = traj, clusterAffectation = clustersInit,
                                 toPlot = toPlot, parAlgo = parAlgo)
        }
        object["add"] <- resultKml
        assign(nameObject, object, envir = parent.frame())
        if (saveCld%%parAlgo["saveFreq"] == 0) {
          # save(list = nameObject, file = paste(nameObject,
          #                                      ".Rdata", sep = ""))
          cat("S\n", saveCld, " ", sep = "")
        }
        else {
          cat("*")
        }
        if (saveCld%%100 == 0) {
          cat("\n")
        }
        else {
        }
        if (toPlot == "both") {
          screen(listScreen[2])
          plotCriterion(as(object, "ListPartition"), nbCriterion = parAlgo["nbCriterion"])
        }
        else {
          if (toPlot == "criterion") {
            plotCriterion(as(object, "ListPartition"),
                          nbCriterion = parAlgo["nbCriterion"])
          }
          else {
          }
        }
      }
    }
    cat("\n")
    ordered(object)
    if (saveCld < Inf) {
      # save(list = nameObject, file = paste(nameObject, ".Rdata",
      #                                      sep = ""))
      cat("S\n")
    }
    else {
      cat("\n")
    }
    if (toPlot == "both") {
      screen(listScreen[2])
      plotCriterion(as(object, "ListPartition"), nbCriterion = parAlgo["nbCriterion"])
    }
    else {
      if (toPlot == "criterion") {
        plotCriterion(as(object, "ListPartition"), nbCriterion = parAlgo["nbCriterion"])
      }
      else {
      }
    }
    if (parAlgo["scale"]) {
      restoreRealData(object)
    }
    else {
    }
    assign(nameObject, object, envir = parent.frame())
    return(invisible())
  }
