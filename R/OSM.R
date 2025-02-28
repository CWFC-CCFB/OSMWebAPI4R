#'
#' A list of three plots that can be passed to OSM Web API
#'
#' @docType data
#'
#' @usage data(OSMThreeStandList)
#'
#' @keywords datasets
#'
#' @examples
#' data(OSMThreeStandList)
"OSMThreeStandList"

VersionWarningStr <- "VersionWarning"
MessageToClientStr <- "MessageToClient"
ShortLicenseStr <- "ShortLicense"

host <- "http://repicea.dynu.net/OSMSimulation"

#host <- "https://localhost:7032/OSMSimulation" #for debugging

.onAttach <- function(libname, pkgname) {
  tryCatch(
    {
      status <- OSMStatus()
      if (VersionWarningStr %in% names(status)) {
        warning(status[[VersionWarningStr]])
      }
      if (MessageToClientStr %in% names(status)) {
        packageStartupMessage(status[[MessageToClientStr]])
      }
      if (ShortLicenseStr %in% names(status)) {
        packageStartupMessage(status[[ShortLicenseStr]])
      }
    },
    error = function(cond) {
      warning("R is unable to connect to CFSStandGrowth Web API! \n It may be that your internet connection is not working or your firewall does not allow for http requests.")
    }
  )

}


.processHttpRequest <- function(url, myQuery = list()) {
  r <- httr::GET(url, query = myQuery);
  if (r$status_code != 200) {
    stop(httr::content(r, "text"))
  }
  result <- httr::content(r, "text")
  resultJSON <- jsonlite::fromJSON(result)
}

#'
#' Provide the List of Variants
#'
#' @return a vector of characters
#'
#' @export
OSMGetVariantList <- function() {
  url <- paste(host, "VariantList", sep="/")
  result <- .processHttpRequest(url)
  return (result)
}


#'
#' Provide the species for
#'
#' @param variant a valid variant (string)
#' @param type either All, Broadleaved, or Coniferous
#' @param outputAsVector a logical true provides a vector of species code
#'
#' @return a vector if outputAsVector is set to true or a list otherwise
#'
#' @export
OSMGetVariantSpecies <- function(variant, type = "All", outputAsVector = T) {
  url <- paste(host, "VariantSpecies", sep="/")
  result <- .processHttpRequest(url, list(variant = variant, type = type))
  if (outputAsVector) {
    resultVector <- vector(mode="character", length=length(result))
    for (i in 1:length(result)) {
      resultVector[i] = result[[i]]$key
    }
    return (resultVector)
  } else {
    return(result)
  }
}


#'
#' Provide the Different Outputs of OSM
#'
#' @return a vector with the different outputs
#'
#' @export
OSMGetOutputRequestTypes <- function() {
  url <- paste(host, "OutputRequestTypes", sep="/")
  result <- .processHttpRequest(url)
  return (result)
}

#'
#' Provide a List of Fields Required by a Variant
#'
#' @param variant a valid variant (string)
#' @return a list of data.frame instances containing the information on
#' the fields
#'
#' @export
OSMGetVariantFields <- function(variant) {
  url <- paste(host, "VariantFields", sep="/")
  result <- .processHttpRequest(url, list(variant = variant))
  return(result)
}

#'
#' Provide the Status of OSM Web API
#'
#' @return a list of data.frame instances containing the information on
#' the fields
#'
#' @export
OSMStatus <- function() {
  url <- paste(host, "Status", sep="/")
  result <- .processHttpRequest(url, list(clientversion = as.character(utils::packageVersion("OSMWebAPI4R"))))
  return(result)
}


.convertDataFrameToCSVString <- function(dataFrameInstance) {
  outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
  outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
  outputString <- paste(outputVector, collapse = "\r\n")
  return(outputString)
}

#'
#' Run a Simulation with OSM
#'
#' @param data a data.frame instance properly formatted
#' @param outputRequestList an OutputRequestList instance
#' @param variant a valid variant (string)
#' @param years an integer that stands for the simulation length (yrs)
#' @param ypc the step length (yrs)
#'
#' @return a SimulationResult instance
#'
#' @export
OSMSimulate <- function(data, outputRequestList, variant, years, ypc) {
  if (!methods::is(outputRequestList, "OutputRequestList")) {
    stop("The outputRequestList argument must be an instance of OutputRequestList class!\r Use the CFSCommonGYModelWebAPI4R::new_OutputRequestList() function.")
  }
  outputRequestListJSON <- outputRequestList$toJSONString()
  csvData <- .convertDataFrameToCSVString(data)
  url <- paste(host, "Simulate", sep="/")
  r <- httr::POST(url,
                  query = list(years = as.character(years), variant = variant, ypc = as.character(ypc)),
                  body = list(data=csvData, output=outputRequestListJSON),
                  encode = "multipart")
  if (r$status_code != 200) {
    stop(httr::content(r, "text"))
  }
  result <- httr::content(r, "text")
  resultJSON <- jsonlite::fromJSON(result)
  osmResult <- CFSCommonGYModelWebAPI4R::new_SimulationResult(resultJSON)
  return(osmResult)
}

