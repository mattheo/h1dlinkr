#' Write HYDRUS input files
#'
#' @return None
#' @import magrittr
#' @export
#'
write_input <- function(path, options) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE)
  file.copy(dir(options, full.names = T), to = path, recursive = TRUE)
}

# helper function for writing sections of data to file
write_section <- function(..., sep = "    ", con) {
  x <- list(...)
  for (el in x) {
    if (all(is.null(el)) | all(is.na(el))) next
    if (is.null(names(el))) {
      writeLines(paste(el, collapse = sep), con = con)
    } else {
      writeLines(
        c(
          paste(names(el), collapse = sep),
          paste(el, collapse = sep)
        ),
        con = con
      )
    }
  }
}

#' Write file SELECTOR.IN for HYDRUS-1D
#'
#' @param path
#'
#' @return NULL
#' @export
#'
write_selector <- function(
  path,
  ...,
  options = list(
    # Block A Basic Inforamtion
    Head = "run from R",
    # units
    LUnit = "cm", TUnit = "days", MUnit = "mmol",
    # 1st section
    lWat = TRUE, lChem = TRUE, lTemp = TRUE, lSink = TRUE, lRoot = FALSE,
    lShort = FALSE, lWDep = FALSE, lScreen = TRUE, lVariabBC = TRUE,
    lEquil = TRUE,
    # 2nd section
    lSnow = TRUE, lMeteo = FALSE, lVapor = FALSE,
    lActRSU = FALSE, lFlux = FALSE, lIsotope = FALSE,
    # 3rd section
    NMat = 1, NLay = 1, CosAlf = 1,
    # Block B Water Flow Information
    # 1st section
    MaxIt = 20, TolTh = 0.001, TolH = 1,
    # 2nd section
    TopInF = TRUE, WLayer = TRUE, KodTop = -1, lInitW = FALSE,
    # 3rd section
    BotInF = FALSE, qGWLF = FALSE, FreeD = TRUE, SeepF = FALSE, KodBot = -1,
    qDrain = FALSE, hSeep = 0,
    # 4th section material
    hTab1 = 1e-6, hTabN = 1e6,
    iModel = 0, iHyst = 0,
    iKappa = 1,
    hydraulics = data.frame(
      thr = 0.08, ths = 0.45, Alfa = 0.036, n = 1.56, Ks = 25, l = 0.5
    ),
    # Block C Time Information
    dt = 0.01, dtMin = 1e-07, dtMax = 1, dMul = 1.3, dMul2 = 0.7, ItMin = 3, ItMax = 7, MPL = 10,
    tInit = 0, tMax = 1, TPrint = NA,
    lPrintD = TRUE, nPrStep = 1, tPrintInt = 1, lEnter = FALSE,
    # Block E Heat Transport
    heat = data.frame(
      Qn = 0.4, Qo = 0, Disper = 5, B1 = 1.47054e+016, B2 = -1.5518e+017,
      B3 = 3.16617e+017, Cn = 1.43327e+014, Co = 1.8737e+014, Cw = 3.12035e+014
    )

  ),
  file_version = 4L
) {
  # insert options that have been supplied at function call
  params <- list(...)
  options[names(params)] <- params
  # Dummy value is always false
  options$lDummy <- FALSE
  # calculate TPrint if missing
  if (any(is.na(options$TPrint))) {
    round(seq(options$tInit, options$tMax, length.out = options$MPL), digits = 3)
  }
  # replace TRUE and FALSE in the options list with "t" and "f"
  options[sapply(options, isTRUE)] <- "t"
  options[sapply(options, function(x) identical(x, FALSE))] <- "f"

  # open file connection
  file_path <- file.path(file.path(path), "SELECTOR.IN")
  f <- file(file_path, open = "w", encoding = "UTF-8")
  on.exit(close(f))

  # write to file
  write_section(
    paste0("Pcp_File_Version=", file_version),
    # BLOCK A
    "*** BLOCK A: BASIC INFORMATION *****************************************",
    options["Head"],
    # units
    "LUnit TUnit MUnit (indicated units are obligatory for all input options)",
    options[["LUnit"]],
    options[["TUnit"]],
    options[["MUnit"]],
    options[c("lWat", "lChem", "lTemp", "lSink", "lRoot", "lShort", "lWDep",
        "lScreen", "lVariabBC", "lEquil", "lDummy")],
    options[c("lSnow", "lDummy", "lMeteo", "lVapor", "lActRSU", "lFlux",
        "lDummy", "lIsotope", "lDummy", "lDummy")],
    options[c("NMat", "NLay", "CosAlf")],
    con = f
  )
  # BLOCK B
  write_section(
    "*** BLOCK B: WATER FLOW INFORMATION ************************************",
    options[c("MaxIt", "TolTh", "TolTh")],
    options[c("TopInF", "WLayer", "KodTop", "lInitW")],
    options[c("BotInF", "qGWLF", "FreeD", "SeepF", "KodBot", "qDrain", "hSeep")],
    con = f
  )
  # 4th section material information
  write_section(
    options[c("hTab1", "hTabN")],
    options[c("iModel", "iHyst")],
    ifelse(options$iHyst > 0, options["iKappa"], NA),
    options$hydraulics,
    con = f
  )
  # BLOCK C
  write_section(
    "*** BLOCK C: TIME INFORMATION ******************************************",
    options[c("dt", "dtMin", "dtMax", "dMul", "dMul2", "ItMin", "ItMax", "MPL")],
    options[c("tInit", "tMax")],
    options[c("lPrintD", "nPrStep", "tPrintInt", "lEnter")],
    "TPrint(1),TPrint(2),...,TPrint(MPL)",
    options$TPrint,
    con = f
  )
  # BLOCK E
  write_section(
    "*** BLOCK E: HEAT TRANSPORT INFORMATION ********************************",
    options$heat,
    con = f
  )

}
