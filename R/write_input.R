#' Write HYDRUS input files
#'
#' @return None
#' @import magrittr
#' @export
#'
write_input <- function(path, parameters) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE)
  file.copy(dir(parameters, full.names = T), to = path, recursive = TRUE)
}

# helper function for writing sections of data to file
write_section <- function(..., con) {
  x <- list(...)
  # EXTREMELY HACKY!
  # used to create fixed width data fields in the file
  # redirect stdout to the specified conncetion
  # sink(file = con, append = TRUE, type = "output")
  for (el in x) {
    # if (all(is.null(el)) || all(is.na(el))) next
    # replace TRUE and FALSE in the pars list with "t" and "f"
    el[sapply(el, isTRUE)] <- "t"
    el[sapply(el, function(x) identical(x, FALSE))] <- "f"

    suppressWarnings(
      write.table(el, file = con, append = TRUE, quote = FALSE, sep = "    ",
                  row.names = FALSE, col.names = !is.null(names(el)))
    )
    # if (is.null(names(el))) {
    #   writeLines(paste0(el, collapse = "    "), con = con)
    # } else {
    #   # print(as.data.frame(el), quote = FALSE, row.names = FALSE)
    #   writeLines(
    #     c(
    #       paste(names(el), collapse = "    "),
    #       paste(el, collapse = "    ")
    #     ),
    #     con = con
    #   )
    # }
  }
  # if (con != stdout ()) sink(NULL)
}

#' Write file SELECTOR.IN for HYDRUS-1D
#'
#' @param folder
#' Path to folder where SELECTOR.IN should be created. If missing, the
#' function writes to stdout.
#' @param pars
#' List of parameters
#' @param ...
#' Additional parmaters
#' @return
#' Listt of parameters
#' @export
#'
write_selector <- function(pars = list(), folder, ...) {
  # insert paramters that have been supplied at function call
  par_dots <- list(...)
  pars[names(par_dots)] <- par_dots
  # calculate TPrint if missing
  pars$TPrint <-
    if (is.null(pars$TPrint)) {
      with(
        pars,
        seq(tInit, tMax, length.out = MPL) %>%
          round(digits = (nchar(MPL) - 1)/ nchar(tMax))
      )
    } else pars$TPrint
  # Dummy value is always false
  pars$lDummy <- FALSE
  # open file connection
  if (missing(folder) || is.null(folder)) {
    con <- stdout()
  } else {
    file_path <- file.path(file.path(folder), "SELECTOR.IN")
    con <- file(file_path, open = "w", encoding = "UTF-8")
    on.exit(close(con))
  }

  # write to file
  write_section(
    paste0("Pcp_File_Version=", pars$file_version),
    "*** BLOCK A: BASIC INFORMATION *****************************************",
    pars["Head"],
    "LUnit TUnit MUnit (indicated units are obligatory for all input options)",
    pars[["LUnit"]],
    pars[["TUnit"]],
    pars[["MUnit"]],
    pars[c("lWat", "lChem", "lTemp", "lSink", "lRoot", "lShort", "lWDep",
              "lScreen", "lVariabBC", "lEquil", "lInverse")],
    pars[c("lSnow", "lHP1", "lMeteo", "lVapor", "lActiveU", "lFluxes",
              "lIrrig", "lIsotope", "lDummy", "lDummy")],
    pars[c("NMat", "NLay", "CosAlpha")],
    con = con
  )
  write_section(
    "*** BLOCK B: WATER FLOW INFORMATION ************************************",
    pars[c("MaxIt", "TolTh", "TolH")],
    pars[c("TopInF", "WLayer", "KodTop", "InitCond")],
    pars[c("BotInF", "qGWLF", "FreeD", "SeepF", "KodBot", "qDrainF", "hSeep")],
    pars[c("hTab1", "hTabN")],
    pars[c("Model", "Hysteresis")],
    pars[["hydraulics"]],
    con = con
  )
  write_section(
    "*** BLOCK C: TIME INFORMATION ******************************************",
    pars[c("dt", "dtMin", "dtMax", "dMul", "dMul2", "ItMin", "ItMax", "MPL")],
    pars[c("tInit", "tMax")],
    pars[c("lPrintD", "nPrintSteps", "tPrintInterval", "lEnter")],
    "TPrint(1),TPrint(2),...,TPrint(MPL)",
    pars[["TPrint"]],
    con = con
  )
  write_section(
    "*** BLOCK E: HEAT TRANSPORT INFORMATION ********************************",
    pars[["heat"]],
    pars[c("tAmpl", "tPeriod", "Campbell", "MeltConst", "lSnowInit", "lDummy",
           "lDummy", "lDummy", "lDummy")],
    pars[c("MeltConst", "SublimConst", "InitSnow")],
    pars[c("kTopT", "TTop", "kBotT", "TBot")],
    con = con
  )
  write_section(
    "*** BLOCK F: SOLUTE TRANSPORT INFORMATION ******************************",
    pars[c("Epsi", "lUpW", "lArtD", "lTDep", "cTolA", "cTolR", "MaxItC",
              "PeCr", "No.Solutes", "lTort", "iBacter", "lFiltr", "nChPar")],
    pars[c("iNonEqul", "lWatDep", "lDualNEq", "lInitM", "lInitEq", "lTortA",
              "lDummy", "lDummy", "lDummy", "lDummy", "lDummy")],
    pars[["solute"]],
    pars[["diffusivity"]],
    pars[["reaction"]],
    pars[c("kTopSolute", "SolTop", "kBotSolute", "SolBot")],
    pars[c("tPulse")],
    con = con
  )
  write_section(
    "*** BLOCK G: ROOT WATER UPTAKE INFORMATION *****************************",
    pars[c("iModSink", "cRootMax", "OmegaC")],
    pars[c("P0", "P2H", "P2L", "P3", "r2H", "r2L")],
    "POptm(1),POptm(2),...,POptm(NMat)",
    pars[["POptm"]],
    pars["lOmegaW"],
    con = con
  )
  writeLines(
    "*** END OF INPUT FILE 'SELECTOR.IN' ************************************",
    con = con
  )
  invisible(pars)
}

