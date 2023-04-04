
# helper function for writing sections of data to file
write_section <- function(..., con) {
  x <- list(...)
  for (el in x) {
    if (is.null(el) | any(sapply(el, is.null))) {
      # warning("in ", deparse(el), ", element with index ", which(sapply(el, is.null)), " is NULL")
      next
    }
    # replace TRUE and FALSE with strings "true" and "false"
    el[sapply(el, isTRUE)] <- "true"
    el[sapply(el, function(x) identical(x, FALSE))] <- "false"
    # convert the element (which might be a list, data frame, vector, or matrix) to a matrix
    el <-
      if (is.null(dim(el))) {
        matrix(unlist(el), nrow = 1, dimnames = list(NULL, names(el)))
      } else as.matrix(el)
    # write the matrix to file and supress the warnings when using colnames while
    # appending to an existing file. This is what HYDRUS wants...
    suppressWarnings(
      write.table(
        # x = el,
        x = format(el, nsmall = 5L, scientific = 3L),
        file = con, append = TRUE, quote = FALSE, sep = "   ",
        row.names = FALSE, na = "", col.names = !is.null(colnames(el))
      )
    )
  }
}

open_con <- function(path, file_name) {
  # open file connection
  if (is.character(path)) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    file <- file.path(path.expand(path), file_name)
    con <- file(
      file,
      open = "w",
      encoding = "UTF-8"
    )
  } else if ("connection" %in% class(path)) {
    con <- path
    if (!isOpen(con)) open(con, open = "w", encoding = "UTF-8")
  } else stop("path must be a character string or a connection.")
  return(con)
}

row_check <- function(x, n) {
  df <- as.matrix(x)
  if(is.null(df)) return(NULL)
  if (nrow(df) == 1) {
    df[rep_len(1, n), ]
  } else if (nrow(df) >= n) {
    df[seq_len(n), ]
  } else {
    stop(deparse(substiute(df)), " needs nrow == 1 or >= ", n)
  }
}


#' Write HYDRUS input files
#'
#' @return None
#' @export
#'
write_input <- function(pars, path = ".") {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  write_selector(pars, path)
  write_bc(pars, path)
  write_profile(pars, path)
  invisible(pars)
}

#' Write file SELECTOR.IN for HYDRUS-1D
#'
#' @param path
#' Path to folder where SELECTOR.IN should be created. If missing, the
#' function writes to stdout.
#' @param pars
#' List of parameters
#' @param file_name
#' Name of file
#' @return
#' List of parameters
#' @export
#'
write_selector <- function(pars, path = ".", file_name = "SELECTOR.IN") {
  # Dummy value is always false
  pars$lDummy <- FALSE
  con <- open_con(path, file_name)
  on.exit(if(!("terminal" %in% class(con))) close(con))

  # write to file
  write_section(
    "Pcp_File_Version=4",
    "*** BLOCK A: BASIC INFORMATION *****************************************",
    pars["Head"],
    "LUnit TUnit MUnit (indicated units are obligatory for all input options)",
    pars$LUnit,
    pars$TUnit,
    pars$MUnit,
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
    row_check(
      pars$hydraulics[, c("thr", "ths", "alpha", "n", "Ks", "l")],
      pars$NMat
    ),
    con = con
  )
  write_section(
    "*** BLOCK C: TIME INFORMATION ******************************************",
    pars[c("dt", "dtMin", "dtMax", "dMul", "dMul2", "ItMin", "ItMax", "MPL")],
    pars[c("tInit", "tMax")],
    pars[c("lPrintD", "nPrintSteps", "tPrintInterval", "lEnter")],
    "TPrint(1),TPrint(2),...,TPrint(MPL)",
    pars$TPrint,
    con = con
  )
  if (pars$lTemp) {
    write_section(
      "*** BLOCK E: HEAT TRANSPORT INFORMATION ********************************",
      row_check(
        pars$heat[, c("Qn", "Qo", "Disper", "B1", "B2", "B3", "Cn", "Co", "Cw")],
        pars$NMat
      ),
      pars[c("tAmpl", "tPeriod", "Campbell", "MeltConst", "lSnowInit", "lDummy",
             "lDummy", "lDummy", "lDummy")],
      pars[c("MeltConst", "SublimConst", "InitSnow")],
      pars[c("kTopT", "TTop", "kBotT", "TBot")],
      con = con
    )
  }
  if(pars$lChem) {
    write_section(
      "*** BLOCK F: SOLUTE TRANSPORT INFORMATION ******************************",
      pars[c("Epsi", "lUpW", "lArtD", "lTDep", "cTolA", "cTolR", "MaxItC",
             "PeCr", "NSolutes", "lTort", "iBacter", "lFiltr", "nChPar")],
      pars[c("iNonEqul", "lWatDep", "lDualNEq", "lInitM", "lInitEq", "lTortA",
             "lDummy", "lDummy", "lDummy", "lDummy", "lDummy")],
      row_check(
        pars$transport[, c("BulkD", "DisperL", "Frac", "MobileWc")],
        pars$NMat
      ),
      pars$diffusivity,
      row_check(
        pars$reaction[, c("Ks", "Eta", "Beta", "Henry","SnkL1", "SnkS1", "SnkG1",
                          "SnkL1.",  "SnkS1.", "SnkG1.", "SnkL0", "SnkS0", "SnkG0", "Alfa")],
        pars$NMat
      ),
      pars[c("kTopSolute", "SolTop", "kBotSolute", "SolBot")],
      pars["tPulse"],
      con = con
    )
  }
  if(pars$lSink) {
    write_section(
      "*** BLOCK G: ROOT WATER UPTAKE INFORMATION *****************************",
      pars[c("iModSink", "cRootMax", "OmegaC")],
      pars$root[c("P0", "P2H", "P2L", "P3", "r2H", "r2L")],
      "POptm(1),POptm(2),...,POptm(NMat)",
      row_check(pars$root$POptm, pars$NMat),
      pars["lOmegaW"],
      con = con
    )
  }
  # End of File
  writeLines(
    "*** END OF INPUT FILE 'SELECTOR.IN' ************************************",
    con = con
  )
  invisible(pars)
}


#' Write boundary conditions to file
#'
#' @export
#'
write_bc <- function(pars, path = ".", file_name = "ATMOSPH.IN") {
  # dummy is always FALSE
  pars$lDummy <- FALSE
  con <- open_con(path, file_name)
  on.exit(if(!("terminal" %in% class(con))) close(con))

  write_section(
    "Pcp_File_Version=4",
    "*** BLOCK I: ATMOSPHERIC INFORMATION  **********************************",
    "MaxAL                    (MaxAL = number of atmospheric data-records)",
    nrow(pars$bc),
    pars[c("DailyVar", "SinusVar", "lLai", "lBCCycles", "lInterc",
           "lDummy", "lDummy", "lDummy", "lDummy", "lDummy")],
    {if (pars$lLai) pars["rExtinct"] else (NULL)},
    {if (pars$lLai && pars$lInterc) pars["aIntercep"] else (NULL)},
    {if (pars$lBCCycles) pars["nCycles"] else (NULL)},
    pars["hCritS"],
    pars$bc,
    "end",
    "*** END OF INPUT FILE 'ATMOSPH.IN' **********************************",
    con = con
  )
  invisible(pars)
}

#' Write soil profile information to file
#'
#' @export
#'
write_profile <- function(pars, path = ".", file_name = "PROFILE.DAT") {
  # dummy is always FALSE
  pars$lDummy <- FALSE
  con <- open_con(path, file_name)
  on.exit(if(!("terminal" %in% class(con))) close(con))

  write_section(
    "Pcp_File_Version=4",
    # number of fixed nodes. Has to be two for running hydrus without the UI
    "2",
    # top node
    c(1, max(pars$profile[, "x"]), 1, 1),
    # bottom node
    c(2, min(pars$profile[, "x"]), 1, 1),
    c(nrow(pars$profile), ifelse(pars$lChem, pars$NSolutes, 0L), as.integer(pars$lTemp), colnames(pars$profile)),
    signif(unname(as.matrix(pars$profile[, c("node", "x", "h", "Mat", "Lay", "Beta", "Axz", "Bxz", "Dxz", "Temp", "Conc")])), 5),
    length(pars$ObsPoints),
    # at which node are the observation points
    which(unlist(pars$profile[, "x"]) %in% -pars$ObsPoints),
    con = con
  )
  invisible(pars)
}
