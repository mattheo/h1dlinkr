#' Read from HYDRUS-1D output files
#'
#' @import magrittr dplyr stringr stats
#' @name read_output
NULL

extr_col_names <- function(string) {
  string %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE) %>%
    str_to_lower() %>%
    str_replace("\\(", "_") %>%
    str_replace("\\)", "")
}

#' @param file_path
#' Path to *.out file
#' @param skip
#' skip number of lines on input
#' @return data.frame
#' @export
read_output <- function(file_path, skip = 0) {
  lines <- readr::read_lines(file_path, skip)

  readr::read_table(
    file = file_path,
    col_names = extr_col_names(lines[1]),
    skip = skip + 3, # The first 3 lines are the header
    # col_types = paste(rep("d", length(col_names)), collapse = ""),
    n_max = length(lines) - 4 # exclude header and last line
  )
}


#' @param path
#' Path to Output directory
#' @param skip
#' Skip number of lines on input
#' @return data.frame
#' @export
#' @rdname read_output
read_t_level <- function(path) {
  read_output(file.path(path, "T_Level.out"), skip = 6)
}

read_nod_inf <- function(path) {
  file_path <- file.path(path, "Nod_Inf.out")
  nod_inf_lines <- readr::read_lines(file_path)

  pattern <-"^\\s*Time:\\s+([0-9]+\\.?[0-9]*)"
  time_stamp <-
    str_match(nod_inf_lines, pattern)[, 2] %>%
    na.omit() %>%
    as.double()

  start <- grep(pattern, nod_inf_lines)
  end <- grep("^end", nod_inf_lines)

  conn <- rawConnection(nod_inf_lines[start[1]:end[1]])
  read_output(conn, skip = 3)

}
