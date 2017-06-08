#' Read from HYDRUS-1D output files
#'
#' @import tibble magrittr stringr
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
#' @rdname read_output
read_output <- function(file_path, skip = 0) {
  lines <- readr::read_lines(file_path, skip)

  readr::read_table(
    file = file_path,
    col_names = extr_col_names(lines[1]),
    skip = skip + 3, # The first 3 lines are the header
    col_types = readr::cols(.default = "d"),
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

#' @param path
#' Path to Output directory
#' @return list of data.frames
#' @export
#' @rdname read_output
read_nod_inf <- function(path) {
  file_path <- file.path(path, "Nod_Inf.out")
  nod_inf_lines <- readr::read_lines(file_path)
  col_names <- extr_col_names(nod_inf_lines[11])

  pattern <- "^[ \\t]*Time:\\s+"
  time_stamp <-
    str_subset(nod_inf_lines, pattern) %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.double()

  skip <- grep(pattern, nod_inf_lines) + 5
  n_max <- grep("^end", nod_inf_lines) - skip - 1

  block_list <- list()
  for (i in seq_along(skip)) {
    block_list[[i]] <-
      readr::read_table(
        file = file_path,
        col_types = readr::cols(.default = "d"),
        col_names = col_names,
        skip = skip[i],
        n_max = n_max[i]
      )
  }
  attr(block_list, "time") <- time_stamp
  return(block_list)
}

#' @param path
#' Path to Output directory
#' @param skip
#' Skip number of lines on input
#'
#' @return data.frame
#' @export
#' @rdname read_output
read_obs_node <- function(path, skip = 11) {
  file_path <- file.path(path, "Obs_Node.out")
  obs_node_lines <- readr::read_lines(file_path, n_max = skip)
  col_names <- extr_col_names(obs_node_lines[skip])

  nodes <-
    str_extract_all(obs_node_lines[skip - 2], "[0-9]+", simplify = TRUE) %>% # two or more numericals
    as.integer()

  columns <- readr::fwf_empty(file_path, skip = skip, col_names = col_names)
  parms <- (length(col_names) - 1) / length(nodes)

}
