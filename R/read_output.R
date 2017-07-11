#' Read from HYDRUS-1D output files
#'
#' @import tibble magrittr stringr
#' @name read_output
NULL

extr_col_names <- function(string) {
  string %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE) %>%
    # str_to_lower() %>%
    str_replace("\\(|\\/", "_") %>%
    str_replace("\\)", "")
}

#' @param path
#' Path to Output directory
#' @param skip
#' skip number of lines on input
#' @return data.frame
#' @export
#' @rdname read_output
read_t_level <- function(path, skip = 6) {
  file_path = file.path(as.character(path), "T_Level.out")
  t_level_lines <- readr::read_lines(file_path, skip)

  readr::read_table(
    file = file_path,
    col_names = extr_col_names(t_level_lines[1]),
    skip = skip + 3, # The first 3 lines are the header
    col_types = readr::cols(.default = "d"),
    n_max = sum(str_count(t_level_lines, "^ +\\d+"))
  )
}

#' @return list of data.frames
#' @export
#' @rdname read_output
read_nod_inf <- function(path) {
  file_path <- file.path(path, "Nod_Inf.out")
  nod_inf_lines <- readr::read_lines(file_path)
  col_names <- extr_col_names(nod_inf_lines[11]) # first column unnecessary

  pattern <- "^[ \\t]*Time:\\s+"
  time_stamp <-
    str_subset(nod_inf_lines, pattern) %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.double()

  skip <- str_which(nod_inf_lines, pattern) + 5
  n_max <- (str_which(nod_inf_lines, "^end") - skip - 1)[1]

  blocks <- array(dim = c(n_max, length(col_names),length(time_stamp)),
                  dimnames = list(NULL, col_names, time_stamp))
  for (i in seq_along(time_stamp)) {
    blocks[, , i] <-
      readr::read_table(
        file = file_path,
        col_types = readr::cols(.default = "d"),
        col_names = col_names,
        skip = skip[i],
        n_max = n_max
      ) %>%
      as.matrix()
    # colnames(blocks[, , i]) <-
  }
  # dimnames(blocks) <-
  aperm(blocks, c(3, 1, 2))
}

#' @param header
#' Number of lines with header information before the actual data
#'
#' @return data.frame
#' @export
#' @rdname read_output
read_obs_node <- function(path, header = 11) {
  file_path <- file.path(path, "Obs_Node.out")
  obs_node_header <- readr::read_lines(file_path, n_max = header)
  col_names <-
    str_subset(obs_node_header, "^[ ]*time") %>%
    extr_col_names() %>%
    unique()

  nodes <-
    str_subset(obs_node_header, pattern = "Node") %>%
    str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
    as.integer()

  par_per_node <- length(col_names)

  data <-
    readr::read_table(
      file = file_path,
      skip = header,
      col_names = FALSE,
      col_types = readr::cols(.default = "d")
    )

  node <- data[1:par_per_node]
  colnames(node) <- col_names
  node["node"] <- nodes[1]
}
