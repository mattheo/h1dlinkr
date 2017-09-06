#' @import tibble magrittr stringr readr
extr_col_names <- function(string) {
  string %>%
    str_trim() %>% # trim whitespace from start and end
    str_split("\\s+", simplify = TRUE) %>% # split the string at whitespace
    # str_to_lower() %>%
    str_replace("\\(|\\/", "_") %>% #replace opening parentheses and slashes with lodash
    str_replace("\\)", "") # remove closing parentheses
}

#' Read HYDRUS-1D output files into R
#'
#' @param project Either a h1d_run object or a path to a file (single string)
#'
#' @return list of data frames
#' @export
#' @name read_output
read_output <- function(project = ".") {
  project_attr <- attributes(project)
  if ("h1d_run" %in% class(project)) {
    path <- project_attr$path
  } else {
    path <- file.path(project[1L])
  }
  stopifnot(!is.null(path))
  files <- list.files(path, full.names = TRUE)
  out <- list(
    t_level = read_t_level(str_subset(files, "(?i)t_Level")),
    a_level = read_a_level(str_subset(files, "(?i)a_Level")),
    nod_inf = read_nod_inf(str_subset(files, "(?i)nod_inf")),
    obs_node = read_obs_node(str_subset(files, "(?i)obs_node")),
    balance = read_balance(str_subset(files, "(?i)balance"))
    # stdout = ifelse("h1d_run" %in% class(project),
                    # as.vector(project),
                    # read_stdout(str_subset(files, "(?i)run.out")))
  )
  structure(
    out,
    success = project_attr$success,
    path = path,
    runtime = project_attr$runtime,
    pid = project_attr$pid,
    class = c("h1d_output")
  )
}

#' @param file_path
#' Path to T_level output file
#' @param skip
#' skip number of lines on input
#' @return data.frame
#' @export
#' @rdname read_output
read_t_level <- function(file_path, skip = 6) {
  t_level_lines <- read_lines(file_path, skip)

  out <- read_table(
    file = file_path,
    col_names = extr_col_names(t_level_lines[1]),
    skip = skip + 3, # The first 3 lines are header
    col_types = cols(.default = "d"),
    n_max = sum(str_count(t_level_lines, "^ +\\d+"))
  )
  attributes(out) <- attributes(out)[c("names", "row.names", "class")]
  return(out)
}


#' @return data.frame
#' @export
#'
#' @rdname read_output
read_a_level <- function(file_path, skip = 2) {
  read_t_level(file_path, skip)
}

#' @return list of data.frames
#' @export
#' @rdname read_output
read_nod_inf <- function(file_path) {
  nod_inf_lines <- read_lines(file_path)

  col_names <- extr_col_names(nod_inf_lines[11])
  time_idx <- str_which(nod_inf_lines, "^[ \\t]*Time:\\s+")
  time_stamp <-
    nod_inf_lines[time_idx] %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.double()
  slice_start <- time_idx + 6
  len <- (str_which(nod_inf_lines, "^end")[1] - slice_start[1])
  idx <- sapply(slice_start, seq.int, length.out = len)
  depth <-
    read_table(
      file_path,
      col_names = col_names,
      col_types = cols_only(Depth = "d"),
      skip = slice_start[1] - 1,
      n_max = len
    ) %>%
    unlist(use.names = FALSE)

  read_table(
    file = paste0(nod_inf_lines[idx], collapse = "\n"),
    col_types = cols(.default = "d"),
    col_names = col_names
  )[, -(1:2)] %>% # remove node and depth columns
    lapply(matrix, nrow = len) %>%
    structure(x = time_stamp, z = depth)
}

#' @param header
#' Number of lines with header information before the actual data
#'
#' @return data.frame
#' @export
#' @rdname read_output
read_obs_node <- function(file_path, header = 11) {
  obs_node_header <- read_lines(file_path, n_max = header)
  col_names <-
    str_subset(obs_node_header, "^[ ]*time") %>%
    extr_col_names() %>%
    unique()
  if (length(col_names) == 0) stop("No columns found.")

  data <-
    read_table(
      file = file_path,
      skip = header,
      col_names = FALSE,
      col_types = cols(.default = "d")
    )

  nodes <-
    str_subset(obs_node_header, pattern = "Node") %>%
    str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
    as.integer()
  par_per_node <- length(col_names) - 1
  out <- lapply(
    seq_along(nodes),
    function(i) {
      node <- data[c(1, ((i - 1) * par_per_node + 2):(i * par_per_node + 1))]
      colnames(node) <- col_names
      node["node"] <- nodes[i]
      return(node)
    })
  do.call(rbind, out)
}

read_balance <- function(file_path) {
  balance_lines <- read_lines(file_path)
  time_idx <- str_which(balance_lines, "^[ \\t]*Time\\s+")
  slice_idx <- sapply(time_idx, `+`, 4:11) %>% as.numeric()
  time_stamp <-
    balance_lines[time_idx] %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.double()
  col_names <-
    balance_lines[time_idx[1] + 4:11] %>%
    str_sub(end = 19) %>%
    str_replace("\\[.*\\]", "") %>%
    str_replace_all("\\s+", "")
  values <- str_sub(balance_lines[slice_idx], start = 20) %>% paste0(collapse = "\n")
  df <-
    read_table(
      values,
      col_types = cols(.default = "d"),
      col_names = FALSE
    )
  n_regions <- ncol(df)
  out <-
    df %>%
    unlist(use.names = FALSE) %>%
    matrix(ncol = length(col_names), dimnames = list(NULL, col_names), byrow = TRUE) %>%
    as_tibble()
  out["Time"] <- rep.int(time_stamp, n_regions)
  out["Region"] <- rep(seq.int(0, n_regions - 1), each = length(time_stamp))
  out[c(ncol(out) - 1, ncol(out), 1:(ncol(out) - 2))]
}

#' @return data.frame
#' @export
#' @rdname read_output
read_stdout <- function(file_path) {
  stdout_lines <- read_lines(file_path)
  # str_which(" Beginning of numerical solution.", stdout_lines[1:50])
}
