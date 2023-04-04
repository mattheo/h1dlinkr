#' @import tibble magrittr stringr readr

# helper function for extracting column names
extr_col_names <- function(string) {
  string %>%
    str_trim() %>% # trim whitespace from start and end
    str_split("\\s+", simplify = TRUE) %>% # split the string at whitespace
    str_replace("\\(|\\/", "_") %>% #replace opening parentheses and slashes with lodash
    str_replace("\\)", "") %>% # remove closing parentheses
    str_replace("-", "_") # replace dash with low dash
}

# helper function for getting a stack trace when reading the files and something fails
get_stackTrace <-  function(expr, project, silent = FALSE) {
  callstack <- NULL
  out <-
    try(withCallingHandlers(
      expr,
      error = function(c) {
        # project <- dynGet("project")
        callstack <<- sys.calls()
        message("Error when reading ", basename(project))
        # print(head(out, -2))
      }
    ), silent = silent)
  if (inherits(out, "try-error")) out <- callstack
  out
}

#' Read HYDRUS-1D output files into R
#'
#' @param project
#' Either a h1d_run object or a path to a file (single string)
#'
#' @return list of data frames
#' @export
read_output <- function(project, ...) {
  UseMethod("read_output")
}

#' @export
read_output.default <- function(project = ".", silent = TRUE, ...) {
  path <- path.expand(project)
  files <- list.files(path, full.names = TRUE)

  t_level <- get_stackTrace(read_t_level(str_subset(files, "(?i)t_level")), project, silent)
  a_level <- get_stackTrace(read_a_level(str_subset(files, "(?i)a_level")), project, silent)
  nod_inf <- get_stackTrace(read_nod_inf(str_subset(files, "(?i)nod_inf")), project, silent)
  obs_node <- get_stackTrace(read_obs_node(str_subset(files, "(?i)obs_node")), project, silent)
  balance <- get_stackTrace(read_balance(str_subset(files, "(?i)balance")), project, silent)

  structure(
    list(
      t_level = t_level,
      a_level = a_level,
      nod_inf = nod_inf,
      obs_node = obs_node,
      balance = balance,
      project = project
    ),
    class = "h1d_output"
  )
}

#' @export
read_output.h1d_run <- function(h1d_run, silent = TRUE, ...) {
  project <- h1d_run$project
  h1d_run$project <- NULL
  out <- c(read_output(project, silent, ...), h1d_run)
  structure(
    .Data = out,
    class = c("h1d_run", class(out))
  )
}

#' @export
print.h1d_output <- function(x) str(x)

#' @param file_path
#' Path to T_level output file
#' @param skip
#' skip number of lines on input
#' @return data.frame
#' @export
#' @rdname read_output
read_t_level <- function(file_path, skip = 6L) {
  t_level_lines <- read_lines(file_path, skip = skip)
  col_names <- extr_col_names(t_level_lines[1L])
  last_line <- str_which(t_level_lines, "^end") - 1L
  if (length(last_line) == 0L) last_line <- str_which(t_level_lines, "^\\s+\\d+") %>% tail(1L)
  fwf <- fwf_widths(c(rep.int(13L, 19L), 7L, 13L, 11L), col_names = col_names)
  read_fwf(
    file = str_c(head(t_level_lines, last_line), collapse = "\n"),
    col_positions = fwf,
    skip = 3L, # The first 3 lines are header
    col_types = cols(.default = "d"),
    na = c("", "NA", "NaN")
    # n_max = last_line[1L] - 3L
  )
}


#' @return data.frame
#' @export
#'
#' @rdname read_output
read_a_level <- function(file_path, skip = 2L) {
  a_level_lines <- read_lines(file_path, skip = skip)
  col_names <- extr_col_names(a_level_lines[1L])
  last_line <- str_which(a_level_lines, "^end") - 1L
  if (length(last_line) == 0L) last_line <- str_which(a_level_lines, "^\\s+\\d+") %>% tail(1)
  fwf <- fwf_widths(c(12L, rep.int(14L, 5L), rep.int(11L, 3L), 8L), col_names = col_names)
  read_fwf(
    file = str_c(head(a_level_lines, last_line), collapse = "\n"),
    col_positions = fwf,
    skip = 3L, # The first 3 lines are header
    col_types = cols(.default = "d"),
    na = c("", "NA", "NaN")
  )
}

#' @return list of data.frames
#' @export
#' @rdname read_output
read_nod_inf <- function(file_path, colnames_row = 11L) {
  nod_inf_lines <- read_lines(file_path)

  time_idx <- str_which(nod_inf_lines, "^[ \\t]*Time:\\s+")
  if (length(time_idx) == 0L) return(tibble())
  slice_end <- str_which(nod_inf_lines, "^end") - 1L
  if (length(slice_end) == 0) return(tibble())
  if (length(time_idx) > length(slice_end)) length(time_idx) <- length(slice_end)
  slice_start <- time_idx + 6L
  slice_idx <- mapply(seq.int, slice_start, slice_end)
  col_names <- extr_col_names(nod_inf_lines[time_idx[1] + 3L]) %>% str_replace("\\.+NS", "")
  time_stamp <-
    nod_inf_lines[time_idx] %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.numeric() %>%
    rep(each = nrow(slice_idx))

  fwf <- fwf_widths(
    c(4L, 11L, 12L, 7L, 13L, rep.int(12L, 3L), 8L, 12L, 8L, rep.int(12L, 2L)),
    col_names = col_names
  )
  read_fwf(
    file = str_c(nod_inf_lines[slice_idx], collapse = "\n"),
    col_positions = fwf,
    col_types = cols(.default = "d"),
    na = c("", "NA", "NaN")
  )[, -1L] %>% # remove node column
    add_column(Time = time_stamp, .before = 1)
}

#' @param header
#' Number of lines with header information before the actual data
#'
#' @return data.frame
#' @export
#' @rdname read_output
read_obs_node <- function(file_path, header = 11L) {
  obs_node_lines <- read_lines(file_path)
  col_names <-
    head(obs_node_lines, header) %>%
    str_subset("^\\s+time") %>%
    extr_col_names() %>%
    unique()
  if (length(col_names) == 0L) return(tibble())
  nodes <-
    head(obs_node_lines, header) %>%
    str_subset(pattern = "Node") %>%
    str_extract_all(pattern = "[0-9]+", simplify = TRUE) %>%
    as.integer()
  if (length(nodes) == 0L) return(tibble())
  last_line <- str_which(obs_node_lines, "^end") - 1L
  if (length(last_line) == 0L) last_line <- str_which(obs_node_lines, "^\\s+\\d+") %>% tail(1L)
  data <-
    read_table2(
      file = str_c(head(obs_node_lines, last_line[1L]), collapse = "\n"),
      skip = header,
      col_names = FALSE,
      col_types = cols(.default = "d"),
      na = c("", "NA", "NaN", "*")
    )
  par_per_node <- length(col_names) - 1L
  out <- lapply(
    seq_along(nodes),
    function(i) {
      idx <- c(1L, seq.int((i - 1L) * par_per_node + 2L, i * par_per_node + 1L))
      node <- data[ ,idx]
      colnames(node) <- col_names
      node["node"] <- nodes[i]
      return(node)
    })
  out_df <- do.call(rbind, out) %>% as_tibble()
  # as_tibble(out_df)
  # rownames(out_df) <- NULL
  attr(out_df, "problems") <- attr(data, "problems")
  out_df
}

read_balance <- function(file_path) {
  balance_lines <- read_lines(file_path)
  time_idx <- str_which(balance_lines, "^[ \\t]*Time\\s+")
  time_stamp <-
    balance_lines[time_idx[-1L]] %>%
    str_extract("[0-9]+\\.?[0-9]*") %>%
    as.double()

  dashes <- str_which(balance_lines, "^-{58}$")
  # every fourth dashed line marks the end of a slice
  slice_end <- dashes[seq_along(dashes) %% 4L == 0L] - 1L
  # cut time_idx to the right length
  if (length(time_idx) > length(slice_end)) length(time_idx) <- length(slice_end)
  slice_idx <- mapply(seq.int, time_idx[-1L] + 4L, slice_end[-1L])
  if (length(slice_idx) == 0) return(tibble())

  col_names <-
    balance_lines[slice_idx[, 1L]] %>%
    str_sub(end = 19L) %>%
    str_replace("\\[.*\\]", "") %>%
    str_replace("-", "_") %>%
    str_replace_all("\\s+", "")

  slices <- apply(slice_idx, 2, function(slice) {
    slice_df <-
      balance_lines[slice] %>%
      str_sub(start = 19L) %>%
      # str_trim() %>%
      str_c(collapse = "\n") %>%
      read_table(
        col_names = FALSE,
        col_types = cols(),
        na = c("NA", "NaN", "")
      ) %>%
      t() %>%
      as_tibble()
    colnames(slice_df) <- col_names
    slice_df
  })
  slices <- do.call(rbind, slices)
  slice_len <- nrow(slices) / length(time_stamp)
  add_column(
    slices,
    Time = rep(time_stamp, each = slice_len),
    Region = rep.int(seq(0L, by = 1L, length.out = slice_len), length(time_stamp)),
    .before = 1L
  )
}

