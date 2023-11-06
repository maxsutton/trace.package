#' Trace functions in a package
#'
#' This adds (and removes) traces to the functions of a package: whenever those
#' functions are called, some diagnostic information is printed to the console.
#'
#' Packages must be loaded and attached to be traced: this function ensures
#' this.
#'
#' @param pkg_name single package name
#'
#' @return the functions traced (invisibly)
#' @export
#'
#' @note This trace does not work with the base package
#'
#' @examples
#' \dontrun{
#' trace_package()
#' }
trace_package <- function(pkg_name) {
  pkg_name <- check_packages(pkg_name)
  if (length(pkg_name) != 1) {
    stop("`pkg_nameq must be a single character vector", call. = FALSE)
  }

  require(pkg_name, quietly = TRUE, character.only = TRUE)
  pkg_env <- rlang::search_env(paste0("package:", pkg_name))

  pkg_fns <- utils::ls.str(envir = pkg_env, mode = "function")

  entry <- quote(cat(
    paste0(
      "#", environmentName(topenv()),
      " [", microbenchmark::get_nanotime(),
      "] "
    )
  ))

  rc <- trace(pkg_fns, entry, entry) |>
    suppressMessages()

  invisible(pkg_fns)
}

#' @rdname trace_package
untrace_package <- function(pkg_name) {
  pkg_name <- check_packages(pkg_name)
  if (length(pkg_name) != 1) {
    stop("`pkg_nameq must be a single character vector", call. = FALSE)
  }

  pkg_env <- rlang::search_env(paste0("package:", pkg_name))

  pkg_fns <- utils::ls.str(envir = pkg_env, mode = "function")

  rc <- untrace(pkg_fns) |>
    suppressMessages()

  invisible(pkg_fns)
}

#' @noRd
check_packages <- function(packages) {
  if (is.null(packages) || all(is.na(packages)) ||
    !is.character(packages) || length(packages) == 0) {
    stop("`packages` must be a character vector of non-NA package names")
  }

  if ("base" %in% packages) {
    warning("Cannot run trace on base package... skipping.")
    packages <- setdiff(packages, "base") |>
      check_packages()
  }

  packages
}

#' Trace how an expression uses packages
#'
#' This evaluates an expression, and traces where functions from specified
#' packages are used.
#'
#' @param expr an expression
#' @param packages a character vector of packages (cannot include base package)
#'
#' @return a data.frame
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' df <- run_trace(dplyr::if_else(TRUE, 1, 100), "rlang")
run_trace <- function(expr, packages) {
  packages <- check_packages(packages)
  cat("Tracing packages...\n")
  lapply(packages, trace_package)

  cat("Evaluating expression...\n")
  tr <- utils::capture.output(eval(expr))

  cat("Removing trace from packages...\n")
  lapply(packages, untrace_package)

  trace_data <- data.frame(raw = tr) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      prefix = stringr::str_extract(.data$raw, "#[^\\s]+ \\[[0-9]+\\]"),
      info = dplyr::lead(.data$prefix)
    ) |>
    dplyr::filter(!is.na(.data$info))

  if (nrow(trace_data) == 0) {
    cat("No trace found.\n")
    return(NULL)
  }

  trace_data <- trace_data |>
    dplyr::mutate(
      pkg = stringr::str_extract(.data$info, "(?<=#)[^\\s]+"),
      time = as.numeric(stringr::str_extract(.data$info, "(?<=\\[)[0-9]+(?=\\])")),
      entry = stringr::str_detect(.data$raw, " on entry"),
      call = stringr::str_extract(.data$raw, "(?<=Tracing ).+(?= on e)"),
      fun = stringr::str_extract(.data$call, "^[^(]+")
    ) |>
    dplyr::select(-.data$prefix) |>
    dplyr::mutate(
      up_down = ifelse(.data$entry, 1, -1),
      depth = cumsum(.data$up_down),
      match_depth = ifelse(.data$entry, .data$depth, .data$depth + 1)
    ) |>
    dplyr::arrange(.data$pkg, .data$call, .data$id) |>
    dplyr::group_by(.data$pkg, .data$call, .data$match_depth) |>
    dplyr::mutate(
      pair_idx = ceiling(seq(1, dplyr::n()) / 2)
    ) |>
    dplyr::group_by(.data$pkg, .data$call, .data$match_depth, .data$pair_idx) |>
    dplyr::mutate(
      pair_id = .data$id[1],
      duration = .data$time[2] - .data$time[1],
      t1 = .data$time[1],
      t2 = .data$time[2],
      x1 = .data$match_depth - 0.125,
      x2 = .data$match_depth + 0.125
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$id) |>
    dplyr::select(-.data$pair_idx)

  attr(trace_data, "expr") <- substitute(expr)
  trace_data
}


#' Flame graph
#'
#' Create a flame graph of trace data
#'
#' @param trace_data a data.frame, output of run_trace()
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' df <- run_trace(ggplot2::ggplot(), c("ggplot2", "scales"))
#' flame_graph(df)
#' }
flame_graph <- function(trace_data) {
  # Avoid no visible binding
  t1 <- t2 <- match_depth <- pkg <- NULL

  graph_data <- trace_data |>
    dplyr::filter(entry = TRUE) |>
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = t1, xmax = t2,
        ymin = match_depth - 0.5, ymax = match_depth + 0.5,
        fill = pkg
      ),
      linewidth = 0.1, colour = "black", alpha = 0.5
    ) +
    ggplot2::ggtitle(rlang::as_label(attr(trace_data, "expr")))

  graph_data
}


#' Conditionally add crayon colours to a text vector
#'
#' Think of this as dplyr::recode, but for adding colours. Colours are added
#' based on the group variable
#'
#' @param text a vector of text
#' @param group a vector of values to
#' @param ...  a list of pairs: <value in group> = <colour>
#' @param .default the default colour for groups not specified
#'
#' @return a character vector same length as text
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cat(recolour(c("hello ", "world!"), c("a", "b"), "a" = "blue", "b" = "red"))
recolour <- function(text, group, ..., .default = "black") {
  pairs <- rlang::dots_list(...)
  colours_df <- data.frame(
    group = names(pairs),
    colour = unlist(pairs)
  )

  if (!"colour" %in% names(colours_df)) {
    colours_df <- colours_df |>
      dplyr::mutate(colour = character())
  }

  df <- data.frame(text = text, group = group) |>
    dplyr::left_join(colours_df, by = "group") |>
    dplyr::mutate(colour = dplyr::coalesce(.data$colour, .default)) |>
    dplyr::rowwise() |>
    dplyr::mutate(out = crayon::style(.data$text, .data$colour))

  df$out
}

#' Print a tree of traced function calls
#'
#' @inheritParams flame_graph
#' @param label the label used at each node. By default, the `call` for each
#'   function call traced.
#'
#' @return invisible(NULL)
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' df <- run_trace(ggplot2::ggplot(), c("ggplot2", "scales"))
#' code_tree(df)
#'
#' code_tree(df, fun)
#'
#' code_tree(df, recolour(fun, pkg, ggplot2 = "red", scales = "blue"))
code_tree <- function(trace_data, label = call) {
  df <- trace_data |>
    dplyr::mutate(.user_label = {{ label }})

  df$parent <- 0
  df$parent_depth <- 0
  parent_stack <- c(0)
  for (i in 1:nrow(df)) {
    df$parent[i] <- parent_stack[length(parent_stack)]
    df$parent_depth[i] <- df$match_depth[i] - 1
    if (isTRUE(df$entry[i])) {
      parent_stack <- c(parent_stack, df$id[i])
    } else {
      parent_stack <- parent_stack[-length(parent_stack)]
    }
  }

  list_data <- df |>
    dplyr::filter(.data$entry == TRUE) |>
    dplyr::mutate(order = dplyr::row_number())

  column_chars <- c("| ", "  ")

  list_data$trunks <- ""
  for (i in seq(nrow(list_data), 1, by = -1)) {
    if (list_data$id[i] %in% parent_stack) {
      parent_stack[list_data$depth[i]] <- NA
    }
    parent_stack[list_data$parent_depth[i]] <- list_data$parent[i]

    if (list_data$match_depth[i] >= 2) {
      trunk <- parent_stack[seq_len(list_data$match_depth[i] - 2)]
      trunk <- trunk / trunk
      trunk <- dplyr::coalesce(trunk, 2)
      list_data$trunks[i] <- paste0(column_chars[trunk], collapse = "")
    }
  }

  list_data |>
    dplyr::group_by(.data$parent) |>
    dplyr::mutate(
      branch = dplyr::case_when(
        depth == 1 ~ "",
        depth > dplyr::lead(.data$depth, default = 0) ~ "\u2514\u2500",
        depth == dplyr::lead(.data$depth, default = 0) ~ "\u251C\u2500",
        TRUE ~ "fffff"
      ),
      tree = paste0(.data$trunks, .data$branch, .data$.user_label)
    ) |>
    dplyr::pull("tree") |>
    paste0(collapse = "\n") |>
    cat()

  invisible(NULL)
}
