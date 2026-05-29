#' @export
startlog <- function(df, clone=FALSE) {
  attr(df, "old_shape") <- dim(df)
  if(clone) {
    attr(df, "old_df") <- df
  }
  return(df)
}

#' @export
endlog <- function(df) {
  old_shape <- attr(df, "old_shape")
  new_shape <- dim(df)
  if (is.null(old_shape)) {
    warning("No old shape found. Did you forget to use startlog()?")
  }
  
  if(!all(old_shape == new_shape)) {
    # Row changes
    delta_rows <- new_shape[1] - old_shape[1]
    if (delta_rows != 0) {
      pct_rows <- round((delta_rows / old_shape[1]) * 100, 2)
      message("Rows changed by ", delta_rows, " (", pct_rows, "%)")
    }
    # Column changes
    delta_cols <- new_shape[2] - old_shape[2]
    if (delta_cols != 0) {
      pct_cols <- round((delta_cols / old_shape[2]) * 100,
                        2)
      message("Columns changed by ", delta_cols, " (", pct_cols, "%)")
    }
  } else {
    old_df <- attr(df, "old_df")
    if(is.null(old_df)) {
      message("No changes in shape. No old dataframe found to compare (please use `clone=TRUE` in `startlog`).")
    } else {
      # Compare dataframes
      n_value_changes <- sum(old_df != df, na.rm = TRUE)
      n_na_changes <- sum(is.na(old_df) != is.na(df))
      n_changes <- n_value_changes + n_na_changes
      ntot <- prod(old_shape)
      pct_cols <- round((n_changes / ntot) * 100,
                        2)
      message("Data changed in ", n_changes, "/", formatC(ntot, big.mark=","), " (", pct_cols, "%) cells")
    }
  }
  attr(df, "old_shape") <- NULL
  attr(df, "old_df") <- NULL
  return(df)
}