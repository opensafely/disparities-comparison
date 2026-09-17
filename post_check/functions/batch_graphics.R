# Prevent Rscript from writing ./Rplots.pdf when print()/get_legend() draw.
# Opens a throwaway PDF device once per process; ggsave() still uses its own.
divert_batch_graphics <- function() {
  if (interactive()) {
    return(invisible(FALSE))
  }
  if (isTRUE(getOption("post_check.batch_graphics_diverted"))) {
    return(invisible(FALSE))
  }
  sink_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file = sink_file)
  options(
    post_check.batch_graphics_diverted = TRUE,
    post_check.batch_graphics_file = sink_file
  )
  invisible(TRUE)
}

divert_batch_graphics()
