#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "MFA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MFA`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
