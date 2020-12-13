##'
##' This function will launch the Game with shiny app.
##' 'W' will rotate the tetrominos, 'A' will move the tetrominos to left,  and 'D' will move the tetrominos to right
##' @title Launch the Game
##' @author Tingting & Chang
##' @export
##' 
##'
play2048 <- function() {
    #appDir <- "C:/project/play2048/inst/shinyApp"
    appDir = system.file('shinyApp', package = 'play2048')
    if (appDir == "") {
        stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")
}
