server <- function(input, output, session) {
    e <- list()
    active <- reactiveVal(FALSE)
    observeEvent(input$pressedKey, {
        if (!is.null(input$keyPressed) & active()) {
            # active(FALSE)
            d <- input$keyPressed
            e <<- appmove(e, d)
            # fail.end(e)
            e <<- create(e)
            # bg()
            gp2 <- bg.matrix(e)
            
            
            output$plot <- renderPlot({
                gp2
            })
            
        }
    })
    
    resource <- system.file("start.gif", package = "play2048")
    # resource = '../../resource' = system.file('resource', package = 'play2048')
    # print(resource)
    
    output$plot <- renderImage({
        # print( paste(resource, '/start.gif'))
        
        list(src = resource, contentType = "image/gif")
    })
    
    
    observeEvent(input$startGame, {
        
        active(TRUE)
        # e <- new.env()
        e <<- e.init()
        # init()
        e <<- bg(e)
        e <<- create(e)
        # bg()
        gp2 <- bg.matrix(e)
        output$plot <- renderPlot({
            gp2
        })
    })
    observeEvent(input$endGame, {
        active(FALSE)
        # showModal(modalDialog(plotOutput('end'), footer = NULL, easyClose = TRUE, size =
        # 'm') )
        shinyalert("Oops!", "Game Over", type = "info", imageWidth = 50, imageHeight = 40)
    })
    
    observeEvent(input$reset, {
        active(FALSE)
        output$plot <- renderImage({
            list(src = resource, contentType = "image/gif")
        })
    })
}



