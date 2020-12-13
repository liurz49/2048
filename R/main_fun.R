##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'

appmove <- function(e,d) {
  move.fun <- function(x, d) {
    # if (d %in% c(68, 83)) {x <- rev(x)}
    x.now <- x[which(x > 0)]
    equal <- which(diff(x.now) == 0)
    if (length(equal) == 1) {
      x.now[equal] <- x.now[equal] * 2
      x.now <- x.now[-(equal + 1)]
    }
    if (length(equal) == 2) {
      if (equal[1] == 1 && equal[2] == 3) {
        x.now <- x.now[equal] * 2
      } else {
        x.now[equal[1]] <- x.now[equal[1]] * 2
        x.now <- x.now[-equal[2]]
      }
    }
    if (length(equal) == 3) {
      x.now <- x.now[c(1, 3)] * 2
    }
    if (d %in% c(68, 83)) {
      x.now <- c(rep(0, 4 - length(x.now)), x.now)
    } else {
      x.now <- c(x.now, rep(0, 4 - length(x.now)))
    }
    # if (d %in% c(68, 83)) {x.now <- rev(x.now)}
    return(x.now)
  }
    lm <- e$m
    if (d == 65) {
        e$m <- t(apply(e$m, 1, move.fun, d = d))
    }
    if (d == 68) {
        e$m <- t(apply(e$m, 1, move.fun, d = d))
    }
    if (d == 87) {
        e$m <- apply(e$m, 2, move.fun, d = d)
    }
    if (d == 83) {
        e$m <- apply(e$m, 2, move.fun, d = d)
    }
    e$stop <- ifelse(length(which(e$m != lm)) == 0, TRUE, FALSE)
    #print(e$m)
    return(e)
}




##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'

bg <- function(e) {
    if (e$isfail) {
        return(NULL)
    }
    e$gp <- ggplot(data = data.frame(0, 0)) + xlim(c(0, 1)) + ylim(c(0, 1)) + geom_hline(yintercept = seq(0,
        1, e$step), aes(col = "gray")) + geom_vline(xintercept = seq(0, 1, e$step),
        aes(col = "gray")) + scale_y_continuous(expand = c(0, 0)) + scale_x_continuous(expand = c(0,
        0)) + xlab("") + ylab("") + theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank())
    return(e)
    # plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = 'n', xaxs = 'i', yaxs = 'i')
    # abline(h = seq(0, 1, e$step), col = 'gray') abline(v = seq(0, 1, e$step), col =
    # 'gray')
}






##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'

bg.matrix <- function(e) {
    if (e$isfail) {
        return(NULL)
    }
    a <- c(t(e$m))
    lab <- c(a[13:16], a[9:12], a[5:8], a[1:4])
    d <- data.frame(x = rep(seq(0, 0.95, e$step), e$width), y = rep(seq(0, 0.95, e$step),
        each = e$height), lab = lab)
    df <- d[which(d$lab > 0), ]
    gp2 <- e$gp
    for (i in 1:nrow(df)) {
        gp2 <- gp2 + geom_rect(xmin = df$x[i], xmax = df$x[i] + 0.25, ymin = df$y[i],
            ymax = df$y[i] + 0.25, fill = e$color[log(df$lab[i], 2)], alpha = 0.5) +
            annotate("text", x = df$x[i] + e$step/2, y = df$y[i] + e$step/2,
                     label = df$lab[i], size = 20, alpha = 0.6)
    }
    return(gp2)
    # e$gp2 <- e$gp + geom_rect(xmin = df$x, xmax = df$x + 0.25, ymin = df$y, ymax =
    # df$y + 0.25, fill = e$color[log(df$lab, 2)], alpha = 0.5) + annotate('text',
    # x=df$x + e$step/2, y=df$y + e$step/2, label = df$lab) points(df$x + e$step/2,
    # df$y + e$step/2, col = e$color[log(df$lab, 2)], pch = 15, cex = 23) text(df$x +
    # e$step/2, df$y + e$step/2, label = df$lab, cex = 2)
}



# key <- function(K) {
#     if (e$stage == 0) {
#         ingame()
#         return(NULL)
#     }
#     if (e$stage == 1) {
#         if (K == "q")
#             end() else {
#             if (tolower(K) %in% c("up", "down", "left", "right")) {
#                 e$d <- tolower(K)
#                 ingame()
#             }
#         }
#         return(NULL)
#     }
#     if (e$stage == 2) {
#         if (K == "q")
#             q() else if (K == " ")
#             start()
#         return(NULL)
#     }
#     return(NULL)
# }



##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'

create <- function(e) {
  index <- function(e, x) {
    return(which(e$m == x))
  }

    if (length(index(e, 0)) > 0 & !e$stop) {
        e$stop <- TRUE
        temp <- 2
        if (length(index(e, 0)) == 16) {
            posit <- sample(index(e, 0), 2)
        } else {
            posit <- ifelse(length(index(e, 0)) == 1, index(e, 0), sample(index(e, 0), 1))
        }
        e$m[posit] <- temp
    }
  return(e)
}






##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'

e.init <- function() {
    #e <- new.env()
    e <- list()
    e$stage <- 0
    e$width <- e$height <- 4
    e$m <- matrix(rep(0, e$width * e$height), nrow = e$width)
    e$max <- 4
    e$step <- 1/e$width
    e$d <- "up"
    e$color <- rainbow(14)
    e$stop <- FALSE
    e$isfail <- FALSE
    return(e)
    # create()
}






##' Add up two numbers (Description)
##'
##' We want to add up two numbers, blalala... (Details)
##' @title add two numbers
##' @param x first number
##' @param y second number
##' @return sum of two numbers
##' @author Caleb
##' @export
##' @examples
##'
fail.end <- function(e) {
  fail.cond <- function(x) {
    return(length(which(diff(x) == 0)))
  }

    if (length(index(0)) == 0) {
        h <- apply(e$m, 1, fail.cond)
        v <- apply(e$m, 2, fail.cond)
        if (length(which(h > 0)) == 0 & length(which(h > 0)) == 0) {
            fail("No space!")
            return(NULL)
        }
    }
}


# play2048 <- function() {
#     options(device = "windows")
#     par(mai = rep(0, 4), oma = rep(0, 4))
#     e <- new.env()
#     start()
#
#     getGraphicsEvent(prompt = "2048", onKeybd = key)
#
# }
