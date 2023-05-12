#' Simple Shiny app to plot a histogram
#'
#' This is a simple Shiny app to plot a histogram.
#' Meant as a gentle introduction to Shiny inputs and outputs.
#'
#' @param data Data frame
#' @param x Name of column in \code{data} plot. Must be a numeric vector.
#' @param title Title of the plot.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param main Main title.
#' @param col Color of the bars.
#' @param bins Number of bins.
#' @param xlim Limits of the x-axis.
#' @param ylim Limits of the y-axis.
#' @param border Color of the border.
#' @param lwd Line width of the border.
#' @param lty Line type of the border.
#' @param cex Axis label size.
#' @param cex.main Main title size.
#' @param cex.lab Axis label size.
#' @param cex.axis Axis tick label size.
#' @param cex.sub Subtitle size.
#'
#' @return A Shiny app to plot a histogram.
#'
#' @import shiny
#' @importFrom graphics hist
#'
#' @seealso \code{\link{hist}}
#' @examples
#' \dontrun{
#' data <- data.frame(x = rnorm(100))
#' shiny_hist(data, x = "x")
#' }
#'
#' @author Jared Andrews
#' @export
shiny_hist <- function(data,
                       x,
                       title = "Histogram",
                       xlab = "x",
                       ylab = "y",
                       main = "Histogram",
                       col = "blue",
                       bins = 10,
                       xlim = range(bins),
                       ylim = NULL,
                       border = "black",
                       lwd = 1,
                       lty = 1,
                       cex = 1,
                       cex.main = 1,
                       cex.lab = 1,
                       cex.axis = 1,
                       cex.sub = 1) {
  # Create the UI
  ui <- fluidPage(
    # Application title
    titlePanel(title),

    # Sidebar with a slider input for number of bins.
    sidebarLayout(
      sidebarPanel(
        sliderInput("bins",
          "Number of bins:",
          min = 1,
          max = 100,
          value = bins
        )
      ),

      # Show a plot of the generated distribution.
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )

  # Create the server.
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      x <- data[[x]]
      hist(x,
        col = col,
        bins = input$bins,
        xlim = xlim,
        ylim = ylim,
        border = border,
        lwd = lwd,
        lty = lty,
        cex = cex,
        cex.main = cex.main,
        cex.lab = cex.lab,
        cex.axis = cex.axis,
        cex.sub = cex.sub,
        xlab = xlab,
        ylab = ylab,
        main = main
      )
    })
  }

  # Create the application.
  shinyApp(ui = ui, server = server)
}
