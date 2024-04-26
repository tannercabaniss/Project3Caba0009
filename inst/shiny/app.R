#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GIBBS SAMPLER"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          sliderInput("iter",
                      "Number of Samples:",
                      min = 2,
                      max = 100000,
                      value = 10000),
            sliderInput("init",
                        "Initial value of sampler:",
                        min = -50,
                        max = 50,
                        value = 1),
            sliderInput("mu1",
                        "Mean for x1:",
                        min = -50,
                        max = 50,
                        value = 3),
            sliderInput("mu2",
                        "Mean for x2:",
                        min = -50,
                        max = 50,
                        value = 6),
            sliderInput("s11",
                        "Variance of x1:",
                        min = 0,
                        max = 25,
                        value = 8),
            sliderInput("s22",
                        "Variance of x2:",
                        min = 0,
                        max = 25,
                        value = 4),
            sliderInput("s12",
                        "Covariance of x1 and x2:",
                        min = -25,
                        max = 25,
                        value = -2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("heatPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent({
      input$s11
      input$s22
    }, {
      max_value <- sqrt(input$s11 * input$s22)
      min_value <- -max_value
      updateSliderInput(session, "s12", min = ceiling(min_value)+1, max = floor(max_value)-1, value = 0)
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        gibbs_sample <- rbvnorm(iter=input$iter, init=input$init, mu=c(input$mu1,input$mu2), sigma = matrix(c(input$s11,input$s12,input$s12,input$s22), nrow=2, byrow=TRUE))
        df <- gibbs_sample$gibbs

        kde <- MASS::kde2d(df$x1, df$x2, n = 200)
        contour_levels <- quantile(kde$z, probs = seq(0, 1.0, by = 0.25))

        #contour(kde$x, kde$y, kde$z, levels = contour_levels, drawlabels = FALSE, col = "black")
        #points(df$x1, df$x2, col = rgb(1, 0, 0, alpha=0.25), pch = 20)

       ggplot2::ggplot(df, ggplot2::aes(x=x1,y=x2)) +
        ggplot2::geom_point(alpha = 0.5, color = "red") +
        ggplot2::geom_density_2d(color = "black") +
        ggplot2::labs(title = "Scatter/Contour Plot of Bivariate Normal Sample", x = "X1 Data", y = "X2 Data") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20)) +
        ggplot2::coord_cartesian(xlim = c(min(df$x1),max(df$x1)), ylim = c(min(df$x2), max(df$x2))) +
        ggplot2::coord_fixed(ratio=1)
    })

    output$heatPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      gibbs_sample <- rbvnorm(iter=input$iter, init=input$init, mu=c(input$mu1,input$mu2), sigma = matrix(c(input$s11,input$s12,input$s12,input$s22), nrow=2, byrow=TRUE))
      df <- gibbs_sample$gibbs

      ggplot2::ggplot(df, ggplot2::aes(x = x1, y = x2)) +
        ggplot2::geom_tile(stat = "density2d", ggplot2::aes(fill = ..density..), contour = FALSE) +
        ggplot2::scale_fill_gradient(low = "blue", high = "red") +
        ggplot2::labs(title = "Heatmap of Bivariate Normal Sample", x = "X1 Data", y = "X2 Data") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 20)) +
        ggplot2::guides(fill = "none") +
        ggplot2::coord_cartesian(xlim = c(min(df$x1),max(df$x1)), ylim = c(min(df$x2), max(df$x2))) +
        ggplot2::coord_fixed(ratio=1)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
