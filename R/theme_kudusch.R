#' A simple custom theme for ggplot.
#'
#' @description A minimal theme based on \code{theme_classic} with a monospaced font.
#' @param base_size base font size
#' @param base_family base font family
#' @examples
#'\dontrun{
#' mtcars2 <- within(mtcars, {
#' vs <- factor(vs, labels = c("V-shaped", "Straight"))
#' am <- factor(am, labels = c("Automatic", "Manual"))
#' cyl  <- factor(cyl)
#' gear <- factor(gear)
#' })
#' p1 <- ggplot(mtcars2) +
#' geom_point(aes(x = wt, y = mpg, colour = gear)) +
#' labs(title = "Fuel economy declines as weight increases",
#' subtitle = "(1973-74)",
#' caption = "Data from the 1974 Motor Trend US magazine.",
#' tag = "Figure 1",
#' x = "Weight (1000 lbs)",
#' y = "Fuel economy (mpg)",
#' colour = "Gears")
#'
#' p1 + theme_kudusch()
#' # Theme examples with panels
#' p2 <- p1 + facet_grid(vs ~ am)
#' p2 + theme_kudusch()
#'}
#'
#' @importFrom ggplot2 %+replace% theme_classic theme element_text margin element_line arrow unit alpha element_rect
#' @importFrom utils flush.console
#' @importFrom magrittr %>%
#'
#' @export
theme_kudusch <- function(base_size = 11, base_family = "mono") {
    theme_classic(base_size = base_size,
                  base_family = base_family
    ) %+replace%
        theme(
            plot.title = element_text(
                face = "bold",
                hjust = 0,
                margin = margin(b = 10)
            ),
            legend.title = element_text(
                margin = margin(b = 5),
                hjust = 0
            ),
            axis.title.x = element_text(
                margin = margin(t = 10)
            ),
            axis.title.y = element_text(
                margin = margin(r = 10),
                angle = 90
            ),
            axis.line = element_line(
                arrow = arrow(length = unit(2, "mm"))
            ),
            panel.grid = element_line(
                linetype = "solid",
                alpha("black", 0.1)
            ),
            panel.grid.major = element_line(
                linetype = "solid",
                alpha("black", 0.3)
            ),
            panel.grid.minor = element_line(
                linetype = "solid"
            ),
            panel.grid.minor.x = element_line(
                linetype = "dotted"
            ),
            panel.spacing = unit(5, "mm"),
            strip.background.x = element_rect(
                linetype = "dotted"
            ),
            strip.background.y = element_rect(
                linetype = "dotted"
            ),
            complete = TRUE
        )
}
