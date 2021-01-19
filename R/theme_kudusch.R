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
theme_kudusch <- function(base_size = 9, base_family = "Menlo") {
    theme_classic(base_size = base_size,
                  base_family = base_family
    ) %+replace%
        theme(
            text = element_text(
                family = base_family,
                size = base_size
            ),
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
            strip.text = element_text(
                margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt"),
                size = 7
            ),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.just = c("top"),
            legend.background = element_rect(
                fill = "#D6D6D6"
            ),
            complete = TRUE
        )
}

#' A simple custom theme for ggplot.
#'
#' @description A minimal dark theme based on \code{theme_classic} with a monospaced font.
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
#' p1 + theme_kudusch_dark()
#' # Theme examples with panels
#' p2 <- p1 + facet_grid(vs ~ am)
#' p2 + theme_kudusch_dark()
#'}
#'
#' @importFrom ggplot2 %+replace% theme_classic theme element_text margin element_line arrow unit alpha element_rect
#' @importFrom utils flush.console
#' @importFrom magrittr %>%
#'
#' @export
theme_kudusch_dark <- function(base_size = 9, base_family = "Menlo") {
    theme_classic(base_size = base_size,
                  base_family = base_family
    ) %+replace%
        theme(
            text = element_text(
                color = "#FFFFFF",
                family = base_family,
                size = base_size
            ),
            line = element_line(
                color = "#FFFFFF"
            ),
            rect = element_rect(
                color = "#FFFFFF",
                fill = "#FFFFFF"
            ),
            plot.title = element_text(
                color = "#FFFFFF",
                face = "bold",
                hjust = 0,
                margin = margin(b = 10)
            ),
            legend.title = element_text(
                color = "#FFFFFF",
                margin = margin(b = 5),
                hjust = 0
            ),
            axis.title.x = element_text(
                color = "#FFFFFF",
                margin = margin(t = 10)
            ),
            axis.title.y = element_text(
                color = "#FFFFFF",
                margin = margin(r = 10),
                angle = 90
            ),
            axis.text = element_text(
                color = "#FFFFFF",
                family = base_family,
                size = base_size
            ),
            axis.line = element_line(
                color = "#FFFFFF",
                arrow = arrow(length = unit(2, "mm"))
            ),
            panel.grid = element_line(
                color = "#FFFFFF",
                linetype = "solid",
                alpha("black", 0.1)
            ),
            panel.grid.major = element_line(
                color = "#999999",
                linetype = "solid",
                alpha("black", 0.3)
            ),
            panel.grid.minor = element_line(
                color = "#777777",
                linetype = "dotted"
            ),
            panel.grid.minor.x = element_line(
                color = "#777777",
                linetype = "dotted"
            ),
            panel.spacing = unit(5, "mm"),
            strip.background.x = element_rect(
                fill = "#303030",
                color = "#BBBBBB",
                linetype = "dotted"
            ),
            strip.background.y = element_rect(
                fill = "#303030",
                color = "#BBBBBB",
                linetype = "dotted"
            ),
            strip.text = element_text(
                color = "#FFFFFF",
                margin = margin(t = 6, r = 6, b = 6, l = 6, unit = "pt"),
                size = 7
            ),
            panel.background = element_rect(
                color = "#303030",
                fill = "#303030"
            ),
            plot.background = element_rect(
                color = "#303030",
                fill = "#303030"
            ),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.just = c("top"),
            legend.background = element_rect(
                fill = "#707070",
                color = "#FFFFFF"
            ),
            complete = TRUE
        )
}
