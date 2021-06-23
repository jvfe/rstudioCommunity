build_bar_col <- function(value, max_col, color) {
  width <-
    paste0(value * 100 / max(max_col), "%")

  value <- format(value, big.mark = ",")
  value <- format(value, width = 9, justify = "right")

  bar <- div(
    class = "bar-chart",
    style = list(marginRight = "6px"),
    div(
      class = "bar",
      style = list(width = width, backgroundColor = color)
    )
  )
  div(class = "bar-cell", span(class = "number", value), bar)
}
