GREEK <- intToUtf8(c(913:929, 931:937), multiple = TRUE)
greek <- intToUtf8(c(945:961, 963:969), multiple = TRUE)

#' @importFrom utils as.roman
next_label <- function(figure)
{
  n <- length(figure$grobs) / 2 + 1L
  panelLabelType <- attr(figure, "multipanelfigure.panelLabelType")
  switch(
    panelLabelType,
    "upper-alpha" = LETTERS[n],
    "lower-alpha" = letters[n],
    "decimal"     = as.character(n),
    "upper-roman" = as.roman(n),
    "lower-roman" = tolower(as.roman(n)),
    "upper-greek" = GREEK[n],
    "lower-greek" = greek[n],
    "none"        = "",
    stop("The label type ", panelLabelType, " is not supported.")
  )
}
