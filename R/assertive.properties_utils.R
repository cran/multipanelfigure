#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
DIM <- function(x)
{
  dim_x <- dim(x)
  if(is.null(dim_x)) length(x) else dim_x
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
n_elements <- function(x)
{
  if(is.recursive(x))
  {
    sum(vapply(x, n_elements, integer(1)))
  } else
  {
    as.integer(prod(DIM(x)))
  }  
}
