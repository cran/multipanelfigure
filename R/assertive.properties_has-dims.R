#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
has_cols <- function(x, .xname = get_name_in_parent(x))
{
  ncolx <- ncol(x)
  if(is.null(ncolx)) 
  {
    return(false("The number of columns in %s is NULL.", .xname))  
  }
  if(ncolx == 0L) 
  {
    return(false("The number of columns in %s is zero.", .xname))
  }
  TRUE
} 

#' @author Richard Cotton <richierocks@gmail.com>
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
#' @noRd
has_dims <- function(x, .xname = get_name_in_parent(x))
{
  dim_x <- dim(x)
  if(is.null(dim_x)) 
  {
    return(false("The dimensions of %s are NULL.", .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
has_rows <- function(x, .xname = get_name_in_parent(x))
{
  nrowx <- nrow(x)
  if(is.null(nrowx)) 
  {
    return(false("The number of rows in %s is NULL.", .xname))  
  }
  if(nrowx == 0L) 
  {
    return(false("The number of rows in %s is zero.", .xname))
  }
  TRUE
} 
