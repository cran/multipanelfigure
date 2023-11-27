#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_not_null <- function(x, .xname = get_name_in_parent(x))
{
  if(is.null(x))
  {
    return(false("%s is NULL.", .xname))
  }
  TRUE
}

#' @importFrom assertive.base safe_deparse
#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_null <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.null(x))
  {
    return(false("%s is not NULL; its value is %s.", .xname, safe_deparse(x)))
  }
  TRUE
}
