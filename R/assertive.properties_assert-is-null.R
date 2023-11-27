#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base assert_engine
assert_is_not_null <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                      
  assert_engine(is_not_null, x, .xname = get_name_in_parent(x))   
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base assert_engine
assert_is_null <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                         
  assert_engine(
    is_null, 
    x, 
    .xname = get_name_in_parent(x),
    severity = severity
  )
}
