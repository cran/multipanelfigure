#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
is_empty <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{  
  metric <- match.arg(metric)
  metric_fn <- get_metric(metric)
  metric_fn(x, 0L, .xname)
}


#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_non_empty <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{
  metric <- match.arg(metric)
  metric_fn <- get_metric(metric)
  if(metric_fn(x, 0)) 
  {
    msg <- switch(
      metric,
      length = gettext("%s has length 0."),
      elements = gettext("%s has 0 elements.")
    )
    return(false(msg, .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_non_scalar <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x))
{
  metric <- match.arg(metric)
  metric_fn <- get_metric(metric)
  if(metric_fn(x, 1)) 
  {
    msg <- switch(
      metric,
      length = gettext("%s has length 1."),
      elements = gettext("%s has 1 element.")
    )
    return(false(msg, .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
is_scalar <- function(x, metric = c("length", "elements"), 
                      .xname = get_name_in_parent(x))
{
  metric <- match.arg(metric)
  metric_fn <- get_metric(metric)
  metric_fn(x, 1L, .xname)
}     


#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
#' @importFrom assertive.base use_first
has_elements <- function(x, n, .xname = get_name_in_parent(x))
{
  n <- use_first(n)
  check_n(n)
  n_elements_x <- n_elements(x)
  if(n_elements_x != n)
  {
    return(
      false(
        ngettext(
          n_elements_x, 
          "%s has %d element, not %d.", 
          "%s has %d elements, not %d."
        ),
        .xname, 
        n_elements_x,
        n
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_of_dimension <- function(x, n, .xname = get_name_in_parent(x))
{
  dim_x <- dim(x)
  # There are two cases to test: n is NULL, or n is a vector of natural 
  # numbers.
  if(is.null(n))
  {
    if(has_dims(x))
    {
      return(
        false(
          ngettext(
            length(dim_x), 
            "%s has dimension %s, not NULL.", 
            "%s has dimensions %s, not NULL."
          ), 
          .xname,
          deparse(dim_x)
        )
      )
    }
    return(TRUE)
  }
  check_n(n)
  if(!is_of_length(dim_x, length(n)))
  {
    return(
      false(
        ngettext(
          length(dim_x), 
          "%s has %d dimension, not %d.", 
          "%s has %d dimensions, not %d."
        ),  
        .xname, 
        length(dim_x),
        length(n)
      )
    )
  }
  differences <- dim_x != n
  if(any(differences))
  {
    bad <- which(differences)
    return(
      false(
        ngettext(
          length(bad), 
          "Dimension %s of %s is incorrect.", 
          "Dimensions %s of %s are incorrect."
        ), 
        toString(bad), 
        .xname
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
#' @importFrom assertive.base use_first
is_of_length <- function(x, n, .xname = get_name_in_parent(x))
{
  n <- use_first(n)
  check_n(n)
  length_x <- length(x)
  if(length_x != n)
  {
    return(false("%s has length %d, not %d.", .xname, length_x, n))
  }
  TRUE
}

check_n <- function(n)
{
  if(any(n < 0 | n != round(n)))
  {
    stop("n should be a non-negative integer vector.")
  }
}

get_metric <- function(metric)
{
  switch(
    metric,
    length   = is_of_length,
    elements = has_elements,
    stop("Bug in assertive; the metric", metric, "is not valid.", domain = NA)
  )
}
