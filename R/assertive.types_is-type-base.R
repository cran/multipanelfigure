#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_array <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "array", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_call <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "call", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_character <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "character", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_complex <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "complex", .xname)
}       

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_data.frame <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "data.frame", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_double <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "double", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_environment <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "environment", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
is_expression <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "expression", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_externalptr <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "externalptr", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_factor <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "factor", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_function <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "function", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_integer <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "integer", .xname)
}


#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_language <- function(x, .xname = get_name_in_parent(x))
{
  if(!is.language(x)) 
  {
    return(
      false(
        gettext("%s is not a language object (name, call or expression)."), 
        .xname
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_list <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "list", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_logical <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "logical", .xname)
}       

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_matrix <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "matrix", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_name <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "name", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_numeric <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "numeric", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_ordered <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_factor(x))) 
  {
    return(ok)
  }
  if(!is.ordered(x))
  {
    return(false(gettext("%s is not an ordered factor."), .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
is_pairlist <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "pairlist", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_primitive <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x))) return(ok)
  if(!is.primitive(x))
  {
    return(false(gettext("%s is not a primitive function."), .xname))
  }
  TRUE
} 

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_qr <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "qr", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_raw <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "raw", .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
is_s4 <- function(x, .xname = get_name_in_parent(x))
{
  if(!isS4(x))
  {
    return(false(gettext("%s is not an S4 object."), .xname))
  }
  TRUE
} 

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
is_S4 <- function(x, .xname = get_name_in_parent(x))
{
  .Deprecated("is_s4")
  is_s4(x, .xname)
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
is_symbol <- is_name


#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base is2
#' @importFrom assertive.base get_name_in_parent
is_table <- function(x, .xname = get_name_in_parent(x))
{
  is2(x, "table", .xname)
}
