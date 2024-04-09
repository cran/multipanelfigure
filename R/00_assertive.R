#----------------------------------------------------------------------------------------------
#
#                      `assertive` functions - Richard Cotton
#
#     `assertive` was a suite of CRAN packages by Richard Cotton.
#      Its usage was beautifully documented in the O Reilly book `Testing R` by Richard Cotton.
#      The suite is still fully available at bitbucket/richierocks.
#      But, sadly, the CRAN packages are being deprecated.
#      Multicrispr, as BioC package, requires dependencies to be available on CRAN (or BioC).
#      Therefore, in response, used assertive functionality is now copied into this file.
#      Explicit dependency on assertive is being phased out.
#      Gratefulness towards Richard Cotton for his amazing functionality remains : )
#
#----------------------------------------------------------------------------------------------
#' @importFrom methods as is

#===========
# BASE
#===========

    #-------
    # engine
    #-------

        print_and_capture <- function(x, ...){
            # call to enc2utf8 is a workaround for
            # https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16539
            enc2utf8(paste(utils::capture.output(print(x, ...)), collapse = "\n"))
        }


        safe_deparse <- function(expr, ...){
            paste0(deparse(expr, width.cutoff = 500L, ...), collapse = "")
        }


        get_name_in_parent <- function(x, escape_percent = TRUE){
            xname <- safe_deparse(  do.call(  substitute,
                                              list(substitute(x), parent.frame()) )  )
            if(escape_percent)  xname <- gsub("%", "%%", xname)
            xname
        }


        cause <- function(x){
            y <- attr(x, "cause")
            if(is.null(y))  return(noquote(character(length(x))))
            y
        }


        `cause<-` <- function(x, value){
            # Can't use is_scalar here due to dependency on this
            if(length(value) != 1 && length(value) != length(x)){
                stop(  sprintf( "The length of value should be 1 or the length of x (%d) but is %d.",
                                length(x),
                                length(value) ) )
            }
            attr(x, "cause") <- noquote(as.character(value))
            x
        }


        false <- function(...){
            msg <- if(nargs() > 0L) sprintf(...) else ""
            x <- FALSE
            cause(x) <- msg[1]
            class(x) <- c("scalar_with_cause", "logical")
            x
        }


        set_cause <- function(x, false_value, missing_value = "missing"){
            if(!anyNA(x) && all(x, na.rm = TRUE)) return(x)  # fast version of all(!is.na(x) & x)
            is_na_x <- is.na(x)
            len_x <- length(x)
            # TRUES
                cause_value <- character(len_x)
            # NAS
                if(length(missing_value) == 1){
                    cause_value[is_na_x] <- missing_value
                } else {
                    missing_value <- rep_len(missing_value, len_x)
                    cause_value[is_na_x] <- missing_value[is_na_x]
                }
            # FALSES
                false_index <- !(x | is_na_x) # more efficient to calc than !x & !is_na_x
                if(length(false_value) == 1){
                    cause_value[false_index] <- false_value
                } else {
                    false_value <- rep_len(false_value, len_x)
                    cause_value[false_index] <- false_value[false_index]
                }
            cause(x) <- cause_value
            class(x) <- c("vector_with_cause", "logical")
            x
        }


        to_names <- function(x){                                 # special handling for double, complex only
            if(is.double(x) && is.vector(x)){
                ifelse(is.na(x), NA_real_, sprintf("%.17g", x))  # is.vector prevents matching to POSIXct
            } else if(is.complex(x)){
                ifelse(is.na(x), NA_complex_, sprintf("%.17g+%.17gi", Re(x), Im(x)))
            } else {
                as.character(x)
            }
        }


        bapply <- function(x, predicate, ...){
            vapply(x, predicate, logical(1L), ..., USE.NAMES = TRUE)
        }


        call_and_name <- function(fn, x, ...){
            y <- fn(x, ...)
            dim(y) <- dim(x)
            names(y) <- to_names(x)
            y
        }


        assertionError <- function(message, call = NULL, predicate_name = NULL){
            aerr <- list(    message = as.character(message),
                                call = call,
                      predicate_name = predicate_name )
            class(aerr) <- c( "assertionError", "assertionCondition",
                              "simpleError", "error", "condition")
            aerr
        }


        assertionWarning <- function(message, call = NULL, predicate_name = NULL){
          awrn <- list(     message = as.character(message),
                               call = call,
                     predicate_name = predicate_name)
          class(awrn) <- c( "assertionWarning", "assertionCondition",
                            "simpleWarning", "warning", "condition" )
          awrn
        }


        assertionMessage <- function(message, call = NULL, predicate_name = NULL){
          amsg <- list(     message = as.character(message),
                               call = call,
                     predicate_name = predicate_name )
          class(amsg) <- c( "assertionMessage", "assertionCondition",
                            "simpleMessage", "message", "condition" )
          amsg
        }


        give_feedback <- function(handler_type, msg, predicate_name){
            handler <- match.fun(handler_type)
            ass_condition <- switch(  handler_type,
                                              stop = assertionError,
                                           warning = assertionWarning,
                                           message = assertionMessage)
            # Throw error/warning/message
            caller <- if(sys.nframe() >= 3){  sys.call(-3)
            } else {                          NULL         }

            # UTF-8 characters do not display correctly under Windows for some
            # LC_CTYPE locale values, but there isn't much assertive can do about that.
            # https://stackoverflow.com/q/32696241/134830
            handler(ass_condition(paste(predicate_name, msg, sep = " : "), caller, predicate_name))
        }


        assert_engine <- function(
            predicate,
                  ...,
                  msg = "The assertion failed.",
                 what = c("all", "any"),
            na_ignore = FALSE,
             severity = c("stop", "warning", "message", "none")
        ){
            handler_type <- match.arg(severity)
            dots <- list(...)
            return_value <- if(length(dots) > 0) dots[[1]] else NULL
            if(handler_type == "none")  return(invisible(return_value))
            what <- match.fun(match.arg(what))
            predicate_name <- get_name_in_parent(predicate)

            ok <- predicate(...)
            if(inherits(ok, "scalar_with_cause")){
                if(!isTRUE(ok)){
                    if(missing(msg))   msg <- cause(ok)
                    give_feedback(handler_type, msg, predicate_name)
                }
            } else { # inherits(ok, "vector_with_cause")
                really_ok <- if(na_ignore){ ok |  is.na(ok)   # ok can be TRUE or NA; FALSE is bad
                             } else {       ok & !is.na(ok) } # ok can be TRUE; FALSE or NA is bad
                if(!what(really_ok)){
                    # Append first few failure values and positions to the error message.
                    msg <- paste(enc2utf8(msg), print_and_capture(ok), sep = "\n")
                    give_feedback(handler_type, msg, predicate_name)
                }
            }
            invisible(return_value)
        }


        is2 <- function(x, class, .xname = get_name_in_parent(x)){
            # Can't use is_empty in next line because that function calls this one.
            if(length(class) == 0L) stop("You must provide a class.")
            if(length(class) > 1L)  return(  set_cause( bapply(class, function(cl) is2(x, cl, "")),
                                                        sprintf("%s is not '%s'",
                                                                type_description(x), class))  )
            ok <- tryCatch(  {   is.class <- match.fun(paste0("is.", class))
                                 is.class(x)
                             },
                             error = function(e) is(x, class)  )
            if(!ok) return( false( "%s is not of class '%s'; it has %s.",
                                   .xname, class, type_description(x)  )  )
            TRUE
        }


        coerce_to <- function(x, target_class, .xname = get_name_in_parent(x)){
            # Can't use is_empty in next line because that function calls this one.
            if(length(target_class) == 0L)   stop("You must provide a class.")
            if(!is.character(target_class))  stop("target_class should be a character vector.")
            for(this_class in target_class){
                if(!is2(x, this_class)){
                    warning(sprintf("Coercing %s to class %s.", .xname, sQuote(this_class)),
                            call. = FALSE)  }
                tryCatch(
                    {   as.this_class <- match.fun(paste0("as.", this_class))
                        return(as.this_class(x))
                    },
                    error = function(e){
                        # as.this_class doesn't exist; try as(, "this_class") instead
                        tryCatch(   return(as(x, this_class)),
                                    error = function(e){
                                        # Can't coerce to this class; warn and move to next class
                                        warning( sprintf("%s cannot be coerced to type %s.",
                                                         .xname, sQuote(this_class)),
                                                 call. = FALSE )
                                    } ) } )
            }
            # Nothing worked; throw an error
            stop(sprintf("%s cannot be coerced to any of these types: %s.",
                         .xname, toString(sQuote(target_class))))
        }


        type_description <- function(x){
          if(is.array(x)){           sprintf(sprintf("class '%s %s'", class(x[FALSE]), toString(class(x))))
          } else if(is.function(x)){ sprintf(sprintf("class '%s %s'", typeof(x), toString(class(x))))
          } else if(isS4(x)){        sprintf(sprintf("S4 class '%s'", toString(class(x))))
          } else {                   sprintf("class '%s'", toString(class(x)))
          }
        }


        strip_attributes <- function(x){
            attributes(x) <- NULL
            x
        }


        use_first <- function(x, indexer = c("[[", "["), .xname = get_name_in_parent(x)){
            len_x <- length(x)
            # Can't use assert_is_non_empty, is_scalar in next lines because those
            # functions calls this one.
            if(len_x == 0L)  stop(sprintf("%s has length 0.", .xname))
            if(len_x == 1L)  return(x)
            indexer <- match.fun(match.arg(indexer))
            x1 <- indexer(x, 1L)
            warning( sprintf("Only the first value of %s (= %s) will be used.",
                             .xname, as.character(x1)),
                     call. = FALSE )
            x1
        }


    #-------------
    # true / false
    #-------------


        is_identical_to_true <- function(
            x, allow_attributes = FALSE, .xname = get_name_in_parent(x)
        ){
            if(allow_attributes)  x <- strip_attributes(x)
            if(!identical(TRUE, x)){
                msg <- gettextf(  "%s is not identical to TRUE; its value is %s.",
                                  .xname,
                                   safe_deparse(x),
                                   domain = "R-assertive.base"  )
                return(false(msg))
            }
            TRUE
        }


        is_identical_to_false <- function(
            x, allow_attributes = FALSE, .xname = get_name_in_parent(x)
        ){
            if(allow_attributes)   x <- strip_attributes(x)
            if(!identical(FALSE, x)){
                msg <- gettextf(  "%s is not identical to FALSE; its value is %s.",
                                  .xname,
                                   safe_deparse(x),
                                   domain = "R-assertive.base"  )
                return(false(msg))
            }
            TRUE
        }

        is_true <- function(x, .xname = get_name_in_parent(x)){
            x <- coerce_to(x, "logical", .xname)
            call_and_name(  function(x){  is_na_x <- is.na(x)
                                          ok <- x & !is_na_x
                                          set_cause(ok, ifelse(is_na_x, "missing", "false"))  },
                            x  )
        }


        assert_is_identical_to_true <- function(
            x, allow_attributes = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            assert_engine(  is_identical_to_true,
                                               x,
                                allow_attributes = allow_attributes,
                                          .xname = get_name_in_parent(x),
                                        severity = severity )
        }


        assert_is_identical_to_false <- function(
            x, allow_attributes = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            assert_engine(  is_identical_to_false,
                                                x,
                                 allow_attributes = allow_attributes,
                                           .xname = get_name_in_parent(x),
                                         severity = severity )
        }


        assert_all_are_true <- function(x, severity = getOption("assertive.severity", "stop")){
            msg <- gettextf(  "The values of %s are not all TRUE.",
                               get_name_in_parent(x),
                               domain = "R-assertive.base"  )
            assert_engine(  is_true,
                                  x,
                                msg = msg,
                             .xname = get_name_in_parent(x),
                           severity = severity )
        }


        assert_any_are_true <- function(x, severity = getOption("assertive.severity", "stop")){
            msg <- gettextf(  "The values of %s are never TRUE.",
                               get_name_in_parent(x),
                               domain = "R-assertive.base"  )
            assert_engine(  is_true,
                                  x,
                                msg = msg,
                               what = "any",
                             .xname = get_name_in_parent(x),
                           severity = severity )
        }


    #-------------
    # na
    #-------------


        is_na <- function(x, coerce_to_logical = FALSE, .xname = get_name_in_parent(x)){
            call_and_name(
                function(x){  if(coerce_to_logical)   x <- coerce_to(x, "logical", .xname)
                              ok <- is.na(x)
                              if(is.logical(x)){      set_cause(ok, ifelse(x, "true", "false"))
                              } else {                set_cause(ok, "not missing")
                              }  },
                x  )
        }


        is_not_na <- function(x, coerce_to_logical = FALSE, .xname = get_name_in_parent(x)){
            call_and_name(
                function(x){
                    if(coerce_to_logical)   x <- coerce_to(x, "logical", .xname)
                    ok <- !is.na(x)
                    set_cause(ok, "missing")
                },
                x  )
        }


        assert_all_are_not_na <- function(x, severity = getOption("assertive.severity", "stop")){
            msg <- gettextf( "The values of %s are sometimes NA.",
                              get_name_in_parent(x),
                              domain = "R-assertive.base" )
            assert_engine(  is_not_na,
                                    x,
                    coerce_to_logical = FALSE,
                                  msg = msg,
                               .xname = get_name_in_parent(x),
                             severity = severity )
        }

    #----------
    # identical
    #----------

        are_identical <- function(
                           x,
                           y,
            allow_attributes = FALSE,
                      .xname = get_name_in_parent(x),
                      .yname = get_name_in_parent(y)
        ){
            if(allow_attributes){
                x <- strip_attributes(x)
                y <- strip_attributes(y)
            }
            if(!identical(x, y))  return( false( gettext("%s and %s are not identical."),
                                                 .xname, .yname ) )
            TRUE
        }


        assert_are_identical <- function(
            x, y, allow_attributes = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            assert_engine(  are_identical,
                                        x,
                                        y = y,
                                   .xname = get_name_in_parent(x),
                                   .yname = get_name_in_parent(y),
                                 severity = severity  )
        }



#===========
# PROPERTIES
#===========

    #-----------
    # null
    #-----------


        is_not_null <- function(x, .xname = get_name_in_parent(x)){
            if(is.null(x))   return(false("%s is NULL.", .xname))
            TRUE
        }

        is_null <- function(x, .xname = get_name_in_parent(x))
        {
          if(!is.null(x))
          {
            return(false("%s is not NULL; its value is %s.", .xname, safe_deparse(x)))
          }
          TRUE
        }

        assert_is_not_null <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(is_not_null, x, .xname = get_name_in_parent(x))
        }

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

    #-----------
    # duplicates
    #-----------


        has_duplicates <- function(x, .xname = get_name_in_parent(x)){
            if(!anyDuplicated(x))  return(false(gettext("%s has no duplicates."), .xname))
            TRUE
        }


        has_no_duplicates <- function(x, .xname = get_name_in_parent(x)){
            if(anyDuplicated(x)){
                dupe_indicies <- which(duplicated(x))
                return( false( ngettext( length(dupe_indicies),
                                     "%s has a duplicate at position %s.",
                                     "%s has duplicates at positions %s." ),
                              .xname,
                               toString(dupe_indicies, width = 100)  )  )
            }
            TRUE
        }


        assert_has_duplicates <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  has_duplicates,
                            x,
                            .xname = get_name_in_parent(x),
                            severity = severity )
        }


        assert_has_no_duplicates <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  has_no_duplicates,
                            x,
                           .xname = get_name_in_parent(x),
                            severity = severity )
        }


    #-------
    # length
    #-------

        are_same_length <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
        ){
            len_x <- length(x)
            len_y <- length(y)
            if(len_x != len_y){
                return( false(  gettext("%s has length %d but %s has length %d."),
                               .xname,
                                len_x,
                               .yname,
                                len_y  ) )
            }
            TRUE
        }

        assert_are_same_length <- function(
            x, y,  severity = getOption("assertive.severity", "stop")){
            assert_engine(
                are_same_length,
                x,
                y = y,
                .xname = get_name_in_parent(x),
                .yname = get_name_in_parent(y),
                severity = severity
            )
        }


        check_n <- function(n)
        {
            if(any(n < 0 | n != round(n)))
            {
                stop("n should be a non-negative integer vector.")
            }
        }

        is_of_length <- function(x, n, .xname = get_name_in_parent(x)){
            n <- use_first(n)
            check_n(n)
            length_x <- length(x)
            if(length_x != n)  return(false("%s has length %d, not %d.", .xname, length_x, n))
            TRUE
        }


        DIM <- function(x){
            dim_x <- dim(x)
            if(is.null(dim_x)) length(x) else dim_x
        }


        n_elements <- function(x){
            if(is.recursive(x)){    sum(vapply(x, n_elements, integer(1)))
            } else {                as.integer(prod(DIM(x)))
            }
        }


        has_elements <- function(x, n, .xname = get_name_in_parent(x)){
            n <- use_first(n)
            check_n(n)
            n_elements_x <- n_elements(x)
            if(n_elements_x != n){
                return(  false(  ngettext( n_elements_x,
                                          "%s has %d element, not %d.",
                                          "%s has %d elements, not %d."),
                                .xname,
                                 n_elements_x,
                                 n ) )
            }
            TRUE
        }


        get_metric <- function(metric){
            switch(  metric,
                     length = is_of_length,
                   elements = has_elements,
                   stop("Bug in assertive; the metric", metric, "is not valid.", domain = NA) )
        }


        is_scalar <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x)){
            metric <- match.arg(metric)
            metric_fn <- get_metric(metric)
            metric_fn(x, 1L, .xname)
        }


        is_non_scalar <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x)){
            metric <- match.arg(metric)
            metric_fn <- get_metric(metric)
            if(metric_fn(x, 1)){
                msg <- switch(  metric,
                                length = gettext("%s has length 1."),
                              elements = gettext("%s has 1 element.") )
                return(false(msg, .xname))
            }
            TRUE
        }


        assert_is_scalar <- function(
            x, metric = c("length", "elements"), severity = getOption("assertive.severity", "stop")
        ){
            metric <- match.arg(metric)
            assert_engine(  is_scalar,
                                    x,
                               metric = metric,
                               .xname = get_name_in_parent(x),
                             severity = severity  )
        }


        is_empty <- function(x, metric = c("length", "elements"), .xname = get_name_in_parent(x)){
            metric <- match.arg(metric)
            metric_fn <- get_metric(metric)
            metric_fn(x, 0L, .xname)
        }



        assert_is_empty <- function(
            x, metric = c("length", "elements"), severity = getOption("assertive.severity", "stop")
        ){
            metric <- match.arg(metric)
            assert_engine(  is_empty,
                                   x,
                              metric = metric,
                              .xname = get_name_in_parent(x),
                            severity = severity  )
        }

    #-------
    # names
    #-------

        has_names <- function(x, .xname = get_name_in_parent(x)){
            namesx <- names(x)
            if(    is.null(namesx))   return(false("The names of %s are NULL.",      .xname))
            if(!any(nzchar(namesx)))  return(false("The names of %s are all empty.", .xname))
            TRUE
        }


        assert_has_names <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  has_names,
                                    x,
                               .xname = get_name_in_parent(x),
                             severity = severity  )
        }

    #-----------
    # attributes
    #-----------

        has_attributes <- function(x, attrs, .xname = get_name_in_parent(x))
        {
          if(is_empty(attrs)) return(logical())
          set_cause(
            bapply(attrs, function(at) is_not_null(attr(x, at))),
            "no attr"
          )
        }

        assert_has_all_attributes <- function(x, attrs,
            severity = getOption("assertive.severity", "stop"))
        {
          msg <- gettextf(
            "%s does not have all the attributes %s.",
            get_name_in_parent(x),
            toString(sQuote(attrs))
          )
          assert_engine(
            has_attributes,
            x,
            attrs = attrs,
            msg = msg,
            severity = severity
          )
        }

#======
# TYPES
#======

    #--------
    # logical
    #--------

        is_logical <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "logical", .xname)
        }


        is_a_bool <- function(x, .xname = get_name_in_parent(x)){
            if(!(ok <- is_logical(x, .xname)))          return(ok)
            if(!(ok <- is_scalar( x, .xname = .xname))) return(ok)
            TRUE
        }


        assert_is_a_bool <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_a_bool,
                                    x,
                               .xname = get_name_in_parent(x),
                             severity = severity  )
        }

        assert_is_logical <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_logical,
                                     x,
                                .xname = get_name_in_parent(x),
                              severity = severity  )
        }


    #-------
    # number
    #-------


        is_numeric <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "numeric", .xname)
        }


        is_a_number <- function(x, .xname = get_name_in_parent(x)){
            if(!(ok <- is_numeric(x, .xname)))           return(ok)
            if(!(ok <- is_scalar( x, .xname = .xname)))  return(ok)
            TRUE
        }

        assert_is_a_number <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_a_number,
                                      x,
                                 .xname = get_name_in_parent(x),
                               severity = severity  )
        }


        assert_is_numeric <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_numeric,
                                     x,
                                .xname = get_name_in_parent(x),
                              severity = severity  )
        }

    #-------
    # factor
    #-------

        is_factor <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "factor", .xname)
        }


        assert_is_factor <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_factor,
                                    x,
                               .xname = get_name_in_parent(x),
                             severity = severity )
        }


    #-------------------
    # character / string
    #-------------------


        is_character <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "character", .xname)
        }


        is_a_string <- function(x, .xname = get_name_in_parent(x)){
            if(!(ok <- is_character(x, .xname)))           return(ok)
            if(!(ok <- is_scalar(   x, .xname = .xname)))  return(ok)
            TRUE
        }


        assert_is_character <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_character,
                                       x,
                                  .xname = get_name_in_parent(x),
                                severity = severity  )
        }


        assert_is_a_string <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_a_string,
                                      x,
                                 .xname = get_name_in_parent(x),
                               severity = severity  )
        }


    #--------
    # formula
    #--------

        is_formula <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "formula", .xname)
        }


        is_one_sided_formula <- function(x, .xname = get_name_in_parent(x)){
            if(!(ok <- is_formula(x, .xname)))       return(ok)
            if(!(ok <- is_of_length(x, 2L, .xname))) return(ok)
            TRUE
        }


        assert_is_formula <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine( is_formula,
                                    x,
                               .xname = get_name_in_parent(x),
                             severity = severity )
        }


    #---------
    # function
    #---------

        is_function <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "function", .xname)
        }


        assert_is_function <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_function,
                                      x,
                                 .xname = get_name_in_parent(x),
                               severity = severity  )
        }

    #-----
    # list
    #-----

        is_list <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "list", .xname)
        }


        assert_is_list <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_list,
                                  x,
                             .xname = get_name_in_parent(x),
                           severity = severity  )
        }




    #-------------------
    # data.(frame|table)
    #-------------------


        is_data.frame <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "data.frame", .xname)
        }


        is_data.table <- function(x, .xname = get_name_in_parent(x)){
            if(!(ok <- is_data.frame(x, .xname)))   return(ok)
            is2(x, "data.table", .xname)
        }



        assert_is_data.frame <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_data.frame,
                                        x,
                                   .xname = get_name_in_parent(x),
                                 severity = severity )
        }

        assert_is_data.table <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_data.table,
                                        x,
                                   .xname = get_name_in_parent(x),
                                 severity = severity  )
        }


    #-----------
    # matrix
    #-----------

        is_matrix <- function(x, .xname = get_name_in_parent(x)){
            is2(x, "matrix", .xname)
        }


        assert_is_matrix <- function(x, severity = getOption("assertive.severity", "stop")){
            assert_engine(  is_matrix,
                                    x,
                               .xname = get_name_in_parent(x),
                             severity = severity  )
        }


    #------
    # class
    #------

        assert_is_all_of <- function(
            x, classes, severity = getOption("assertive.severity", "stop")
        ){
            msg <- gettextf(  "%s is not in all of the classes %s.",
                               get_name_in_parent(x),
                               toString(sQuote(classes)) )
            assert_engine(  is2,
                              x,
                          class = classes,
                            msg = msg,
                       severity = severity  )
        }

   #------------
   # inheritance
   #------------

        is_inherited_from <- function(x, classes, .xname = get_name_in_parent(x))
        {
          ok <- bapply(classes, function(class) inherits(x, class))
          if(!any(ok))
          {
            msg <- ngettext(
              length(classes),
              "%s does not inherit from the class %s. It has class %s.",
              "%s does not inherit from any of the classes %s. It has class %s."
            )
            return(
              false(msg, .xname, toString(classes), toString(class(x)))
            )
          }
          TRUE
        }

        assert_is_inherited_from <- function(x, classes,
            severity = getOption("assertive.severity", "stop"))
        {
          assert_engine(
            is_inherited_from,
            x,
            classes = classes,
            .xname = get_name_in_parent(x)
          )
        }

#=========
# STRINGS
#=========


        is_missing_or_empty_character <- function(x, .xname = get_name_in_parent(x)){
            x <- coerce_to(x, "character", .xname)
            ok <- !nzchar(x) | is_na(x)
            set_cause(ok, "nonempty")
        }


        is_non_missing_nor_empty_character <- function(x, .xname = get_name_in_parent(x)){
            x <- coerce_to(x, "character", .xname)
            ok <- nzchar(x) & !is_na(x)
            set_cause(ok, ifelse(is.na(x), "missing", "empty"))
        }



        assert_all_are_non_missing_nor_empty_character <- function(
            x, severity = getOption("assertive.severity", "stop")
        ){
            .xname <- get_name_in_parent(x)
            msg <- gettextf( "%s are not all non-missing nor non-empty strings.", .xname )
            assert_engine(  is_non_missing_nor_empty_character,
                                                             x,
                                                        .xname = .xname,
                                                           msg = msg,
                                                      severity = severity )
        }

        #-------------------------------------------------------------------------------------


        is_matching_regex <- function(
            x, pattern, opts_regex = NULL, .xname = get_name_in_parent(x)
        ){
            x <- coerce_to(x, "character", .xname)
            call_and_name(
                function(x){
                    ok <- stringi::stri_detect_regex(x, pattern, opts_regex = opts_regex)
                    set_cause(ok, gettextf("does not match '%s'", pattern))
                },
                x
            )
        }


        assert_any_are_matching_regex <- function(
            x, pattern, opts_regex = NULL, na_ignore = FALSE,
            severity = getOption("assertive.severity", "stop")
        ){
            .xname <- get_name_in_parent(x)
            .fixedname <- get_name_in_parent(pattern)
            msg <- sprintf( "%s does not match %s", .xname, .fixedname )
            assert_engine(  is_matching_regex,
                                            x,
                                      pattern,
                                   opts_regex = opts_regex,
                                       .xname = .xname,
                                          msg = msg,
                                         what = 'any',
                                    na_ignore = na_ignore,
                                     severity = severity  )
        }

        #-------------------------------------------------------------------------------------

        is_numeric_string <- function(x, .xname){
            x <- coerce_to(x, "character", .xname)
            ok <- call_and_name(
                    function(x)  suppressWarnings(  {   numx <- as.numeric(x)
                                                       is_not_na(numx)        } ),
                    x )
            set_cause(ok, ifelse(is.na(x), "missing", "bad format"))
        }


#=========
# NUMBERS
#=========

        is_in_range <- function(
            x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, upper_is_strict = FALSE,
           .xname = get_name_in_parent(x)
        ){
            x <- coerce_to(x, "numeric", .xname)
            lower <- coerce_to(lower, "numeric")
            upper <- coerce_to(upper, "numeric")
            lower_is_strict <- coerce_to(use_first(lower_is_strict), "logical")
            upper_is_strict <- coerce_to(use_first(upper_is_strict), "logical")
            ok <- rep.int(TRUE, length(x))
            ok[is.na(x)] <- NA
            too_low  <- (if(lower_is_strict) `<=` else `<`)(x, lower)
            too_high <- (if(upper_is_strict) `>=` else `>`)(x, upper)
            ok[too_low ] <- FALSE
            ok[too_high] <- FALSE
            names(ok) <- x
            set_cause(ok, ifelse(too_low, "too low", "too high") )
        }


        is_in_closed_range <- function(
            x, lower = -Inf, upper = Inf, .xname = get_name_in_parent(x)
        ){
            is_in_range(x, lower, upper, FALSE, FALSE, .xname = .xname)
        }


        is_in_left_open_range <- function(
            x, lower = -Inf, upper = Inf, .xname = get_name_in_parent(x)
        ){
            is_in_range(x, lower, upper, TRUE, FALSE, .xname = .xname)
        }


        make_range_string <- function(lower, upper, lower_is_strict, upper_is_strict){
             left <- if(lower_is_strict) "(" else "["
            right <- if(upper_is_strict) ")" else "]"
            paste0(left, lower, ",", upper, right)
        }


        assert_all_are_in_range <- function(
            x, lower = -Inf, upper = Inf, lower_is_strict = FALSE, upper_is_strict = FALSE,
            na_ignore = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            .xname <- get_name_in_parent(x)
            msg <- gettextf( "%s are not all in the range %s.",
                             .xname,
                             make_range_string(lower, upper, lower_is_strict, upper_is_strict) )
            assert_engine(  is_in_range,
                                      x,
                                  lower = lower,
                                  upper = upper,
                        lower_is_strict = lower_is_strict,
                        upper_is_strict = upper_is_strict,
                                 .xname = .xname,
                                    msg = msg,
                              na_ignore = na_ignore,
                               severity = severity
            )
        }


        assert_all_are_in_left_open_range <- function(
            x, lower = -Inf, upper = Inf, na_ignore = FALSE,
            severity = getOption("assertive.severity", "stop")
        ){
            .xname <- get_name_in_parent(x)
            msg <- gettextf( "%s are not all in the range %s.",
                             .xname,
                              make_range_string(lower, upper, TRUE, FALSE) )
            assert_engine(  is_in_left_open_range,
                                                x,
                                            lower = lower,
                                            upper = upper,
                                           .xname = .xname,
                                              msg = msg,
                                        na_ignore = na_ignore,
                                         severity = severity   )
        }


        is_greater_than <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(x)
        ){
            if(!is.integer(x))   x <- coerce_to(x, "numeric", .xname)
            if(!is.integer(y))   y <- coerce_to(y, "numeric", .yname)
            ok <- x > y
            call_and_name(
                function(x)  set_cause(ok, paste("less than or equal to", y)),
                rep_len(x, length(ok)) )
        }


        is_greater_than_or_equal_to <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(x)
        ){
            if(!is.integer(x))   x <- coerce_to(x, "numeric", .xname)
            if(!is.integer(y))   y <- coerce_to(y, "numeric", .yname)
            ok <- x >= y
            call_and_name(
                function(x) set_cause(ok, paste("less than", y)),
                rep_len(x, length(ok))
            )
        }

        is_less_than_or_equal_to <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(x)){
            if(!is.integer(x))  x <- coerce_to(x, "numeric", .xname)
            if(!is.integer(y))  y <- coerce_to(y, "numeric", .yname)
            ok <- x <= y
            call_and_name(
                function(x)  set_cause(ok, paste("greater than", y)) ,
                rep_len(x, length(ok)) )
        }


        assert_all_are_less_than_or_equal_to <- function(
            x, y, na_ignore = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            .xname <- get_name_in_parent(x)
            .yname <- get_name_in_parent(y)
            msg <- gettextf( "%s are not all less than or equal to %s.", .xname, .yname )
            assert_engine(  is_less_than_or_equal_to,
                                                   x,
                                                   y = y,
                                              .xname = .xname,
                                              .yname = .yname,
                                                 msg = msg,
                                           na_ignore = na_ignore,
                                            severity = severity )
        }

        is_divisible_by <- function(x, n, tol = 100 * .Machine$double.eps,
                                    .xname = get_name_in_parent(x))
        {
          if(!is.integer(x))
          {
            x <- coerce_to(x, "numeric", .xname)
          }
          call_and_name(
            function(x)
            {
              ok <- abs(x %% n) <= tol
              set_cause(ok, "indivisible")
            },
            x
          )
        }

        is_greater_than <- function(x, y,
                                    .xname = get_name_in_parent(x), .yname = get_name_in_parent(x))
        {
          if(!is.integer(x))
          {
            x <- coerce_to(x, "numeric", .xname)
          }
          if(!is.integer(y))
          {
            y <- coerce_to(y, "numeric", .yname)
          }
          ok <- x > y
          call_and_name(
            function(x)
            {
              set_cause(ok, paste("less than or equal to", y))
            },
            rep_len(x, length(ok))
          )
        }

        assert_all_are_greater_than <- function(x, y,
                                                na_ignore = FALSE, severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          .yname <- get_name_in_parent(y)
          msg <- gettextf(
            "%s are not all greater than %s.",
            .xname,
            .yname
          )
          assert_engine(
            is_greater_than,
            x,
            y = y,
            .xname = .xname,
            .yname = .yname,
            msg = msg,
            na_ignore = na_ignore,
            severity = severity
          )
        }



        assert_all_are_in_closed_range <- function(x, lower = -Inf, upper = Inf,
                                                   na_ignore = FALSE, severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          msg <- gettextf(
            "%s are not all in the range %s.",
            .xname,
            make_range_string(lower, upper, FALSE, FALSE)
          )
          assert_engine(
            is_in_closed_range,
            x,
            lower = lower,
            upper = upper,
            .xname = .xname,
            msg = msg,
            na_ignore = na_ignore,
            severity = severity
          )
        }

        is_whole_number <- function(x, tol = 100 * .Machine$double.eps, .xname = get_name_in_parent(x))
        {
          x <- coerce_to(x, "numeric", .xname)
          call_and_name(
            function(x)
            {
              ok <- abs(x - round(x)) <= tol & !is.infinite(x)
              set_cause(ok, ifelse(is.infinite(x), "infinite", "fractional"))
            },
            x
          )
        }

        assert_all_are_whole_numbers <- function(x,
                                                 tol = 100 * .Machine$double.eps, na_ignore = FALSE,
                                                 severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          msg <- gettextf(
            "%s are not all whole numbers (tol = %g).",
            .xname,
            tol
          )
          assert_engine(
            is_whole_number,
            x,
            tol = tol,
            .xname = .xname,
            msg = msg,
            na_ignore = na_ignore,
            severity = severity
          )
        }

        is_non_negative <- function(x, .xname = get_name_in_parent(x))
        {
          is_in_range(x, 0)
        }

        assert_all_are_non_negative <- function(x, na_ignore = FALSE,
                                                severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          msg <- gettextf("%s are not all non-negative.", .xname)
          assert_engine(
            is_non_negative,
            x,
            .xname = .xname,
            msg = msg,
            na_ignore = na_ignore,
            severity = severity
          )
        }

        is_positive <- function(x, .xname = get_name_in_parent(x))
        {
          is_in_range(x, 0, lower_is_strict = TRUE, .xname = .xname)
        }

        assert_all_are_positive <- function(x, na_ignore = FALSE,
                                            severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          msg <- gettextf("%s contains non-positive values.", .xname)
          assert_engine(
            is_positive,
            x,
            .xname = .xname,
            msg = msg,
            na_ignore = na_ignore,
            severity = severity
          )
        }

#=====
# SETS
#=====


        is_subset <- function(
            x, y, strictly = FALSE, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
        ){
            diffxy <- setdiff(x, y)
            if(length(diffxy) > 0)   return( false( ngettext( length(diffxy),
                                                             "The element %s in %s is not in %s.",
                                                             "The elements %s in %s are not in %s." ),
                                                    toString(sQuote(diffxy), width = 100),
                                                   .xname,
                                                   .yname ) )
            if(strictly && length(setdiff(y, x)) == 0)   return( false( "%s and %s are set equal.",
                                                                        .xname,
                                                                        .yname))
            TRUE
        }


        are_disjoint_sets <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
        ){
            intersectionxy <- intersect(x, y)
            if(length(intersectionxy) > 0){
                return(  false( gettext( "%s and %s have common elements: %s."),
                               .xname,
                               .yname,
                                toString(intersectionxy, width = 100) ) )
            }
            TRUE
        }


        are_set_equal <- function(
            x, y, .xname = get_name_in_parent(x), .yname = get_name_in_parent(y)
        ){
            x <- unique(x)
            y <- unique(y)
            if(length(x) != length(y)){
                return( false( gettext( "%s and %s have different numbers of elements (%d versus %d)."),
                              .xname,
                              .yname,
                               length(x),
                               length(y) ) )
            }
            if(!(ok <- is_subset(x, y, FALSE, .xname, .yname)))  return(ok)
            if(!(ok <- is_subset(y, x, FALSE, .yname, .xname)))  return(ok)
            TRUE
        }


        assert_is_subset <- function(
            x, y, strictly = FALSE, severity = getOption("assertive.severity", "stop")
        ){
            assert_engine( is_subset,
                                   x,
                                   y = y,
                            strictly = strictly,
                              .xname = get_name_in_parent(x),
                              .yname = get_name_in_parent(y),
                            severity = severity )
        }

        assert_are_disjoint_sets <- function(
            x, y, severity = getOption("assertive.severity", "stop")
        ){
            assert_engine(  are_disjoint_sets,
                                            x,
                                            y = y,
                                       .xname = get_name_in_parent(x),
                                       .yname = get_name_in_parent(y),
                                     severity = severity  )
        }

#======
# FILES
#======

        is_existing_file <- function(x, .xname = get_name_in_parent(x)){
            x <- coerce_to(x, "character", .xname)
            # file.exists returns FALSE under Windows when there is a trailing slash
            x <- sub("[\\/]+$", "", x)
            call_and_name( function(x){
                                ok <- file.exists(x)
                                set_cause(ok, ifelse(ok, "", "nonexistent"))
                            },
                            x )
        }

        is_dir <- function(x, .xname = get_name_in_parent(x)){
            x <- coerce_to(x, "character", .xname)
            call_and_name(  function(x){
                                ok <- file.info(x)$isdir
                                causes <- ifelse( is.na(ok), "nonexistent", ifelse(ok, "", "file"))
                                ok <- is_true(ok)
                                set_cause(ok, causes)
                            },
                            x )
        }

        is_readable_file <- function(x, warn_about_windows = TRUE, .xname = get_name_in_parent(x))
        {
          if(warn_about_windows)
          {
            warn_about_file.access_under_windows()
          }
          x <- coerce_to(x, "character", get_name_in_parent(x))
          call_and_name(
            function(x)
            {
              ok <- file.access(x, mode = 4) == 0L
              set_cause(
                ok,
                ifelse(file.exists(x), "unreadable", "nonexistent")
              )
            },
            x
          )
        }

        warn_about_file.access_under_windows <- function()
        {
          if(.Platform$OS.type == "windows") # as per is_windows, without the dependency
          {
            warning(
              "This function depends on file.access, which can give unexpected results under Windows."
            )
          }
        }

        assert_all_are_dirs <- function(x, severity = getOption("assertive.severity", "stop")){
            .xname <- get_name_in_parent(x)
            msg <- gettextf("Some or all of the paths specified by %s are not directories.", .xname)
            assert_engine( is_dir,
                                x,
                           .xname = .xname,
                              msg = msg,
                             what = "any",
                         severity = severity )
        }

        assert_all_are_existing_files <- function(x, severity = getOption("assertive.severity", "stop")){
            .xname <- get_name_in_parent(x)
            msg <- gettextf("Some or all of the files specified by %s do not exist.", .xname)
            assert_engine( is_existing_file,
                                          x,
                                     .xname = .xname,
                                        msg = msg,
                                   severity = severity )
        }

        assert_all_are_readable_files <- function(x, warn_about_windows = TRUE,
                                                 severity = getOption("assertive.severity", "stop"))
        {
          .xname <- get_name_in_parent(x)
          msg <- gettextf(
            "Some or all of the paths specified by %s are not readable files.",
            .xname
          )
          assert_engine(
            is_readable_file,
            x,
            warn_about_windows = warn_about_windows,
            .xname = .xname,
            msg = msg,
            severity = severity
          )
        }

