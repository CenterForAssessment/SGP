#' FUNCTIONS COPIED FROM THE `gtools` package
#' (8/6/26 - https://github.com/r-gregmisc/gtools/blob/master/R/mixedsort.R -- c8c3ce1)
#' Modified slightly only to remove Roman numeral use cases.
#' All `gtools` examples and exports have been removed from this text (internal SGP use only)

#' Order or Sort strings with embedded numbers so that the numbers are in the
#' correct order
#'
#' These functions sort or order character strings containing embedded numbers
#' so that the numbers are numerically sorted rather than sorted by character
#' value.  I.e. "Aspirin 50mg" will come before "Aspirin 100mg".  In addition,
#' case of character strings is ignored so that "a", will come before "B" and
#' "C".
#'
#' I often have character vectors (e.g. factor labels), such as compound and
#' dose, that contain both text and numeric data.  This function is useful for
#' sorting these character vectors into a logical order.
#'
#' It does so by splitting each character vector into a sequence of character
#' and numeric sections, and then sorting along these sections, with numbers
#' being sorted by numeric value (e.g. "50" comes before "100"), followed by
#' characters strings sorted by character value (e.g. "A" comes before "B")
#' \emph{ignoring case} (e.g. 'A' has the same sort order as 'a').
#'
#' By default, sort order is ascending, empty strings are sorted to the front,
#' and \code{NA} values to the end.  Setting \code{descending=TRUE} changes the
#' sort order to descending and reverses the meanings of \code{na.last} and
#' \code{blank.last}.
#'
#' Parsing looks for decimal numbers unless \code{numeric.type="roman"}, in
#' which parsing looks for roman numerals, with character case specified by
#' \code{roman.case}.
#'
#' @aliases mixedsort mixedorder
#' @param x Vector to be sorted.
#' @param decreasing logical.  Should the sort be increasing or decreasing?
#' Note that \code{descending=TRUE} reverses the meanings of \code{na.last} and
#' \code{blanks.last}.
#' @param na.last for controlling the treatment of \code{NA} values.  If
#' \code{TRUE}, missing values in the data are put last; if \code{FALSE}, they
#' are put first; if \code{NA}, they are removed.
#' @param blank.last for controlling the treatment of blank values.  If
#' \code{TRUE}, blank values in the data are put last; if \code{FALSE}, they
#' are put first; if \code{NA}, they are removed.
#' @param numeric.type either "decimal" (default) or "roman".  Are numeric
#' values represented as decimal numbers (\code{numeric.type="decimal"}) or as
#' Roman numerals (\code{numeric.type="roman"})?
#' @param roman.case one of "upper", "lower", or "both".  Are roman numerals
#' represented using only capital letters ('IX') or lower-case letters ('ix')
#' or both?
#' @param scientific logical. Should exponential notation be allowed for numeric values.
#' @param hyphen.separator logical. Should hyphenated strings be treated as separators? (If not, they will be treated as potential components of numeric values.)
#' @return \code{mixedorder} returns a vector giving the sort order of the
#' input elements. \code{mixedsort} returns the sorted vector.
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[base]{sort}}, \code{\link[base]{order}}
#' @keywords univar manip

mixedsort <- function(x,
                      decreasing = FALSE,
                      na.last = TRUE,
                      blank.last = FALSE,
                      numeric.type = c("decimal", "roman"),
                      roman.case = c("upper", "lower", "both"),
                      scientific = TRUE,
                      hyphen.separator = FALSE) {
  ord <- mixedorder(x,
    decreasing = decreasing,
    na.last = na.last,
    blank.last = blank.last,
    numeric.type = numeric.type,
    roman.case = roman.case,
    scientific = scientific,
    hyphen.separator = hyphen.separator
  )
  x[ord]
}

mixedorder <- function(x,
                       decreasing = FALSE,
                       na.last = TRUE,
                       blank.last = FALSE,
                       numeric.type = c("decimal", "roman"),
                       roman.case = c("upper", "lower", "both"),
                       scientific = TRUE,
                       hyphen.separator = FALSE) {
  # - Split each each character string into an vector of strings and
  #   numbers
  # - Separately rank numbers and strings
  # - Combine orders so that strings follow numbers

  numeric.type <- match.arg(numeric.type)
  roman.case <- match.arg(roman.case)

  if (length(x) < 1) {
    return(NULL)
  } else if (length(x) == 1) {
    return(1)
  }

  if (!is.character(x)) {
    return(order(x, decreasing = decreasing, na.last = na.last))
  }

  delim <- "\\$\\@\\$"

  nums <- "[0123456789]"
  pm <- if (hyphen.separator) "[+]" else "[+-]"
    
  if (numeric.type == "decimal") {
    if (scientific) {
      regex <- sprintf("((?:(?i)(?:%s?)(?:(?=[.]?%s)(?:%s*)(?:(?:[.])(?:%s{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:%s+))|)))", pm, nums, nums, nums, nums)
    } # uses PERL syntax
    else {
      regex <- sprintf("((?:(?i)(?:%s?)(?:(?=[.]?%s)(?:%s*)(?:(?:[.])(?:%s{0,}))?)))", pm, nums, nums, nums)
    } # uses PERL syntax

    numeric <- function(x) as.numeric(x)
  }  ##  No roman numeral support in SGP
  # else if (numeric.type == "roman") {
  #   regex <- switch(roman.case,
  #     "both"  = "([IVXCLDMivxcldm]+)",
  #     "upper" = "([IVXCLDM]+)",
  #     "lower" = "([ivxcldm]+)"
  #   )
  #   numeric <- function(x) roman2int(x)
  # }
  else {
    stop("Unknown value for numeric.type: ", numeric.type)
  }

  nonnumeric <- function(x) {
    ifelse(is.na(numeric(x)), toupper(x), NA)
  }

  x <- as.character(x)

  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")

  ####
  # - Convert each character string into an vector containing single
  #   character and  numeric values.
  ####

  # find and mark numbers in the form of +1.23e+45.67
  delimited <- gsub(regex,
    paste(delim, "\\1", delim, sep = ""),
    x,
    perl = TRUE
  )

  # separate out numbers
  step1 <- strsplit(delimited, delim)

  # remove empty elements
  step1 <- lapply(step1, function(x) x[x > ""])

  # create numeric version of data
  suppressWarnings(step1.numeric <- lapply(step1, numeric))

  # create non-numeric version of data
  suppressWarnings(step1.character <- lapply(step1, nonnumeric))

  # now transpose so that 1st vector contains 1st element from each
  # original string
  maxelem <- max(sapply(step1, length))

  step1.numeric.t <- lapply(
    1:maxelem,
    function(i) {
      sapply(
        step1.numeric,
        function(x) x[i]
      )
    }
  )

  step1.character.t <- lapply(
    1:maxelem,
    function(i) {
      sapply(
        step1.character,
        function(x) x[i]
      )
    }
  )

  # now order them
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(
    step1.character.t,
    function(x) as.numeric(factor(x))
  )

  # and merge
  rank.numeric[!is.na(rank.character)] <- 0 # mask off string values

  rank.character <- t(
    t(rank.character) +
      apply(matrix(rank.numeric), 2, max, na.rm = TRUE)
  )

  rank.overall <- ifelse(is.na(rank.character), rank.numeric, rank.character)

  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) {
    if (is.na(na.last)) {
      order.frame[which.nas, ] <- NA
    } else if (na.last) {
      order.frame[which.nas, ] <- Inf
    } else {
      order.frame[which.nas, ] <- -Inf
    }
  }

  if (length(which.blanks) > 0) {
    if (is.na(blank.last)) {
      order.frame[which.blanks, ] <- NA
    } else if (blank.last) {
      order.frame[which.blanks, ] <- 1e99
    } else {
      order.frame[which.blanks, ] <- -1e99
    }
  }

  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA

  retval <- do.call("order", order.frame)

  return(retval)
}
