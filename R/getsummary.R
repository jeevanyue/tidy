
'%!in%' <- function(x,y)!('%in%'(x,y))

#' getsummary is a generic function used to detects variables and produce a comprehensive summary
#' @param object variable that you want get summary
#' @return Returns the summary
#' @examples
#' data(iris)
#' getsummary(iris$Species)
#' @export
getsummary <- function (object, ...)
{
  UseMethod("getsummary")
}

getsummary.default <- function (object, ..., digits)
{
  if (is.factor(object))
    return(getsummary.factor(object, ...))

  value <- if (is.logical(object))
    return(getsummary.logical(object, ...))
  else if (is.numeric(object)) {
    return(getsummary.numeric(object, ...))
  }
  else summary(object, ...)

  value
}

getsummary.numeric <- function (object, maxsum = 10L, ...)
{
  nas <- is.na(object)

  sumry <- summary(object, ...)
  if (!any(nas))
    sumry <- c(sumry, `NA's` = 0)
  class(sumry) <- 'numeric'

  if(length(unique(object)) > maxsum){
    hst <- hist(object, plot = FALSE)

    h <- list()
    h <- lapply(seq(1, length(hst$breaks) - 1), function(x) {
      paste0(hst$breaks[x], "-", hst$breaks[x + 1])
    })
    names(hst$counts) <- h

    hst$counts <- hst$counts[hst$counts != 0]

    hst <- hst$counts
  }else{
    of <- as.factor(object)
    ll <- levels(of)
    getsumry <- getsummary.factor(of, maxsum, ...)

    hst <- getsumry$hist

  }

  list(
    class = class(object),
    hist = hst,
    summary = sumry
  )

}

getsummary.factor <- function (object, maxsum = 10L, ...)
{
  nas <- is.na(object)
  ll <- levels(object)
  sumry <- summary(object, maxsum, ...)
  if (!any(nas))
    sumry <- c(sumry, `NA's` = 0)

  list(
    class = class(object),
    hist = sumry,
    summary = c(sumry["NA's"], Levels = length(ll), Min.Length = min(nchar(ll)), Max.Length = max(nchar(ll)))
  )
}

getsummary.character <- function (object, maxsum = 10L, ...)
{
  object <- as.factor(object)
  ll <- levels(object)
  getsumry <- getsummary.factor(object, maxsum, ...)

  getsumry$class <- "character"
  names(getsumry$summary) <- c("NA's", "Unique", "Min.Length", "Max.Length")

  getsumry
}

getsummary.logical <- function (object, maxsum = 10L, ...)
{
  nas <- is.na(object)
  sumry <- summary(object, maxsum, ...)

  nas <- is.na(object)
  tb <- table(object, exclude = NULL, useNA = "ifany")
  if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"

  if (!any(nas))
    tb <- c(tb, `NA's` = 0)

  if ('FALSE' %!in% names(tb))
    tb <- c(tb, "FALSE" = 0)

  if ('TRUE' %!in% names(tb))
    tb <- c(tb, "TRUE" = 0)

  list(
    class = "logical",
    hist = tb,
    summary = tb
  )
}

getsummary.Date <- function (object, maxsum = 10L, ..., digits = 12L)
{
  nas <- is.na(object)

  days <- as.numeric(max(object, na.rm = TRUE) - min(object, na.rm = TRUE))
  hst <- hist(object, breaks = 'months', plot = FALSE)
  hst$breaks <- as.Date(hst$breaks, origin = '1970-01-01')
  hst$mids <- as.Date(hst$mids, origin = '1970-01-01')

  h <- list()
  h <- lapply(seq(1, length(hst$breaks) - 1), function(x) {
    h[x] <- paste0(hst$breaks[x], "-", hst$breaks[x + 1])
  })
  names(hst$counts) <- h

  hst$counts <- hst$counts[hst$counts != 0]

  sumry <- summary(object, maxsum, ..., digits)

  class(sumry) <- 'Date'

  sumry <- as.character(sumry)

  # if (!any(nas))
  #   sumry <- c(sumry, `NA's` = "0")
  sumry <- c(sumry, `NA's` = sum(nas))

  list(
    class = class(object),
    hist = hst$counts,
    summary = sumry
  )
}


getsummary.POSIXct <- function (object, digits = 15L, ...)
{
  x <- summary.default(unclass(object), digits = digits, ...)
  if (m <- match("NA's", names(x), 0)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  class(x) <- c("summaryDefault", "table", oldClass(object))
  attr(x, "tzone") <- attr(object, "tzone")
  x
}
