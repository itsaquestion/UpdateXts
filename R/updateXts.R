#' updateXts
#'
#' use new data to update an xts object.
#'
#' @param x old xts object
#' @param new_data an xts object with new data
#'
#' @return an updated xts object
#' @aliases update.xts
#' @export
#' @import xts
#' @import zoo
#' @import checkmate
#' @examples
#' \dontrun{
#' library(xts)
#' a = as.xts(1:10, order.by = seq(Sys.Date() - 15, Sys.Date()-6, "days"))
#' a
#'
#' b = as.xts(101:110, order.by = seq(Sys.Date() - 9, Sys.Date(), "days"))
#' b
#'
#' updateXts(a,b)
#' update(a,b)
#' }

updateXts = function(x, new_data){
  assertClass(x,"xts")
  assertClass(new_data,"xts")

  assertSameNames(x, new_data)
  assertLength(x, new_data)

  old_date = index(x)
  new_date = index(new_data)

  new_xts_p1 = x[!(old_date %in% new_date)]

  suppressWarnings({
    new_xts =
      rbind(new_xts_p1, new_data)
  })

  new_xts
}


#' @export
update.xts = function(object,new_data,...){
  updateXts(object, new_data)
}

assertSameNames = function(x, y){
  #checkmate::assertClass(x,"xts")
  #checkmate::assertClass(y,"xts")

  ok = isTRUE(all.equal(names(x),names(y)))

  if(!ok){
    stop("x and y must has the same colnames!")
  }
  invisible(ok)
}

assertLength = function(x, y){
  #checkmate::assertClass(x,"xts")
  #checkmate::assertClass(y,"xts")

  ok = !(length(x) == 0 & length(y) == 0)

  if(!ok){
    stop("x and y both has 0 length!")
  }
  invisible(ok)
}

