#' updateXts
#'
#' use new data to update an xts object.
#'
#' @param x old xts object
#' @param new_data an xts object with new data
#'
#' @return an updated xts object
#' @export
#' @import xts
#' @import zoo
#' @import checkmate
#'
#' @examples

updateXts = function(x, new_data){
  checkmate::assertClass(x,"xts")
  checkmate::assertClass(new_data,"xts")

  assertSameNames(x, new_data)
  assertLength(x, new_data)

  old_date = zoo::index(x)
  new_date = zoo::index(new_data)

  new_xts_p1 = x[!(old_date %in% new_date)]

  suppressWarnings({
    new_xts = rbind(new_xts_p1, new_data)
  })

  new_xts
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

