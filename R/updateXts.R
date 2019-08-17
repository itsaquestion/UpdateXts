#' updateXts
#'
#' use new data to update the old xts object(x)
#'
#' @param x old xts object
#' @param new_data new data
#'
#' @return a updated xts object
#' @export
#' @import xts
#' @import checkmate
#'
#' @examples
#'
#'
#'
updateXts = function(x, new_data){
  checkmate::assertClass(x,"xts")
  checkmate::assertClass(new_data,"xts")

  assertSameNames(x, new_data)
  assertLength(x, new_data)

  old_date = index(x)
  new_date = index(new_data)

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

  wrong = (length(x) == 0 & length(y) == 0)

  if(wrong){
    stop("x and y both has 0 length!")
  }
  invisible(ok)
}
