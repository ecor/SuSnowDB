NULL
#' "tbl" implemantation for a list
#'
#' @param src argument
#' @param ... argument
#' 
#'
#' @importFrom dplyr tbl
#'
#' @title tbl
#' @rdname tbl
#' @method tbl list
#' @aliases tbl
#' @export
#' 
#' 
setMethod("tbl","list",function(src,...){src[[list(...)[[1]]]]})