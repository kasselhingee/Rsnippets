#' @title Catch errors and warnings, and put into a text object
#' @description Another package redefines a function, which might be the best way to do it. I find this way simpler at the moment.
#' @details Much of information about capturing the warnings I got from [http://adv-r.had.co.nz/beyond-exception-handling.html]
#' @examples
#' catchew({warning("a test warning"); "hello"})
#' catchew({stop("a test stop"); "hello"})
#' @export
catchew <- function(expr){
  messages <- vector(mode = "character")
  catchmessage <- function(e){
    assign("messages",
           c(messages, e$message),
           envir = parent.env(as.environment(-1)))
  }
  res <- tryCatch(
           withCallingHandlers(expr,
                               warning =  function(w) {
                                 if (inherits(w, "warning")){ 
                                   catchmessage(w)
                                   tryInvokeRestart("muffleWarning")
                               } }),
                  error = catchmessage)
  return(list(res = res, messages = messages)) 
}
