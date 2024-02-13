#' @title Catch errors, warnings, stderr and put all into a text object
#' @description Another package redefines a function, which might be the best way to do it. I find this way simpler at the moment. I find this function useful for simulation studies.
#' @details Much of information about capturing the warnings I got from [http://adv-r.had.co.nz/beyond-exception-handling.html]
#' @examples
#' catchew({warning("a test warning"); "hello"})
#' catchew({stop("a test stop"); "hello"})
#' @export
catchew <- function(expr){
  messages <- vector(mode = "character")
  catchmessage <- function(e){
    assign("messages",
           c(messages, paste0(paste0(class(e)[[1]], collapse=""), ": ", e$message)),
           envir = parent.env(as.environment(-1)))
  }
  stderr <- utils::capture.output({res <- tryCatch(
           withCallingHandlers(expr,
                               warning =  function(w) {
                                 if (inherits(w, "warning")){ 
                                   catchmessage(w)
                                   tryInvokeRestart("muffleWarning")
                               }},
                               message =  function(w) {
                                 catchmessage(w)
                               }),
                  error = catchmessage)},
    type = "message")
  if (length(stderr) > 0){
    messages <- c(messages, paste("stderr:", stderr))
  }

  return(list(res = res, messages = messages)) 
}


