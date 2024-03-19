#' Get install information about a package
#' @description Quickly get LibPath, version, built and package date of a package
#' @return A data frame
#' @param name A package name
#' @export
installinfo <- function(name){
info <- utils::packageDescription(name)
pkgimportant <- paste(info[c("Suggests", "Imports", "Depends")], collapse = ", ")
pkgimportant <- unlist(strsplit(gsub("[^,ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", "", pkgimportant), ","))
pkgimportant <- c(pkgimportant, name)
pkginfo <- as.data.frame(utils::installed.packages(fields = c("Built", "Packaged")))
pkginfo <- pkginfo[pkginfo$Package %in% pkgimportant, 
                   c("Package", "LibPath", "Version",
                     "Built", "Packaged")]
return(pkginfo)
}

