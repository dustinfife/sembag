`%.%` = function(a, b) {
  paste0(a, b)
}

`%-%` = function(a, b, sep) {
  paste0(a, b, collapse=sep)
}

remove_names = function(object) {
  names(object) = NULL
  return(object)
}
