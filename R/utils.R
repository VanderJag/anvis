serial_next <- function(name_base, ext) {
  if (!file.exists(paste0(name_base, ext))) return(name_base)
  i = 1
  repeat {
    f = paste0(name_base, "_", i)
    if(!file.exists(paste0(f, ext))){return(f)}
    i = i + 1
  }
}
