file_sequence <- function(name_base, ext) {
  if (!file.exists(paste0(name_base, ext))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext))) return(seq_next)
    i = i + 1
  }
}
