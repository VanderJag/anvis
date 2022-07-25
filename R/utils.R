file_sequence <- function(name_base, ext) {
  if (!file.exists(paste0(name_base, ext))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext))) return(seq_next)
    i = i + 1
  }
}

file_pair_seq <- function(name_base, ext1, ext2) {
  if (!file.exists(paste0(name_base, ext1)) &&
      !file.exists(paste0(name_base, ext2))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext1)) &&
        !file.exists(paste0(seq_next, ext2))) return(seq_next)
    i = i + 1
  }
}

# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

# From gplot package
col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}

