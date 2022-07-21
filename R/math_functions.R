sigmoid_xB <- function(x, B){
  T = (1/(1+((x/(1-x))^-B)))
  return(T)
}

nthroot = function(x,n) {
  (abs(x)^(1/n))*sign(x)
}

# create vector of angles for text based on number of nodes
# (flipping the orientation of the words half way around so none appear upside down)
radial_angle <- function(x, y) {
  ifelse(atan(-(x/y))*(180/pi) < 0,
                 90 + atan(-(x/y))*(180/pi),
                 270 + atan(-x/y)*(180/pi))
}
# formula from: https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
