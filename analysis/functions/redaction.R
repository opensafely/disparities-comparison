#define function for midpoint 10 rounding
roundmid_any <- function(x, to=10){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}
