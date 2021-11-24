#' @author Abhijit Suryavanshi, \email{abhi.surya@@gmail.com}
#' @title a function for creating custom groups of elements in an ordered sequence
#' @param start a logical TRUE/FALSE vector which indicates starting positions of the group window
#' @param end a logical TRUE/FALSE vector which indicates ending positions of the group window
#' @param include.start include the starting position in the group, TRUE/FALSE, Defaults to TRUE
#' @param include.end include the ending position in the group, TRUE/FALSE, Defaults to TRUE
#' @importFrom dplyr group_by summarize ungroup
#' @importFrom magrittr %>%
#' @export
edgefinder <- local( function( start, end, include.start = TRUE, include.end = TRUE) {
  if (include.start == TRUE) {
    start.edge = diff(c(FALSE,start, FALSE) ) ## rising edge
    start.edge[start.edge <= 0] = 0
  } else {
    start.edge = - diff(c(FALSE,start,FALSE)   )   ## falling edge
    start.edge[start.edge <= 0] = 0
  }

  if (include.end == TRUE) {
    end.edge = - diff(c(FALSE,end, FALSE) )   ## falling edge
    end.edge[end.edge <= 0] = 0
  } else {
    end.edge = diff(c(FALSE,end, FALSE) ) ## rising edge
    end.edge[end.edge <= 0] = 0
  }

  a = which(start.edge == 1)
  p = data.frame(b = which(end.edge == 1) )

  if(length(a) == 0 | nrow(p) == 0 ) {
    return(rep(0,length(start) ))
  }


  # p$b1 = sapply(p$b,function(x) ifelse(include.end== T,max(a[a<x]),max(a[a<=x]) ) )
  # p1 = p%>%group_by(b1)%>%summarize(b = min(b) )%>%ungroup()
  # p1 = p1[!is.infinite(p1$b1),]

  ind = findInterval(p$b,a)
  ind[ind==0] = -Inf
  p$b1 = a[ind]
  p1 = p[p$b == ave(p$b, p$b1, FUN=min),]
  p1 = p1[!is.na(p1$b1),]
  
  grps = rep(0,length(start)+1)
  grps[p1$b1] = 1
  grps[p1$b] = grps[p1$b]-1

  return(cumsum(grps)[1:length(start)])

} )

#' @examples
#' a = c(1,2,3,22,4,554,667,2,89,9,6,22,899,4)
#' edgefinder(a==554, a==9, include.end = FALSE)
#' edgefinder(a==2, a==22)
