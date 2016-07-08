#####################################################
# Costing Algorithm
#
costs <- function(arrivals)
{
  stats <- as.data.frame(do.call(rbind, lapply(split(arrivals, arrivals$name), FUN=function(x)
  {
    life <- x[x$resource=="life",]
    results <- c(sum(x$discounted_cost),
                (sum(life$discounted_time) - sum(x$disutility))/365,
                sum(life$activity_time)/365)
    names(results) <- c("Discount.Cost", "QALY", "Life")
    results
  })))
  
  stats$ce_ratio <- stats$Discount.Cost / stats$QALY
  
  stats
}
