#####################################################
# Costing Algorithm
#
library(boot)

costs <- function(arrivals)
{
  stats <- do.call(rbind, lapply(split(arrivals, arrivals$name), FUN=function(x)
  {
    life <- x[x$resource=="life",]
    results <- c(sum(x$discounted_cost),
                (sum(life$discounted_time) - sum(x$disutility))/365,
                sum(life$activity_time)/365)
    names(results) <- c("Discounted Cost", "QALY", "Life")
    results
  }))

  bo <- boot(stats, R=500, statistic=function (x,idx){
  colSums(x[idx,])/length(idx)
  })
  
  dc95   <- boot.ci(bo, type="basic", index=1)
  qaly95 <- boot.ci(bo, type="basic", index=2)
  life95 <- boot.ci(bo, type="basic", index=3)
  
  list(stats=stats,boot=bo,disc_cost95=dc95,qaly95=qaly95,life95=life95)
}