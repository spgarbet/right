library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)

icer <- function(results) 
{
  x <- results %>% arrange(dCOST) %>% mutate(ICER = (lag(dCOST)-dCOST)/(lag(dQALY)-dQALY)) 

  #strong dominance (cross out strategies with a negative ICER)
  str.dom <- NULL
  if(any(x$ICER[-1]<0)==FALSE) {
    x$dominated[2:(nrow(x))] = 0
  } 
  while(any(x$ICER[-1]<0))
    {
      y <- x %>% filter(ICER<0)
      x <- x %>% filter(ICER>0 | is.na(ICER)) %>% arrange(dCOST) %>% mutate(ICER = (dCOST-lag(dCOST))/(dQALY-lag(dQALY)))
      x$dominated[2:(nrow(x))] = 0
      str.dom <- rbind.fill(str.dom, y)
    }
  if(is.null(str.dom)==FALSE) {str.dom <- str.dom %>% mutate(ICER=NA, dominated=1)}
  
  #extended dominance (cross out weakly dominated strategies until ICERs always increase with costs)
  ext.dom <- NULL
  while(any(order(x$ICER[-1])!=1:(nrow(x)-1))) 
  {
    r <- nrow(x)
    x$ext.dominated <- NA
    for (i in 2:(r-1)) {
      x$ext.dominated[i] = as.integer(x$ICER[i] > x$ICER[i+1])   
    }
    y <- x %>% filter(ext.dominated==1)
    x <- x %>% filter(ext.dominated==0 | is.na(ext.dominated)) %>% arrange(dCOST) %>% mutate(ICER = (dCOST-lag(dCOST))/(dQALY-lag(dQALY)))
    ext.dom <- rbind.fill(ext.dom, y)
  }
  if(is.null(ext.dom)==FALSE) {ext.dom <- ext.dom %>% mutate(ICER=NA, ext.dominated=1) }

  out = plyr::rbind.fill(x, str.dom, ext.dom) %>% arrange(dCOST)
  out
}

cost_plane <- function(sum, title) {
  dt <- sum
  dt$line <- 1
  if (any(names(dt) %in% "dominated")==TRUE)
  {dt$line[dt$dominated==1] <- 0 }
  if (any(names(dt) %in% "ext.dominated")==TRUE)
  {dt$line[dt$ext.dominated==1] <- 0}
  if (any(names(dt) %in% "Y")==TRUE) 
  {dt <- dt %>% mutate(label=paste0(preemptive,"-",reactive,"-",X,"-",Y))} # With sensitivity analyses
  if (any(names(dt) %in% "Y")==FALSE) 
  {dt <- dt %>% mutate(label=paste0(preemptive,"-",reactive))}
  ggplot(dt, aes(x=dQALY,y=dCOST)) + geom_point() + 
    geom_line(data=dt[dt$line==1,]) + 
    geom_label_repel(aes(label=label)) +
    ggtitle(title)
}


