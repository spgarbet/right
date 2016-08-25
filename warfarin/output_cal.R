warfarin_out <- function(env,inputs) 
{
  attributes <- arrange(get_mon_attributes(env),name,key,time) %>%  mutate(name = paste0(name,"_",replication))
  last.attributes  <- spread(attributes %>% group_by(name,key) %>% summarize(last = last(value)),key,last)
  dt_INR <- last.attributes %>% select(initial = aINRInitial, end = aINR, aWarfarinIndication) %>% filter(initial>0)
  
  ind_tab <- table(dt_INR$aWarfarinIndication)
  
  INR_cat <- data.frame(cat=c("I_below2","I_2to3","I_3to4","I_over4",
                             "E_below2","E_2to3","E_3to4","E_over4"),
                       val=c(sum(dt_INR$initial<2)/nrow(dt_INR),
                             sum(dt_INR$initial>=2 & dt_INR$initial<=3)/nrow(dt_INR),
                             sum(dt_INR$initial>3 & dt_INR$initial<=4)/nrow(dt_INR),
                             sum(dt_INR$initial>4)/nrow(dt_INR),
                             sum(dt_INR$end<2)/nrow(dt_INR),
                             sum(dt_INR$end>=2 & dt_INR$end<=3)/nrow(dt_INR),
                             sum(dt_INR$end>3 & dt_INR$end<=4)/nrow(dt_INR),
                             sum(dt_INR$end>4)/nrow(dt_INR)))
  
  events <- events
  out.start <- events %>% filter(resource=="Initial_OutRange") %>% select(name) #those initial out of range
  in.end <- events %>% filter(resource=="in_range") %>% select(name) #those in range initial/later
  out.list1 <- merge(in.end, out.start, by="name") 
  out.end <- events %>% filter(resource=="out_of_range") %>% select(name,activity_time) #time out of range
  out.list <- merge(out.list1,out.end, by="name") %>% arrange(activity_time)
  sum_t2e <- summary(out.list$activity_time, digits=5)  
  
  out=list(indication=ind_tab,INR_cat=INR_cat,t2inrange=sum_t2e)
  return(out)
}

