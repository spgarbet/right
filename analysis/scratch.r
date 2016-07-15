
# 
# 
# icer = xx[order(xx$avgcost),]; 
# icer$icer =NA
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer[,c("scenario","avgcost","avgqaly","icer")]
# icer$dominated = icer$ext.dominated =  0
# icer$dominated = as.numeric(icer$icer<0);icer[,c("scenario","avgcost","avgqaly","icer","dominated")]
# icer.d = subset(icer,dominated==1)
# icer = subset(icer,dominated==0 | is.na(dominated)); icer
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer
# icer[which(diff(icer$icer)<0),]$ext.dominated = 1; icer
# icer.d = rbind.data.frame(icer.d,subset(icer,ext.dominated==1)); icer.d
# 
# icer = subset(icer,ext.dominated==0); icer
# icer[2:nrow(icer),]$icer = with(icer,diff(avgcost)/diff(avgqaly)); icer
# ICER = icer[,c("scenario","icer")]; ICER
# 
# 
# 
# 
# 
# Sc0 = Baseline_events  %>% count(resource)  
# Sc1 = PREDICT_Panel_events  %>% count(resource) 
# Delta = Sc0 %>% full_join(Sc1,"resource") %>% mutate(n.y = ifelse(is.na(n.y),0,n.y),n.x=ifelse(is.na(n.x),0,n.x)) %>% mutate(diff = n.y-n.x) %>% data.frame()
# 
# 
# dapt= c("clopidogrel","ticagrelor","prasugrel","dapt_switched")
# Delta %>% filter(resource %in% dapt)
# 
# # # Any Event
# # Delta %>% filter(grepl("_event",resource))
# # 
# # randpat(event="revasc_pci")
# # # Bleeding Events
# # Delta %>% filter(grepl("bleed_",resource))
# # 
# # # Stent Thrombosis
# # Delta %>% filter(grepl("^st_",resource))
# # 
# # # MI Events
# # Delta %>% filter(grepl("^mi_",resource))
# # 
# # # Revascularization Events
# # Delta %>% filter(grepl("^revasc_",resource))
# # 
# # 
# 
# # attributes <- arrange(get_mon_attributes(env),name,key,time) %>% mutate(name=paste0(name,"_",replication))
# # first.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(first = first(value)),key,first)
# # last.attributes <- spread(attributes %>% group_by(name,key) %>% summarize(last = last(value)),key,last)
# # all.attributes <- spread(attributes %>% group_by(name,key,time) %>% summarize(first = mean(value)),key,first)
# # 
# # 
# 