####
## Cleanup 
cleanup_clopidogrel <- function(traj)
{
  traj %>% 
  branch(
    function(attrs) ifelse(attrs[['aDAPT.Rx']] %in% c(1:3),attrs[['aDAPT.Rx']],4),
    continue=rep(TRUE,4),
    create_trajectory() %>% release("clopidogrel") ,
    create_trajectory() %>% release("ticagrelor") ,
    create_trajectory() %>% release("prasugrel") ,
    create_trajectory() %>% timeout(0)
  ) 
}

cleanup_aspirin <- function(traj)
{
  traj %>% 
    branch(
      function(attrs) ifelse(attrs[["aAspirin"]]==1,1,2),
      continue = c(TRUE,TRUE),
      create_trajectory() %>% release("aspirin"),
      create_trajectory() %>% timeout(0)
    ) 
}
