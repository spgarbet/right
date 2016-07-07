####
## Cleanup 
cleanup_clopidogrel <- function(traj)
{
  traj %>% 
    branch(
        function(attrs) ifelse(attrs[["aAspirin"]]==1,1,2),
        continue = c(TRUE,TRUE),
        create_trajectory() %>% release("aspirin"),
        create_trajectory() %>% timeout(0)
      )
}
