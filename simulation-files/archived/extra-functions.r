##################
# Extra Functions
##################
exec <- function(traj, func)
{
  traj %>%
    timeout(function(attrs) {func(attrs); 0})
}

print_attrs <- function(traj)
{
  exec(traj, function(attrs) print(attrs))
}


write_attrs <- function(traj,file=patients)
{
  exec(traj, function(attrs) patients = rbind.fill(file,as.data.frame(attrs)))
}

