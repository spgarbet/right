library(Hmisc)

env  <- simmer("RIGHT-v1.1")

exec.simulation <- function(inputs)
{
  set.seed(12345)
  env  <<- simmer("RIGHT-v1.1")
  traj <- simulation(env, inputs)
  env %>% create_counters(counters)

  env %>%
    add_generator("patient", traj, at(rep(0, inputs$vN)), mon=2) %>%
    run(365*inputs$vHorizon+1) %>% # Simulate just past horizon
    wrap()

  get_mon_arrivals(env, per_resource = T)
}

inputs$vN <- 100000

results <- NULL
for(preemptive in c("None", "Panel", "PREDICT", "Age >= 50"))
{
  for(reactive in c("None", "Single"))
  {
      if(preemptive == "Panel" && reactive == "Single") {next}
      cat("Running ", preemptive, "/", reactive, "\n")

    
      inputs$vPreemptive <- preemptive
      inputs$vReactive   <- reactive
      run <- exec.simulation(inputs)
      run$preemptive <- preemptive
      run$reactive   <- reactive
      
      if(is.null(results)) { results <- run } else  {results <- rbind(results, run)}
  }
}

write.csv(results, "simvastatin-raw.csv")


results <- read.csv("simvastatin-withsomecosts.csv")
library(data.table)
DT <- data.table(results)
DT[, .N, by = list(resource, preemptive, reactive)]
