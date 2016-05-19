library(simmer)

  ##############################################
 ##
## Helper functions for managing counters
##
## Hopefully, no modification required.
##

# Create the counters, takes a list
create_counters <- function(env, counters)
{
  sapply(counters, FUN=function(counter)
  {
    env <- add_resource(env, counter, Inf, 0)
  })
  
  env
}

# Mark a counter
mark <- function(traj, counter)
{
  traj               %>%
    seize(counter,1)   %>%
    timeout(0)         %>%
    release(counter,1)
}

  ##############################################
 ##
## Helper functions for managing events
##
## Hopefully, no modification required.
##
assign_events <- function(traj, inputs)
{
  sapply(event_registry, FUN=function(event)
  {
    traj <- set_attribute(traj, event$attr, function(attrs)
    {
      event$time_to_event(attrs)
    })
  })
  traj
}

# Find the next event based on time
next_event <- function(attrs)
{
  event_time <- Inf
  event      <- NA
  id         <- 0
  for(i in 1:length(event_registry))
  {
    e <- event_registry[[i]]
    tmp_time   <- attrs[[e$attr]]
    if(tmp_time < event_time)
    {
      event      <- e
      event_time <- tmp_time
      id         <- i
    }
  }
  
  return(list(event=event, event_time=event_time, id=id))
}

# Process events in main loop
process_events <- function(traj, env)
{
  # Find the next event from possible events, and timeout (wait) till that moment
  traj <- timeout(traj, function(attrs)
  {
    # Determine next up
    ne <- next_event(attrs)
    event <- ne[['event']]
    event_time <- ne[['event_time']]
    
    #cat(" Next up => ",event$name,"\n")
    #cat("            waiting", event_time-now(env), '\n')
    
    # Wait the clock time for the nearest event, minus now()
    event_time - now(env)
  })
  
  # Age them by clock
  traj <- set_attribute(traj,'aAge',function(attrs) attrs[['aAgeInitial']]+(now(env)/365.0))

  # Create a handler for every possible event, using their
  # list position as the branch number
  # This will determine the id of the next event
  # Call it's modification function
  # and then update it's next time to event
  args <- lapply(event_registry,FUN=function(e) {
    create_trajectory(e$name) %>%
      e$func() %>%
      set_attribute(e$attr, function(attrs) {now(env)+e$time_to_event(attrs)})
  })
  args$traj   <- traj
  args$option <- function(attrs) next_event(attrs)$id
  args$merge  <- rep(TRUE,length(event_registry))
  
  do.call(branch, args)
}

  ##############################################
 ##
## MAIN LOOP
##
## This should not require modification
## This creates a patient simulation (trajectory)
## 
## It uses a branch in a manner to prevent the
## rollback from looking further up the stack
## of the event loop. 
##
simulation <- function(env, inputs)
{
  create_trajectory("Patient")     %>%
    assign_attributes(inputs)        %>%
    assign_events(inputs)            %>%
    assign_additional_attributes(inputs) %>%
    branch( # Used branch, to prevent rollback from looking inside event loop function
      function() 1,
      merge=TRUE,
      create_trajectory("main_loop") %>% process_events(env)
    ) %>% 
    rollback(amount=1, times=100) # Process up to 100 events per person
}