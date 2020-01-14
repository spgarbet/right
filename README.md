RIGHT - Rational Integration of Genomic Healthcare Testing
===================================

Simulation code used for publications on the [RIGHT](https://rightsim.org) project.

1/14/2020: Updated for current version of simmer


Creating Events Checklist
-------------------------
1. Add event to event registry in main simulation file.
2. Create a time to event in the events file.
3. Create an event function in the events file
4. Add the initial relative risk (=1) in the patient attributes file.  
5. If event is downstream of another event (e.g., drug initiation), add the new event to the main function for the upstream event. 
