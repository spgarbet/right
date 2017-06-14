numerical-status
========================================================
author: Shawn Garbett
date: May 23rd, 2017
autosize: true

Aim 3
========================================================
Aim 3 of our grant includes a section on probablistic 
sensitivity analysis (PSA) to determine impact of various
factors upon genomic testing strategies. 

- Discrete event simulation is too slow to solve PSA
- Faster Numerical Methods are required
- Clopidogrel model has been attempted, and all major pieces have fast numerical solutions

Problems with Clopidogrel Numerical Model
========================================================

- Drowning in a sea of parameters (40+).
- Model is very complex 
- Model is rigid (Could not be translated easily to IGNITE!)
- Requires another container model to compare strategies.
- Only one parameter is under human control (Cost)
- Need 2 more of these models

What we control (Parameters of Interest)
========================================================

- Cost of testing and drugs
- Likilihood of physician prescribing genomic testing
- Likilihood of physician using genomic testing
- Sensitivity / Specificity of predictive testing
- Prevalence of problematic genes.

Proposal
========================================================

- Parameterize single drug models on cost of drug
- Run each single drug model on a sub population
- Construct high level model on parameters of interest
- Run PSA on that high level model

Benefits
========================================================

- Model is no longer restricted to specific drugs
- Explores the idea of genomic testing at a high level
- Would work well on the web (far fewer parameters).
- Of far broader interest than very ridgid models.

