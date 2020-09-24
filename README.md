# Virtual Twins

This code was created as a supplement to the manuscript of Practical Guidance on Modeling Choices for the Virtual Twins Method.

## File Directory

1) **Func.R**  -  This file contains all the helper functions I use in Cond.R. 
      
2) **Cond.R**  -  This is the main simulation file. The values of "dg", "vt1" and "sims" need to be specified prior to running the simulations: 
    - Options for "dg", the data generation scheme: 
      - **regDG** - The linear, regular case where the covariates are linearly related to the outcome. 
      - **corDG** - The linear, correlated case where some of the covariates are correlated to each other, but only one is linearly related to the outcome.
      - **sbDG** - The linear, selection bias case where the covariate distribution of the training sample and testing samples are different. 
      - **treeregDG** - The nonlinear, regular case where the covariates are nonlinearly related to the outcome. 
      - **treecorDG** - The nonlinear, correlated case where some of the covariates are correlated to each other, but only one is nonlinearly related to the outcome.
      - **treesbDG** - The nonlinear, selection bias case where the covariate distribution of the training sample and testing samples are different. 
    - Options for "vt1", the method for step 1 of VT:
      - **i.las** - LASSO is used for step 1. 
      - **i.rf** - Random forest is used for step 1. 
      - **i.mars** - MARS is used for step 1. 
      - **i.super** - Superlearner is used for step 1. 
      - **i.svm** - SVM is used for step 1. 
    - "sims" specifies the number of total iterations of the simulation. 

## Notes
- The simulation code automatically includes the methods of "None", "Linear model", "Regression tree", and "Conditional inference tree" for step 2 of VT as described in the manuscript. 
- The simulation outputs the results for a sample size of N=1000, N=200, and N=80. This can be edited in the Cond.R file.
