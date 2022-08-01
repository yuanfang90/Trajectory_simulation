# Trajectory_simulation
This is the GitHub repository for the simulation work discussed in the manuscript “Evaluation of latent-class mixed-effect models for trajectory clustering in complex data sets through simulation studies”.

A function for simulating datasets with variability options (follow-up length, within-subject variation, between-total ratio) discussed in the manuscript and others (stochastic residual error for within-subject, group specific variance-covarianc matrix structure) as arguments is presented in the file _functions.R_.

## commonvarcov
This folder contains the code for generating the simulated datasets, the datasets, best fitted LCMM models, and the code for generating summary tables and plots in our manuscript.

- There are 24 R code file for creating the simulation datasets corresponding to the 24 variability scenarios discussed in our manuscript. Name of each R code file shows which case this code is corresponding to. E.g., "ul_msmr_0595.R" simulate datasets from the long followup, moderate sigma (within-subject standard error/residual standard error) moderate ratio (between-total variance ratio) and 5:95 unbalanced group size scenario. Fitting LCMMs for G=1,2,3 are also included in this code.
- There is one shell script "ul_msmr_0595.qsub" showing an example of how we submit each of the 24 R codes to the computer cluster to create 100 replicate datasets varying the sampling seeds.
- Each of the scenario folders contains 100 .RData file saving the dataset generated, and 100 .csv file saving the estimated parameters, estimated between-total ratio and within-subject variation by both LMM and the best selected LCMM model, and cluster validation measures calculated based on the best model selected by ICL. Notice that if ICL did not select the correct number of components (i.e. select G != 2), only cluster validation measures and estimation of between-total ratio and within-subject variation were saved, parameter estimations were setted to be NA.
