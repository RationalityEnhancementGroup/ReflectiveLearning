# Data Analysis 

This folder contains all scripts to reproduce the statistical analysis. 

## Structure

```
+-- data:
|   +-- strategies: contains the strategies described in the computational microscope
|   +-- experiment: contains the datasets of the experiment
|
+-- preprocessing: 
|   +-- 01: converts the raw records of the jsPsych experiment into a Dataframe, applies the computational microscope to infer the participants planning strategies, anonymises the data and stores it in data/experiment/dataframe.csv
|   +-- 02: a script to rate the answers to the reflection prompts, the result is stored in data/experiment/dataframe_prompts.csv
|   +-- 03: a script that groups the strategies wrt. their adaptiveness, the result is stored in data/strategies/strategies_clustered.csv
|
+-- results:
|   +-- models: contains the fitted models in the format .RData
|   +-- tex: contains the model coefficients with corrected p-values as pdf files
|
+-- analysis:
|   +-- Each of the scripts refer to a specific analysis described in the article, referenced by the name. The scripts can be run independent of each other. Please, see the file headings for more information.
|
+-- plots:
|   +-- The plots used in the article stored as pdf files.
```

### How to Run Analysis
* Analysis:
  * Open the project in RStudio
  * Change the working directory to the `analysis` folder (`Session > Set Working Directory`) 
  * Install packages `rcompanion`, `psych`, `lme4`, `lmerTest`, `interactions`, `filesstrings`, `cowplot`, `dplyr`, `igraph`, `tinytex`
  * Install tinytex by running command `tinytex::install_tinytex()` in the RStudio terminal
  * Run the analysis scripts in the following order:
    * `02_controlcondition_comparison.R`
    * `03_trialdata_1_baselinevalue.R`
      * To run analysis for the "learning phase" (up to trial 7), set variable `performance_phase` in the first few lines to `FALSE`, and to `TRUE` for the "performance phase" (trials 7 to 21)
    * `03_trialdata_2_adapativness.R`
      * Run analysis for both learning and performance phases, same as above.
    * `03_trialdata_3_type.R`
      * Run analysis for both learning and performance phases, same as above.
    * `04_transitiondata_1_previousvalue.R`
    * `04_transitiondata_2_previousscore.R`
    * `04_transitiondata_3_adapativeness.R`
    * `04_transitiondata_4_type.R`
    * `05_transitiondata_temporal.R`
    * `06_transitiondata_engagement.R`
    * `07_plots.R`
    
The regression results are compiled as pdf files in the results/tex folder. For a detailed mapping of the results reported in the article and where they can be found in the analysis scripts please view results/resultsMapping.md 
    
