### Comparing Paper Statistics to Analysis Results:

Here is information on where the statistics that are reported in the paper can be found in the results of the analysis scripts.

* **Section 2.1 and 2.3**
  * Open script `analysis/01_descriptive_stats.R` in RStudio
  * Change the working directory to the `analysis` folder (`Session > Set Working Directory`)
  * Run individual lines under the sections 'Demographics'
* **Section 2.2.3**
  * In script `analysis/01_descriptive_stats.R`
  * Run individual lines pertaining to 'RT' under the section 'Engagement Scores'
* **Section 2.4.2:**
  * _Pooling of control conditions_
    * Open script `analysis/02_controlcomparison.R` in RStudio
      * Run individual lines under the sections "2 Score" and "3 Strategy Score"
        * Take note of console output
      * F-values and Expected Score means + SD
    * In script `analysis/01_descriptive_stats.R`
      * Run individual lines under the sections 'Demographics' and 'Trial data: Comparison of control conditions'
        * Take note of console output
      * Average times
* **Section 3.1**
  * In script `analysis/01_descriptive_stats.R`
    * Run individual lines under the sections 'Trial data: ...'
      * Take note of console output
* **Table 4:**
  * _Learning Phase (Trials 3-7):_
    * Expected Score: results/tex/03_trialdata_baselinevalue/learning_phase/7_strategyscore_corrected.pdf
    * Score: results/tex/03_trialdata_baselinevalue/learning_phase/7_score_corrected.pdf
    * Clicks: results/tex/03_trialdata_baselinevalue/learning_phase/7_clicks.pdf
  * _Performance Phase (Trials 7-21):_
    * Expected Score: results/tex/03_trialdata_baselinevalue/performance_phase/21_strategyscore_corrected.pdf
    * Score: results/tex/03_trialdata_baselinevalue/performance_phase/21_score_corrected.pdf
    * Clicks: results/tex/03_trialdata_baselinevalue/performance_phase/21_clicks.pdf
* **Section 3.2.1:**
  * _Reflection improves how people plan_
    * Run script `00_format_data.R`
      * Open variable `types` in workspace
      * % of trials in which near-/far-sighted strategies were used
    * results/tex/03_trialdata_baselinevalue/learning_phase/7_typens_corrected.pdf
      * Significance of effect of reflection on use of no-planning strategies (learning phase)
    * results/tex/03_trialdata_baselinevalue/performance_phase/21_typens_corrected.pdf
      * Significance of effect of reflection and use of no-planning strategies in baseline trials on performance (performance phase)
    * results/tex/03_trialdata_baselinevalue/performance_phase/21_typefs_corrected.pdf
      * Significance of effect of reflection on use of far-sighted strategies (performance phase)
    * results/tex/03_trialdata_baselinevalue/performance_phase/followup/21_typefs_corrected.pdf
      * Follow-up analysis
  * _Reflection fosters strategy exploration_
    * In script `analysis/01_descriptive_stats.R`
    * Run individual lines under the sections 'Transition data: Frequency of planning change' and Transition data: Average improvement per trial'
      * Take note of console output 
  * _Reflection improves how much people plan_
    * In script `analysis/01_descriptive_stats.R` in RStudio
    * Run line describing 'amount of planning' under the section 'Trial data: Overall performance per reflection'
    * Run line describing `number_clicks` under the section 'Trial data: Learning Rates'
    * Run line aggregating `clicks_change` under the section 'Transition data: Frequency of planning change'
    * Take note of console output for all of the above
* **Section 3.2.2:**
  * In script `analysis/01_descriptive_stats.R` in RStudio
  * Run lines under the section 'Transition data: Reflection prompts vs. Normal transition'
    * Take note of console output
* **Table 5:**
  * _Change of:_
    * Strategy: results/tex/04_transitiondata_previousvalue/21_strategy-change_corrected.pdf
    * Type: results/tex/04_transitiondata_previousvalue/21_type-change_corrected.pdf
    * Clicks: results/tex/04_transitiondata_previousvalue/21_clicks-change_corrected.pdf
  * _Magnitude of change of:_
    * Expected score: results/tex/04_transitiondata_previousvalue/21_strategy-magnitude_corrected.pdf
    * Number of clicks: results/tex/04_transitiondata_previousvalue/21_clicks-magnitude_corrected.pdf
* **Section 3.3.1:**
  * _Reflection is especially helpful for near-sighted planners_
    * results/tex/03_trialdata_type/performance_phase/21_score_corrected.pdf
    * results/tex/03_trialdata_type/performance_phase/21_strategyscore_corrected.pdf
    * results/tex/03_trialdata_type/performance_phase/21_clicks.pdf
* **Section 3.3.2:**
  * results/tex/04_transitiondata_previousscore/21_type-change_corrected.pdf
  * results/tex/04_transitiondata_previousscore/21_strategy-magnitude_corrected.pdf
  * results/tex/04_transitiondata_type/21_strategy-magnitude_corrected.pdf
* **Table 6:**
  * _Learning Phase (Trials 3-7):_
    * Expected Score: results/tex/03_trialdata_baselinevalue/learning_phase/7_strategyscore_corrected.pdf
    * Score: results/tex/03_trialdata_baselinevalue/learning_phase/7_score_corrected.pdf
    * Clicks: results/tex/03_trialdata_baselinevalue/learning_phase/7_clicks.pdf
  * _Performance Phase (Trials 8-21):_
    * Expected Score: results/tex/03_trialdata_baselinevalue/performance_phase/21_strategyscore_corrected.pdf
    * Score: results/tex/03_trialdata_baselinevalue/performance_phase/21_score_corrected.pdf
    * Clicks: results/tex/03_trialdata_baselinevalue/performance_phase/21_clicks.pdf
* **Section 3.4:**
  * _The frequency of change appears to decrease over time_
    * In script `analysis/01_descriptive_stats.R`
      * Run individual lines under the section 'Temporal Dynamics: Comparison of reflection periods' > 'cumulative frequency of change'
        * Take note of console output
      * % of participants changing their strategy
    * results/tex/05_transitiondata_temporal/strategy_change.pdf
      * Significance of decrease in frequency of change
    * In script `analysis/05_transitiondata_temporal.R`
      * Run section '3 Amount of change over time'
      * Take note of results of Chi-squared tests in console output (second result for each of the 3 reflection periods)
  * _The amount of improvement decreases over time_
    * In script `analysis/01_descriptive_stats.R`
      * Run individual lines under the section 'Temporal Dynamics: Comparison of reflection periods' > 'cumulative magnitude of change'
        * Take note of console output
      * Points/trial improvement by group
    * results/tex/05_transitiondata_temporal/strategyscore_delta.pdf
      * Significance of decrease in improvement of expected score
    * In script `analysis/05_transitiondata_temporal.R`
      * Run section '2 Amount of improvement over time'
      * Take note of Wilcoxon test results in console output (second result for each of the 3 reflection periods)
  * _Engagement does not decrease over time:_
    * In script `analysis/01_descriptive_stats.R`
    * Run individual lines under the section 'Engagement Scores:'
      * Take note of console output
* **Section 3.5:**
  * In script `analysis/01_descriptive_stats.R`
    * Run individual lines under the section 'NFC Correlations'
      * Take note of console output
    * Correlation coefficients and descriptive statistics
  * results/tex/03_trialdata_baselinevalue/performance_phase/21_strategyscore_corrected.pdf
    * Significance of effect of NFC on expected score in performance phase
  * results/tex/03_trialdata_baselinevalue/learning_phase/7_strategyscore_corrected.pdf
    * Significance of effect of NFC x Trial No. on expected score in learning phase
  * results/tex/06_transitiondata_engagement
    * Significance of main effect of High Engagement (HE) on all outcome variables
