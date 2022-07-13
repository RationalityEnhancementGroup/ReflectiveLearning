
### How to Run Preprocessing

* Request `dataclip.csv` from authors and place in folder `preprocessing`
* Request package `mcl_toolbox_root` from authors and also place in folder `preprocessing`
* Create a python virtual environment and activate it:
  * python -m venv env
  * source env/bin/activate
* Install all necessary packages
  * `pip install numpy pandas seaborn mpmath hyperopt sklearn toolz joblib gym statsmodels`
* Pre-processing:
  * Backup the file `data/experiment/dataframe.csv` somewhere
  * Navigate to folder `preprocessing` in terminal
  * Run 1st preprocessing script: `python 01_convert_raw_data_to_dataframe.py`
    * Inferring of click strategies for all participants would take several hours and likely require a computing cluster! Just for testing purposes, you can run this script on only a subset of participants by adding the line `dataframe = dataframe.iloc[10:15] # selecting rows 10 to 15` before the call to function `bs.compute_strategies` in the script `01_convert_raw_data_to_dataframe.py`.
    * Compare the strategies of the participants in the new dataframe `data/experiment/dataframe.csv` (column `strategies`) with those of the same participants (with same `pid`) in the backed up dataframe
    * Once you are satisfied that the strategies are computed correctly, replace the file `data/experiment/dataframe.csv` with the original backed up file and continue
  * Classifying engagement of responses:
    * `data/experiment/dataframe_prompts.csv` already contains the assessment of the meaningfulness of all of the participants' responses, as evaluated by human judges
    * If you want to do this yourself, run the script `02_engagement_classification.R` in RStudio, where you will be shown the list of responses of each participant, and for each list of responses, you would enter either "1" if the set of responses is meaningful, "2" if it is not. Press enter to receive the next set of responses and repeat until all the responses have been shown.
  * Run 3rd preprocessing script:
    * Open the project in RStudio
    * Install the packages `tibble` and `plyr` 
    * Change the current working directory to the `preprocessing` directory.
    * Run the script `03_determine_adaptiveness_cluster.R`