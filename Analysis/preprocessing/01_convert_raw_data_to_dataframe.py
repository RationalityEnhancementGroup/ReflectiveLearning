import read_csv_to_dataframe as ecf
import pandas as pd
import json
import mcl_toolbox_root.bridge_script as bs

"""
Pre-processing I: This script takes the raw csv with the jspsych recordings, extracts all relevant informations, parses them into a dataframe, computes the participants planning strategies using the computational microscope and then stores the file as 'dataframe.csv'.

This script will throw an error because dataclip.csv is not included in this repository as it contains non-anonymized data.
In case you would like to run this scipt, please contact the authors.
"""

# import data as csv
dataclip = pd.read_csv("../data/experiment/dataclip.csv")

# create dataframe with participant infos
dataframe = ecf.makeDataframe(dataclip, ecf.getParticipantInfo)
dataframe.insert(0, "pid", dataframe.index)

# compute the participants strategies using the computational microscope
dataframe = bs.compute_strategies(dataframe, "strategies", 21, testing=False)

# aonymize and store
data = dataframe.drop(
    columns=["WorkerId", "hitId", "assignmentId", "bonus", "datastring"]
)
data.apply(
    lambda x: (json.dumps(a) for a in x) if x.name in ["mouselabTrials"] else x
).to_csv("../data/experiment/dataframe.csv", index=False)
