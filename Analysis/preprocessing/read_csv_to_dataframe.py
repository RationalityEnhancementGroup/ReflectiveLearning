import json
from datetime import datetime
import pandas as pd
import numpy as np
import math

# ---------HELPER-----------------------
# -------------------------------------
def reduceNodeString(string):
    while string.find(".") != -1:
        index = string.find(".")
        string = string[:index] + string[index + 3 :]
    return string


def getMapping(dataclip, subject, timeline=None):

    if timeline is None:
        timeline = eval(
            json.loads(dataclip.iloc[subject]["datastring"])["questiondata"][
                "global_timeline"
            ]
        )
        return getMapping(dataclip, subject, timeline)

    internal_node_ids = []
    descriptions = []
    keys = []

    for trial in json.loads(dataclip.iloc[subject]["datastring"])["data"]:

        node_id = trial["trialdata"]["internal_node_id"]
        key = reduceNodeString(node_id)

        t = timeline
        for c in key:
            try:
                t = t[int(c)]
            except:
                t = "undefined"

        internal_node_ids.append(node_id)
        descriptions.append(t)
        keys.append(key)

    trial_df = pd.DataFrame(
        list(zip(keys, descriptions, internal_node_ids)), columns=["Key", "Des", "Id"]
    )

    return trial_df


# defines initial dataframe
def makeDataframe(dataclip, getParticipantInfo):
    if dataclip.shape[0] == 0:
        return pd.DataFrame([], columns=[])

    df = pd.DataFrame([], columns=getParticipantInfo(dataclip, 0).keys())
    for i in range(len(dataclip)):
        df = df.append(getParticipantInfo(dataclip, i), ignore_index=True)
    return df


def json2dict(data):
    """
    Turn a json database file into a dicitonary
    """
    sub_all = data.get("values")
    keys = data.get("fields")
    dic_all = []
    for sub in sub_all:
        x = dict(zip(keys, sub))
        if not x["datastring"] is None:
            x["datastring"] = json.loads(x.get("datastring"))
        dic_all.append(x)
    return dic_all


# --------- CUSTOM FUNCTION -----------------------
# -------------------------------------
# takes data_dict as dataclip and a subject index: extracts all relevant data for that partcipant


def getParticipantInfo(dataclip, subject):

    data = {
        "WorkerId": "-",
        "hitId": "-",
        "assignmentId": "-",
        "status": "-",
        "dropoutIndex": "-",
        "reflection": "-",
        "condition": "-",
        "conditionType": "-",
        "age": "-",
        "gender": "-",
        "naive": "-",
        "effort": "-",
        "totalTime": "-",
        "ncs": [],
        "promptsContent": [],
        "promptsRt": [],
        "promptsLengths": [],
        "attemptsQuiz": 0,
        "ML_clicksNumber": [],
        "ML_cheatTrials": 0,
        "ML_scores": [],
        "feedback": "-",
        "bonus": "-",
        "exclusion": "",
        "mouselabTrials": [],
        "datastring": "-",
    }

    # main variables
    data["WorkerId"] = dataclip.iloc[subject]["workerid"]
    data["hitId"] = dataclip.iloc[subject]["hitid"]
    data["assignmentId"] = dataclip.iloc[subject]["assignmentid"]
    data["status"] = dataclip.iloc[subject]["status"]

    # compute total time
    begin_hit = dataclip.iloc[subject]["beginhit"]
    begin_exp = dataclip.iloc[subject]["beginexp"]
    end_hit = dataclip.iloc[subject]["endhit"]

    if end_hit is not None and isinstance(end_hit, str):
        time_format = "%Y-%m-%d %H:%M:%S.%f"
        total_time = datetime.strptime(end_hit, time_format) - datetime.strptime(
            begin_hit, time_format
        )
        data["totalTime"] = round(total_time.seconds / 60, 1)

    # extract experiment content
    datastring = json.loads(dataclip.iloc[subject]["datastring"])
    if datastring is None:
        print("Aborted Experiment: No trial data")
        data["dropoutIndex"] = 0
    else:

        # condition
        data["datastring"] = datastring
        data["condition"] = datastring["questiondata"]["condition"]
        data["conditionType"] = ["passive_control", "active_control", "reflection"][
            data["condition"]
        ]
        data["reflection"] = data["conditionType"] == "reflection"

        # extract exclusion
        try:
            data["exclusion"] = datastring["questiondata"]["exclusion"]
        except:
            data["exclusion"] = "-"

        # extract bonus
        try:
            data["bonus"] = datastring["questiondata"]["final_bonus"]
        except:
            data["bonus"] = 0

        # set trial codes
        if data["condition"] == 1:
            reflection_codes = [
                "0213",
                "0214",
                "0215",
                "0223",
                "0224",
                "0225",
                "0233",
                "0234",
                "0235",
                "0243",
                "0244",
                "0245",
                "0253",
                "0254",
                "0255",
                "0263",
                "0264",
                "0265",
            ]
            last_reflection_codes = ["0215", "0225", "0235", "0245", "0255", "0265"]
            ML_codes = [
                "02100",
                "02101",
                "02102",
                "02200",
                "02201",
                "02202",
                "02300",
                "02301",
                "02302",
                "02400",
                "02401",
                "02402",
                "02500",
                "02501",
                "02502",
                "02600",
                "02601",
                "02602",
                "0270",
                "0271",
                "0272",
            ]

        elif data["condition"] == 2:
            reflection_codes = [
                "0213",
                "0214",
                "0215",
                "0216",
                "0217",
                "0218",
                "0219",
                "02110",
                "02111",
                "0223",
                "0224",
                "0225",
                "0226",
                "0227",
                "0228",
                "0229",
                "02210",
                "02211",
                "0233",
                "0234",
                "0235",
                "0236",
                "0237",
                "0238",
                "0239",
                "02310",
                "02311",
                "0243",
                "0244",
                "0245",
                "0246",
                "0247",
                "0248",
                "0249",
                "02410",
                "02411",
                "0253",
                "0254",
                "0255",
                "0256",
                "0257",
                "0258",
                "0259",
                "02510",
                "02511",
                "0263",
                "0264",
                "0265",
                "0266",
                "0267",
                "0268",
                "0269",
                "02610",
                "02611",
            ]
            last_reflection_codes = [
                "02111",
                "02211",
                "02311",
                "02411",
                "02511",
                "02611",
            ]
            ML_codes = [
                "02100",
                "02101",
                "02102",
                "02200",
                "02201",
                "02202",
                "02300",
                "02301",
                "02302",
                "02400",
                "02401",
                "02402",
                "02500",
                "02501",
                "02502",
                "02600",
                "02601",
                "02602",
                "0270",
                "0271",
                "0272",
            ]
        else:
            reflection_codes = []
            last_reflection_codes = []
            ML_codes = [
                "0210",
                "0211",
                "0212",
                "0220",
                "0221",
                "0222",
                "0230",
                "0231",
                "0232",
                "0240",
                "0241",
                "0242",
                "0250",
                "0251",
                "0252",
                "0260",
                "0261",
                "0262",
                "0270",
                "0271",
                "0272",
            ]

        # helper variables
        prompts_lengths_temp = []
        prompts_rt_temp = []

        # loop through all trials and categorize  --------------------------------
        for trial in datastring["data"]:

            trial_key = reduceNodeString(trial["trialdata"]["internal_node_id"])

            # NCS
            if trial_key in ["001"]:
                data["dropoutIndex"] = 1
                resp = eval(trial["trialdata"]["responses"])
                for r in resp:
                    data["ncs"].append(int(r[0]))

            # QUIZ
            elif trial_key in ["011"]:
                data["attemptsQuiz"] += 1

            # PROMPTS
            elif trial_key in reflection_codes:
                try:
                    resp = eval(trial["trialdata"]["responses"])
                    data["promptsContent"].append(resp["Q0"])

                    prompts_lengths_temp.append(len(resp["Q0"]))
                    prompts_rt_temp.append(trial["trialdata"]["rt"])
                except:
                    pass

                if trial_key in last_reflection_codes:
                    data["promptsContent"].append("---------------------------------")
                    data["promptsLengths"].append(sum(prompts_lengths_temp))
                    data["promptsRt"].append(round(sum(prompts_rt_temp) / 1000, 2))

                    # reset
                    prompts_lengths_temp = []
                    prompts_rt_temp = []

            # MOUSELAB TEST TRIALS
            elif trial_key in ML_codes:
                data["dropoutIndex"] = 2
                data["mouselabTrials"].append(trial["trialdata"])

                # clicks
                num_clicks = len(
                    trial["trialdata"]["queries"]["click"]["state"]["target"]
                )
                data["ML_clicksNumber"].append(num_clicks)

                if num_clicks < 1:
                    data["ML_cheatTrials"] += 1

                # scores
                data["ML_scores"].append(trial["trialdata"]["score"])

            # FINAL SURVEY
            elif trial_key in ["04"]:
                data["dropoutIndex"] = 3
                resp = eval(trial["trialdata"]["responses"])
                data["naive"] = resp["Q0"] == "No"
                data["age"] = resp["Q1"]
                data["gender"] = resp["Q2"]

                if data["condition"] == 2:
                    data["effort"] = resp["Q3"]

            elif trial_key in ["05"]:
                resp = eval(trial["trialdata"]["responses"])
                data["feedback"] = resp["Q0"]

    if not len(data["mouselabTrials"]) == 21 and not data["status"] == 6:
        print("")
        print(subject)
        print("mouselab parts missing")

    return data
