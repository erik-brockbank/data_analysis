"""
To run this:
- cd /Users/erikbrockbank/web/vullab/data_analysis/rps_data
- python v2_analysis.py
"""

import io
import json
from os import listdir
from os.path import join
from operator import itemgetter

EXPERIMENT = "rps_v2" # useful identifier for experiment data: modify this to reflect the particular experiment
DATA_PATH = "/Users/erikbrockbank/web/vullab/rps/data/v2/" # path to data files: modify as needed for particular experiments


files = [f for f in listdir(DATA_PATH) if f.endswith(".json")
            and not "TEST" in f
            and not "freeResp" in f
            and not "sliderData" in f]

print("Processing: {} files".format(len(files)))

file_tracker = {}

for f in files:
    with io.open(join(DATA_PATH + f), "r", encoding = "utf-8", errors = "ignore") as readfile:
        #print("Processing: {}".format(f))
        content = readfile.read()
        parsed_data = json.loads(content)
        round_data = parsed_data["rounds"]

        credit_token = parsed_data["survey_code"]
        if credit_token not in file_tracker:
            file_tracker[credit_token] = [len(round_data)]
        else:
            file_tracker[credit_token].append(len(round_data))


print(len(file_tracker))
print(file_tracker)
#print(sorted(file_tracker.items(), key=itemgetter(1)))
