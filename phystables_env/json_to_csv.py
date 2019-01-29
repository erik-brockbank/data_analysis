import io
import json
import csv
from os import listdir
from os.path import isfile, join

EXPERIMENT = "phystables_env" # useful identifier for experiment data: modify this to reflect the particular experiment
DATA_PATH = "/Users/erikbrockbank/web/vullab/phystables_env/data_backup/" # path to data files: modify as needed for particular experiments
OUTPUT_FILE = "{}_raw.csv".format(EXPERIMENT) # name of csv file to write to


csv_output = open(OUTPUT_FILE, "w")
csvwriter = csv.writer(csv_output)

write_index = 0
files = [f for f in listdir(DATA_PATH) if f.endswith(".json") and "user" in f]
for f in files:
    with io.open(join(DATA_PATH + f), "r", encoding = "utf-8", errors = "ignore") as readfile:
        print("Processing: {}".format(f))
        content = readfile.read()
        parsed = json.loads(content)
        subjID = parsed["client"]["sid"]
        subjData = parsed["trials"]

        if write_index == 0:
            header = ["subjID"] # init header array
            header.extend(subjData[0].keys())
            csvwriter.writerow(header)
            write_index = 1

        for s in subjData:
            vals = [subjID] # init data array
            vals.extend(s.values())
            csvwriter.writerow(vals)

csv_output.close()
