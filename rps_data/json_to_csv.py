import io
import json
import csv
from os import listdir
from os.path import isfile, join

EXPERIMENT = "rps" # useful identifier for experiment data: modify this to reflect the particular experiment
DATA_PATH = "/Users/erikbrockbank/web/vullab/rps/data/" # path to data files: modify as needed for particular experiments
OUTPUT_FILE = "{}_raw.csv".format(EXPERIMENT) # name of csv file to write to


csv_output = open(OUTPUT_FILE, "w")
csvwriter = csv.writer(csv_output)

write_index = 0
files = [f for f in listdir(DATA_PATH) if f.endswith(".json")]
for f in files:
    with io.open(join(DATA_PATH + f), "r", encoding = "utf-8", errors = "ignore") as readfile:
        print("Processing: {}".format(f))
        content = readfile.read()
        parsed_data = json.loads(content)
        round_data = parsed_data["rounds"]

        if write_index == 0:
            header = [] # init header array
            header.extend(round_data[0].keys())
            csvwriter.writerow(header)
            write_index = 1

        for r in round_data:
            vals = [] # init data array
            vals.extend(r.values())
            csvwriter.writerow(vals)

csv_output.close()
