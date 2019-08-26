import io
import json
import csv
from os import listdir
from os.path import isfile, join

EXPERIMENT = "rps" # useful identifier for experiment data: modify this to reflect the particular experiment
DATA_PATH = "/Users/erikbrockbank/web/vullab/rps/data/" # path to data files: modify as needed for particular experiments
OUTPUT_FILE = "{}_raw.csv".format(EXPERIMENT) # name of csv file to write to
INDIVID_PATH = "individual_files/" # pathway writing individual game files

with io.open(OUTPUT_FILE, "w") as csv_output:
    csvwriter = csv.writer(csv_output)
    write_index = 0
    files = [f for f in listdir(DATA_PATH) if f.endswith(".json") and not "TEST" in f]
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

        # write results of this game to individual game csv (for use with RPS_Data visualizing)
        output_individ = f.split(".")[0] + ".csv"
        with io.open(join(INDIVID_PATH + output_individ), "w") as csv_output_individ:
            csvwriter_individ = csv.writer(csv_output_individ)
            header = [] # init header array
            header.extend(round_data[0].keys())
            csvwriter_individ.writerow(header)
            for r in round_data:
                vals = [] # init data array
                vals.extend(r.values())
                csvwriter_individ.writerow(vals)
        csv_output_individ.close()


csv_output.close()
