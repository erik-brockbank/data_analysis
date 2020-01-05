"""
To run this:
- cd /Users/erikbrockbank/web/vullab/data_analysis/rps_data
- python json_to_csv.py
"""

import io
import json
import csv
from os import listdir
from os.path import isfile, join

EXPERIMENT = "rps" # useful identifier for experiment data: modify this to reflect the particular experiment
DATA_PATH = "/Users/erikbrockbank/web/vullab/rps/data/pilot/" # path to *pilot* data files: modify as needed for particular experiments
OUTPUT_FILE = "{}_pilot.csv".format(EXPERIMENT) # name of csv file to write pilot data to
INDIVID_PATH = "individual_files/pilot/" # pathway for writing individual pilot game files

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
                # init header array
                header = ["game_id", "round_index", "player_id", "round_begin_ts",
                    "player_move", "player_rt", "player_outcome",
                    "player_points", "player_total"]
                csvwriter.writerow(header)
                write_index = 1

            for r in round_data:
                p1_vals = [r["game_id"], r["round_index"], r["player1_id"], r["round_begin_ts"],
                    r["player1_move"], r["player1_rt"], r["player1_outcome"],
                    r["player1_points"], r["player1_total"]]
                p2_vals = [r["game_id"], r["round_index"], r["player2_id"], r["round_begin_ts"],
                    r["player2_move"], r["player2_rt"], r["player2_outcome"],
                    r["player2_points"], r["player2_total"]]
                csvwriter.writerow(p1_vals)
                csvwriter.writerow(p2_vals)

        # write results of this game to individual game csv (for use with RPS_Data visualization tool)
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
