import io
import json
import csv
from os import listdir
from os.path import isfile, join

"""
    To run this:
    cd to data_analysis/go_fish
    python json_to_csv.py
    Outputs: csv files listed below
"""

# TODO data worth adding at some point
# - Additional bool in eval task for whether something was a distractor rule
# - More data in memory task: what order was each item, when was it shown

EXPERIMENT = "go_fish" # useful identifier for experiment data
DATA_PATH = "/Users/erikbrockbank/web/go_fish/data/" # path to data files

OUTPUT_FILE_SUMMARY = "01_{}_meta.csv".format(EXPERIMENT)
OUTPUT_FILE_TRIALS = "02_{}_trials.csv".format(EXPERIMENT)
OUTPUT_FILE_GENERATION = "03_{}_generation_free_resp.csv".format(EXPERIMENT)
OUTPUT_FILE_JUDGMENT = "04_{}_generation_judgment.csv".format(EXPERIMENT)
OUTPUT_FILE_EVALUATION = "05_{}_evaluation.csv".format(EXPERIMENT)
OUTPUT_FILE_MEMORY = "06_{}_memory.csv".format(EXPERIMENT)

csv_output_summary = open(OUTPUT_FILE_SUMMARY, "w")
csvwriter_summary = csv.writer(csv_output_summary)

csv_output_trials = open(OUTPUT_FILE_TRIALS, "w")
csvwriter_trials = csv.writer(csv_output_trials)

csv_output_generation = open(OUTPUT_FILE_GENERATION, "w")
csvwriter_generation = csv.writer(csv_output_generation)

csv_output_judgment = open(OUTPUT_FILE_JUDGMENT, "w")
csvwriter_judgment = csv.writer(csv_output_judgment)

csv_output_evaluation = open(OUTPUT_FILE_EVALUATION, "w")
csvwriter_evaluation = csv.writer(csv_output_evaluation)

csv_output_memory = open(OUTPUT_FILE_MEMORY, "w")
csvwriter_memory = csv.writer(csv_output_memory)

write_index = 0
files = [f for f in listdir(DATA_PATH) if f.endswith(".json") and "user" in f]
for f in files:
    with io.open(join(DATA_PATH + f), "r", encoding = "utf-8", errors = "ignore") as readfile:
        print("Processing: {}".format(f))
        content = readfile.read()
        parsed_content = json.loads(content)
        subjID = parsed_content["client"]["sid"]
        is_control = parsed_content["trials"]["is_control"]
        # Fetch relevant data to write to csv
        trial_data = parsed_content["trials"]["trial_data"]
        generation_data_free_resp = parsed_content["trials"]["generation_data"]["input_free_response"]
        generation_data_judgments = parsed_content["trials"]["generation_data"]["judgment_task"]
        eval_data = parsed_content["trials"]["evaluation_data"]["eval_ratings"]
        memory_data = parsed_content["trials"]["memory_data"]["memory_responses"]

        if write_index == 0:
            # Write summary data
            header_summary = ["expt_version", "subjID", "is_test", "is_control",
                "instruction_start_ts", "instruction_end_ts",
                "expt_start_ts", "expt_end_ts",
                "generation_start_ts", "generation_end_ts",
                "judgment_start_ts", "judgment_end_ts",
                "evaluation_start_ts", "evaluation_end_ts",
                "memory_start_ts", "memory_end_ts"]
            csvwriter_summary.writerow(header_summary)

            header_trials = ["subjID", "is_control"] # init header array
            header_trials.extend(trial_data[0].keys())
            csvwriter_trials.writerow(header_trials)

            header_generation = ["subjID", "is_control", "free_response",
                                    "generation_start_ts", "generation_end_ts"]
            csvwriter_generation.writerow(header_generation)

            header_judgment = ["subjID", "is_control"] # init header array
            header_judgment.extend(generation_data_judgments[0].keys())
            csvwriter_judgment.writerow(header_judgment)

            header_eval = ["subjID", "is_control"] # init header array
            # print(eval_data)
            header_eval.extend(eval_data[0].keys())
            csvwriter_evaluation.writerow(header_eval)

            header_mem = ["subjID", "is_control"] # init header array
            header_mem.extend(memory_data[0].keys())
            csvwriter_memory.writerow(header_mem)
            write_index = 1

        # Write initial trial (evidence) data
        for s in trial_data:
            vals = [subjID, is_control] # init data array
            vals.extend(s.values())
            csvwriter_trials.writerow(vals)

        # Write generation task free response data
        generation_start_ts = parsed_content["trials"]["generation_data"]["generation_start_ts"]
        generation_end_ts = parsed_content["trials"]["generation_data"]["generation_end_ts"]
        csvwriter_generation.writerow([subjID, is_control, generation_data_free_resp,
                                        generation_start_ts, generation_end_ts])

        # Write generation task judgment data
        for s in generation_data_judgments:
            vals = [subjID, is_control] # init data array
            vals.extend(s.values())
            csvwriter_judgment.writerow(vals)

        # Write evaluation task data
        for s in eval_data:
            vals = [subjID, is_control] # init data array
            vals.extend(s.values())
            csvwriter_evaluation.writerow(vals)

        # Write evaluation task data
        for s in memory_data:
            vals = [subjID, is_control] # init data array
            vals.extend(s.values())
            csvwriter_memory.writerow(vals)

        # Write summary data
        vals = [parsed_content["expt"]["exptversion"],
                subjID,
                parsed_content["trials"]["is_test"],
                parsed_content["trials"]["is_control"],
                parsed_content["trials"]["instruction_data"]["instruction_start_ts"],
                parsed_content["trials"]["instruction_data"]["instruction_end_ts"],
                parsed_content["trials"]["expt_start_ts"],
                parsed_content["trials"]["expt_end_ts"],
                parsed_content["trials"]["generation_data"]["generation_start_ts"],
                parsed_content["trials"]["generation_data"]["generation_end_ts"],
                parsed_content["trials"]["generation_data"]["judgment_start_ts"],
                parsed_content["trials"]["generation_data"]["judgment_end_ts"],
                parsed_content["trials"]["evaluation_data"]["evaluation_start_ts"],
                parsed_content["trials"]["evaluation_data"]["evaluation_end_ts"],
                parsed_content["trials"]["memory_data"]["memory_start_ts"],
                parsed_content["trials"]["memory_data"]["memory_end_ts"]
            ]
        csvwriter_summary.writerow(vals)




csv_output_summary.close()
csv_output_trials.close()
csv_output_generation.close()
csv_output_judgment.close()
csv_output_evaluation.close()
csv_output_memory.close()
