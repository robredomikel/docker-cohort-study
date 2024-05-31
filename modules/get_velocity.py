import pandas as pd
from commons import DATA_PATH
from dateutil.relativedelta import relativedelta
import numpy as np
import os
import csv

STATUS_COL = "state"
OPENED_COL = "created_at"
CLOSED_COL = "closed_at"
DATE_FORMAT = "%Y-%m-%dT%H:%M:%SZ"  # In case pandas doesn't figure out the dates automatically, specify format to it


def get_velocity(start_date, end_date, df):
    """
    Calculates different metrics for the velocity calculation of each of the projects
    :param start_date: Beginning date for the velocity calculation window (3 months)
    :param end_date: End date for the velocity calculation window (3 months)
    :param df: Dataframe containing issue data from the given project
    """

    # Add a boolean column in the dataframe which indicates whether the issue was fixed within time frame
    df["fixed_during"] = np.where((start_date <= df[CLOSED_COL]) & (df[CLOSED_COL] <= end_date), True, False)
    # Calculate number of issues created during the time frame
    n_issues_created_during = np.count_nonzero((start_date <= df[OPENED_COL]) & (df[OPENED_COL] <= end_date))
    # Calculate number of issues open at the start of the time frame
    n_open_at_start = np.count_nonzero(
        (df[OPENED_COL] <= start_date) & ((pd.isnull(df[CLOSED_COL])) | (df[CLOSED_COL] > start_date)))

    closed_issues = df.loc[df["fixed_during"] == True]
    fixed_mean = closed_issues["open_time"].mean()
    fixed_median = closed_issues["open_time"].median()
    n_closed_issues = len(closed_issues)

    return fixed_mean, fixed_median, n_open_at_start, n_closed_issues, n_issues_created_during


def getWindowDates(commits_file_path):
    """
    calculate the velocity calculation windows based on the creation date of the projects (first commit).
    """
    df = pd.read_csv(commits_file_path)
    df["date"] = pd.to_datetime(df["date"])
    project_creation_date = df["date"].min()

    velocity_start_1 = project_creation_date + relativedelta(months=9)
    velocity_start_2 = project_creation_date + relativedelta(months=12)
    velocity_end_1 = project_creation_date + relativedelta(months=21)
    velocity_end_2 = project_creation_date + relativedelta(months=24)

    return velocity_start_1, velocity_start_2, velocity_end_1, velocity_end_2


def velocityAnalysis(input_folder):
    """
    :param input_folder: Folder containing or cases or controls issue data files
    """

    # Project name: name of the project
    # n_closed_issues_start: number of issues closed during the start velocity time frame
    # start_mean_velocity: mean time to close issues fixed within the start velocity time frame
    # start_median_velocity: median time to close issues fixed in the start velocity time frame
    # open_issues_at_at_start: number of open issues at the start of start velocity time frame
    # n_created_issues_start: number of issues created during the start velocity time frame
    # The rest of the variables are the same but for the end velocity time frame
    output = [["full_name", "n_closed_issues_start", "start_mean_velocity", "start_median_velocity",
               "open_issues_at_start", "n_created_issues_start", "n_closed_issues_end", "end_mean", "end_median",
               "open_issues_at_end", "difference_of_means", "difference_of_medians", "n_created_issues_end"]]

    files = os.listdir(input_folder)
    # iterate issue csv files in the input folder
    for count, filename in enumerate(files):

        if filename[-4:] != ".csv":
            print("File not a csv file", filename)
            continue

        project_name = filename[:-4]  # I assumed the project name is the name of the csv file
        df = pd.read_csv(os.path.join(input_folder, filename), usecols=[STATUS_COL, OPENED_COL, CLOSED_COL],
                         parse_dates=[OPENED_COL, CLOSED_COL])

        df[OPENED_COL] = pd.to_datetime(df[OPENED_COL]).dt.tz_localize(None).dt.tz_localize('UTC')
        df[CLOSED_COL] = pd.to_datetime(df[CLOSED_COL]).dt.tz_localize(None).dt.tz_localize('UTC')

        # Calculate time the issue was opened. Value is NaT if it's still open.
        df["open_time"] = df[CLOSED_COL] - df[OPENED_COL]  # In days

        # Since the projects are young projects and we don't require a fixed window for the velocity calculation, we
        # calculate the velocity calculation windows based on the creation date of the projects (first commit).
        project_type_path = input_folder.rstrip("issue_history")
        commits_path = os.path.join(project_type_path, "commits")
        velocity_start_1, velocity_start_2, velocity_end_1, velocity_end_2 = getWindowDates(commits_file_path=os.path.join(commits_path, filename))


        # Calculate velocity at the start and end of follow-up period
        start_mean, start_median, start_n_issues, n_closed_issues_start, n_created_issues_start = get_velocity(
            velocity_start_1, velocity_start_2, df)
        end_mean, end_median, end_n_issues, n_closed_issues_end, n_created_issues_end = get_velocity(
            velocity_end_1, velocity_end_2, df)
        mean_difference = end_mean - start_mean
        median_difference = end_median - start_median

        output.append([project_name, n_closed_issues_start, start_mean, start_median, start_n_issues,
                       n_created_issues_start, n_closed_issues_end, end_mean, end_median, end_n_issues, mean_difference,
                       median_difference, n_created_issues_end])

        print("VELOCITY CALCULATION: Project {} processed. Processing {}/{}".format(filename, count+1, len(files)))

    # write velocities to a csv file
    results_path = os.path.join(DATA_PATH, "velocity_results.csv")
    if not os.path.exists(results_path):

        with open(results_path, "a+") as f:
            write = csv.writer(f)
            write.writerows(output)
    else:
        with open(results_path, "a+") as f:
            write = csv.writer(f)
            del output[0]
            write.writerows(output)
