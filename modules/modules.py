import numpy
import requests
import os
import csv
import pandas as pd
import numpy as np
from datetime import datetime
import tqdm as tqdm
from urllib.parse import urljoin
from commons import github_token, GITHUB_REPO_PATH, PROJECT_CREATION_FIRST, PROJECT_CREATION_LAST, CASES_PATH, \
    CONTROLS_PATH
import time
import statsmodels.stats.power as smp
import scipy.stats as stats
import math
import statistics


def countdown(total):
    """
    Sets a countdown of 1h
    """

    while total:
        mins, secs = divmod(total, 60)
        timer = 'REMAINING: {:02d}:{:02d}'.format(mins, secs)
        print(timer, end='\r')
        time.sleep(1)
        total -= 1


def storeData(data_dict, full_name, content, project_type):
    """
    Stores the data provided in dict format into a csv using pandas
    """
    full_name = full_name.replace("/", "#")

    df = pd.DataFrame.from_dict(data=data_dict)
    if project_type == "cases":
        data_path = os.path.join(CASES_PATH, content)
    else:
        data_path = os.path.join(CONTROLS_PATH, content)

    df.to_csv(os.path.join(data_path, full_name + ".csv"), index=False)


def firstCommitFinder(project_path):
    """
    Finds the first commit of a repository in the fastest way.
    """
    headers = {'Authorization': f'token {github_token}'}
    params = {'state': 'all', "page": 1, "per_page": 100}
    page = 0
    while 1 == 1:
        params["page"] = page
        out = requests.get(project_path, headers=headers, params=params)
        out_json = out.json()

        if int(out.headers['x-ratelimit-remaining']) < 10:
            print('SLEEPING TIME: 1H')
            countdown(3600)

        if not out_json:
            params["page"] = page - 1
            out_last = requests.get(project_path, headers=headers, params=params)
            out_last_json = out_last.json()
            first_commit = out_last_json[-1]
            pro_creation_date = pd.to_datetime(first_commit.get("commit", None).get("author", None).get("date", None))
            print(f"first commit date: {pro_creation_date}")
            return pro_creation_date
        else:
            try:
                out_first_commit = out_json[-1]
            except KeyError:  # If it happens to give some error, then we skip this project. Based on the logic it should provide output.
                return PROJECT_CREATION_FIRST

        out_commit_date = pd.to_datetime(out_first_commit.get("commit", None).get("author", None).get("date", None))
        time.sleep(1)


        # If we are already crawling outside the specified bounds we stop considering this project:
        if out_commit_date < PROJECT_CREATION_FIRST:
            return PROJECT_CREATION_FIRST

        # If the json hus full of commits:
        print(f"api call full: {len(out.json())} - page {page}")
        page += 1


def dateFilter(full_name, content):
    """
    Fetches the project creation date to meet our criteria. The output is Boolean so that there's binary answer to
    discard or not.
    """

    name_path = urljoin(GITHUB_REPO_PATH, full_name + '/')
    project_path = urljoin(name_path, content)
    pro_creation_date = firstCommitFinder(project_path=project_path)

    # If while crawling the project the some of the commits were already outside the bounds:
    if pro_creation_date == PROJECT_CREATION_FIRST:
        print(f"PROJECT: {full_name} EXCLUDED - TOO old")
        return False

    if PROJECT_CREATION_FIRST < pro_creation_date < PROJECT_CREATION_LAST:
        print(f"PROJECT: {full_name} INCLUDED based on date {pro_creation_date}")
        return True
    else:
        print(f"PROJECT: {full_name} EXCLUDED based on date {pro_creation_date}")
        return False


def projectCrawler(full_name, content, project_type, remaining_project_num, lost_project_num):
    """
    Crawls projects in GitHub for their commits and issues and stores them in csv files
    """
    headers = {'Authorization': f'token {github_token}'}

    # Languages we don't consider for counting their presence.
    # not_considered = ['Shell', 'Dockerfile', 'Jupiter Notebook', 'Makefile']
    name_path = urljoin(GITHUB_REPO_PATH, full_name + '/')
    query_path = os.path.join(name_path, content)
    params = {'per_page': 100, 'state': 'all'}
    page = 0
    has_content = False

    if content == "commits":
        commit_dict = {"sha": [], "date": [], "author_email": [], "author_login": []}
    else:
        issue_dict = {"number": [], "created_at": [], "user_login": [], "user_id": []}

    while True:

        params['page'] = page
        out = requests.get(query_path, headers=headers, params=params)
        time.sleep(1)

        # Controls the API call rate limit
        if int(out.headers['x-ratelimit-remaining']) < 10:
            print('SLEEPING TIME: 1H')
            countdown(3600)

        # Deals with the status code
        if out.status_code == 200:
            print("fetch response status code: ", out.status_code)
            json_response = out.json()

        # If the json didn't have output or the output was over
        if len(out.json()) == 0 or out.status_code != 200:
            print("fetch response status code: ", out.status_code)
            print(f"If 200 -> End of {content} data for project {full_name}")
            print(f"JSON: {out.json()}")

            # Store the obtained data
            if content == "commits":
                if commit_dict.get("sha") != []:
                    has_content = True
                    storeData(data_dict=commit_dict, full_name=full_name, content=content, project_type=project_type)
            else:
                if issue_dict.get("number") != []:
                    has_content = True
                    storeData(data_dict=issue_dict, full_name=full_name, content=content, project_type=project_type)
            break

        if content == "commits":  # We collect the data as per the commits data format
            for count, commit_data in enumerate(json_response):
                commit_dict["sha"].append(commit_data.get("sha", None))
                commit_dict["date"].append(commit_data.get("commit", None).get("author", None).get("date", None))
                commit_dict["author_email"].append(commit_data.get("commit", None).get("author", None).get("email", None))
                try:
                    commit_dict["author_login"].append(commit_data.get("author", None).get("login", None))
                except AttributeError:  # No information about the author
                    commit_dict["author_login"].append(None)

                if count == 99:
                    print(f"sha : {commit_data.get('sha', None)}")

        else:  # We collect the data as per the issues data format
            for count, issue_data in enumerate(json_response):
                issue_dict["number"].append(issue_data.get("number", None))
                issue_dict["created_at"].append(pd.to_datetime(issue_data.get("created_at", None)))
                issue_dict["user_login"].append(issue_data.get("user", None).get("login", None))
                issue_dict["user_id"].append(issue_data.get("user", None).get("id", None))

        print(f"processing project {full_name}, page {page}")
        page += 1

    # Add a new count for projects which stored data
    if has_content is True:
        remaining_project_num += 1
    else:
        lost_project_num += 1

    return remaining_project_num, lost_project_num


def createSerialData(data_type, original_files, path, periodicity):
    """
    Creates monthly data out from original GitHub data
    """
    empty_issue_files = 0
    empty_commit_files = 0

    for file in original_files:

        # Creates a new dataframe with the number of commits per month (local case)
        project_file = file
        if data_type == "commits":
            commits_path = os.path.join(path, "commits")
            project_df = pd.read_csv(f"{commits_path}/{project_file}")
            if project_df.empty:
                empty_commit_files += 1

        else:  # Issues
            issues_path = os.path.join(path, "issues")
            project_df = pd.read_csv(f"{issues_path}/{project_file}")
            project_df.rename(columns={'created_at': 'date'}, inplace=True)
            if project_df.empty:
                empty_issue_files += 1

        project_df.date = pd.to_datetime(project_df.date)
        project_df.set_index("date", inplace=True)
        if periodicity == "monthly":
            periodical_counts = project_df.resample('M').size()
        elif periodicity == "bimonthly":
            periodical_counts = project_df.resample('2M').size()
        elif periodicity == "trimonthly":
            periodical_counts = project_df.resample('3M').size()
        else:
            periodical_counts = project_df.resample('4M').size()

        # Store new dataframe as a csv.
        if data_type == "commits":

            period_counts_df = periodical_counts.reset_index(name="commit_count")
            commits_path = os.path.join(f"{path}/serialized_data", f"{periodicity}_commits")
            period_counts_df.to_csv(f"{commits_path}/{project_file}", index=False)

        else:

            # Monthly
            period_counts_df = periodical_counts.reset_index(name="issue_count")
            issues_path = os.path.join(f"{path}/serialized_data", f"{periodicity}_issues")
            period_counts_df.to_csv(f"{issues_path}/{project_file}", index=False)

    if data_type == "commits":
        print(f"NUMBER OF EMPTY commit FILES: {empty_commit_files}")
    else:
        print(f"NUMBER OF EMPTY issues FILES: {empty_issue_files}")


def cropProject(project_path, project_file, periodicity):
    """
    Crops projects data length to 2 years since the start of the project creation so that the trend is only analyzed during
    that time.
    project_file: csv file
    """
    operation_flag = False
    project_df = pd.read_csv(os.path.join(project_path, project_file))
    project_shape = project_df.shape

    if periodicity == "monthly":
        minimum_periods = 24
    elif periodicity == "bimonthly":
        minimum_periods = 12
    elif periodicity == "trimonthly":
        minimum_periods = 8
    else:
        minimum_periods = 6

    if project_shape[0] >= minimum_periods:  # Checks if the history of the project is more than 20 months.
        subset_df = project_df.iloc[0:minimum_periods,]
        operation_flag = True
        return operation_flag, subset_df
    else:
        return operation_flag, project_df


def calculateSampleSize(effect_size, alpha, power, ratio):
    """
    Calculates the sample size for the controls
    """
    analysis = smp.TTestIndPower()
    sample_size = analysis.solve_power(effect_size, power=power, alpha=alpha, ratio=ratio)
    return sample_size


"""
def calculateControlsSize(cases_size, cases_std, controls_std, alpha, beta, cases_mean, controls_mean):

    z_std_controls = stats.norm.ppf(1 - alpha/2)
    z_beta = stats.norm.ppf(1-beta)
    numerator = (cases_std**2 + controls_std**2) * (z_std_controls + z_beta)**2
    denominator = (cases_mean - controls_mean)**2
    controls_size = math.ceil(numerator / denominator) - cases_size  # rounds a number UP to the nearest integer,
    # if necessary, and returns the result.
    return controls_size
"""


def powerAnalysis():
    """
    Performs the power analysis of the study
    """
    effect_sizes = [0.2, 0.5, 0.8]  # Small, medium & Big
    alpha = 0.05  # Significance level
    power = 0.8  # Desired statistical power
    ratios = [1.0, 0.5, 1/3, 1/4, 1/8]

    # Summary table
    results = []
    for effect_size in effect_sizes:
        for ratio in ratios:
            sample_size = calculateSampleSize(effect_size=effect_size, power=power, alpha=alpha, ratio=ratio)
            cases_sample_size = sample_size * ratio
            total_sample_size = sample_size + cases_sample_size
            results.append({
                'Effect Size': effect_size,
                'Ratio': ratio,
                'Sample Size for Controls': round(sample_size),
                'Sample Size for Cases': round(cases_sample_size),
                'Total Sample Size': round(total_sample_size)
            })

    # Display the summary table
    summary_table = pd.DataFrame(results)
    print(summary_table)

    # CALCULATE specific size of the controls samples
    cases_path = os.path.join(CASES_PATH, "resulting_data/lm_resulting_pros_trimonthly.txt")
    controls_path = os.path.join(CONTROLS_PATH, "resulting_data/lm_resulting_pros_trimonthly.txt")
    # Read the list of projects to clone
    with open(cases_path, "r") as f:
        cases = f.read().splitlines()
    f.close()
    with open(controls_path, "r") as fc:
        controls = fc.read().splitlines()
    fc.close()

    cases_size = len(cases)
    controls_size = len(controls)

    results_2 = []
    for effect_size in effect_sizes:

        power_analysis = smp.TTestIndPower()
        ratio = power_analysis.solve_power(effect_size=effect_size, nobs1=controls_size, power=power, alpha=alpha, ratio=None, alternative="two-sided")
        print(f"Required ratio:{ratio}")

        results_2.append({
            'Effect Size': effect_size,
            'Sample Size for Controls': controls_size,
            'Sample Size for Cases':  cases_size * ratio,
            'Total Sample Size': controls_size + round(cases_size*ratio),
            "ratio": ratio
        })

    summary_table_2 = pd.DataFrame(results_2)
    print(summary_table_2)


def issueCrawler(full_name, project_type):
    """
    Crawls the GitHub repository of the project and obtains specific creation, update and closing data of the identified
    issues.
    :param full_name: Long repository name of the project
    :param project_type: Cases or controls
    """

    headers = {'Authorization': f'token {github_token}'}

    path_name = full_name[:-4]
    path_name = path_name.replace("#", "/")
    name_path = urljoin(GITHUB_REPO_PATH, path_name + '/')
    query_path = os.path.join(name_path, "issues")
    params = {'per_page': 100, 'state': 'all'}
    page = 0
    has_content = False

    issue_dict = {"number": [], "created_at": [], "updated_at": [], "closed_at": [], "state": [], "user_login": [],
                  "user_id": []}

    while True:

        params['page'] = page
        out = requests.get(query_path, headers=headers, params=params)
        time.sleep(1)

        # Controls the API call rate limit
        if int(out.headers['x-ratelimit-remaining']) < 10:
            print('SLEEPING TIME: 1H')
            countdown(3600)

        # Deals with the status code
        if out.status_code == 200:
            print("fetch response status code: ", out.status_code)
            json_response = out.json()

        if len(out.json()) == 0 or out.status_code != 200:
            print("fetch response status code: ", out.status_code)
            print(f"If 200 -> End of ISSUE data for project {full_name}")
            print(f"JSON: {out.json()}")

            if issue_dict.get("number") != []:
                has_content = True
                store_name = path_name.replace("/", "#")
                df = pd.DataFrame.from_dict(data=issue_dict)
                if project_type == "cases":
                    issue_history_path = os.path.join(CASES_PATH, "issue_history")
                    if not os.path.exists(issue_history_path):
                        os.mkdir(issue_history_path)
                else:
                    issue_history_path = os.path.join(CONTROLS_PATH, "issue_history")
                    if not os.path.exists(issue_history_path):
                        os.mkdir(issue_history_path)

                data_path = os.path.join(issue_history_path, f"{store_name}.csv")
                df.to_csv(data_path, index=False)

            break

        for count, issue_data in enumerate(json_response):
            issue_dict["number"].append(issue_data.get("number", None))
            issue_dict["created_at"].append(pd.to_datetime(issue_data.get("created_at", None)))
            issue_dict["updated_at"].append(pd.to_datetime(issue_data.get("updated_at", None)))
            issue_dict["closed_at"].append(pd.to_datetime(issue_data.get("closed_at", None)))
            issue_dict["state"].append(issue_data.get("state", None))
            issue_dict["user_login"].append(issue_data.get("user", None).get("login", None))
            issue_dict["user_id"].append(issue_data.get("user", None).get("id", None))

        print(f"processing project {full_name}, page {page}")
        page += 1

    print(f"Issue collection for project {full_name} COMPLETED")


def getFirstHash(project_name, project_type, excluded_hashes):

    if project_type == "cases":
        project_path = os.path.join(CASES_PATH, "commits", project_name)
    else:
        project_path = os.path.join(CONTROLS_PATH, "commits", project_name)

    df = pd.read_csv(project_path)
    first_row_idx = df["date"].idxmin() - excluded_hashes # Gets the row index of the earliest commit date
    earliest_row = df.loc[first_row_idx]

    return earliest_row['sha']


def get_subset_resulting_projects():
    """
    Gets the subset of projects that accomplish the criteria for the trend analysis and adjusts the format for the required
    path for the microservices analysis step.
    """

    trend_accepted_pro_list_path = os.path.join(CASES_PATH, "resulting_data", "lm_resulting_pros_trimonthly.txt")
    with open(trend_accepted_pro_list_path, "r") as f:
        project_list = f.read().splitlines()
    f.close()

    github_prefix = "https://github.com/"
    clean_list = []
    for project in project_list:

        project_name = project[:-4]
        path_format_name = project_name.replace("#", "/")
        github_project_path = github_prefix + path_format_name
        clean_list.append(github_project_path)

    initial_project_dataset = pd.read_csv(os.path.join(CASES_PATH, "filtered_dataset.csv"))
    subset_df = initial_project_dataset[initial_project_dataset["URL"].isin(clean_list)]

    if not os.path.exists(os.path.join(CASES_PATH, "repos")):
        os.mkdir(os.path.join(CASES_PATH, "repos"))
    subset_df.to_csv(os.path.join(CASES_PATH, "repos", "selected_projects_repo_links.csv"), index=False)