"""
basic statistics regarding the selected projects should be reported to help readers better understand the dataset.
This includes information on project size, the number of MS components, stars, and the number of issues and commits.
"""

import os
from typing import final
import requests
from codes.commons import DATA_PATH, CASES_PATH, CONTROLS_PATH
import pandas as pd
from codes.utils import read_token
from dateutil.relativedelta import relativedelta
from datetime import datetime
import json
import random


def read_csv_to_dict(save_path):
    """
    Loads an existing CSV file into a dictionary matching the changes_data format.
    If the file does not exist, returns an empty dictionary.

    :param save_path: Path to the CSV file.
    :return: Dictionary with existing data or an empty initialized dictionary.
    """
    df = pd.read_csv(save_path)

    # Convert the DataFrame into a dictionary with lists
    existing_data = df.to_dict(orient="list")
    #print(f"Loaded existing data from {save_path}")
    return existing_data


def get_stars(project_name, project_type):

    stargazers_url = f"https://api.github.com/repos/{project_name}/stargazers"
    github_token = read_token()
    headers = {'Accept': 'application/vnd.github.v3.star+json', 'Authorization': f'token {github_token}'}
    page_num = 1

    project_filename = project_name.replace("/", "#", 1)
    # Get first commit crawled from GitHub
    if project_type == "cases":
        df = pd.read_csv(os.path.join(CASES_PATH, "commits", f"{project_filename}.csv"))
    else:
        df = pd.read_csv(os.path.join(CONTROLS_PATH, "commits", f"{project_filename}.csv"))
    df['date'] = pd.to_datetime(df['date'], format='%Y-%m-%dT%H:%M:%SZ')
    creation_datetime = df['date'].min()

    #creation_datetime = datetime.strptime(, "%Y-%m-%dT%H:%M:%SZ")
    followup_start_date = creation_datetime + relativedelta(months=12)
    stargazers = 0
    while True:

        params = {'per_page': 100, 'state': 'all', 'page': page_num}

        repo_response = requests.get(stargazers_url, headers=headers, params=params)
        response_json = repo_response.json()
        if len(response_json) == 0:
            return stargazers, followup_start_date

        for stargazer in response_json:
            try:
                star_datetime = datetime.strptime(stargazer["starred_at"], "%Y-%m-%dT%H:%M:%SZ")
            except TypeError:
                return "*", followup_start_date
            if star_datetime <= followup_start_date:
                stargazers += 1
            else:
                return stargazers, followup_start_date

        page_num += 1


def get_microservices(project_name, follow_up_start_datetime):

    ms_analysis_path = os.path.join("/Users/mrobredo23/OULU/docker_cohort-24/data/ms_analysis_files", project_name)
    list_files = os.listdir(ms_analysis_path)
    ordered_analysis_dates = [datetime.strptime(analysis_file.removesuffix(".json"), "%Y-%m-%dT%H:%M:%SZ") for analysis_file in list_files]
    sorted_analysis_dates = sorted(ordered_analysis_dates)
    previous_datetime = None
    print("Project name: ", project_name, "follow_up_start_datetime: ", follow_up_start_datetime)
    for analysis_datetime in sorted_analysis_dates:
        if analysis_datetime > follow_up_start_datetime:
            break
        previous_datetime = analysis_datetime

    with open(os.path.join(ms_analysis_path, f"{previous_datetime.strftime("%Y-%m-%dT%H:%M:%SZ")}.json"), mode="r", encoding="utf-8") as json_file:
        ms_analysis_dict = json.load(json_file)

    return ms_analysis_dict["num_services"]


def write_csv_from_dict(dictionary, path):
    """
    Writes in an scalable format all dictionaries
    :param dictionary:
    :param path:
    """

    data_df = pd.DataFrame.from_dict(dictionary)
    data_df.to_csv(path, index=False)


def get_eloc(repo_name):
    pass


def make_table():
    # Define the new dictionary structure with nested columns
    table_dict = {
        "Project": [],
        "Creation year": [],
        "Subject Type": [],
        "Size": {"FU_start": [], "FU_end": []},
        "Microservices": {"FU_start": [], "FU_end": []},
        "Main Language": {"FU_start": [], "FU_end": []},
        "Stars": {"FU_start": [], "FU_end": []},
        "Issues": {"FU_start": [], "FU_end": []},
        "Commits": {"FU_start": [], "FU_end": []},
        "Contributors": {"FU_start": [], "FU_end": []},
        "Languages": {"FU_start": [], "FU_end": []}
    }

    final_projects_path = os.path.join(DATA_PATH, "final_data_file.csv")
    final_projects_dict = read_csv_to_dict(final_projects_path)

    for i in range(0, len(final_projects_dict["full_name"])):

        print(f"Starting processing of project: {final_projects_dict["full_name"][i]}")
        table_dict["Project"].append(final_projects_dict["full_name"][i].replace("#", "/"))
        table_dict["Creation year"].append(final_projects_dict["creation_year"][i])
        # Determine subject type
        is_microservices = final_projects_dict["MS/NonMS"][i] == "MS"
        table_dict["Subject Type"].append("Case" if is_microservices else "Control")

        # Get GitHub stars
        project_type = "cases" if is_microservices else "controls"
        num_stars_beginning, num_stars_end, follow_up_start_datetime, follow_up_end_datetime = get_stars(
            project_name=final_projects_dict["full_name"][i].replace("#", "/"),
            project_type=project_type
        )

        # Assign FU_start values
        table_dict["Size"]["FU_start"].append(final_projects_dict["size"][i])
        table_dict["Main Language"]["FU_start"].append(final_projects_dict["main_language"][i])
        table_dict["Stars"]["FU_start"].append(num_stars_beginning)
        table_dict["Issues"]["FU_start"].append(final_projects_dict["n_issues"][i])
        table_dict["Commits"]["FU_start"].append(final_projects_dict["n_commits"][i])
        table_dict["Contributors"]["FU_start"].append(final_projects_dict["n_contributors"][i])
        table_dict["Languages"]["FU_start"].append(final_projects_dict["n_languages"][i])

        if is_microservices:
            num_microservices = get_microservices(
                project_name=final_projects_dict["full_name"][i],
                period="start",
                follow_up_start_datetime=follow_up_start_datetime,
                follow_up_end_datetime=follow_up_end_datetime
            )
            table_dict["Microservices"]["FU_start"].append(num_microservices)
        else:
            table_dict["Microservices"]["FU_start"].append("-")

        # Fitting end of follow-up information into the dictionary
        end_fu_size, end_fu_main_language, end_fu_num_languages = get_eloc(repo_name=final_projects_dict["full_name"][i])

        table_dict["Size"]["FU_end"].append(final_projects_dict["size"][i])  # Placeholder
        table_dict["Main Language"]["FU_end"].append(final_projects_dict["main_language"][i])  # Placeholder
        table_dict["Stars"]["FU_end"].append(num_stars_end)  # Placeholder
        table_dict["Issues"]["FU_end"].append(final_projects_dict["n_issues"][i])  # Placeholder
        table_dict["Commits"]["FU_end"].append(final_projects_dict["n_commits"][i])  # Placeholder
        table_dict["Contributors"]["FU_end"].append(final_projects_dict["n_contributors"][i])  # Placeholder
        table_dict["Languages"]["FU_end"].append(final_projects_dict["n_languages"][i])  # Placeholder

        if is_microservices:
            num_microservices_end = get_microservices(
                project_name=final_projects_dict["full_name"][i],
                period="end",
                follow_up_start_datetime=follow_up_start_datetime,
                follow_up_end_datetime=follow_up_end_datetime
            )
            table_dict["Microservices"]["FU_end"].append(num_microservices_end)  # Placeholder
        else:
            table_dict["Microservices"]["FU_end"].append("-")

        print(
            f"Project {final_projects_dict['full_name'][i]} completed - {i + 1}/{len(final_projects_dict['full_name'])}")

    # Convert to DataFrame and restructure for CSV output
    flat_table_dict = {
        "Project": table_dict["Project"],
        "Creation year": table_dict["Creation year"],
        "Subject Type": table_dict["Subject Type"]
    }

    for col in ["Size", "Microservices", "Main Language", "Stars", "Issues", "Commits", "Contributors", "Languages"]:
        flat_table_dict[f"{col} FU_start"] = table_dict[col]["FU_start"]
        flat_table_dict[f"{col} FU_end"] = table_dict[col]["FU_end"]

    final_df = pd.DataFrame(flat_table_dict)

    # Save to CSV
    output_path = os.path.join(DATA_PATH, "basic_statistics_table.csv")
    final_df.to_csv(output_path, index=False)
    print(f"> Table saved at {output_path}")
