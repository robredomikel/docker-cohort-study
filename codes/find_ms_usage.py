"""
Fai il traverse di tutti i commit, per ogni commit ti fai dare i modified_files, controlli se nei modified_files c’è un
docker compose o un dockerfile e ti salvi l’hash del commit.

Una volta che hai tutti gli hash, per ogni hash, sempre con pydriller o pygit, fai il checkout e lanci il tool per il
calcolo del numero di microservizi
"""

import pydriller as pydrill
from commons import CASES_PATH, CHECKOUT_BACK_TO_MASTER, CONTROLS_PATH
import os
import yaml
import pandas as pd
from datetime import datetime
from dateutil.relativedelta import relativedelta


def getFirstCommit(project_name, project_type):
    """
    Checks which is the first commit registered in each stored project
    params: - project_name: "project1/project2.csv"
    """

    if project_type == "cases":
        project_path = os.path.join(CASES_PATH, "commits", project_name)
    else:
        project_path = os.path.join(CONTROLS_PATH, "commits", project_name)
    df = pd.read_csv(project_path)
    df["date"] = pd.to_datetime(df["date"])
    project_creation_date = df["date"].min()
    return project_creation_date


# Traverse through all the commits of the repository to fetch the commits in which the docker related files where
# modified
def findHash(ms_usage_path, project_name):
    """
    Finds the hash of the commit in which either the Dockerfile or the docker-compose files are modified
    params: - ms_usage_path: Directory path for storing the list of hashes in which the change is made.
    """

    if project_name[-4:] == ".csv":
        project_name = project_name[:-4]

    # The repos are named as the last project identifier
    for commit in pydrill.Repository(os.path.join(CASES_PATH, f"repositories/{project_name}")).traverse_commits():
        for file in commit.modified_files:
            print(f'Author {commit.author} modified {file.filename} in commit {commit.hash}')
            if file.filename == "Dockerfile" or file.filename == "docker-compose.yml":
                with open(os.path.join(ms_usage_path, f"{project_name}.txt"), mode='a+') as f:
                    f.write(f"{commit.hash};")
                    f.write(f"{commit.author_date};")
                    f.write(file.filename)
                    f.write("\n")
                f.close()


def getHashes(ms_usage_path, project_name, project_creation_date):
    """
    This function gets all the commit hashes during the follow-up period in which the Dockerfile or docker-compose files are
    modified.
    params: - ms_usage_path: Directory path for storing the list of hashes in which the change is found.
    - project_name: Name of the project
    - project_creation_date: Date of the project creation
    """

    if project_name[-4:] == ".csv":
        project_name = project_name[:-4]

    # Follow-up period margins
    start_follow_up_date = project_creation_date + relativedelta(months=12)
    end_follow_up_date = start_follow_up_date + relativedelta(months=12)


# From the obtained commit hashes get the
def findMsUsage(hash_path, project_name, project_creation_date):
    """
    Performs git checkout in the hashes where there were changes and modified
    """

    eligible = True
    if project_name[-4:] == ".csv":
        project_name = project_name[:-4]

    # The repos are named as the last project identifier
    gr = pydrill.Git(os.path.join(CASES_PATH, f"repositories/{project_name}"))
    commit_lines = open(os.path.join(hash_path, f"{project_name}.txt"), mode='r').read().splitlines()
    for commit_line in commit_lines:
        hash = commit_line.split(";")[0]
        date = commit_line.split(";")[1]
        filename = commit_line.split(";")[0]
        gr.checkout(hash)
        files = gr.files()
        yaml_path = os.path.join(CASES_PATH, f"repositories/’{project_name}", "docker-compose.yml")

        # Maybe there's still no docker-compose file
        try:
            compose_file_index = files.index(yaml_path)
        except ValueError:
            continue

        with open(yaml_path, mode='r') as file:
            yaml_content = file.read()
        file.close()
        data = yaml.safe_load(yaml_content)

        # Navigate to the services seciton of the YAML file and count the services
        services_count = len(list(data['services'].keys()))

        # Check if they are enough during the follow-up period
        print("Number of microservices: ", services_count, "for DATE: ", date)
        start_follow_up_date = project_creation_date + relativedelta(months=12)
        end_follow_up_date = start_follow_up_date + relativedelta(months=12)
        if start_follow_up_date < date < end_follow_up_date:
            if services_count < 2:
                print(f"PROJECT EXCLUDED: {project_name} - NOT ENOUGH MS DURING FOLLOW-UP PERIOD")
                eligible = False

    # We include in the next stage only the projects that consistently relied on microservices architecture.
    if eligible == True:

        if not os.path.exists(os.path.join(CASES_PATH, "ms_eligible_projects")):
            os.mkdir(os.path.join(CASES_PATH, "ms_eligible_projects"))
        with open(os.path.join(CASES_PATH, "ms_eligible_projects/eligibles.txt"), mode='a+') as file:
            file.write(project_name + ".csv")
            file.write("\n")
        file.close()

    if CHECKOUT_BACK_TO_MASTER:  # Check out to the current stage of the repository
        try:
            # gr.reset("--hard")
            gr.checkout("master")
        except:
            print(gr.project_name, ": could not checkout back to MASTER.")


###################################
# Main functions

def getMsUsage():

    # Gets the first commit from each given project
    eligible_cases_path = os.path.join(CASES_PATH, "resulting_data/lm_resulting_pros_trimonthly.txt")
    cases_projects = open(eligible_cases_path, mode="r").read().splitlines()
    for count, case_project in enumerate(cases_projects):
        first_commit_date = getFirstCommit(case_project, project_type="cases")

        # Creates the path for the ms usage track folders
        ms_usage_path = os.path.join(CASES_PATH, "ms_usage")
        if not os.path.exists(ms_usage_path):
            os.makedirs(ms_usage_path)

        project_name = case_project[:-4]  # Removes the .csv extension
        # Find the hashes in which Dockerfile or docker-compose.yml have been used.
        findHash(ms_usage_path=ms_usage_path, project_name=project_name)
        findMsUsage(hash_path=ms_usage_path, project_name=project_name, project_creation_date=first_commit_date)

        print("MS USAGE: Case project {} processed. Processing {}/{}".format(case_project, count+1, len(cases_projects)))