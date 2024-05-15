
import time
import git
import pydriller as drill
import os
from commons import CASES_PATH, CONTROLS_PATH, CHECKOUT_BACK_TO_MASTER, LOG_FILE_PATH
from modules import getFirstHash
from find_ms_usage import getFirstCommit
from io import StringIO
import pandas as pd
from datetime import datetime
from dateutil.relativedelta import relativedelta
import numpy as np
import pytz
import subprocess


def get_default_branch(repo_path):
    """
    Identifies the default branch of the concerning repository
    """
    # Command to get the default branch name
    result = subprocess.run(["git", "remote", "show", "origin"], cwd=repo_path, text=True, capture_output=True)
    if result.returncode == 0:
        for line in result.stdout.split('\n'):
            if 'HEAD branch' in line:
                return line.split(':')[1].strip()
    else:
        print("--Failed to get default branch information")
        with open(LOG_FILE_PATH, 'a') as f:
            f.write(str("-- Failed to get default branch information") + ";" + repo_path + '\n')


def is_commit_in_branch(repo_dir_path, commit_hash, branch_name):
    repo = git.Repo(repo_dir_path)

    if not repo.bare:
        try:
            # Get the commit object from the hash
            commit = repo.commit(commit_hash)
            # Iterate through all commits in the branch
            for branch_commit in repo.iter_commits(branch_name):
                if commit == branch_commit:
                    return True
            return False
        except ValueError as e:
            print(f"Error: {e}")
            with open(LOG_FILE_PATH, 'a') as f:
                f.write(str("-- Failed to get match commit") + commit_hash + "with branch name" + branch_name + "for Project" + ";" + repo_dir_path + '\n')
            return False
    else:
        print("Repository not found.")
        with open(LOG_FILE_PATH, 'a') as f:
            f.write(str("-- Failed to get match commit") + commit_hash + "with branch name" + branch_name + "for Project" + ";" + repo_dir_path + '\n')
        return False


def checkoutToCommitBeforeFollowUp(git_repo, project_name, project_type, repo_dir):
    """
    Checks out to the commit before the follow-up period starts

    :param git_repo - Repository object
    """

    project_name = project_name + ".csv"
    project_creation_date = getFirstCommit(project_name=project_name, project_type=project_type)
    start_follow_up_date = project_creation_date + relativedelta(months=12)
    # end_follow_up_date = start_follow_up_date + relativedelta(months=15)
    first_commit_hash = getFirstHash(project_name=project_name, project_type=project_type)

    # Get the default branch of the repository
    default_branch = get_default_branch(repo_path=repo_dir)

    # commits = drill.Repository(repo_dir, from_commit=first_commit_hash, order="reverse").traverse_commits()
    commit_to_checkout = None

    excesive_traversal = 0
    # Traverses all the commits from the repository from the last commit in the repository until the one specified (the first one). But stops based on our date criteria
    for commit in drill.Repository(repo_dir, from_commit=first_commit_hash, order="reverse").traverse_commits():
        commit_time = commit.author_date
        # Gets the last commit before the follow-up date from the default branch
        if commit_time < start_follow_up_date:
            # Returns true if the selected commit is within the main branch
            if is_commit_in_branch(repo_dir, commit_hash=commit.hash, branch_name=default_branch) is False:
                excesive_traversal += 1
                continue

            # We check if the activity right before the follow-up period date start is not given in the main branch.
            if excesive_traversal > 10:
                with open(LOG_FILE_PATH, 'a') as f:
                    f.write(str(f"-- LOG TRAVERSAL ERROR for project: {repo_dir}, more than 10 commits before the "
                                f"start follow-up date without pushing into the default branch\n"))

            commit_to_checkout = commit
            print("checking out to", commit_time, commit)
            break

    try:
        # Stash any uncommitted changes
        git_repo.git.stash('push', '-m', 'Stashing changes before checkout')
        git_repo.git.checkout(commit_to_checkout.hash)
    except Exception as e:
        print(f"Error during checkout: {e}")


def getRepositoryStatistics(git_repo_path):
    """
    Uses the tool scc to read statistics from the git repository of the provided project
    """

    file_analysis_results_csv = os.popen("scc -f json {}".format(git_repo_path)).read()
    results_df = pd.read_json(StringIO(file_analysis_results_csv))

    # Add total row
    total_row = results_df.sum(numeric_only=True)
    total_row["Name"] = "Total"
    results_df.loc[len(results_df.index)] = total_row
    return results_df


def getConfounders(project_list, project_type):
    """
    Performs the entire process of analyzing confounders
    """

    if project_type == "cases":
        repositories_dir = os.path.join(CASES_PATH, "repositories")
        output_confounders_dir = os.path.join(CASES_PATH, "confounders_data")
    else:
        repositories_dir = os.path.join(CONTROLS_PATH, "repositories")
        output_confounders_dir = os.path.join(CONTROLS_PATH, "confounders_data")

    # Check if the obtained path exists for the repositories
    if not os.path.exists(repositories_dir):
        print(f"The given project repository {repositories_dir} does not exist")
        exit()

    # Create folder for the ms_analysis_files
    if not os.path.exists(output_confounders_dir):
        os.mkdir(output_confounders_dir)

    # Read the list of projects to clone
    n_repos = len(project_list)

    for count, repo_name in enumerate(project_list):

        repo_dir = os.path.join(repositories_dir, repo_name)
        try:
            git_repo = git.Repo(repo_dir)
        except:
            print("Cloned repository in path '{}' does not exist".format(repo_dir))
            exit()

        print("{}/{} {}: ".format(count + 1, n_repos, repo_name), end="")

        # Need to check out to the commit before the FU existing in the DEFAULT branch.
        checkoutToCommitBeforeFollowUp(git_repo=git_repo, project_name=repo_name,
                                       project_type=project_type, repo_dir=repo_dir)
        results_df = getRepositoryStatistics(git_repo_path=repo_dir)
        results_df.rename({"Count": "Files"}, axis="columns")

        results_df.to_csv(os.path.join(output_confounders_dir, repo_name + ".csv"), index=False)

        if CHECKOUT_BACK_TO_MASTER:
            try:
                git_repo.git.checkout("master")
            except:
                print(repo_name, ": could not checkout back to master !!")

        print("GETTING CONFOUNDERS: Project {} from project group {} processed. Processing {}/{}".format(repo_name,
                                                                                                         project_type,
                                                                                                         count+1,
                                                                                                         len(project_list)))




