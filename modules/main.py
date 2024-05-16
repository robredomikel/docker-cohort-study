import pandas as pd
from trendAnalysis import trendAnalysis
from modules import projectCrawler, dateFilter, powerAnalysis, issueCrawler, get_subset_resulting_projects
from commons import (COMPLETE_ANALYSIS, CASES_PATH, CONTROLS_PATH, CLONE_PROJECTS, POWER_ANALYSIS, REPO_ANALYSIS,
                     FINAL_FILE_CREATION)
from clone_projects import cloneProjects
from get_confounders import getConfounders
from find_ms_usage import getMsUsage
from get_velocity import velocityAnalysis
from format_data_for_analysis import merge_data
from analyze_repo_multi_thread_per_commit import main_micro

import os


def prefiltering():
    """
    Prefilters considered projects based on project creation date and based on usage of GitHub as issue tracker.
    """

    # Output counts for the date filter
    remaining_cases_date = 0
    lost_cases_date = 0
    remaining_controls_date = 0
    lost_controls_date = 0
    # Output counts for the issue filter
    remaining_cases_issues = 0
    lost_cases_issues = 0
    remaining_controls_issues = 0
    lost_controls_issues = 0
    # Output counts for the issue filter
    remaining_cases_commits = 0
    lost_cases_commits = 0
    remaining_controls_commits = 0
    lost_controls_commits = 0

    # file handling for the eligible projects if they have already been elected or not
    if os.path.exists(os.path.join(CONTROLS_PATH, "eligibles.txt")):
        eligibles_file = open(os.path.join(CONTROLS_PATH, "eligibles.txt"), "r")
        eligible_projects = [line.rstrip("\n") for line in eligibles_file]
        eligibles_file.close()
    else:
        eligible_projects = []
        with open(os.path.join(CONTROLS_PATH, "eligibles.txt"), "w") as file:
            pass

    # File handling for the processed projects
    if os.path.exists(os.path.join(CONTROLS_PATH, "processed.txt")):
        processed_file = open(os.path.join(CONTROLS_PATH, "processed.txt"), "r")
        processed_projects = [line.rstrip("\n") for line in processed_file]
        processed_file.close()
    else:
        processed_projects = []
        with open(os.path.join(CONTROLS_PATH, "processed.txt"), "w") as mifi:
            pass

    # Deal with cases: the filtered dataset contains cases fulfilling the microservices criteria
    filtered_dataset_path = os.path.join(CASES_PATH, "filtered_dataset.csv")
    filtered_dataset = pd.read_csv(filtered_dataset_path, delimiter=";")
    cases_list = list(filtered_dataset.Identifier)

    # Issues
    for case_project in cases_list:
        print(f"> Processing issues for case project {case_project}")
        folder_name = case_project.replace("/", "#")
        # Project already processed
        if folder_name in os.listdir(os.path.join(CASES_PATH, "issues")):
            print(f"> Issues already processed for case project {case_project}")
            continue

        if dateFilter(case_project, content="commits"):  # It only checks whether the creation date is inside or not
            print(f"> Project {case_project} is created during the considered creation window")
            remaining_cases_issues, lost_cases_issues = projectCrawler(full_name=case_project, content="issues",
                                                                       project_type="cases",
                                                                       remaining_project_num=remaining_cases_issues,
                                                                       lost_project_num=lost_cases_issues)
            remaining_cases_date += 1
        else:
            lost_cases_date += 1

    # Commits
    for case_project in cases_list:
        print(f"> Processing commits for case project {case_project}")
        # If the project has been already processed.
        folder_name = case_project.replace("/", "#")
        if folder_name in os.listdir(os.path.join(CASES_PATH, "commits")):
            print(f"> Commits already processed for case project {case_project}")
            continue

        if dateFilter(case_project, content="commits"):
            print(f"> Project {case_project} is created during the considered creation window")
            remaining_cases_commits, lost_cases_commits = projectCrawler(full_name=case_project, content="commits",
                                                                         project_type="cases",
                                                                         remaining_project_num=remaining_cases_commits,
                                                                         lost_project_num=lost_cases_commits)

    # Deal with controls
    all_projects_df = pd.read_csv(os.path.join(CONTROLS_PATH, "P_U.csv"), delimiter=";")
    controls_df = all_projects_df[all_projects_df["Is a Microservices?"] == 'Unknow']
    controls_list = list(controls_df['Identifier'])

    process_count = 0
    # Checks if based on creation date the projects are eligible:
    for control_project in controls_list:

        # If we have already addressed this project, we don't process it again
        if control_project in processed_projects:
            print(f"Project PROCESSED: {control_project}")
            process_count += 1
            continue

        if dateFilter(control_project, content="commits"):
            eligible_projects.append(control_project)
            f = open(os.path.join(CONTROLS_PATH, "eligibles.txt"), "a")
            f.write(f"{control_project}\n")
            f.close()

            remaining_controls_date += 1
        else:
            lost_controls_date += 1

        processed_projects.append(control_project)
        with open(os.path.join(CONTROLS_PATH, "processed.txt"), "a") as fi:
            fi.write(f"{control_project}\n")

        process_count += 1
        print(f"Processed {control_project}, count: {process_count}/{len(controls_list)}")

    for count, control_project in enumerate(eligible_projects):
        # Issues
        print(f"processing issues for control project {control_project}")
        remaining_controls_issues, lost_controls_issues = projectCrawler(full_name=control_project, content="issues",
                                                                         project_type="controls",
                                                                         remaining_project_num=remaining_controls_issues,
                                                                         lost_project_num=lost_controls_issues)

        # Commits
        print(f"processing commits for control project {control_project}")
        remaining_controls_commits, lost_controls_commits = projectCrawler(full_name=control_project, content="commits",
                                                                           project_type="controls",
                                                                           remaining_project_num=remaining_controls_commits,
                                                                           lost_project_num=lost_controls_commits)

        print(f"Eligible project {control_project} PROCESSED, count: {count}/{len(eligible_projects)}")
    """
    # Issues
    for control_project in controls_list:
        print(f"processing issues for control project {control_project}")
        if dateFilter(control_project, content="commits"):
            remaining_controls_issues, lost_controls_issues = projectCrawler(full_name=control_project, content="issues",
                                                                             project_type="controls",
                                                                             remaining_project_num=remaining_controls_issues,
                                                                             lost_project_num=lost_controls_issues)
            remaining_controls_date += 1
        else:
            lost_controls_date += 1

    # Commits
    for control_project in controls_list:
        print(f"processing commits for control project {control_project}")
        if dateFilter(control_project, content="commits"):
            remaining_controls_commits, lost_controls_commits = projectCrawler(full_name=control_project,
                                                                               content="commits",
                                                                               project_type="controls",
                                                                               remaining_project_num=remaining_controls_commits,
                                                                               lost_project_num=lost_controls_commits)
    """

    print(f"--------DATE FILTER:")
    print(f"Remaining CASES: {remaining_cases_date}")
    print(f"Lost CASES: {lost_cases_date}")
    print(f"Remaining CONTROLS: {remaining_controls_date}")
    print(f"Lost CONTROLS: {lost_controls_date}")
    print(f"--------COMMIT FILTER:")
    print(f"Remaining CASES: {remaining_cases_commits}")
    print(f"Lost CASES: {lost_cases_commits}")
    print(f"Remaining CONTROLS: {remaining_controls_commits}")
    print(f"Lost CONTROLS: {lost_controls_commits}")
    print(f"--------ISSUE FILTER:")
    print(f"Remaining CASES: {remaining_cases_issues}")
    print(f"Lost CASES: {lost_cases_issues}")
    print(f"Remaining CONTROLS: {remaining_controls_issues}")
    print(f"Lost CONTROLS: {lost_controls_issues}")


def main():

    # Consider all the projects existing in the cited dataset
    if COMPLETE_ANALYSIS:
        prefiltering()
        # Monthly
        trendAnalysis(project_type="cases", periodicity="monthly")
        trendAnalysis(project_type="controls", periodicity="monthly")
        # Bimonthly
        trendAnalysis(project_type="cases", periodicity="bimonthly")
        trendAnalysis(project_type="controls", periodicity="bimonthly")
        # Trimonthly
        trendAnalysis(project_type="cases", periodicity="trimonthly")
        trendAnalysis(project_type="controls", periodicity="trimonthly")
        # Cuatrimonthly
        trendAnalysis(project_type="cases", periodicity="cuatrimonthly")
        trendAnalysis(project_type="controls", periodicity="cuatrimonthly")

        print(f"> Prefiltering and trend analysis stage performed")

    if POWER_ANALYSIS:

        # Run power analysis
        powerAnalysis()
        # Obtains the subset of controls needed to run the analysis
        # sampleControls()

    if CLONE_PROJECTS:

        # Clone projects
        cloneProjects(project_type="cases")  # Covers the entire stage of project cloning
        cloneProjects(project_type="controls")

        print(f"> Clone project stage performed")

    # Collect all the necessary variables from the cloned repositories
    if REPO_ANALYSIS:

        # Check which case projects have an active MS usage (From the Microservices dataset)
        
        get_subset_resulting_projects()  # We get them from the resulting trend analysis
        main_micro()
        
        # getMsUsage()

        print(f"> MS usage analysis stage performed")

        # Collect confounders from the chosen projects
        getConfounders(project_list=os.listdir(os.path.join(CASES_PATH, "repositories")), project_type="cases")
        getConfounders(project_list=os.listdir(os.path.join(CONTROLS_PATH, "repositories")), project_type="controls")

        print(f"> Confounders collection stage performed")
        # Crawl the issues from the final projects
        # Get eligible cases from the ms_usage
    
        eligible_cases = os.listdir(os.path.join(CASES_PATH, "complete-results"))
        [issueCrawler(full_name=project_name, project_type="cases") for project_name in eligible_cases]
        eligible_controls = open(os.path.join(CONTROLS_PATH, "resulting_data/lm_resulting_pros_trimonthly.txt"), mode='r').read().splitlines()
        [issueCrawler(full_name=project_name, project_type="controls") for project_name in eligible_controls]
        
        print(f"> Issue collection stage performed")

        # Get velocity of the obtained projects.
        
        cases_issues = os.path.join(CASES_PATH, "issue_history")
        velocityAnalysis(input_folder=cases_issues)
        controls_issues = os.path.join(CONTROLS_PATH, "issue_history")
        velocityAnalysis(input_folder=controls_issues)
        
        print("> Velocity measuring stage performed")

    if FINAL_FILE_CREATION:

        merge_data()  # Merge all the variables into one final file

        print("> Merge data stage performed")


if __name__ == "__main__":
    main()