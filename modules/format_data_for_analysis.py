"""
This script formats the data into the format it is used in the analysis. Outputs a csv file.
Additionally, it prints some statistics on the data if wanted
"""

import pandas as pd
import numpy as np
import subprocess
import os
from dateutil.relativedelta import relativedelta

from commons import DATA_PATH, CASES_PATH, CONTROLS_PATH, TIOBE_LIST, LOG_FILE_PATH
from find_ms_usage import getFirstCommit

INPUT_DATA = "data/projectFinalFile.csv"
OUTPUT_FILE = "data/SQ_cohort_data_in_days.csv"
DEPENDENT_VAR = "end_velocity_mean_days"
INDEPENDENT_VAR = "MS/NonMS"
INCLUDED_COLUMNS = ["full_name", "velocity_mean_end", "velocity_mean_start", "MS/NonMS", "size",
                    "main_language", "n_languages", "creation_year", "n_commits", "n_issues", "n_contributors"]
# PRINTS = {"languages": True, "repo_descriptive_statistics": True, "df_info": True}

NON_PROGRAMMING_LANGUAGES = [
    "Markdown",  # .md
    "Plain Text",  # .txt
    "Makefile",  # Makefile
    "YAML",  # .yaml, .yml
    "JSON",  # .json
    "XML",  # .xml
    "Dockerfile",  # Dockerfile
    "Batchfile",  # .bat (for Windows batch scripts)
    "INI",  # .ini (configuration files)
    "TOML",  # .toml (configuration files)
    "Properties",  # .properties (Java properties files)
    "Shell",
    "Properties File",
    "License",
    "RestructuredText",
    "Extensible Stylesheet Language Transformations",
    "BASH",
    "CMake",
    "AsciiDoc",
    "ClojureScript",
    "gitignore",
    "XML Schema",
    "MSBuild",
    "reStructuredText",
    "ReStructuredText",
    "Autoconf",
    "Systemd",
    "Docker ignore",
    "CSV",
    "SVG",
    "CloudFormation (YAML)",
    "Gemfile",
    "Gherkin Specification",
    "Rakefile",
    "Batch",
    "Powershell",
    "Gradle",
    "Freemarker Template",
    "Bitbucket Pipeline",
    "Arvo",
    "Bazel"
]


def checkCommits(data):
    """
    Checks the number of commits made per project since the start of the project until the start of the follow-up
    period.
    Checks the creation date and classify it.
    Checks the number of contributors until the date of the follow-up start
    """
    case_pros_list = os.listdir(os.path.join(CASES_PATH, "confounders_data"))
    case_pros_clean_list = [name[:-4] for name in case_pros_list]

    contributors_list = []
    creation_years = []
    n_commits_list = []

    for pro in data:
        if pro in case_pros_clean_list:  # It is a case
            path = os.path.join(CASES_PATH, "commits", f"{pro}.csv")
            pro_type = "cases"
        else:
            path = os.path.join(CONTROLS_PATH, "commits", f"{pro}.csv")
            pro_type = "controls"

        df = pd.read_csv(path, header=0)
        project_creation_date = getFirstCommit(project_name=f"{pro}.csv", project_type=pro_type)
        start_follow_up_date = project_creation_date + relativedelta(months=12)

        df.date = pd.to_datetime(df.date, utc=True)
        # We do not consider commits made by bots.
        subset_df = df[df.date.between(project_creation_date, start_follow_up_date) & ~df.author_login.str.contains("bot", case=False)]
        contributors = len(subset_df.author_login.unique())  # Likewise, we have exclude those commits that are made by bots.
        creation_year = project_creation_date.year
        n_commits = len(subset_df)

        contributors_list.append(contributors)
        creation_years.append(creation_year)
        n_commits_list.append(n_commits)

    return n_commits_list, creation_years, contributors_list


def aggregate_lists(series):
    # Use a set to avoid duplicate items, if that's desirable
    all_items = set()
    for items in series:
        all_items.update(items)
    return list(all_items)  # Convert set back to list if order is not important


def is_header_row(row):
    # Assume that a header will not contain purely numeric values and will be strings
    # This function could be enhanced based on the knowledge of what headers are expected.
    return all(cell.isalpha() or not cell.replace('.', '', 1).isdigit() for cell in row)


def read_csv_correctly(file_path):
    # Check the first row and determine if it's a header
    with open(file_path, 'r') as file:
        first_row = file.readline().strip().split(',')
        has_header = is_header_row(first_row)

    if has_header:
        # If the first row is header, read normally
        return pd.read_csv(file_path)
    else:
        # If the first row is not header, read with no header and then fix it
        # Assuming the column names come from the second row
        df = pd.read_csv(file_path, header=None)
        # Set the second row as header
        new_header = df.iloc[0]  # grab the first row for the header
        df = df[1:]  # take the data less the header row
        df.columns = new_header  # set the header row as the df header
        df.reset_index(drop=True, inplace=True)
        return df


def get_all_issues_until_followup(projects_list):
    """
    Gets the list of open and closed issues from the start of the project until the beginning of the follow-up period.
    """

    cases_issues_files = os.listdir(os.path.join(CASES_PATH, "issues"))
    num_issues_list = []
    for count, project in enumerate(projects_list):

        pro_csv_extension = project + ".csv"
        if pro_csv_extension in cases_issues_files:  # It's case project
            issue_pro_file_path = os.path.join(CASES_PATH, "issues", pro_csv_extension)
            pro_type = "cases"
        else:
            issue_pro_file_path = os.path.join(CONTROLS_PATH, "issues", pro_csv_extension)
            pro_type = "controls"

        project_creation_date = getFirstCommit(project_name=f"{project}.csv", project_type=pro_type)
        follow_up_start_date = project_creation_date + relativedelta(months=12)
        df = read_csv_correctly(file_path=issue_pro_file_path)
        df.date = pd.to_datetime(df.created_at, utc=True)
        subset_df = df[df.created_at.between(project_creation_date, follow_up_start_date)]
        n_issues = len(subset_df)
        num_issues_list.append(n_issues)
        print(f"Processed projects for the issue counting: {count}/{len(projects_list)}")

    return num_issues_list


def check_header_cases(df):
    """
    Checks in case C and it's header version exist we should consider them as the same and therefor combine their results.
    """
    lang_names = df.Name.tolist()

    # NOTE: Sorry for this spaghetti code if you are going over this function, but it was done in a moment of extreme lack
    # of time

    # C Header
    if "C" in lang_names and "C Header" in lang_names:
        df['Name'] = df['Name'].replace({'C Header': 'C'})
    elif "C Header" in lang_names and "C" not in lang_names:
        df['Name'] = df['Name'].replace({'C Header': 'C'})
    # C Shell case
    if "C" in lang_names and "C Shell" in lang_names:
        df['Name'] = df['Name'].replace({'C Shell': 'C'})
    elif "C Shell" in lang_names and "C" not in lang_names:
        df['Name'] = df['Name'].replace({'C Shell': 'C'})
    # C++ Header case
    if "C++" in lang_names and "C++ Header" in lang_names:
        df['Name'] = df['Name'].replace({'C++ Header': 'C++'})
    elif "C++ Header" in lang_names and "C++" not in lang_names:
        df['Name'] = df['Name'].replace({'C++ Header': 'C++'})

    # Go Template case
    if "Go" in lang_names and "Go Template" in lang_names:
        df['Name'] = df['Name'].replace({'Go Template': 'Go'})
    elif "Go Template" in lang_names and "Go" not in lang_names:
        df['Name'] = df['Name'].replace({'Go Template': 'Go'})

    # Blade Template case
    if "Blade" in lang_names and "Blade template" in lang_names:
        df['Name'] = df['Name'].replace({'Blade template': 'Blade'})
    elif "Blade template" in lang_names and "Blade" not in lang_names:
        df['Name'] = df['Name'].replace({'Blade template': 'Blade'})

    # Mako case
    if "Python" in lang_names and "Mako" in lang_names:
        df['Name'] = df['Name'].replace({'Mako': 'Python'})
    elif "Mako" in lang_names and "Python" not in lang_names:
        df['Name'] = df['Name'].replace({'Mako': 'Python'})

    # Jinja case
    if "Python" in lang_names and "Jinja" in lang_names:
        df['Name'] = df['Name'].replace({'Jinja': 'Python'})
    elif "Jinja" in lang_names and "Python" not in lang_names:
        df['Name'] = df['Name'].replace({'Jinja': 'Python'})

    grouped = df.groupby('Name')
    df = grouped.agg({
        'Bytes': 'sum',
        'CodeBytes': 'sum',
        'Lines': 'sum',
        'Code': 'sum',
        'Comment': 'sum',
        'Blank': 'sum',
        'Complexity': 'sum',
        'Count': 'sum',
        'WeightedComplexity': 'sum',
    })
    df.reset_index(inplace=True)
    return df


def get_project_size(data):
    """
    Gets the size of the project based on lines of Code from used programming languages within the TIOBE index
    """

    subset_df = data[data['Name'].str.lower() in TIOBE_LIST & data['Name'] not in NON_PROGRAMMING_LANGUAGES]
    size = subset_df.Code.sum()
    return size


def checkLanguages(data):
    """
    Checks the number of programming languages the project has at the beginning of the follow-up period.
    Checks the principal programming language the project has at the beginning of the follow-up period.
    Checks the size of the project in terms of lines of code.
    """

    n_languages = []
    programmming_language = []
    sizes = []

    case_pros_list = os.listdir(os.path.join(CASES_PATH, "confounders_data"))
    case_pros_clean_list = [name[:-4] for name in case_pros_list]
    # controls_pros_list = os.listdir(os.path.join(CONTROLS_PATH, "confounders_data"))
    # controls_pros_clean_list = [name[:-4] for name in case_pros_list]
    for pro in data:
        if pro in case_pros_clean_list:  # Is a case.
            path = os.path.join(CASES_PATH, "confounders_data", f"{pro}.csv")

        else:
            path = os.path.join(CONTROLS_PATH, "confounders_data", f"{pro}.csv")

        df = pd.read_csv(path)

        # Control for Header cases in C and C++ programming languages
        df = check_header_cases(df=df)

        # Includes purely programming languages.
        languages = [lang for lang in df.Name.tolist() if lang not in NON_PROGRAMMING_LANGUAGES]

        for i in range(len(df)):
            if df.Name[i] in languages:
                prog_lang = df.Name[i]  # Most used language
                break

        size = get_project_size(data=df)

        # OLD FORMAT: df[df["Name"] == "Total"]["Code"].values[0]  # Total value of Code column
        n_languages.append(len(languages))
        if prog_lang == "Total":
            programmming_language.append(None)
        else:
            programmming_language.append(prog_lang)
        sizes.append(size)

    return programmming_language, n_languages, sizes


def check_trimmed_languages(data):
    """
    Checks the languages listed in the TIOBE index existing in the project folder
    """
    n_languages = []
    case_pros_list = os.listdir(os.path.join(CASES_PATH, "confounders_data"))
    case_pros_clean_list = [name[:-4] for name in case_pros_list]

    for pro in data:
        if pro in case_pros_clean_list:  # Is a case.
            path = os.path.join(CASES_PATH, "confounders_data", f"{pro}.csv")

        else:
            path = os.path.join(CONTROLS_PATH, "confounders_data", f"{pro}.csv")

        df = pd.read_csv(path)

        # Control for Header cases in C and C++ programming languages
        df = check_header_cases(df=df)

        # Includes purely programming languages.
        languages = [lang for lang in df.Name.tolist() if lang not in NON_PROGRAMMING_LANGUAGES]
        # Check if these languages exist in the TIOBE INDEX
        languages_clean = [lang for lang in languages if lang.lower() not in TIOBE_LIST]
        n_languages.append(len(languages_clean))

    return n_languages


def timedelta_to_days_decimal(data, n_decimals=3):
    """
    The column is originally saved as timedelta (e.g. 14 days 19:47:42.594827586).
    This function converts it to decimal number (e.g. 14.792)
    :param data: Column of the data to be transformed
    :param n_decimals: Number of decimals used to save the data
    :return: The transformed data
    """
    timedelta_data = pd.to_timedelta(data)
    return timedelta_data.apply(lambda x: round(x.days + x.components.hours/24, n_decimals) if pd.notnull(x) else None)
    # return timedelta_data.apply(lambda x: round(x.days + x.components.hours/24, n_decimals) if x.days >= 0 else
    # round(x.days - x.components.hours/24, n_decimals)))


def checkExposure(data):
    """
    Checks whether the project is a case or a control
    """

    case_pros_list = os.listdir(os.path.join(CASES_PATH, "confounders_data"))
    case_pros_clean_list = [name[:-4] for name in case_pros_list]
    ms_nonms_list = ["MS" if pro in case_pros_clean_list else "~MS" for pro in data]

    return ms_nonms_list


def checkMissingValues(data_df):
    """
    Checks which projects have NaT values in the velocity calculations
    """

    velocity_start_subset = data_df.loc[data_df['velocity_mean_start'].isnull()]['full_name']
    velocity_end_subset = data_df.loc[data_df['velocity_mean_end'].isnull()]['full_name']
    main_language_subset = data_df.loc[data_df['main_language'].isnull()]['full_name']
    all_names = pd.concat([velocity_start_subset, velocity_end_subset, main_language_subset])
    missing_values_pros = all_names.unique()
    cases = 0
    controls = 0
    missing_values_indices = []

    for pro_name in missing_values_pros:
        pro_index = data_df.index[data_df['full_name'] == pro_name]
        pro_type = data_df.iloc[pro_index, 3].values[0]  # Column number of "MS/NonMS"
        missing_values_indices.append(pro_index.values[0])
        if pro_type == "MS":
            cases += 1
        else:
            controls += 1

    df = data_df.drop(missing_values_indices)

    return len(velocity_start_subset), len(velocity_end_subset), len(missing_values_pros), cases, controls, df


def merge_data():
    """
    Merge process of the data based on the order of projest names in the velocity_results file
    """

    features_dict = {key: None for key in INCLUDED_COLUMNS}
    # Get all the project names
    velocity_data_df = pd.read_csv(os.path.join(DATA_PATH, "velocity_results.csv"))
    features_dict['full_name'] = velocity_data_df.full_name.tolist()
    features_dict['velocity_mean_end'] = timedelta_to_days_decimal(data=velocity_data_df.end_mean, n_decimals=3).tolist()
    features_dict['velocity_mean_start'] = timedelta_to_days_decimal(data=velocity_data_df.start_mean_velocity, n_decimals=3).tolist()
    features_dict['n_issues'] = get_all_issues_until_followup(projects_list=velocity_data_df.full_name.tolist())
    # OLD IMPLEMENTATIONvelocity_data_df["n_closed_issues_start"] + velocity_data_df["open_issues_at_start"])
    features_dict['MS/NonMS'] = checkExposure(data=features_dict['full_name'])
    prog_language, languages, size = checkLanguages(data=features_dict['full_name'])
    n_commits, creation_date, contributors = checkCommits(data=features_dict['full_name'])
    features_dict['main_language'] = prog_language
    features_dict['n_languages'] = languages
    features_dict['trimmed_languages'] = check_trimmed_languages(data=features_dict['full_name'])
    features_dict['size'] = size
    features_dict['n_commits'] = n_commits
    features_dict['creation_year'] =  creation_date
    features_dict['n_contributors'] = contributors

    df = pd.DataFrame.from_dict(features_dict)
    missing_vals_start, missing_vals_end, pros_affected, cases, controls, df = checkMissingValues(data_df=df)
    df.to_csv(os.path.join(DATA_PATH, "final_data_file.csv"), index=False)

    print(f"Number of projects for commits: {len(features_dict['n_commits'])}")
    print(f"Number of projects for names: {len(features_dict['full_name'])}")
    print(f"Number of projects for velocity end: {len(features_dict['velocity_mean_end'])}")
    print(f"Number of projects for velocity start: {len(features_dict['velocity_mean_start'])}")
    print(f"Number of projects for issues: {len(features_dict['n_issues'])}")
    print(f"Number of projects for languages: {len(features_dict['main_language'])}")
    print(f"Number of projects for n_languages: {len(features_dict['n_languages'])}")
    print(f"Number of projects for size: {len(features_dict['size'])}")
    print(f"Number of projects for creation year: {len(features_dict['creation_year'])}")
    print(f"Number of projects for n of contributors: {len(features_dict['n_contributors'])}")
    print("-------------------------------------------------")
    print(f"Number of missing observations in start velocity: {missing_vals_start}")
    print(f"Number of missing observations in end velocity: {missing_vals_end}")
    print(f"Number of affected projects: {pros_affected}")
    print(f"Number of affected cases: {cases}")
    print(f"Number of affected projects: {controls}")
