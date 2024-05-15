import pandas as pd
import pymannkendall as mk
import os
from commons import CASES_PATH, CONTROLS_PATH
from modules import createSerialData, cropProject
import plotly.express as px
from tsanalysis import arimaAnalysis, lmAnalysis


def reportLinearity(commit_output, issue_output, project_name, path, periodicity):
    """
    Reports if the project is included or not based on the output of Kendall's TAU.
    """

    # Creates txt files to add included and non included pros
    if not os.path.exists(os.path.join(f"{path}/serialized_data", f"project_notrend_{periodicity}_MK.txt")):
        with open(os.path.join(f"{path}/serialized_data", f"project_notrend_{periodicity}_MK.txt"), "w") as fp:
            pass
        fp.close()
    if not os.path.exists(os.path.join(f"{path}/serialized_data", f"project_trend_{periodicity}_MK.txt")):
        with open(os.path.join(f"{path}/serialized_data", f"project_trend_{periodicity}_MK.txt"), "w") as fn:
            pass
        fn.close()

    reason = ""
    project = project_name.rstrip(".csv")
    if commit_output[0] is False:
        if commit_output[1] > 0.05:  # p_val: There is no evidence of a trend
            # print(f"Project {project} EXCLUDED: Commit data has NO trend - p_val: {commit_output[1]} > 0.05")
            reason = "notrend"
        elif commit_output[2] < 0:  # negative correlation between T and Y, hence means negative trend
            # print(f"Project {project} EXCLUDED: Commit data has NEG trend - K_tau: {commit_output[2]} < 0")
            reason = "negativetrend"
        elif commit_output[3] < 0:  # We can even check the slope, if it's negative then...
            # print(f"Project {project} EXCLUDED: Commit data has NEG trend - Slope: {commit_output[2]} < 0")
            reason = "negativeslope"

        # Store the project as non valid
        with open(os.path.join(f"{path}/serialized_data", f"project_notrend_{periodicity}_MK.txt"), "a+") as f:
            f.write(f"{project_name}\n")
        f.close()

        return False, "commit", reason

    if issue_output[0] is False:
        if issue_output[1] > 0.05:
            # print(f"Project {project} EXCLUDED: Issue data has NO trend - p_val: {issue_output[1]} > 0.05")
            reason = "notrend"
        elif issue_output[2] < 0:
            # print(f"Project {project} EXCLUDED: Issue data has NEG trend - K_tau: {issue_output[2]} < 0")
            reason = "negativetrend"
        elif issue_output[3] < 0:
            # print(f"Project {project} EXCLUDED: Commit data has NEG trend - Slope: {issue_output[2]} < 0")
            reason = "negativeslope"

        # Store the project as non valid
        with open(os.path.join(f"{path}/serialized_data", f"project_notrend_{periodicity}_MK.txt"), "a+") as f:
            f.write(f"{project_name}\n")
        f.close()

        return False, "issue", reason

    # Means that it passed the trend analysis test
    with open(os.path.join(f"{path}/serialized_data", f"project_trend_{periodicity}_MK.txt"), "a+") as f:
        f.write(f"{project_name}\n")
    f.close()

    return True, project, reason


def makePlot(project_data, data_type, fig_path, linearity_output, periodicity):
    """
    Plots a histogram of the monthly commit density of each project
    """
    # Make directory for the figures
    if not os.path.exists(os.path.join(fig_path, "figures")):
        os.mkdir(os.path.join(fig_path, "figures"))

    os.makedirs(os.path.join(fig_path, f"figures/{periodicity}_commits"), exist_ok=True)  # If it already exists is okay
    os.makedirs(os.path.join(fig_path, f"figures/{periodicity}_issues"), exist_ok=True)

    figure_name = project_data.rstrip(".csv")
    if data_type == f"{periodicity}_commits":

        commit_df = pd.read_csv(os.path.join(f"{fig_path}/serialized_data", f"{data_type}/{project_data}"))
        if linearity_output is False:
            fig = px.line(commit_df, x="date", y="commit_count")
            fig.update_traces(line=dict(color='red'))
            fig.update_layout(title={'text': f"{figure_name}",
                                 "xanchor": "center",
                                 'x': 0.5})
        else:
            fig = px.line(commit_df, x="date", y="commit_count")
            fig.update_layout(title={'text': f"{figure_name}",
                                     "xanchor": "center",
                                     'x': 0.5})

        # Store the figure as pdf
        fig.write_image(os.path.join(fig_path, f"figures/{periodicity}_commits/{figure_name}.pdf"))

    else:
        issue_df = pd.read_csv(os.path.join(f"{fig_path}/serialized_data", f"{data_type}/{project_data}"))
        if linearity_output is False:
            fig = px.line(issue_df, x="date", y="issue_count")
            fig.update_traces(line=dict(color='red'))
            fig.update_layout(title={'text': f"{figure_name}",
                                     "xanchor": "center",
                                     'x': 0.5})
        else:
            fig = px.line(issue_df, x="date", y="issue_count")
            fig.update_layout(title={'text': f"{figure_name}",
                                     "xanchor": "center",
                                     'x': 0.5})

        # Store the figure as pdf
        fig.write_image(os.path.join(fig_path, f"figures/{periodicity}_issues/{figure_name}.pdf"))


def kendallTau(ts_data):
    """
    Tests if the input time series has a trend (Mann-Kendall test), and if so, if it's positive or negative
    (Kendall's TAU test)
    Both tests are non-parametric
    """

    # We converts the Pandas series into a list
    ts_list = ts_data.tolist()
    # The list has 0 observations, hence he do not consider it
    try:
        mk_results = mk.original_test(ts_list)
    except ZeroDivisionError:  # The length of the list is 0 or 1 therefore making the computation mathematically imposible.
        return [False, 0, 0, 0]
    p_val = mk_results.p
    tau_val = mk_results.Tau
    slope_val = mk_results.slope

    if p_val > 0.05:  # We do not reject the null hypothesis, there's no significant trend in the time series data
        return [False, p_val, tau_val, slope_val]
    elif tau_val < 0:  # Time and Y are negatively correlated, hence we can say that there is a decreasing trend.
        return [False, p_val, tau_val, slope_val]
    elif slope_val < 0:  # We additionally check the shape of the trend's slope.
        return [False, p_val, tau_val, slope_val]
    else:
        return [True, p_val, tau_val, slope_val]


def trendAnalysis(project_type, periodicity):
    """
    Function coordinating the trend analysis process
    """

    if project_type == "cases":
        path = CASES_PATH
    else:
        path = CONTROLS_PATH

    # Check which csv files match between commits and issues
    commit_files = os.listdir(os.path.join(path, "commits"))
    issue_files = os.listdir(os.path.join(path, "issues"))
    common_files = [file for file in issue_files if file in commit_files]  # 282 cases do appear to have commits and issues

    # Create specific directory for serialized data and resulting data
    if not os.path.exists(os.path.join(path, "serialized_data")):
        os.mkdir(os.path.join(path, "serialized_data"))
    if not os.path.exists(os.path.join(path, "resulting_data")):
        os.mkdir(os.path.join(path, "resulting_data"))

    # If the periodical data has not been calculated, else we already have it
    if not os.path.exists(os.path.join(f"{path}/serialized_data", f"{periodicity}_commits")):
        os.mkdir(os.path.join(f"{path}/serialized_data", f"{periodicity}_commits"))
        os.mkdir(os.path.join(f"{path}/serialized_data", f"{periodicity}_issues"))

        # For commit data
        createSerialData(data_type="commits", original_files=common_files, path=path, periodicity=periodicity)
        # For issue data
        createSerialData(data_type="issues", original_files=common_files, path=path, periodicity=periodicity)

    # Trend analysis automatized pipeline.
    commits_files = os.listdir(os.path.join(f"{path}/serialized_data", f"{periodicity}_commits"))
    issues_files = os.listdir(os.path.join(f"{path}/serialized_data", f"{periodicity}_issues"))

    discarded_projects = 0
    # Counts for the Mann-Kendall trend test
    non_trend_commits = 0
    non_trend_issues = 0
    negative_tau_commits = 0
    negative_slope_commits = 0
    negative_tau_issues = 0
    negative_slope_issues = 0

    # Counts for the ARIMA and LM model trend
    negative_slope_arima_commits = 0
    negative_slope_arima_issues = 0
    negative_slope_lm_commits = 0
    negative_slope_lm_issues = 0
    valid_arima_projects = 0
    valid_lm_projects = 0

    processed_count = 0
    for period_commits, period_issues in zip(commits_files, issues_files):

        # Crop the monthly time series to fit the time considered by the paper (24 months)
        ts_commits_project = cropProject(project_path=os.path.join(f"{path}/serialized_data", f"{periodicity}_commits"),
                                         project_file=period_commits, periodicity=periodicity)  # 0: flag, 1: subset df
        ts_issues_project = cropProject(project_path=os.path.join(f"{path}/serialized_data", f"{periodicity}_issues"),
                                        project_file=period_issues, periodicity=periodicity)

        # If some project does not have enough data for 2 years of observation, we don't count it
        if (ts_commits_project[0] or ts_issues_project[0]) == False:
            discarded_projects += 1
            continue

        ts_commits_subset = ts_commits_project[1]["commit_count"]
        ts_issues_subset = ts_issues_project[1]["issue_count"]

        # Test linearity for observations
        commit_linearity = kendallTau(ts_data=ts_commits_subset)
        issue_linearity = kendallTau(ts_data=ts_issues_subset)

        # Report linearity ms_analysis_files for commits and issues (returns 0: Boolean, 1: Pro name)
        linearity_output = reportLinearity(commit_output=commit_linearity, issue_output=issue_linearity,
                                           project_name=period_commits, path=path, periodicity=periodicity)

        if linearity_output[0] is False:  # If the project didn't pass the test
            if linearity_output[1] == "commit":
                if linearity_output[2] == "notrend":
                    non_trend_commits += 1
                elif linearity_output[2] == "negativetrend":
                    negative_tau_commits += 1
                elif linearity_output[2] == "negativeslope":
                    negative_slope_commits += 1
            else:
                if linearity_output[2] == "notrend":
                    non_trend_issues += 1
                elif linearity_output[2] == "negativetrend":
                    negative_tau_issues += 1
                elif linearity_output[2] == "negativeslope":
                    negative_slope_issues += 1

        # Visualize the monthly series for all the projects
        makePlot(project_data=period_commits, data_type=f"{periodicity}_commits", fig_path=path,
                 linearity_output=linearity_output[0], periodicity=periodicity)
        makePlot(project_data=period_issues, data_type=f"{periodicity}_issues", fig_path=path,
                 linearity_output=linearity_output[0], periodicity=periodicity)

        # ARIMA trend analysis
        arima_commit_slope = arimaAnalysis(ts_data=ts_commits_subset, stationarity=False, outliers=True, seasonal_period=periodicity)
        arima_issue_slope = arimaAnalysis(ts_data=ts_issues_subset, stationarity=False, outliers=True, seasonal_period=periodicity)

        if arima_commit_slope < 0:
            negative_slope_arima_commits += 1
        elif arima_issue_slope < 0:
            negative_slope_arima_issues += 1
        else:
            valid_arima_projects += 1
            if not os.path.exists(os.path.join(f"{path}/resulting_data", f"arima_resulting_pros_{periodicity}.txt")):
                with open(os.path.join(f"{path}/resulting_data", f"arima_resulting_pros_{periodicity}.txt"), "w") as myf:
                    myf.write(f"{period_commits}\n")
                myf.close()
            else:
                with open(os.path.join(f"{path}/resulting_data", f"arima_resulting_pros_{periodicity}.txt"), "a+") as myf:
                    myf.write(f"{period_commits}\n")
                myf.close()

        # LM trend analysis
        lm_commit_slope = lmAnalysis(ts_data=ts_commits_subset, outliers=True)
        lm_issue_slope = lmAnalysis(ts_data=ts_issues_subset, outliers=True)

        if lm_commit_slope < 0:
            negative_slope_lm_commits += 1
        elif lm_issue_slope < 0:
            negative_slope_lm_issues += 1
        else:
            valid_lm_projects += 1
            if not os.path.exists(os.path.join(f"{path}/resulting_data", f"lm_resulting_pros_{periodicity}.txt")):
                with open(os.path.join(f"{path}/resulting_data", f"lm_resulting_pros_{periodicity}.txt"), "w") as myf:
                    myf.write(f"{period_commits}\n")
                myf.close()
            else:
                with open(os.path.join(f"{path}/resulting_data", f"lm_resulting_pros_{periodicity}.txt"), "a+") as myf:
                    myf.write(f"{period_commits}\n")
                myf.close()

        processed_count += 1
        print(f"PROJECT: {period_commits} completed - INDEX: {processed_count}/{len(commits_files)}")

    print(f"PROJECT TYPE: {project_type} - PERIODICITY: {periodicity}")
    # Report how many projects have been discarded due to lack of 24 months of project history
    print(f"Number of discarded projects due to lack of {periodicity} periodicity project history: {discarded_projects}")
    print(f"Number of included projects with equal or more than {periodicity} periodicity project history: "
          f"{len(common_files) - discarded_projects}")

    print(f"OUTPUT FOR Mann-Kendall Trend Test")
    # Report how many projects have been discarded due to lack of linearity
    print(f"Number of discarded projects due to lack commit trend: {non_trend_commits}")
    print(f"Number of discarded projects due to lack issue trend: {non_trend_issues}")
    print(f"Number of discarded projects due to negative commit trend: {negative_tau_commits}")
    print(f"Number of discarded projects due to negative commit slope: {negative_slope_commits}")
    print(f"Number of discarded projects due to negative issue trend: {negative_tau_issues}")
    print(f"Number of discarded projects due to negative issue slope: {negative_slope_issues}")

    print(f"Number of FINALLY included projects: {len(common_files) - (non_trend_issues + non_trend_commits + discarded_projects + negative_slope_commits + negative_slope_issues + negative_tau_commits + negative_tau_issues)}\n")

    # Report for ARIMA outputs
    print(f"OUTPUT FOR ARIMA MODEL TREND")
    print(f"Number of discarded projects due to negative COMMIT trend: {negative_slope_arima_commits}")
    print(f"Number of discarded projects due to negative ISSUES trend: {negative_slope_arima_issues}")
    print(f"Number of FINALLY included projects: {valid_arima_projects}")

    # Report for LM outputs
    print(f"OUTPUT FOR LM MODEL TREND")
    print(f"Number of discarded projects due to negative COMMIT trend: {negative_slope_lm_commits}")
    print(f"Number of discarded projects due to negative ISSUES trend: {negative_slope_lm_issues}")
    print(f"Number of FINALLY included projects: {valid_lm_projects}")
