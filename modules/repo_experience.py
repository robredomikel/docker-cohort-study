import os
from urllib.parse import urljoin
import requests
import pandas as pd
import time
from datetime import datetime
import smtplib
import re
from csv import writer

DATA_PATH = "C:/OULU/SQpaper_2023/datasets/"
API_PREFIX = "https://api.github.com/"
LOG_PATH = "C:/OULU/SQpaper_2023/codes/log_experience"
github_token = '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'
BEGIN_FOLLOW_UP = datetime.strptime('2021-01-01', '%Y-%m-%d')
END_FOLLOW_UP =datetime.strptime('2023-01-01', '%Y-%m-%d')


def countdown(total):

    while total:
        mins, secs = divmod(total, 60)
        timer = 'REMAINING: {:02d}:{:02d}'.format(mins, secs)
        print(timer, end='\r')
        time.sleep(1)
        total -= 1


def notification(scriptname, jsonLength):

    sender_email = 'mrobredomanero@gmail.com'
    rec_email = 'robredomikel@gmail.com'
    with open('C:/OULU/SQpaper_2023/email_pw.txt', 'r', encoding='utf-8') as file:
        pw = file.readline()
    file.close()
    message = [
        f"From: {scriptname} mrobredomanero@gmail.com",
        "To: Mikel <robredomikel@gmail.com>",
        "Subject: Script notification",
        "",
        f"- {scriptname} FINISHED",
        f"- Length of JSON (N'repos): {jsonLength}"]
    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(sender_email, pw)
    server.sendmail(sender_email, rec_email, "\n".join(message))
    server.quit()


def makeQuery(query, headers, params, repo_name):

    out = requests.get(query, headers=headers, params=params)
    try:
        rate_limit = int(out.headers['X-RateLimit-Remaining'])
    except TypeError:
        rate_limit = 0

    if rate_limit < 10:
        print(f'RAN OUT OF API CALLS - REPO {repo_name}\n')
        countdown(3600)
    if out.status_code == 200:
        out_json = out.json()
        if len(out_json) == 2 and ("errors" or "message") in out_json:
            print(f"Output for repo {repo_name} didn't retrieve list of results!")
            return None
        else:
            return out_json
    else:
        with open(LOG_PATH, '+a', encoding='utf-8') as f:
            f.write(f"API CALL ERROR: status code = {out.status_code} - REPO {repo_name}")
            f.write(query)
        f.close()
        return None


def getBranches(repo_name):

    headers = {'Authorization': f'token {github_token}'}
    extension = f"repos/apache/{repo_name}/branches"
    query = urljoin(API_PREFIX, extension)
    params = {'per_page': 100, 'state': 'all'}
    page = 1
    total_branches = 0
    branch_list = []

    while 1 == 1:

        params['page'] = page
        outcome_json = makeQuery(query, headers, params, repo_name)

        if not outcome_json:  # Empty
            break
        elif outcome_json is None:  # There was an error in the API call.
            break
        else:

            for count, branch in enumerate(outcome_json):
                branch_name = branch.get('name')
                branch_list.append(branch_name)
                print("{}/{} branches collected, branch {} for repo {}".format(total_branches + count+1,
                                                                     total_branches + len(outcome_json),
                                                                     branch_name, repo_name))
            total_branches += len(outcome_json)
        page += 1

    return branch_list


def append_list_as_row(file_name, list_of_elem):

    try:
        with open(file_name, mode='a+', newline='', encoding='utf-8') as file:

            csv_writer = writer(file)
            csv_writer.writerow(list_of_elem)

    except UnicodeDecodeError:
        pass


def getCommits(branch_list, repo_name):

    headers = {'Authorization': f'token {github_token}'}
    extension = f"repos/apache/{repo_name}/commits"
    query = urljoin(API_PREFIX, extension)
    flag = True

    if not os.path.exists(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv")):
        df = pd.DataFrame(columns=['name', 'email', 'commit_time', 'sha', 'login', 'id', 'branch'])
        df.to_csv(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv"),
                  index=False, encoding='utf-8')
        collected_branches = []
        collected_commits = []
    else:
        df = pd.read_csv(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv"), encoding='utf-8')
        collected_branches = list(df['branch'].unique())
        collected_commits = list(df['sha'])

    for count, branch in enumerate(branch_list):
        params = {'per_page': 100, 'state': 'all', 'sha': branch}
        page = 0
        while 1 == 1:
            params['page'] = page
            out_json = makeQuery(query=query, headers=headers, params=params, repo_name=repo_name)
            if not out_json:  # Empty
                break
            elif out_json is None:  # There was an error in the API call.
                break
            else:
                for commit in out_json:

                    author_name = commit['commit']['author']['name']
                    author_email = commit['commit']['author']['email']
                    match = re.search(r'bot', author_email)
                    if match is True:
                        continue
                    commit_date = commit['commit']['author']['date']
                    author_data = commit.get("author", None)
                    if author_data is None:
                        continue
                    author_login = author_data.get("login", None)
                    author_id = author_data.get("id", None)
                    sha = commit.get('sha', None)

                    if branch in collected_branches:
                        if sha in collected_commits:  # This instance has already been collected.
                            continue
                        else:
                            out_list = [author_name, author_email, commit_date, sha, author_login, author_id, branch]
                            df.loc[len(df)] = out_list
                            append_list_as_row(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv"), out_list)
                    else:
                        out_list = [author_name, author_email, commit_date, sha, author_login, author_id, branch]
                        df.loc[len(df)] = out_list
                        append_list_as_row(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv"), out_list)

            print("page {}, for branch {}".format(page+1, branch))
            page += 1

        print("{}/{} BRANCH {} processed, for repo {}".format(count+1, len(branch_list), branch, repo_name))

    df.to_csv(os.path.join(DATA_PATH, f"repoExperience/{repo_name}.csv"), index=False)


def main():

    # Gets a list of repositories by their github repo and analyses their contributor's experience by number of
    # commits in the repository all the branches.
    with open(os.path.join(DATA_PATH, "repositories_list.txt"), 'r', encoding='utf-8') as file:
        repos = file.read().splitlines()
    file.close()
    repos_total = len(repos)

    for count, repo_link in enumerate(repos):

        link_list = repo_link.split("/")
        repo_key = link_list[-1]

        # In the current case, we are only interested in the HADOOP repo
        if repo_key != "hadoop":
            print("{}/{} REPO SKIPPED: {}".format(count+1, repos_total, repo_key))
            continue

        branches = getBranches(repo_key)
        getCommits(branches, repo_key)
        print("{}/{} repos processed - REPO: {}".format(count+1, repos_total, repo_key))


if __name__ == "__main__":
    main()