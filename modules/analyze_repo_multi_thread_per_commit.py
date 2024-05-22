import concurrent.futures
import datetime
import getopt
import smtplib
import sys
import traceback
import pandas as pd
from dateutil.relativedelta import relativedelta

import git
from os import path
from pathlib import Path
import os
import dockerfile
from collections import Counter
import nltk
import pandas as pd
from tqdm import tqdm
import pydriller as pydrill

# nltk.download('punkt')
import string
import subprocess
import json
import shutil
import yaml
from filelock import Timeout, FileLock
import networkx as nx
from threading import Lock
from commons import PROJECT_PATH, CASES_PATH, DATA_PATH

# Global variables for path definitions



with open(PROJECT_PATH + '/consts/db.csv') as db_file:
    dbs = [db.lower() for db in db_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/db-2.csv') as db_file:
    dbs += [db.lower() for db in db_file.read().splitlines()]
dbs = list(set(dbs))
with open(PROJECT_PATH + '/consts/bus.csv') as bus_file:
    buses = [bus.lower() for bus in bus_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/lang.csv') as lang_file:
    langs = [lang.lower() for lang in lang_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/server.csv') as server_file:
    servers = [server.lower() for server in server_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/gateway.csv') as gate_file:
    gates = [gate.lower() for gate in gate_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/monitor.csv') as monitor_file:
    monitors = [monitor.lower() for monitor in monitor_file.read().splitlines()]
with open(PROJECT_PATH + '/consts/discovery.csv') as disco_file:
    discos = [disco.lower() for disco in disco_file.read().splitlines()]


DATA = {
    'dbs': dbs, 'servers': servers, 'buses': buses, 'langs': langs, 'gates': gates, 'monitors': monitors,
    'discos': discos
}

LOG_FILES = {}
def are_similar(name, candidate):
    return name == candidate


def match_one(name, l):
    for candidate in l:
        if are_similar(name, candidate):
            return [candidate]
    return []


def match_alls(names, l):
    alls = set()
    for name in names:
        alls.update(match_one(name, l))
    return list(alls)


def match_ones(names, l):
    for name in names:
        res = match_one(name, l)
        if res:
            return res
    return []


def clone(repo_url, full_repo_name, wlock):
    #full_repo_name = full_repo_name.replace("_", "/")
    parts = full_repo_name.split('/')
    if len(parts) != 2:
        return None
    username, repo_name = full_repo_name.split('/')
    workdir = path.join("/home/mikel/projects/docker_cohort/microservices_analyzer/temp", username)
    full_workdir = workdir + f"-{repo_name}"

    if not path.exists(full_workdir):
        Path(full_workdir).mkdir(parents=True, exist_ok=True)
        print('-cloning repo')
        endpoint = 'https://api.github.com/repos/%s' % (full_repo_name,)
        p1 = subprocess.run(['curl', endpoint], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,)
        data = json.loads(p1.stdout.decode("utf-8"))
        # if 'size' not in data or data['size'] < 512000:  # Removing the size condition for this small sample of projects
        # if 'size' not in data:
        try:
            # force SSH protocol
            #repo_url = repo_url.replace("git://github.com/", "git@github.com:")
            # repo_url = repo_url.replace("https://github.com/", "git@github.com:")
            print("--repo_url", repo_url)
            git.Git(full_workdir).clone(repo_url)
        except Exception as e:
            with open(LOG_FILES['errors_on_cloning'], 'a') as f:
                f.write(repo_url + '\n')
            with wlock:
                with open(LOG_FILES['num_errors'], 'r') as f:
                    errors = int(f.read())
                with open(LOG_FILES['num_errors'], 'w') as f:
                    f.write(str(errors + 1))
            print("cloning repo exception", e)
            return None
        """
        else:
            print('repo too big')
            return None
        """
    else:
        print('repo already cloned')

    return path.join(full_workdir, repo_name)


def locate_files(workdir, filename):
    print('-locating ', filename)
    res = []
    try:
        for df in Path(workdir).rglob(filename):
            if not df.is_file():
                continue
            df = str(df)
            res.append(df.split(workdir)[-1])
    except OSError:
        pass
    return res


def get_words(data, unique=False):
    data = data.translate(str.maketrans(string.punctuation, ' ' * len(string.punctuation)))
    data = data.translate(str.maketrans(string.digits, ' ' * len(string.digits)))
    data = data.lower()
    words = [w for w in nltk.word_tokenize(data) if len(w) > 2]
    if unique:
        words = set(words)
    return words


def keywords(data, n=5):
    words = get_words(data)
    counter = Counter(words)
    most_commons = [x[0] for x in counter.most_common(n)]
    return most_commons


def analyze_languages(workdir):
    # print('-analyzing languages')
    result = subprocess.run(['github-linguist --json'], stdout=subprocess.PIPE, shell=True, cwd=workdir)
    output = result.stdout.decode("utf-8")
    dict_langs = json.loads(output)
    languages_list = [lang.lower() for lang in dict_langs if float(dict_langs[lang]['percentage']) > 10]
    return languages_list


def analyze_dockerfile(workdir, df):
    print('-analyzing dockerfile', df)
    analysis = {'path': df, 'cmd': '', 'cmd_keywords': [], 'from': ''}
    try:
        commands = dockerfile.parse_file(workdir + df)
        runs = ''
        for command in commands:
            if command.cmd.lower() == 'from' and command.value:
                analysis['from'] = command.value[0].split(':')[0]
                analysis['from_full'] = command.value[0]
            if command.cmd.lower() == 'run':
                runs += '%s ' % (' '.join(command.value),)
            if command.cmd.lower() == 'cmd':
                analysis['cmd'] = ' '.join(command.value)
                analysis['cmd_keywords'] = keywords(analysis['cmd'])
            analysis['keywords'] = keywords(runs)
        if 'from' in analysis:
            for k, v in DATA.items():
                analysis[k] = match_one(analysis['from'], v) \
                              or match_ones(get_words(analysis['from']), v) \
                              or match_ones(get_words(analysis['cmd']), v) \
                              or match_ones(get_words(runs), v)
    except dockerfile.GoParseError as e:
        pass
        # print(e)
    return analysis


def analyze_file(workdir, f):
    # print('-analyzing file', f)
    analysis = {'path': f}
    try:
        with open(workdir + f) as fl:
            data = ' '.join(fl.read().splitlines())
            for k, v in DATA.items():
                if k == 'langs':
                    continue
                analysis[k] = match_alls(get_words(data), v)
    except UnicodeDecodeError as e:
        pass
        # print(e)
    return analysis


def check_shared_db(analysis):
    db_services = set(analysis['detected_dbs']['services'])
    dependencies = []
    for service in analysis['services']:
        dependencies += set(service['depends_on']) & db_services
    return len(set(dependencies)) != len(dependencies)


def committers(workdir):
    try:
        result = subprocess.run(['git', '--git-dir', os.path.join(workdir, '.git'), 'shortlog', '-s'],
                                stdout=subprocess.PIPE, timeout=5)
        output = result.stdout.decode("utf-8")
        return len(output.splitlines())
    except:
        return 0


def analyze_docker_compose(workdir, dc):
    print('-analyzing docker-compose')
    dep_graphs = {'full': nx.DiGraph(), 'micro': None}
    nodes_not_microservice = []
    analysis = {'path': dc, 'num_services': 0, 'services': [],
                'detected_dbs': {'num': 0, 'names': [], 'services': [], 'shared_dbs': False}}
    with open(workdir + dc) as f:
        try:
            data = yaml.load(f, Loader=yaml.FullLoader)
            services = []
            detected_dbs = []
            if not data or 'services' not in data or not data['services']:
                return analysis
            for name, service in data['services'].items():
                if not service:
                    continue
                s = {}
                s['name'] = name
                if 'image' in service and service['image']:
                    s['image'] = service['image'].split(':')[0]
                    s['image_full'] = service['image']
                elif 'build' in service and service['build']:
                    s['image'] = s['image_full'] = service['build']
                else:
                    s['image'] = s['image_full'] = ''
                if isinstance(s['image'], dict):
                    s['image'] = s['image_full'] = str(list(s['image'].values())[0])

                for k, v in DATA.items():
                    if k == 'langs':
                        continue
                    s[k] = match_ones(get_words(s['image']), v)

                if s['dbs']:
                    detected_dbs.append({'service': name, 'name': s['dbs'][0]})

                if 'depends_on' in service:
                    if isinstance(service['depends_on'], dict):
                        s['depends_on'] = list(service['depends_on'].keys())
                    else:
                        s['depends_on'] = service['depends_on']
                elif 'links' in service:
                    s['depends_on'] = list(service['links'])
                else:
                    s['depends_on'] = []

                if s['depends_on'] is None:
                    s['depends_on'] = []
                services.append(s)

                # add the node to the dependencies graph
                dep_graphs['full'].add_node(name)
                # add the edges to the dependencies graph
                dep_graphs['full'].add_edges_from([(name, serv) for serv in s['depends_on']])
                # append the node to the nodes_not_microservice list if the node is not a microservice
                if s['dbs'] or s['servers'] or s['buses'] or s['gates'] or s['monitors'] or s['discos']:
                    nodes_not_microservice.append(name)
            analysis['services'] = services
            analysis['num_services'] = len(services)
            analysis['detected_dbs'] = {'num': len(detected_dbs), \
                                        'names': list({db['name'] for db in detected_dbs}), \
                                        'services': [db['service'] for db in detected_dbs]}
            analysis['detected_dbs']['shared_dbs'] = check_shared_db(analysis)

            # copy the full graph
            dep_graphs['micro'] = dep_graphs['full'].copy()
            # delete the not-microservice nodes from the micro dependencies graph
            for node in nodes_not_microservice:
                dep_graphs['micro'].remove_node(node)
            for g in dep_graphs:
                analysis['dep_graph_' + g] = {'nodes': dep_graphs[g].number_of_nodes(),
                                              'edges': dep_graphs[g].number_of_edges(),
                                              'avg_deps_per_service': sum(
                                                  [out_deg for name, out_deg in dep_graphs[g].out_degree]) / dep_graphs[
                                                                          g].number_of_nodes() if dep_graphs[
                                                                                                      g].number_of_nodes() != 0 else 0,
                                              'acyclic': nx.is_directed_acyclic_graph(dep_graphs[g]),
                                              'longest_path': nx.dag_longest_path_length(dep_graphs[g])}

            analysis["number_of_microservices"] = analysis['dep_graph_micro']['nodes']

        except (UnicodeDecodeError, yaml.parser.ParserError, yaml.scanner.ScannerError) as e:
            print(e)
            pass

    return analysis


def compute_size(workdir):
    try:
        root_directory = Path(workdir)
        return sum(
            f.stat().st_size for f in root_directory.glob('**/*') if f.is_file() and '.git' not in f.parts) // 1000
    except:
        return 0


def synthetize_data(analysis):
    keys = DATA.keys()

    def add_data(data):
        for d in data:
            for k in keys:
                if k in d:
                    analysis[k].update(d[k])

    for k in keys:
        analysis[k] = set()

    add_data(analysis['files'])
    add_data(analysis['structure']['services'])
    add_data(analysis['dockers'])
    analysis['num_services'] = analysis['structure']['num_services']
    analysis['shared_dbs'] = analysis['structure']['detected_dbs']['shared_dbs']
    analysis['langs'].update(analysis.get('languages', set()))
    analysis['num_dockers'] = len(analysis['dockers'])
    analysis['images'] = list({s['from'] for s in analysis['dockers'] if s['from']})
    for db in set(analysis['dbs']):
        if 'db' == db[-2:]:
            analysis['dbs'].discard(db)
            analysis['dbs'].add(db[-2:])

    if len(analysis['dbs']) > 1:
        analysis['dbs'].discard('db')
    if len(analysis['gates']) > 1:
        analysis['gates'].discard('gateway')
    if len(analysis['monitors']) > 1:
        analysis['monitors'].discard('monitoring')
    if len(analysis['buses']) > 1:
        analysis['buses'].discard('bus')

    for k in keys:
        analysis['num_%s' % (k,)] = len(analysis[k])
        analysis[k] = list(analysis[k])
    analysis['num_dockers'] = len(analysis['dockers'])
    analysis['num_files'] = analysis['num_dockers'] + len(analysis['files']) + 1
    # We don't look for this observation
    # analysis['avg_size_service'] = analysis['size'] / max(analysis['num_dockers'], 1)


def get_followup_dates(project_name):
    """
    The project name comes in [user]/[repo] format. We transform it and get the commits in which
    the Dockerfile and docker-compose files were modified.
    """

    project_csv_path = project_name + ".csv"
    df = pd.read_csv(os.path.join(CASES_PATH, "commits", project_csv_path))
    df['date'] = pd.to_datetime(df['date'])
    first_commit_project = df['date'].min()

    # Follow-up period margins
    start_follow_up_date = first_commit_project + relativedelta(months=9)
    end_follow_up_date = start_follow_up_date + relativedelta(months=15)

    return start_follow_up_date, end_follow_up_date


def get_ms_usage_commits(project_name, start_date, end_date):
    """
    From an already existing list of commits where it has been proven that there are modifications in
    both Dockerfile or docker-compose, it processes the commits occurred during the follow-up period for
    further analysis.

    return: List of lists where each component inner list is composed by [0] hash; [1] datetime; [2] file changed
    """

    output_list = []
    project_file = project_name + ".csv"
    ms_usage_file_path = os.path.join(CASES_PATH, "commits", project_file)
    file_df = pd.read_csv(ms_usage_file_path, delimiter=",")
    commit_list = file_df['sha'].tolist()
    datetime_list = file_df['date'].tolist()
    for commit, datetime in zip(commit_list, datetime_list):  # Last item is the empty line at the end of the text
        #print(commit)
        # data_content = commit.split(";") # [0] hash; [1] datetime; [2] file changed
        if start_date < pd.to_datetime(datetime) < end_date:
            output_list.append((commit, datetime))

    return output_list


def get_default_branch(repo_path, wlock):
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
        with wlock:
            with open(LOG_FILES["generic_error"], 'a') as f:
                f.write(str("Failed to get default branch information") + ";" + repo_path + '\n')


def analyze_repo(url, wlock, project_id=None):
    lockfile = "temp/%s.lock" % (''.join(get_words(url)),)
    lock = FileLock(lockfile, timeout=0.01)
    workdir = None
    try:
        with lock:
            analysis = {'url': url}
            # analysis['name'] = url.split('.git')[0].split('git://github.com/')[-1]
            # analysis['name'] = url.split("https://github.com/")[-1]
            if project_id is None:
                analysis['name'] = url.split("https://github.com/")[-1]
            else:
                analysis['name'] = project_id
            print('analyzing', analysis['name'])

            # Check if the specific folder for the case project is created within results
            # outfile = path.join('results', analysis['name'].replace('/', '#').replace('_', '#',1))
            # outfile = "%s.json" % (outfile,)
            outdir = path.join('results', analysis['name'].replace('/', '#').replace('_', '#',1))
            project_rels_dir_path = os.path.join(CASES_PATH, outdir)

            if not path.exists(project_rels_dir_path):  # If the directory with the results doesn't exist
                os.mkdir(project_rels_dir_path)
                workdir = clone(url, analysis['name'], wlock)
                if not workdir:
                    return
                # analysis['commiters'] = committers(workdir)
                # analysis['size'] = compute_size(workdir)
                # analysis['languages'] = analyze_languages(workdir)
                # print("Language analysis completed")

                ####### Starting the commitwise analysis stage:
                pro_name = analysis['name'].replace('/', '#') # .replace('_', '#',1)
                start_follow_up_date, end_follow_up_date = get_followup_dates(project_name=pro_name) # Follow-up date limits
                ms_usage_commits = get_ms_usage_commits(project_name=pro_name,
                                                        start_date=start_follow_up_date,
                                                        end_date=end_follow_up_date) # Commits with ms activity within the window

                # Parse the stage of the project within commits where MS activity was identified.
                gr = pydrill.Git(workdir)

                # Get the default branch
                default_branch = get_default_branch(repo_path=workdir, wlock=wlock)

                for count, commit in enumerate(ms_usage_commits):
                    hash = commit[0]
                    commit_date = commit[1]
                    # modified_file = commit[2]

                    # CHECK: We will only consider those commits in the MAIN branch
                    try:
                        branches = gr.get_commit(hash).branches
                        if default_branch in branches:
                            print(f"--Commit {hash} is in DEFAULT branch")
                            pass
                        else:
                            print(f"--Commit {hash} is NOT in DEFAULT branch")
                            continue
                    except Exception as e:
                        print(gr.project_name, ": could not checkout back to commit in DEFAULT")
                        with wlock:
                            with open(LOG_FILES["generic_error"], 'a') as f:
                                f.write(str(e) + ";" + gr.project_name + '\n')

                    print(f"--Checking out to commit {hash}")
                    gr.checkout(hash)

                    dfs = locate_files(workdir, 'Dockerfile')
                    dockers_analysis = []
                    for df in dfs:
                        dockers_analysis.append(analyze_dockerfile(workdir, df))
                    analysis['dockers'] = dockers_analysis
                    dc = locate_files(workdir, 'docker-compose.yml')
                    analysis['structure'] = {'path': dc, 'num_services': 0, 'services': [],
                                             'detected_dbs': {'num': 0, 'names': [], 'services': [], 'shared_dbs': False}}
                    if len(dc):
                        dc = dc[0]
                        analysis['structure'] = analyze_docker_compose(workdir, dc)

                    fs = locate_files(workdir, 'requirements.txt')
                    fs += locate_files(workdir, '*.gradle')
                    fs += locate_files(workdir, 'pom.xml')
                    fs += locate_files(workdir, 'package.json')

                    file_analysis = []
                    for f in fs:
                        file_analysis.append(analyze_file(workdir, f))
                    analysis['files'] = file_analysis
                    synthetize_data(analysis)

                    # Store the analysis
                    outfile = os.path.join(project_rels_dir_path, str(commit_date))
                    outfile = "%s.json" % (outfile,)
                    with open(outfile, 'w', encoding='utf-8') as file:
                        analysis = remove_invalid_char(analysis)
                        json.dump(analysis, file, ensure_ascii=False, indent=4)

                    # Checkout back to master
                    try:
                        print(f"--Checking out back to {default_branch}")
                        gr.checkout(default_branch)
                    except Exception as e:
                        print(gr.project_name, ": could not checkout back to MASTER")
                        with wlock:
                            with open(LOG_FILES["generic_error"], 'a') as f:
                                f.write(str(e) + ";" + gr.project_name + '\n')

                    print(f"--Analyzing project: {pro_name}, hash: {hash}, date: {commit_date} - {count+1}/{len(ms_usage_commits)}")

                # Delete the Repo once all the analysis is done
                shutil.rmtree(path.dirname(workdir))
            # else:
            # print('skipped')
    # except Timeout:
    # print('in progress')
    # except FileNotFoundError as e:
    # print('FileNotFoundError skipped')
    #     with open('errors.txt', 'a') as f:
    #         f.write(str(e) + ";" + url + '\n')
    except Exception as e:
        print('Error, continuing...', e)
        with wlock:
            with open(LOG_FILES["generic_error"], 'a') as f:
                f.write(str(traceback.format_exc()) + ";" + url + '\n')
    # finally:
    #     # print(workdir)


def remove_invalid_char(d):
    if isinstance(d, str):
        return d.encode('utf-16', 'surrogatepass').decode('utf-16')
    if isinstance(d, dict):
        for k, v in d.items():
            d[k] = remove_invalid_char(v)
    elif isinstance(d, list) or isinstance(d, set) or isinstance(d, tuple):
        for i, v in enumerate(list(d)):
            d[i] = remove_invalid_char(v)
    return d


URL_PREFIXES = {
    "bitbucket.org": "bitbucket.org",
    "gitlab.com": "gitlab.com",
    "android.googlesource.com": "android.googlesource.com",
    "bioconductor.org": "bioconductor.org",
    "drupal.com": "git.drupal.org",
    "git.eclipse.org": "git.eclipse.org",
    "git.kernel.org": "git.kernel.org",
    "git.postgresql.org": "git.postgresql.org",
    "git.savannah.gnu.org": "git.savannah.gnu.org",
    "git.zx2c4.com": "git.zx2c4.com",
    "gitlab.gnome.org": "gitlab.gnome.org",
    "kde.org": "anongit.kde.org",
    "repo.or.cz": "repo.or.cz",
    "salsa.debian.org": "salsa.debian.org",
    "sourceforge.net": "git.code.sf.net/p"
}

def url(project_id):

    """ Get the URL for a given project URI
    Project('CS340-19_lectures').toURL()
    'http://github.com/CS340-19/lectures'
    """
    chunks = project_id.split("_", 1)
    prefix = chunks[0]
    if (len(chunks) > 2 or prefix == "sourceforge.net") and prefix in URL_PREFIXES:
        platform = URL_PREFIXES [prefix]
    else:
        platform = '/'.join ( ['github.com', chunks[0]])
    try:
        res = '/'.join( [ 'https:/', platform, chunks[1] ] )
    except IndexError:
        res = '/'.join(['https:/', platform ])
        with open(LOG_FILES['probably_invalid_url'], 'a') as f:
            f.write(f"{res},{project_id}" + '\n')
    if (len (chunks) > 2): res = '/' .join ( [res, '_'.join(chunks[2:])] )
    return res


def analyze_all(max_workers=None, fix_errors=False, debug=False):
    content = ""
    try:
        repo_dir = os.path.join(CASES_PATH, "repos")
        repos = Path(repo_dir).glob('*.csv')
        repos = sorted([str(x) for x in repos])
        os.makedirs('temp', exist_ok=True)
        analyzed = os.listdir(os.path.join(CASES_PATH, 'results'))
        if fix_errors:
            analyzed = [x.replace('https://github.com/', '').replace("#", "_", 1).replace(".json", "") for x in analyzed]

        for source in repos:
            data = pd.read_csv(source, sep=',', encoding='utf-8')
            if "P.U.csv" in source:
                data = data[~data['ProjectID'].isin(analyzed)]

                # data['ProjectID'] = [x for x in data['ProjectID'] if x not in analyzed]
                data['URL'] = data['ProjectID'].apply(lambda x: url(x))
            data = data[data['URL'].str.contains("github")]
            if debug:
                data = data.head(10)
                max_workers = 1
            with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
                writer_lock = Lock()
                if data.get("ProjectID", None) is None:
                    _ = list(tqdm(executor.map(analyze_repo, data["URL"],
                                               [writer_lock] * len(data["URL"])), total=len(data["URL"])))
                else:
                    #data['ProjectID'] = data['ProjectID'].apply(lambda x: 'https://github.com/' + str(x).replace("_", "/", 1))
                    _ = list(tqdm(executor.map(analyze_repo, data["URL"], [writer_lock]*len(data["Identifier"]), data["Identifier"]), total=len(data["Identifier"])))
                # for repo in data["ProjectID"]:
                #     repo = "https://github.com/" + repo.replace("_", "/", 1)
                #
                #     _ = executor.submit(analyze_repo, repo, writer_lock)

    except Exception as e:
        number_of_analyzed_project = len(os.listdir(os.path.join(CASES_PATH, 'results')))
        errors = open(LOG_FILES["num_errors"], 'r').read()
        content = f'Subject: MS DATASET\n\nTHE PROCESS IS INTERRUPTED DUE TO THE FOLLOWING ERROR:\n{e}\n{traceback.format_exc()}\n\t- Analyzed projects: {number_of_analyzed_project}\n\t- Missing projects: {412030 - number_of_analyzed_project - int(errors)}\n\t- Error projects: {int(errors)}'
        with open(LOG_FILES["generic_error"], 'a') as f:
            f.write(str(e) + '\n')
    else:
        number_of_analyzed_project = len(os.listdir(os.path.join(CASES_PATH, 'results')))
        content = f"Subject: MS DATASET\n\nTHE PROCESS IS COMPLETED:\n\t- Analyzed project: {number_of_analyzed_project}"

    finally:

        # send_email_notification(content)
        print(content)


'''
def send_email_notification(content):

    mail = smtplib.SMTP('smtp.gmail.com',587)


    mail.ehlo()


    mail.starttls()

    #TODO: change the email and password (see https://support.google.com/accounts/answer/185833?hl=en)
    mail.login('USER','PSWD')


    mail.sendmail('MAIL_FROM','MAIL_TO',content)


    mail.close()
'''

def create_log_file():
    date = str(datetime.datetime.now()).replace(":", "-").replace(" ", "-").split(".")[0]
    os.makedirs(f"logs/{date}", exist_ok=True)
    LOG_FILES["generic_error"] = f"logs/{date}/generic_error.txt"
    LOG_FILES["num_errors"] = f"logs/{date}/num_errors.txt"
    LOG_FILES["probably_invalid_url"] = f"logs/{date}/probably_invalid_url.txt"
    LOG_FILES["errors_on_cloning"] = f"logs/{date}/errors_on_cloning.txt"

    open(f"logs/{date}/generic_error.txt", "w")
    with open(f"logs/{date}/num_errors.txt", "w") as f:
        f.write("0")
    open(f"logs/{date}/probably_invalid_url.txt", "w")
    open(f"logs/{date}/errors_on_cloning.txt", "w")


def main_micro():

    fix_errors = False
    debug = True
    num_workers = None
    if not os.path.exists(os.path.join(CASES_PATH, "results")):
        os.mkdir(os.path.join(CASES_PATH, "results")) 
    """
    if len(argv) > 1:
        opts, args = getopt.getopt(argv,"fdw:")
        for opt, arg in opts:
            if opt == '-f':
                fix_errors = True
            if opt == '-d':
                debug = True
            if opt == '-w':
                num_workers = int(arg)
    """
    create_log_file()
    # I set for debug purposes the number of threads as 1 and the debug mode as True
    analyze_all(fix_errors=fix_errors, debug=debug, max_workers=num_workers)

"""
if __name__ == "__main__":
    main(sys.argv[1:])
"""