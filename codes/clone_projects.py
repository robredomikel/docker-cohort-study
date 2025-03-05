"""
Clones the projects specified in REPOSITORIES_LIST
Each line should be one
"""

from commons import CASES_PATH, CONTROLS_PATH
from os import path, makedirs
import git


def clone_project(git_url, repository_path, project_name=""):

    # Combine repository path and project name if it's not already combined in repository path
    if project_name != "":
        clone_path = path.join(repository_path, project_name)
    else:
        clone_path = repository_path

    # If the project is not already cloned, clone it
    if not path.exists(clone_path):
        try:
            _ = git.Repo.clone_from(git_url, clone_path)
            return 1
        except git.GitCommandError:
            print("Error: Repository couldn't be cloned", clone_path)
            return 0
    else:
        return 1  # Meaning that it is already cloned


def cloneProjects(project_type):
    """
    Clones the projects and stores them in the disk

    params: project_type -> Says if the projects are cases or controls
    return: Nothing
    """

    # Check the folder where the repositories will be saved exists
    # If not, create it

    if project_type == "cases":
        repositories_path = path.join(CASES_PATH, "repositories")
        repositories_list = path.join(CASES_PATH, "ms_eligible_projects.txt")
    else:
        repositories_path = path.join(CONTROLS_PATH, "repositories")
        repositories_list = path.join(CONTROLS_PATH, "resulting_data/lm_resulting_pros_trimonthly.txt")

    if not path.exists(repositories_path):
        makedirs(repositories_path)

    # Read the list of projects to clone
    with open(repositories_list, "r") as f:
        repositories = f.read().splitlines()

    repositories = [repository[:-4] for repository in repositories]
    repositories = [repository.replace("#", "/") for repository in repositories]

    # Clone the projects
    n_projects = len(repositories)
    cloned_projects = 0
    for count, repo in enumerate(repositories):

        repo_name = repo.replace("/", "#")
        print("Cloning {}/{}: {}".format(count+1, n_projects, repo))

        repo = f"https://github.com/{repo}.git"
        cloned_projects += clone_project(git_url=repo, repository_path=repositories_path, project_name=repo_name)

    print("{}/{} projects sucessfully cloned!".format(cloned_projects, n_projects))


