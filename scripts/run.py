#!/usr/bin/python3

import argparse
import json
import os
import shutil
import sys
from fabric.api import task, abort, lcd, get, settings
from fabric.api import local as fablocal
from fabric.contrib.console import confirm
from fabric.contrib import files
import csv
import subprocess
from termcolor import colored
import datetime

BASE_CONFIG = {
    "debug_info": True,
    "sbt_projects": "../materials/singlecodebase",
    "projects_dest": "./projects",
    "temporal_git_repo_storage": "_gitrepotmp",

    "force_recompile_on_fail": False,
    "allow_partial_semanticdb_files": False,
    "max_backwards_steps": 5,

    "report_files": [
        "project.csv",
        "funs.clean.csv",
        "params_funs.csv",
        "params.clean.csv"
    ],

    "supported_build_systems": [
        "sbt"
    ],

    "tools_dir": "tools",
    "tools_base_url": "https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/implicit-context/scripts/",
    "semanticdb_plugin_name": "scalameta_config",
    "analyzer_name": "implicit-analyzer.jar",
    "analyzer_jvm_options": "-Xmx2g",
    "cleanup_tool_name": "clean_data.py",
    "db_push_tool_name": "push-to-db.R",
    "push_to_db_enabled": False,

    "condensed_report": "condensed_report.txt",

    "reports_folder": "_reports",
    "compilation_report": "COMPILATION_REPORT.TXT",
    "semanticdb_report": "SEMANTICDB_REPORT.TXT",
    "analyzer_report": "ANALYZER_REPORT.TXT",
    "cleanup_report": "CLEANUP_REPORT.TXT",
    "db_push_report": "DB_PUSH_REPORT.TXT"
}

def log(msg, color='magenta'):
    timestamp = colored("{:%H:%M:%S.%f}".format(datetime.datetime.now()), 'yellow')
    label = colored("Runner", color)
    print("%s[%s]%s" % (timestamp, label, msg))

def error(msg):
    log(msg, color='red')

def raw(stdout, stderr):
    sys.stdout.write(stdout + '\n')
    sys.stderr.write(stderr + '\n')

class Pipeline():
    def local(self, command, directory, verbose=False):
        with lcd(directory):
            res = fablocal(command, capture=True)
            if verbose:
                raw(str(res.stdout), str(res.stderr))
            return res

    def local_canfail(self, name, command, directory, verbose=False):
        failed = False
        with settings(warn_only=True):
            result = self.local(command, directory, verbose=verbose)
            if result.failed:
                with lcd(directory):
                    with open(os.path.join(directory, name.replace(" ", "_")), 'w') as command_report:
                        command_report.write(result.stdout)
                        command_report.write(result.stderr)
                failed = True
        return failed

    def get_report(self, project_path, kind):
        for root, subdir, files in os.walk(project_path):
            for f in files:
                if f == BASE_CONFIG[kind]:
                    with open(os.path.join(root, f)) as report:
                        return report.read()
        return None

    def write_report(self, content, project_path, kind):
        report_folder = os.path.join(project_path, BASE_CONFIG["reports_folder"])
        if not os.path.exists(report_folder):
            os.mkdir(report_folder)
        report_path = os.path.join(report_folder, BASE_CONFIG[kind])
        with open(report_path, 'w') as report:
            return report.write(content)

    def info(self, msg):
        if BASE_CONFIG["debug_info"]:
            log(msg)

    def error(self, msg):
        if BASE_CONFIG["debug_info"]:
            error(msg)

    def has_file_with_ending(self, folder, extension):
        for root, dirs, files in os.walk(folder):
            for f in files:
                if f.endswith(extension):
                    return True

    def get_build_systems(self, project_path):
        systems = []
        if self.has_file_with_ending(project_path, "pom.xml"):
            systems.append("maven")
        if self.has_file_with_ending(project_path, ".sbt"):
            systems.append("sbt")
        if self.has_file_with_ending(project_path, "gradle"):
            systems.append("gradle")
        return systems

    def is_build_system_supported(self, src_path):
        project_build_systems = self.get_build_systems(src_path)
        return any(system in BASE_CONFIG["supported_build_systems"] for system in project_build_systems)

@task
def create_project_info(
    project_path,
    url=None,
    last_commit=None,
    ):
    P = Pipeline()
    def count_locs(project_path):
        if not os.path.exists(os.path.join(project_path, "cloc_report.csv")):
            P.local("cloc --out=cloc_report.csv --csv .", project_path)

        with open(os.path.join(project_path, "cloc_report.csv")) as csv_file:
            scala_lines = 0
            total_lines = 0
            reader = csv.DictReader(csv_file)
            for line in reader:
                lines = int(line["code"])
                total_lines += lines
                if line["language"] == "Scala":
                    scala_lines += lines
        return {"total": total_lines, "scala": scala_lines}

    def generate_project_metadata(repo_path):
        repo_url = P.local("git config --get remote.origin.url", repo_path)
        commit = P.local("git rev-parse HEAD", repo_path)
        version = P.local("git describe --always", repo_path, verbose=True)
        reponame = repo_url.split('.com/')[1]
        sloc = count_locs(repo_path)
        repo_info = json.loads(P.local("curl 'https://api.github.com/repos/%s'" % reponame, repo_path))
        gh_stars = repo_info["stargazers_count"] if "stargazers_count" in repo_info else (repo_info["watchers_count"] if "watchers_count" in repo_info else -1)
        build_system = P.get_build_systems(repo_path)
        project_info = {
            "name": str(project_name),
            "version": version,
            "last_commit": commit,
            "url": repo_url,
            "total_loc": sloc["total"],
            "scala_loc": sloc["scala"],
            "reponame": reponame,
            "gh_stars": gh_stars,
            "build_system": '|'.join(build_system)
        }
        return project_info

    def transform_to_gh_url(original):
        if original.startswith("git@github.com:"):
            return original \
                .replace("git@", "https://") \
                .replace("github.com:", "github.com/") \
                .replace(".git", "")
        if original.endswith(".git"):
            return original.replace(".git", "")
        return original

    project_name = os.path.split(project_path)[1]
    P.info("[Metadata][%s] Creating project info..." % project_name)

    project_info = {}
    if url is None:
        # We assume the git repo is in the project_path
        P.info("[Metadata][%s] No custom url. Assume this is a git repo" % project_name)
        project_info = generate_project_metadata(project_path)
    else:
        # We have a custom url, we need to download the repo
        P.info("[Metadata][%s] Custom url provided. Downloading git repo" % project_name)
        url = transform_to_gh_url(url)
        P.local("git clone --depth 300 %s %s" % (url, BASE_CONFIG["temporal_git_repo_storage"]), project_path)
        repo_path = os.path.join(project_path, BASE_CONFIG["temporal_git_repo_storage"])
        if last_commit != None:
            P.local("git checkout %s" % last_commit, repo_path)
        project_info = generate_project_metadata(repo_path)
        shutil.rmtree(repo_path)

    P.info("[Metadata][%s] Capture project info: %s" % (project_name, project_info))
    with open(os.path.join(project_path, "project.csv"), "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        writer.writerow(project_info.keys())
        writer.writerow(project_info.values())
    sys.exit(0)

@task
def import_single(src_path, dest_dir=BASE_CONFIG["projects_dest"]):

    def do_import(subdir, srcpath, destpath):
        cwd = os.getcwd()
        shutil.copytree(srcpath, destpath)
        P.info("[Import] Done!")
        return False

    P = Pipeline()
    subdir = os.path.basename(src_path)
    dest_path = os.path.join(dest_dir, subdir)
    import_failed = False
    if os.path.isdir(src_path) and P.is_build_system_supported(src_path):
        P.info("[Import] %s" % str(src_path))
        if not os.path.exists(dest_path):
            import_failed = do_import(subdir, src_path, dest_path)
        else:
            P.info("[Import] Already imported")
        sys.exit(1 if import_failed else 0)


@task
def compile(project_path, backwards_steps=BASE_CONFIG["max_backwards_steps"]):
    def handle_success(project_path, project_name, tag):
        P.info("[CCompile][%s] Compilation successful on master" % (project_name))
        report = "SUCCESS\n%s" % tag
        P.write_report(report, project_path, "compilation_report")
        sys.exit(0)

    def handle_failure(project_path, project_name, tags):
        P.error("[CCompile][%s] Could not find a tag that could compile in \n%s" % (project_name, tags))
        report = "FAILURE\n%s" % tag
        P.write_report(report, project_path, "compilation_report")
        sys.exit(1)

    project_name = os.path.split(project_path)[1]
    cwd = os.getcwd()
    P = Pipeline()
    P.info("[CCompile][%s] Attempting compilation of %s" % (project_name, project_name))
    report = P.get_report(project_path, "compilation_report")
    if report:
        P.info("[CCompile][%s] Compilation report found. Skipping" % project_name)
        sys.exit(0 if report.startswith("SUCCESS") else 1)

    current_commit = P.local("git describe --always --abbrev=0", project_path)
    # Fetch the latest commits so they show up in `git describe`
    P.local("git fetch --depth %s" % backwards_steps, project_path)
    tag_stream = P.local("git describe --always --abbrev=0 --tags `git rev-list --tags --max-count=%s %s`" % (backwards_steps, current_commit), project_path)
    tags = [current_commit] + tag_stream.split('\n')
    for tag in tags:
        checkout_failed = P.local_canfail("git checkout %s" % tag, "git checkout %s" % tag, project_path)
        if not checkout_failed:
            failed = P.local_canfail("Compile tag %s" % tag, "sbt -batch compile", project_path, verbose=True)
            if not failed:
                handle_success(project_path, project_name, tag)
        else:
            P.error("[CCompile][%s] Checkout for tag %s failed. Skipping" % (project_name, tag))

    handle_failure(project_path, project_name, tags)

@task
def gen_sdb(
    project_path,
    force_recompile=BASE_CONFIG["force_recompile_on_fail"],
    allow_partial_semanticdbs=BASE_CONFIG["allow_partial_semanticdb_files"]
    ):
    cwd = os.getcwd()
    P = Pipeline()
    project_name = os.path.split(project_path)[1]

    def sdb_files_exist(path):
        for wd, subdirs, files in os.walk(path):
            for file in files:
                if file.endswith("semanticdb"):
                    return True

    def handle_success(project_path, project_name):
        P.info("[GenSDB][%s] Compilation completed succesfuly" % project_name)
        P.write_report("SUCCESS", project_path, "semanticdb_report")
        sys.exit(0)

    def handle_partial(project_path, project_name):
        if sdb_files_exist(project_path):
            P.info("[GenSDB][%s] Compilation completed with errors. Some SDB files found" % project_name)
            P.write_report("PARTIAL", project_path, "semanticdb_report")
            sys.exit(0)
        else:
            handle_error(project_path, project_name)

    def handle_error(project_path, project_name):
        P.error("[GenSDB][%s] Skipping project" % project_name)
        P.write_report("ERROR", project_path, "semanticdb_report")
        sys.exit(1)

    def clean_compile_succeeded(project_path, project_name):
        report = P.get_report(project_path, "compilation_report")
        if report is None:
            P.info("[GenSDB][%s] Clean compilation report not found" % project_name)
            return False
        elif not report.startswith("SUCCESS"):
            P.info("[GenSDB][%s] Clean compilation report was not successful (%s)" % (project_name, report[:15]))
            return False
        else:
            return True

    def needs_recompile(report):
        return report is None or (report.startswith("ERROR") and force_recompile)

    if not clean_compile_succeeded(project_path, project_name):
        sys.exit(1)

    P.info("[GenSDB][%s] Looking for report from previous compilation..." % project_name)
    report = P.get_report(project_path, "semanticdb_report")
    if needs_recompile(report):
        P.info("[GenSDB][%s] Not found. Recompiling..." % project_name)
        failed = P.local_canfail("Generate semanticdb", "sbt -batch semanticdb compile", project_path, verbose=True)
        if not failed:
            handle_success(project_path, project_name)
        if failed and allow_partial_semanticdbs:
            handle_partial(project_path, project_name)
        else:
            handle_error(project_path, project_name)
    else:
        if report.startswith("ERROR"):
            handle_error(project_path, project_name)
        else:
            P.info("[GenSDB][%s] Compilation report found (%s). Skipping" % (project_name, report))
            sys.exit(0)

@task
def analyze(
    project_path,
    tools_dir=BASE_CONFIG["tools_dir"],
    always_abort=True,
    push_to_db=BASE_CONFIG["push_to_db_enabled"]
    ):
    def analysis_command(project_path, project_name, title, command, report_kind):
        success = True
        report = P.get_report(project_path, report_kind)
        if report is None or report == 'ERROR':
            failed = P.local_canfail(title, command, project_path)
            if not failed:
                P.write_report('SUCCESS', project_path, report_kind)
                P.info("[Analysis][%s] Done" % project_name)
            else:
                P.write_report('ERROR', project_path, report_kind)
                P.error("[Analysis][%s] Skipping project" % project_name)
                success = False
        else:
            P.info("[Analysis][%s] %s report found (%s). Skipping" % (project_name, title, report))
        return success

    def run_analysis_tool(project_name, project_path, tool_path, jvm_options):
        P.info("[Analysis][%s] Analyzing semanticdb files..." % project_name)
        title = "Semanticdb file analysis"
        command = "java -jar %s %s ." % (jvm_options, tool_path)
        return analysis_command(project_path, project_name, title, command, "analyzer_report")

    def run_cleanup_tool(project_name, project_path, tool_path):
        P.info("[Analysis][%s] Cleaning up the data..." % project_name)
        title = "Data cleanup"
        command = "python %s ." % (tool_path)
        return analysis_command(project_path, project_name, title, command, "cleanup_report")

    def upload_to_database(project_name, project_path, tool_path):
        P.info("[Analysis][%s] Uploading to database..." % project_name)
        command = "Rscript %s ." % (tool_path)
        title = "DB upload"
        success = analysis_command(project_path, project_name, title, command, "db_push_report")
        if success:
            P.write_report('SUCCESS', project_path, "db_push_report")
            P.info("[Analysis][%s] Changes commited to the database" % project_name)
        return success

    P = Pipeline()
    setup(tools_dir)

    cwd = os.getcwd()
    jvm_options = BASE_CONFIG["analyzer_jvm_options"]
    analysis_tool_path = os.path.join(cwd, tools_dir, BASE_CONFIG["analyzer_name"])
    cleanup_tool_path = os.path.join(cwd, tools_dir, BASE_CONFIG["cleanup_tool_name"])
    db_tool_path = os.path.join(cwd, tools_dir, BASE_CONFIG["db_push_tool_name"])

    project_name = os.path.split(project_path)[1]

    compilation_failed = P.local_canfail("SDB File generation", "fab gen_sdb:project_path=%s" % (project_path), cwd, verbose=True)
    continue_analysis = not compilation_failed
    if continue_analysis:
        continue_analysis = run_analysis_tool(project_name, project_path, analysis_tool_path, jvm_options)
    if continue_analysis:
        continue_analysis = run_cleanup_tool(project_name, project_path, cleanup_tool_path)
    if continue_analysis and push_to_db:
       continue_analysis = upload_to_database(project_name, project_path, db_tool_path)
    P.info("[Analysis][%s] Analysis concluded" % project_name)
    sys.exit(0 if continue_analysis else 1)

@task
def setup(tools_dest=BASE_CONFIG["tools_dir"]):
    cwd = os.getcwd()
    P = Pipeline()

    def download_plugin(sbt_folder, version):
        # Stick to default plugin urls
        version_name = version.replace(".", "_")
        plugin_name = "%s_%s.scala" % (BASE_CONFIG["semanticdb_plugin_name"], version_name)
        plugin_url = BASE_CONFIG["tools_base_url"] + plugin_name
        plugins_folder = os.path.join(sbt_folder, version, "plugins")
        if not os.path.exists(plugins_folder):
            os.mkdir(plugins_folder)
        if not os.path.exists(os.path.join(plugins_folder, plugin_name)):
            failed = P.local_canfail(
                "download sbt plugin for v%s" % version,
                "wget -O %s %s" % (plugin_name, plugin_url),
                plugins_folder
                )
            if failed:
                P.error("[Setup] Plugin download failed")
                sys.exit(1)

    def download_sbt_plugins():
        sbt_folder = os.path.expanduser(os.path.join("~", ".sbt"))
        sbt_versions = ["0.13", "1.0"]
        if not os.path.exists(sbt_folder):
            P.error("[Setup] No .sbt folder found. Exiting")
            sys.exit(1)
        for version in sbt_versions:
            if os.path.exists(os.path.join(sbt_folder, version)):
                download_plugin(sbt_folder, version)

    def download_tool(title, name):
        url = BASE_CONFIG["tools_base_url"] + name
        tool_path = os.path.join(tools_dir, name)
        if not os.path.exists(tool_path):
            P.info("[Setup][%s] Not found. Dowloading..." % title)
            P.local("wget -O %s %s" % (name, url), tools_dir)

    P.info("[Setup] Setting up...")
    tools_dir = os.path.join(cwd, tools_dest)
    download_sbt_plugins()
    if not os.path.exists(tools_dir):
        os.mkdir(tools_dir)
    # Stick to default tool names
    download_tool("Semanticdb Analyzer", BASE_CONFIG["analyzer_name"])
    download_tool("Data cleanup tool", BASE_CONFIG["cleanup_tool_name"])
    download_tool("Database upload tool", BASE_CONFIG["db_push_tool_name"])
    P.info("[Setup] Done")

@task
def merge_csv(projects_path=BASE_CONFIG["projects_dest"]):
    cwd = os.getcwd()
    P.info("[Reports] Gathering reports")
    # I don't think it's worth it to parametrize the report filenames
    for report in BASE_CONFIG["report_files"]:
        P.info("[Reports] %s..." % report)
        headers = False
        with open("report_"+report, 'w') as report_file:
            writer = csv.writer(report_file)
            for subdir in os.listdir(projects_path):
                P.info("[Reports] Extracting from %s" % subdir)
                project_report_path = os.path.join(projects_path, subdir, report)
                if os.path.exists(project_report_path):
                    with open(project_report_path) as project_report_file:
                        project_report = csv.DictReader(project_report_file)
                        if not headers:
                            header = list(project_report.fieldnames)
                            header.append("project")
                            writer.writerow(header)
                            headers = True
                        for line in project_report:
                            data = list(line.values())
                            data.append(subdir)
                            writer.writerow(data)

@task
def condense_reports(
    report_name=BASE_CONFIG["condensed_report"],
    projects_path=BASE_CONFIG["projects_dest"]
    ):
    def write_header(report_file):
        report_file.write("Condensed analysis reports, %s\n" % datetime.datetime.now())

    def append_report(project_path, report_file, report_kind):
        status = str(P.get_report(project_path, report_kind)).replace('\n', ' \\ ')
        report_file.write("  - %s: %s\n" % (report_kind, status))

    cwd = os.getcwd()
    P = Pipeline()
    P.info("[Reports] Generating analysis report")
    with open(os.path.join(cwd, report_name), 'w') as report_file:
        write_header(report_file)
        for subdir in os.listdir(projects_path):
            P.info("[Reports] Extracting from %s" % subdir)
            project_path = os.path.join(projects_path, subdir)
            project_name = subdir

            report_file.write("%s:\n" % project_name)
            reports = ["compilation_report", "semanticdb_report", "analyzer_report", "cleanup_report", "db_push_report"]
            for report in reports:
                append_report(project_path, report_file, report)

@task
def cleanup_reports(project_path):
    P = Pipeline()
    report_path = os.path.join(project_path, BASE_CONFIG["reports_folder"])
    if os.path.exists(report_path):
        P.info("[Cleanup][%s] Removing reports folder" % project_path)
        shutil.rmtree(report_path)
        sys.exit(0)
    else:
        P.info("[Cleanup][%s] Report folder not found" % project_path)
        sys.exit(1)
