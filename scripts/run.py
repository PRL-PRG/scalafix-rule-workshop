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
    "projects_dest": "./projects",
    "default_location_depth": 0,
    "temporal_git_repo_storage": "_gitrepotmp",

    "force_recompile_on_fail": False,
    "allow_partial_semanticdb_files": False,
    "max_backwards_steps": 5,

    "supported_build_systems": [
        "sbt"
    ],

    "tools_dir": "tools",
    "tools_base_url": "https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/master/scripts/",
    "analyzer_name": "implicit-analyzer.jar",
    "callsite_counter_name": "callsite-counter.jar",
    "analyzer_jvm_options": "-Xmx2g",

    "sbt_plugins": ["scalameta-config"],
    "sbt_versions": ["0.13", "1.0"],

    "condensed_report_long": "condensed-report.long.csv",

    "reports_folder": "_reports",
    "phase_reports_folder": "_phases",
    "phase_reports": {
        "compilation_report": "COMPILATION_REPORT.TXT",
        "semanticdb_report": "SEMANTICDB_REPORT.TXT",
        "analyzer_report": "ANALYZER_REPORT.TXT",
        "classpath_report": "CLEANUP_REPORT.TXT",
        "callsite_count_report": "CALLSITES_REPORT.TXT",
        "paths_extraction_report": "PATHS_REPORT.TXT",
        "project_info_report": "PROJECTINFO_REPORT.TXT"
    }
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

    def get_phase_reports_folder(self, project_path, reports_folder_name=BASE_CONFIG["reports_folder"]):
        report_folder = os.path.join(os.getcwd(), project_path, reports_folder_name, BASE_CONFIG["phase_reports_folder"])
        if not os.path.exists(report_folder):
            os.mkdir(report_folder)
        return report_folder

    def read_phase_report(self, project_path, kind):
        def get_report_in(path, kind):
            for f in os.listdir(path):
                if f == BASE_CONFIG["phase_reports"][kind]:
                    with open(os.path.join(path, f)) as report:
                        return report.read()
            return None

        reports_folder = self.get_phase_reports_folder(project_path)
        res = get_report_in(reports_folder, kind)
        # For projects that were processed before we had the _reports folder
        if res is None: res = get_report_in(os.path.join(project_path, BASE_CONFIG["reports_folder"]), kind)
        if res is None: res = get_report_in(project_path, kind)
        return res if res is None else res.strip()

    def write_phase_report(self, content, project_path, kind):
        report_folder = self.get_phase_reports_folder(project_path)
        report_path = os.path.join(report_folder, BASE_CONFIG["phase_reports"][kind])
        with open(report_path, 'w') as report:
            return report.write(content)

    def exclude_non_successful(self, projects, report_kind):
        return filter(
            lambda proj: self.read_phase_report(proj, report_kind) == "SUCCESS",
            projects
        )

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

class Phase:
    def __init__(self, project_path, phase_name):
        self.project_path = project_path
        self.project_name = os.path.split(project_path)[1]
        self.phase_name = phase_name
        self.P = Pipeline()

    def info(self, message):
        self.P.info("[%s][%s] %s" % (self.phase_name, self.project_name, message))

    def error(self, message):
        self.P.error("[%s][%s] %s" % (self.phase_name, self.project_name, message))

    def depend_on(self, phase):
        report = self.P.read_phase_report(self.project_path, phase)
        if report is None:
            self.error("Dependency %s is None for project %s" % (phase, self.project_path))
            sys.exit(1)
        elif report.startswith("SUCCESS"):
            self.info("Dependency %s met for project %s" % (phase, self.project_path))
        else:
            self.error("Dependency %s unmet for project %s" % (phase, self.project_path))
            sys.exit(1)


    def run_and_resume(self, command, error_title ="runAndResume ERROR"):
        failed = self.P.local_canfail(error_title, command, os.getcwd())
        if failed:
            self.error("Failed command %s on project %s" % (command, self.project_path))
            sys.exit(1)

    def stop_if_already_reported(self, phase):
        report = self.P.read_phase_report(self.project_path, phase)
        if report is not None:
            self.info("Phase report found (%s). Skipping" % (report))
            sys.exit(0 if report.startswith("SUCCESS") else 1)

    def succeed(self, phase, payload = ''):
        self.info("SUCCESS")
        self.P.write_phase_report("SUCCESS %s" % payload, self.project_path, phase)
        sys.exit(0)

    def fail(self, phase, payload = ''):
        self.error("ERROR")
        self.P.write_phase_report("ERROR %s" % payload, self.project_path, phase)
        sys.exit(1)



@task
def test_analyze(
        project_path,
        expected_path
):
    def test_report(report_kind):
        expected_res = P.read_phase_report(expected_path, report_kind)
        actual_res = P.read_phase_report(project_path, report_kind)
        if not (expected_res == actual_res):
            P.error("[TEST][FAILED] In report %s, expected %s, got %s for project %s" %
                    (report_kind, expected_res, actual_res, project_path))
        else:
            P.info("[TEST][SUCCESS] In report %s for project %s" %(report_kind, project_path))

    P = Pipeline()
    P.info("[TEST][Setup] Cleaning up project %s" % project_path)
    reports_folder = os.path.join(os.getcwd(), project_path, BASE_CONFIG["reports_folder"])
    P.local_canfail("Cleanup tests", "rm -rf %s && mkdir %s" % (reports_folder, reports_folder), project_path)
    P.info("[TEST][Setup] Running analysis on %s..." % project_path)
    P.local("fab analyze:project_path=%s" % project_path, ".")
    #P.info("[TEST] Counting callsites on %s..." % project_path)
    #P.local("fab count_callsites:project_path=%s" % project_path, ".")
    P.info("[TEST][Setup] Extracting paths on %s..." % project_path)
    P.local("fab extract_paths:project_path=%s" % project_path, ".")
    P.info("[TEST][Setup] Comparing results...")
    for report in BASE_CONFIG["phase_reports"]:
        test_report(report)

@task
def create_project_info(
        project_path,
        url=None,
        last_commit=None,
):
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

    P = Pipeline()
    phase = Phase(project_path, "Metadata")
    project_name = os.path.split(project_path)[1]

    phase.stop_if_already_reported("project_info_report")
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
    phase.succeed("project_info_report")

@task
def compile(project_path, backwards_steps=BASE_CONFIG["max_backwards_steps"], pull=True):
    project_name = os.path.split(project_path)[1]
    cwd = os.getcwd()
    P = Pipeline()
    phase = Phase(project_path, "CCompile")
    project_name = os.path.split(project_path)[1]

    phase.stop_if_already_reported("compilation_report")

#    if pull:
#        P.local("git reset --hard HEAD && git checkout master && git pull", project_path)

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
                phase.succeed("compilation_report", payload = tag)
        else:
            P.error("[CCompile][%s] Checkout for tag %s failed. Skipping" % (project_name, tag))

    phase.fail("compilation_report", payload = tags)

@task
def gen_sdb(
        project_path
):
    P = Pipeline()
    phase = Phase(project_path, "Semanticdb")
    phase.depend_on("compilation_report")
    phase.stop_if_already_reported("semanticdb_report")

    failed = P.local_canfail("Generate semanticdb", "sbt -batch semanticdb compile", project_path, verbose=True)
    if failed:
        phase.fail("semanticdb_report")
    else:
        phase.succeed("semanticdb_report")

@task
def analyze(
        project_path,
        tools_dir=BASE_CONFIG["tools_dir"]
):
    def run_analysis_tool(tool_path, jvm_options):
        return Pipeline().local_canfail("Extract Implicits", "java -jar %s %s . ./classpath.dat ./%s" % (jvm_options, tool_path, BASE_CONFIG["reports_folder"]), project_path)

    cwd = os.getcwd()
    phase = Phase(project_path, "ExtractImplicits")
    jvm_options = BASE_CONFIG["analyzer_jvm_options"]
    analysis_tool_path = os.path.join(cwd, tools_dir, BASE_CONFIG["analyzer_name"])

    phase.stop_if_already_reported("analyzer_report")

    phase.run_and_resume("fab setup:tools_dest=%s" % tools_dir, "Setup")
    phase.run_and_resume("fab compile:project_path=%s" % project_path, "Clean compilation")
    phase.run_and_resume("fab create_project_info:project_path=%s" % project_path, "Project info")
    phase.run_and_resume("fab create_project_info:project_path=%s" % project_path, "SDB File generation")
    phase.run_and_resume("fab classpath:project_path=%s" % project_path, "Classpath generation")

    failed = run_analysis_tool(analysis_tool_path, jvm_options)
    if failed:
        phase.fail("analyzer_report")
    else:
        phase.succeed("analyzer_report")

@task
def classpath(project_path):

    def run_classpath_tool(project_path):
        return Pipeline().local_canfail("Classpath generation", """sbt -batch "show test:fullClasspath" | sed -n -E 's/Attributed\(([^)]*)\)[,]?/\\n\\1\\n/gp' | grep "^/" > classpath.dat""", project_path)

    phase = Phase(project_path, "Classpath")

    phase.stop_if_already_reported("classpath_report")

    phase.depend_on("compilation_report")

    failed = run_classpath_tool(project_path)
    if failed:
        phase.fail("classpath_report")
    else:
        phase.succeed("classpath_report")


@task
def setup(tools_dest=BASE_CONFIG["tools_dir"]):
    cwd = os.getcwd()
    P = Pipeline()

    def download_plugin(sbt_folder, plugin_name, version):
        # Stick to default plugin urls
        plugin_url = BASE_CONFIG["tools_base_url"] + plugin_name
        plugins_folder = os.path.join(sbt_folder, version, "plugins")
        if not os.path.exists(plugins_folder):
            os.mkdir(plugins_folder)
        if not os.path.exists(os.path.join(plugins_folder, plugin_name)):
            failed = P.local_canfail(
                "download sbt plugin %s for v%s" % (plugin_name, version),
                "wget -O %s %s" % (plugin_name, plugin_url),
                plugins_folder
            )
            if failed:
                P.error("[Setup] Plugin download failed")
                sys.exit(1)

    def download_sbt_plugins():
        sbt_folder = os.path.expanduser(os.path.join("~", ".sbt"))
        sbt_versions = BASE_CONFIG["sbt_versions"]
        sbt_plugins = BASE_CONFIG["sbt_plugins"]
        if not os.path.exists(sbt_folder):
            P.error("[Setup] No .sbt folder found. Exiting")
            sys.exit(1)
        for version in sbt_versions:
            for name in sbt_plugins:
                plugin_name = "%s-%s.scala" % (name, version.replace(".", "-"))
                if os.path.exists(os.path.join(sbt_folder, version)):
                    download_plugin(sbt_folder, plugin_name, version)

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
    download_tool("CallSite Counter", BASE_CONFIG["callsite_counter_name"])
    P.info("[Setup] Done")

def get_project_list(projects_path, depth):
    if depth == 0:
        return list(map(lambda x: os.path.join(projects_path, x), os.listdir(projects_path)))
    else:
        paths = []
        for p in os.listdir(projects_path):
            paths = paths + get_project_list(os.path.join(projects_path, p), depth - 1)
        return paths

@task
def condense_reports(
        projects_path=BASE_CONFIG["projects_dest"],
        project_depth=BASE_CONFIG["default_location_depth"]
):
    def write_manifest(manifest):
        with open(os.path.join(cwd, "manifest.json"), 'w') as manifest_file:
            manifest_file.write("{\"projects\":[\n")
            for proj in manifest[:-1]:
                if os.path.exists(proj[0]) and os.path.exists(proj[1]):
                    manifest_file.write("{\"metadata\":\"%s\",\"results\":\"%s\", \"paths\": \"%s\"},\n" % (proj[0], proj[1], proj[2]))
            last = manifest[-1]
            if os.path.exists(last[0]) and os.path.exists(last[1]):
                manifest_file.write("{\"metadata\":\"%s\",\"results\":\"%s\",\"paths\": \"%s\"}\n" % (last[0], last[1], last[2]))
            manifest_file.write("]}")

    def read_reports(report_kinds, project):
        P.info("[Reports] Reading reports for %s" % project)
        return map(lambda kind: str(P.read_phase_report(project, kind)).split('\n')[0], report_kinds)


    cwd = os.getcwd()
    P = Pipeline()
    P.info("[Reports] Generating analysis report")

    manifest = []

    reports_summaries = { report: (0, 0) for report in BASE_CONFIG["phase_reports"]}
    long = create_csv(reports_summaries.keys())
    projects = get_project_list(projects_path, int(float(project_depth)))

    reports = map(lambda project: read_reports(reports_summaries.keys(), project), projects)

    long_csv = long
    for report in reports: long_csv = add_row(long_csv, report)

    with open(BASE_CONFIG["condensed_report_long"], 'w') as long_summary:
        long_summary.write(print_csv(long_csv))

    manifest = map(lambda proj: (
        "%s/project.csv" % os.path.join(cwd, proj),
        "%s/%s/results.json" % (os.path.join(cwd, proj), BASE_CONFIG["reports_folder"]),
        "%s/%s/paths.csv" % (os.path.join(cwd, proj), BASE_CONFIG["reports_folder"])
        ), projects)
    write_manifest(manifest)

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

@task
def merge_metadata(
        project_depth=BASE_CONFIG["default_location_depth"],
        projects_path=BASE_CONFIG["projects_dest"],
        exclude_unfinished=True
):
    #import csvmanip
    P = Pipeline()
    depth = int(float(project_depth))
    projects = get_project_list(projects_path, depth)
    if exclude_unfinished:
        projects = P.exclude_non_successful(projects, "analyzer_report")
    metadata_files = map(lambda proj: proj + "/project.csv", projects)
    projects_info = merge_all(load_many(metadata_files))
    with open("project-metadata.csv", 'w') as metadata:
        metadata.write(print_csv(projects_info))
    sloc_files = map(lambda proj: proj+"/sloc.csv", projects)
    sloc_csvs = load_many(sloc_files)
    headers_clean = map(lambda i: drop_header(sloc_csvs[i], 5), range(1, len(sloc_csvs))) # Drop the annoying cloc timestamp
    with_project = map(lambda i: extend_csv(headers_clean[i], "project", os.path.split(projects[i])[1]), range(1, len(headers_clean)))
    all_in_one = merge_all(with_project)
    with open("slocs.csv", 'w') as slocs_file:
        slocs_file.write(print_csv(all_in_one))

# Run sbt to extract test and compile source paths
# Store them in _reports/paths.csv
@task
def extract_paths(
        project_path,
):
    P = Pipeline()
    phase = Phase(project_path, "Paths")
    project_name = os.path.split(project_path)[1]

    phase.stop_if_already_reported("paths_extraction_report")

    def gen_paths_command(project, scope):
        # Generates an AWK script that matches all lines that look like paths,
        # And outputs them in nice CSV format.
        def gen_processor_script(project, scope):
            return ("$2 ~ /^\// { path = $2; print \"%s, \" path \", %s\";}" %
                    (project, scope))
        return ("sbt -batch -Dsbt.log.noformat=true %s:scalaSource | awk '%s' >> %s/paths.csv" %
                (scope, gen_processor_script(project_name, scope), BASE_CONFIG["reports_folder"]))


    P.info("[Paths][%s] Cleaning up previous paths" % project_name)
    P.local(
        "echo 'project, path, kind' > %s/paths.csv" % BASE_CONFIG["reports_folder"],
        project_path
    )

    P.info("[Paths][%s] Extracting Compile path" % project_name)
    failed = P.local_canfail(
        "Get Source Paths",
        gen_paths_command(project_name, "compile"),
        project_path
    )
    if failed:
        phase.fail("paths_extraction_report")

    P.info("[Paths][%s] Extracting Test path" % project_name)
    failed = P.local_canfail(
        "Get Test Paths",
        gen_paths_command(project_name, "test"),
        project_path
    )
    if failed:
        phase.fail("paths_extraction_report")

    phase.succeed("paths_extraction_report")

@task
def merge_paths(
        project_depth=BASE_CONFIG["default_location_depth"],
        projects_path=BASE_CONFIG["projects_dest"],
        exclude_unfinished=True
):
    P = Pipeline()
    reports_folder = BASE_CONFIG["reports_folder"]
    projects = get_project_list(projects_path, project_depth)
    if exclude_unfinished:
        projects = P.exclude_non_successful(projects, "analyzer_report")
    paths_files = load_many(map(lambda p: os.path.join(p, reports_folder, "paths.csv"), projects))
    merged = merge_all(paths_files)

    with open("paths.all.csv", 'w') as pathsfile:
        pathsfile.write(print_csv(merged))

@task
def count_callsites(project_path):
    def run_count_command(project_path, tool_path):
        return Pipeline().local_canfail("Count Call Sites", "java -jar %s . ./%s" % (tool_path, BASE_CONFIG["reports_folder"]), project_path)

    phase = Phase(project_path, "CallSites")
    cwd = os.getcwd()
    counter_tool_path = os.path.join(cwd, BASE_CONFIG["tools_dir"], BASE_CONFIG["callsite_counter_name"])

    phase.stop_if_already_reported("callsite_count_report")
    phase.depend_on("semanticdb_report")

    failed = run_count_command(project_path, counter_tool_path)
    if failed:
        phase.fail("callsite_counter_name")
    else:
        phase.succeed("callsite_couner_name")

@task
def merge_callsite_counts(
        project_depth=BASE_CONFIG["default_location_depth"],
        projects_path=BASE_CONFIG["projects_dest"],
        exclude_unfinished=True
):
    P = Pipeline()
    reports_folder = BASE_CONFIG["reports_folder"]
    projects = get_project_list(projects_path, int(float(project_depth)))
    if exclude_unfinished:
        projects = P.exclude_non_successful(projects, "callsite_count_report")
    files = load_many(map(lambda p: os.path.join(p, reports_folder, "callsite-counts.csv"), projects))
    with_project = map(lambda i: extend_csv(files[i], "project", os.path.split(projects[i])[1]), range(1, len(files)))
    merged = merge_all(with_project)

    with open("callsite-counts.all.csv", 'w') as pathsfile:
        pathsfile.write(print_csv(merged))



####################
# CSVManip
####################

import csv

# CSVFile: {
#   header: List[Str],
#   data: List[List[Str]]
#}

def load_csv(path):
    data = []
    headers = []
    with open(path, 'rb') as csvFile:
        reader = csv.reader(csvFile)
        headers = reader.next()
        for line in reader:
            data.append(line)

    return {
        "headers": headers,
        "data": data
    }

def load_many(paths):
    return map(load_csv, paths)

def print_csv(csvf):
    headers = ",".join(csvf["headers"]) + '\n'
    data = '\n'.join(map(lambda line: ", ".join(line), csvf["data"]))
    return "%s%s" % (headers, data)

def merge_csvs(one, other):
    assert(one["headers"] != None)
    assert(other["headers"] != None)
    assert(one["headers"] == other["headers"])
    #assert(((len(one["data"]) == 0) or (len(other["data"]) == 0)) or
    #        (len(one["data"][1]) == len(other["data"][1])))
    return {
        "headers": one["headers"],
        "data": one["data"] + other["data"]
    }

def merge_all(csvs):
    return reduce(lambda f1, f2: merge_csvs(f1, f2), csvs)

def extend_csv(csvf, colname, coldata):
    if (isinstance(coldata, list)):
        assert(len(coldata) == len(csvf["data"]))
        return {
            "headers": csvf["headers"] + [colname],
            "data": map(lambda i: csvf["data"][i] + [coldata[i]], range(1, len(csvf["data"])))
        }
    else:
        return {
            "headers": csvf["headers"] + [colname],
            "data": map(lambda i: csvf["data"][i] + [coldata], range(1, len(csvf["data"])))
        }

def drop_header(csvf, index):
    assert(len(csvf["headers"]) > len(csvf["data"][0]))
    headers = csvf["headers"]
    return {
        "headers": headers[:index] + headers[index + 1:],
        "data": csvf["data"]
    }

def create_csv(headers):
    return {
        "headers": headers,
        "data": []
    }

def add_row(csvf, row):
    return {
        "headers": csvf["headers"],
        "data": csvf["data"] + [row]
    }

        


