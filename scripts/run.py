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

    "report_files": [
        "project.csv"
    ],

    "supported_build_systems": [
        "sbt"
    ],

    "tools_dir": "tools",
    "tools_base_url": "https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/master/scripts/",
    "analyzer_name": "implicit-analyzer.jar",
    "analyzer_jvm_options": "-Xmx2g",

    "sbt_plugins": ["scalameta-config"],
    "sbt_versions": ["0.13", "1.0"],

    "condensed_report": "condensed-report.txt",

    "reports_folder": "_reports",
    "phase_reports": {
        "compilation_report": "COMPILATION_REPORT.TXT",
        "semanticdb_report": "SEMANTICDB_REPORT.TXT",
        "analyzer_report": "ANALYZER_REPORT.TXT",
        "classpath_report": "CLEANUP_REPORT.TXT",
        "callsite_count_report": "CALLSITES_REPORT.TXT",
        "path_extraction_report": "PATHS_REPORT.TXT"
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

    def get_report(self, project_path, kind, reports_folder_name=BASE_CONFIG["reports_folder"]):
        def get_report_in(path, kind):
            for f in os.listdir(path):
                if f == BASE_CONFIG["phase_reports"][kind]:
                    with open(os.path.join(path, f)) as report:
                        return report.read()
            return None

        reports_folder = os.path.join(project_path, reports_folder_name)
        res = get_report_in(reports_folder, kind)
        # For projects that were processed before we had the _reports folder
        if res is None: res = get_report_in(project_path, kind)
        return res if res is None else res.strip()

    def write_report(self, content, project_path, kind):
        report_folder = os.path.join(project_path, BASE_CONFIG["reports_folder"])
        if not os.path.exists(report_folder):
            os.mkdir(report_folder)
        report_path = os.path.join(report_folder, BASE_CONFIG["phase_reports"][kind])
        with open(report_path, 'w') as report:
            return report.write(content)

    def exclude_non_successful(self, projects, report_kind):
        return filter(
            lambda proj: self.get_report(proj, report_kind) == "SUCCESS",
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

@task
def test_analyze(
        project_path,
        expected_path
):
    def test_report(report_kind):
        expected_res = P.get_report(expected_path, report_kind)
        actual_res = P.get_report(project_path, report_kind)
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

def analysis_command(project_path, project_name, title, command, report_kind):
    P = Pipeline()
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


def run_classpath_tool(project_name, project_path):
    P = Pipeline()
    P.info("[Analysis][%s] Generating classpath..." % project_name)
    title = "Classpath generation"
    command = """sbt -batch "show test:fullClasspath" | sed -n -E 's/Attributed\(([^)]*)\)[,]?/\\n\\1\\n/gp' | grep "^/" > classpath.dat"""
    return analysis_command(project_path, project_name, title, command, "classpath_report")

def run_analysis_tool(project_name, project_path, tool_path, jvm_options):
    P = Pipeline()
    P.info("[Analysis][%s] Analyzing semanticdb files..." % project_name)
    title = "Semanticdb file analysis"
    command = "java -jar %s %s . ./classpath.dat ./%s" % (jvm_options, tool_path, BASE_CONFIG["reports_folder"])
    return analysis_command(project_path, project_name, title, command, "analyzer_report")

@task
def analyze(
        project_path,
        tools_dir=BASE_CONFIG["tools_dir"],
        always_abort=True
):

    P = Pipeline()
    setup(tools_dir)

    cwd = os.getcwd()
    jvm_options = BASE_CONFIG["analyzer_jvm_options"]
    analysis_tool_path = os.path.join(cwd, tools_dir, BASE_CONFIG["analyzer_name"])

    project_name = os.path.split(project_path)[1]

    analysis_failed = P.local_canfail("Clean compilation", "fab compile:project_path=%s" % (project_path), cwd, verbose=True)
    if not analysis_failed:
        analysis_failed = P.local_canfail("Project info", "fab create_project_info:project_path=%s" % (project_path), cwd, verbose=True)
    if not analysis_failed:
        analysis_failed = P.local_canfail("SDB File generation", "fab gen_sdb:project_path=%s" % (project_path), cwd, verbose=True)
    if not analysis_failed:
        analysis_failed = P.local_canfail("Classpath generation", "fab classpath:project_path=%s" % project_path, cwd, verbose=True)
    if not analysis_failed:
        analysis_failed = not run_analysis_tool(project_name, project_path, analysis_tool_path, jvm_options)
    P.info("[Analysis][%s] Analysis concluded" % project_name)
    sys.exit(1 if analysis_failed else 0)

@task
def classpath(project_path):
    cwd = os.getcwd()
    P = Pipeline()
    project_name = os.path.split(project_path)[1]
    compilation_report = P.get_report(project_path, "compilation_report")
    if compilation_report is None or compilation_report == 'ERROR':
        P.error("[Classpath][%s] Clean compilation reported unsuccessful. Aborting" % project_name)
        sys.exit(1)

    classpath_report = P.get_report(project_path, "classpath_report")
    if classpath_report is None:
        P.info("[Classpath][%s] Previous report not found. Running task" % project_name)
        success = run_classpath_tool(project_name, project_path)
        sys.exit(0 if success else 1)
    else:
        P.info("[Classpath][%s] Project report found (%s). Skipping." % (project_name, classpath_report))
        sys.exit(0 if classpath_report.startswith("SUCCESS") else 1)

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
    P.info("[Setup] Done")

@task
def merge_csv(projects_path=BASE_CONFIG["projects_dest"]):
    cwd = os.getcwd()
    P = Pipeline()
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
                        if not headers: # We are processing project.csv files
                            header = list(["build", "gh_stars", "scala_loc", "project", "url", "version", "reponame", "last_commit", "total_loc", "name"])
                            writer.writerow(header)
                            headers = True
                        for line in project_report:
                            data = list(line.values())
                            data.append(subdir)
                            writer.writerow(data)

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
        report_name=BASE_CONFIG["condensed_report"],
        projects_path=BASE_CONFIG["projects_dest"],
        project_depth=BASE_CONFIG["default_location_depth"]
):
    def write_header(report_file):
        report_file.write("Condensed analysis reports, %s\n" % datetime.datetime.now())

    def write_summary(reports, total, report_file):
        report_file.write("Summary --------------------------\n Total projects: %d\n" % total)
        for report in reports:
            report_file.write(" - %s: Success: %d, Failure: %d\n" % (report, reports[report][0], reports[report][1]))
        report_file.write("----------------------------")

    def append_report(project_path, report_file, report_kind):
        status = str(P.get_report(project_path, report_kind)).replace('\n', ' \\ ')
        report_file.write("  - %s: %s\n" % (report_kind, status))
        return status

    def write_manifest(manifest):
       with open(os.path.join(cwd, "manifest.json"), 'w') as manifest_file:
            manifest_file.write("{\"projects\":[\n")
            for proj in manifest[:-1]:
                if os.path.exists(proj[0]) and os.path.exists(proj[1]): 
                    manifest_file.write("{\"metadata\":\"%s\",\"results\":\"%s\", \"paths\":\"%s\"},\n" % (proj[0], proj[1], proj[2]))
            last = manifest[-1]
            if os.path.exists(last[0]) and os.path.exists(last[1]): 
                manifest_file.write("{\"metadata\":\"%s\",\"results\":\"%s\", \"paths\":\"%s\"}\n" % (last[0], last[1], last[2]))
            manifest_file.write("]}")

    cwd = os.getcwd()
    P = Pipeline()
    P.info("[Reports] Generating analysis report")

    manifest = []
 
    with open(os.path.join(cwd, report_name), 'w') as report_file:
        write_header(report_file)
        reports = {
            "compilation_report": (0, 0),
            "semanticdb_report": (0, 0),
            "analyzer_report": (0, 0),
            "classpath_report": (0, 0)
        }
        total_projects = 0
        for project_path in get_project_list(projects_path, int(float(project_depth))):
            P.info("[Reports] Extracting from %s" % project_path)
            project_name = project_path
            total_projects += 1
            report_file.write("%s:\n" % project_name)
            for report in reports:
                status = append_report(project_path, report_file, report)
                res = ((1, 0) if status.startswith("SUCCESS") else (0, 1))
                reports[report] = tuple(map(sum, zip(reports[report], res)))
            full_path = os.path.join(cwd, project_path)
            manifest.append((
                "%s/project.csv" % full_path,
                "%s/%s/results.json" % (full_path, BASE_CONFIG["reports_folder"]),
                "%s/%s/paths.csv" % (full_path, BASE_CONFIG["reports_folder"])
            ))
        write_summary(reports, total_projects, report_file)
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
    project_name = os.path.split(project_path)[1]
    P.info("[Paths][%s] Extracting paths" % project_name)
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
    P.local_canfail(
        "Get Source Paths",
        gen_paths_command(project_name, "compile"),
        project_path
    )
    P.info("[Paths][%s] Extracting Test path" % project_name)
    P.local_canfail(
        "Get Test Paths",
        gen_paths_command(project_name, "test"),
        project_path
    )

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



        


