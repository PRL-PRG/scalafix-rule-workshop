#!/usr/bin/python3

import argparse
import json
import os
import shutil
import sys
from fabric.api import task, local, abort, lcd, get, settings
from fabric.contrib.console import confirm
from fabric.contrib import files
import csv
import subprocess
from termcolor import colored
import datetime

class Logger:
    def log(self, msg, color='magenta'):
        timestamp = colored("{:%H:%M:%S.%f}".format(datetime.datetime.now()), 'yellow')
        label = colored("Runner", color)
        print("%s[%s]%s" % (timestamp, label, msg))

    def error(self, msg):
        self.log(msg, color='red')

    def raw(self, stdout, stderr):
        sys.stdout.write(stdout + '\n')
        sys.stderr.write(stderr + '\n')

class Config:
    def __init__(self, config_file, logger):
        self.config = self.baseconfig(logger) if not config_file or config_file == "None" else self.load_config(config_file, logger)
        self.logger = logger

    def get(self, key):
        if key not in self.config:
            self.logger.error("[Config] Key %s not found" % key)
            return None
        else:
            return self.config[key]

    def baseconfig(self, logger):
        base_path = os.path.join(os.getcwd(), ".baseconfig.json")
        if not os.path.exists(base_path):
            logger.log("[Config] Downloading default config")
            local("wget -O .baseconfig.json https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/implicit-context/scripts/.baseconfig.json", capture=True)
        with open(os.path.join(base_path)) as base_file:
            return json.load(base_file)

    def load_config(self, config_file, logger):
        config = self.baseconfig(logger)
        with open(config_file) as data_file:
            userconfig = json.load(data_file)
            return self.override_config(config, userconfig)

    def override_config(self, base, userconfig):
        config = base
        for field in userconfig:
            config[field] = userconfig[field]
        return config

class PipelineImpl:
    def __init__(self, config_file):
        self.logger = Logger()
        self.config = Config(config_file, self.logger)

    def local(self, command, directory, verbose=False):
        with lcd(directory):
            res = local(command, capture=True)
            if verbose:
                self.logger.raw(str(res.stdout), str(res.stderr))
            return res

    def local_canfail(self, name, command, directory, verbose=False, interactive=False):
        failed = False
        with settings(warn_only=True):
            result = self.local(command, directory, verbose=verbose)
            if result.failed:
                with lcd(directory):
                    with open(os.path.join(directory, name.replace(" ", "_")), 'w') as command_report:
                        command_report.write(result.stdout)
                        command_report.write(result.stderr)
                if not interactive:
                    failed = True
                else:
                    if confirm("%s failed. Abort?" % name):
                        abort("Aborting at user request.")
                    else:
                        failed = True
        return failed

    def load_project_info(self, path):
        info_path = os.path.join(path, "project.csv")
        if not os.path.exists(info_path):
            create_project_info(path)
        with open(info_path) as csv_file:
            reader = csv.reader(csv_file)
            names = next(reader)
            data = next(reader)
            info = dict(zip(names, data))
            self.info("[%s] Load project info" % info["name"])
            return info

    def get_report(self, project_path, kind):
        report_path = os.path.join(project_path, self.config.get(kind))
        if not os.path.exists(report_path):
            return None
        with open(report_path) as report:
            return report.read()

    def write_report(self, content, project_path, kind):
        report_path = os.path.join(project_path, self.config.get(kind))
        with open(report_path, 'w') as report:
            return report.write(content)

    def info(self, msg):
        if self.config.get("debug_info"):
            self.logger.log(msg)

    def error(self, msg):
        if self.config.get("debug_info"):
            self.logger.error(msg)

    def checkout_latest_tag(self, project_name, project_path):
        self.info("[Import][%s] Checkout latest tag..." % project_name)
        self.local("git fetch --tags --depth 1", project_path)
        self.local_canfail("Load latest tag", "latestTag=$( git describe --tags `git rev-list --tags --max-count=1` )", directory=project_path)
        self.local("git checkout $latestTag", project_path)

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
        if self.has_file_with_ending(project_path, "Vagrantfile"):
            systems.append("vagrant")
        return systems

    def is_build_system_supported(self, src_path):
        project_build_systems = self.get_build_systems(src_path)
        return any(system in self.config.get("supported_build_systems") for system in project_build_systems)

class Pipeline:
    __instance = None
    def get(self, config_file):
        if Pipeline.__instance is None:
            Pipeline.__instance = PipelineImpl(config_file)
        return Pipeline.__instance

    def __call__(self):
        return self

@task
def create_project_info(project_path, config_file=None):
    P = Pipeline().get(config_file)
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

    project_name = os.path.split(project_path)[1]
    P.info("[Import][%s] Creating project info..." % project_name)

    version = P.local("git describe --always", project_path)
    commit = P.local("git rev-parse HEAD", project_path)
    url = P.local("git config --get remote.origin.url", project_path)
    reponame = url.split('.com/')[1]
    sloc = count_locs(project_path)
    repo_info = json.loads(P.local("curl 'https://api.github.com/repos/%s'" % reponame, project_path))
    gh_stars = repo_info["stargazers_count"] if "stargazers_count" in repo_info else -1
    build_system = P.get_build_systems(project_path)
    project_info = {
       "name": str(project_name),
       "version": version,
       "last_commit": commit,
       "url": url,
       "total_loc": sloc["total"],
       "scala_loc": sloc["scala"],
       "reponame": reponame,
       "gh_stars": gh_stars,
       "build_system": '|'.join(build_system)
    }
    P.info("[Import][%s] Capture project info: %s" % (project_name, project_info))
    with open(os.path.join(project_path, "project.csv"), "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        writer.writerow(project_info.keys())
        writer.writerow(project_info.values())
    sys.exit(0)

@task
def import_single(src_path, config_file=None):

    def do_import(subdir, srcpath, destpath):
        cwd = os.getcwd()
        shutil.copytree(srcpath, destpath)
        P.info("[Import] Done!")
        return False

    P = Pipeline().get(config_file)
    dest_dir = P.config.get("projects_dest")
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
def compile(project_path, config_file=None):
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
    P = Pipeline().get(config_file)
    P.info("[CCompile][%s] Attempting compilation of %s" % (project_name, project_name))
    report = P.get_report(project_path, "compilation_report")
    if report:
        P.info("[CCompile][%s] Compilation report found. Skipping" % project_name)
        sys.exit(0 if report.startswith("SUCCESS") else 1)

    backwards_steps = P.config.get("max_backwards_steps")
    tag_stream = P.local("git describe --always --abbrev=0 --tags `git rev-list --tags --max-count=%s`" % backwards_steps, project_path)
    current_commit = P.local("git describe --always --abbrev=0", project_path)
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
def gen_sdb(project_path, config_file=None):
    cwd = os.getcwd()
    P = Pipeline().get(config_file)
    plugin_url = P.config.get("semanticdb_plugin_url")
    project_name = os.path.split(project_path)[1]

    def download_sbt_plugin(plugin_url, project_path):
        dest_folder = os.path.join(project_path, "project")
        if not os.path.exists(os.path.join(dest_folder, "SemanticdbConfigure.scala")):
            P.local("wget -O SemanticdbConfigure.scala %s" % plugin_url, dest_folder)

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

    if not clean_compile_succeeded(project_path, project_name):
        sys.exit(1)

    P.info("[GenSDB][%s] Looking for report from previous compilation..." % project_name)
    report = P.get_report(project_path, "semanticdb_report")
    force_recompile = P.config.get("force_recompile_on_fail")
    if report is None or (report == "ERROR" and force_recompile):
        P.info("[GenSDB][%s] Not found. Recompiling..." % project_name)
        download_sbt_plugin(plugin_url, project_path)
        failed = P.local_canfail("Generate semanticdb", "sbt -batch semanticdb compile", project_path, verbose=True, interactive=False)
        if not failed:
            handle_success(project_path, project_name)
        if failed and P.config.get("allow_partial_semanticdb_files"):
            handle_partial(project_path, project_name)
        else:
            handle_error(project_path, project_name)
    else:
        P.info("[GenSDB][%s] Compilation report found (%s). Skipping" % (project_name, report))
        sys.exit(0)

@task
def analyze(project_path, always_abort=True, config_file=None):
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

    def upload_to_database(project_name, project_path, tool_path, commit_to_db):
        P.info("[Analysis][%s] Uploading to database...(commit changes: %s)" % (project_name, commit_to_db))
        commit_option = "-y" if commit_to_db else "-n"
        command = "python %s %s ." % (tool_path, commit_option)
        title = "DB upload"
        success = analysis_command(project_path, project_name, title, command, "db_push_report")
        if success:
            if commit_to_db:
                P.write_report('SUCCESS', project_path, "db_push_report")
                P.info("[Analysis][%s] Changes commited to the database" % project_name)
            else:
                P.write_report('UNCOMMITED', project_path, "db_push_report")
                P.info("[Analysis][%s] DB changes NOT COMMITED" % project_name)
        return success

    P = Pipeline().get(config_file)
    setup(config_file)

    cwd = os.getcwd()
    jvm_options = P.config.get("analyzer_jvm_options")
    analysis_tool_path = os.path.join(cwd, P.config.get("tools_dir"), P.config.get("analyzer_name"))
    cleanup_tool_path = os.path.join(cwd, P.config.get("tools_dir"), P.config.get("cleanup_tool_name"))
    db_tool_path = os.path.join(cwd, P.config.get("tools_dir"), P.config.get("db_push_tool_name"))
    push_to_db = P.config.get("push_to_db_enabled")
    commit_to_db = P.config.get("commit_to_db")

    project_info = P.load_project_info(project_path)
    project_name = project_info["name"]

    compilation_failed = P.local_canfail("SDB File generation", "fab gen_sdb:project_path=%s,config_file=%s" % (project_path, config_file), cwd, verbose=True)
    continue_analysis = not compilation_failed
    if continue_analysis:
        continue_analysis = run_analysis_tool(project_name, project_path, analysis_tool_path, jvm_options)
    if continue_analysis:
        continue_analysis = run_cleanup_tool(project_name, project_path, cleanup_tool_path)
    if continue_analysis and push_to_db:
       continue_analysis = upload_to_database(project_name, project_path, db_tool_path, commit_to_db)
    P.info("[Analysis][%s] Analysis concluded" % project_name)
    sys.exit(0 if continue_analysis else 1)

@task
def setup(config_file=None):
    cwd = os.getcwd()
    P = Pipeline().get(config_file)

    def download_tool(title, name, url, dest_path):
        tool_path = os.path.join(tools_dir, name)
        if not os.path.exists(tool_path):
            P.info("[Setup][%s] Not found. Dowloading..." % title)
            P.local("wget -O %s %s" % (name, url), dest_path)

    P.info("[Setup] Setting up...")
    tools_dir = os.path.join(cwd, P.config.get("tools_dir"))
    if not os.path.exists(tools_dir):
        os.mkdir(tools_dir)
    download_tool("Semanticdb Analyzer", P.config.get("analyzer_name"), P.config.get("analyzer_url"), tools_dir)
    download_tool("Data cleanup tool", P.config.get("cleanup_tool_name"), P.config.get("cleanup_tool_url"), tools_dir)
    download_tool("Data cleanup library", P.config.get("cleanup_library_name"), P.config.get("cleanup_library_url"), tools_dir)
    download_tool("Database upload tool", P.config.get("db_push_tool_name"), P.config.get("db_push_tool_url"), tools_dir)
    P.info("[Setup] Done")

@task
def merge_csv(config_file=None):
    cwd = os.getcwd()
    P = Pipeline().get(config_file)
    projects_path = P.config.get("projects_dest")
    P.info("[Reports] Gathering reports")
    for report in P.config.get("report_files"):
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
def condense_reports(config_file=None):
    def write_header(report_file):
        report_file.write("Condensed analysis reports, %s\n" % datetime.datetime.now())

    def append_report(project_path, report_file, report_kind):
        status = str(P.get_report(project_path, report_kind)).replace('\n', ' \\')
        report_file.write("  - %s: %s\n" % (report_kind, status))

    cwd = os.getcwd()
    P = Pipeline().get(config_file)
    projects_path = P.config.get("projects_dest")
    P.info("[Reports] Generating analysis report")
    with open(os.path.join(cwd, P.config.get("condensed_report")), 'w') as report_file:
        write_header(report_file)
        for subdir in os.listdir(projects_path):
            P.info("[Reports] Extracting from %s" % subdir)
            project_path = os.path.join(projects_path, subdir)
            project_name = subdir

            report_file.write("%s:\n" % project_name)
            reports = ["compilation_report", "semanticdb_report", "analyzer_report", "cleanup_report", "db_push_report"]
            for report in reports:
                append_report(project_path, report_file, report)