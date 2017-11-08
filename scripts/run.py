#!/usr/bin/python

import argparse
import json
import os
import shutil
import sys
from fabric.api import local, abort, lcd, get, settings
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

class Config:
    def __init__(self, config_file, logger):
        self.config = self.load_config(config_file, logger) if config_file else self.baseconfig(logger)
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
                self.info("[%s] STDOUT:\n%s" % (command[:10]+"...", u'%s' % res.stdout))
                self.info("[%s] STDERR:\n%s" % (command[:10]+"...", u'%s' % res.stderr))
                self.info("[%s] DONE" % (command[:10]+"..."))
            return res
  
    def local_canfail(self, name, command, directory, verbose=False, interactive=False):
        failed = False
        with settings(warn_only=True):
            result = self.local(command, directory, verbose=verbose)
            if result.failed:
                if not interactive:
                    failed = True
                else:
                    if confirm("%s failed. Abort?" % name):
                        abort("Aborting at user request.")
                    else:
                        failed = True
        return failed

    def load_project_info(self, path):
        with open(os.path.join(path, "project.csv")) as csv_file:
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
        self.logger.log(msg)

    def error(self, msg):
        self.logger.error(msg)
   
class Pipeline:
    __instance = None        
    def get(self, config_file):
        if Pipeline.__instance is None:
            Pipeline.__instance = PipelineImpl(config_file)
        return Pipeline.__instance

    def __call__(self):
        return self

def import_single(src_path, config_file=None):

    def checkout_latest_tag(project_name, project_path):
        P.info("[Import][%s] Checkout latest tag..." % project_name)
        P.local("git fetch --tags --depth 1", project_path)
        P.local_canfail("Load latest tag", "latestTag=$( git describe --tags `git rev-list --tags --max-count=1` )", directory=project_path)
        P.local("git checkout $latestTag", project_path)

    def create_project_info(project_name, project_path):
        def count_locs():
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

        version = P.local("git describe --always", project_path)
        commit = P.local("git rev-parse HEAD", project_path)
        url = P.local("git config --get remote.origin.url", project_path)        
        reponame = url.split('.com/')[1]
        sloc = count_locs()
        repo_info = json.loads(P.local("curl 'https://api.github.com/repos/%s'" % reponame, project_path))
        gh_stars = repo_info["stargazers_count"] if "stargazers_count" in repo_info else -1
        project_info = {
            "name": str(project_name),
            "version": version,
            "last_commit": commit,
            "url": url,
            "total_loc": sloc["total"],
            "scala_loc": sloc["scala"],
            "reponame": reponame,
            "gh_stars": gh_stars
        }
        P.info("[Import][%s] Capture project info: %s" % (project_name, project_info))
        with open(os.path.join(project_path, "project.csv"), "w") as csv_file:
            writer = csv.writer(csv_file, delimiter=',')
            writer.writerow(project_info.keys())
            writer.writerow(project_info.values())
        return project_info

    def do_import(subdir, srcpath, destpath):
        shutil.copytree(srcpath, destpath)        
        checkout_latest_tag(subdir, destpath)
        create_project_info(subdir, destpath)
        P.info("[Import] Done!")

    P = Pipeline().get(config_file)
    dest_dir = P.config.get("projects_dest")
    subdir = os.path.basename(src_path)
    dest_path = os.path.join(dest_dir, subdir)
    if os.path.isdir(src_path) and os.path.exists(os.path.join(src_path, "build.sbt")):
        try:
            P.info("[Import] %s" % str(src_path))
            if not os.path.exists(dest_path):
                do_import(subdir, src_path, dest_path)
            else:
                if os.path.exists(os.path.join(dest_path, "project.csv")):
                    P.info("[Import] Already imported")
                else:
                    P.info("[Import] Failed last time. Removing")
                    shutil.rmtree(dest_path)
                    do_import(subdir, src_path, dest_path)
        except RuntimeError as error:
            print(error)
            
def import_all(source, config_file=None):
    P = Pipeline().get(config_file)
    dest_folder = P.config.get("projects_dest")
    P.info("[Import] Importing projects into %s..." % dest_folder)
    for subdir in os.listdir(source):
        src_path = os.path.join(source, subdir)
        import_single(src_path, config_file)
      
def gen_sdb(project_path, config_file=None):
    cwd = os.getcwd()
    P = Pipeline().get(config_file)
    plugin_url = P.config.get("semanticdb_plugin_url")
    project_info = P.load_project_info(project_path)
    project_name = project_info["name"]

    def download_sbt_plugin(plugin_url, project_path):
        dest_folder = os.path.join(project_path, "project")
        if not os.path.exists(os.path.join(dest_folder, "SemanticdbConfigure.scala")):            
            P.local("wget -O SemanticdbConfigure.scala %s" % plugin_url, dest_folder)

    def sdb_files_exist(path):
        for wd, subdirs, files in os.walk(path):
            for file in files:
                if file.endswith("semanticdb"):
                    return True

    P.info("[%s] Looking for compilation report..." % project_name)
    continue_analysis = True
    report = P.get_report(project_path, "compilation_report")
    if report is None or report == "ERROR":
        P.info("[%s] Not found. Recompiling..." % project_name)       
        download_sbt_plugin(plugin_url, project_path)
        project_info = P.load_project_info(project_path)
        failed = P.local_canfail("Generate semanticdb", "sbt -batch semanticdb compile", project_path, verbose=True, interactive=False)
        if sdb_files_exist(project_path):
            if failed:
                P.info("[%s] Compilation completed with errors. Some SDB files found" % project_name)
                P.write_report("PARTIAL", project_path, "compilation_report")
            else:
                P.info("[%s] Compilation completed succesfuly" % project_name)
                P.write_report("SUCCESS", project_path, "compilation_report")
        else:
            P.error("[%s] Skipping project" % project_name)
            P.write_report("ERROR", project_path, "compilation_report")
            continue_analysis = False        
    else:
        P.info("[%s] Compilation report found (%s). Skipping" % (project_name, report))
    return continue_analysis

def analyze(project_path, always_abort=True, config_file=None):
    def analysis_command(project_path, project_name, title, command, report_kind):
        success = True
        report = P.get_report(project_path, report_kind)
        if report is None or report == 'ERROR':
            failed = P.local_canfail(title, command, project_path)
            if not failed:
                P.write_report('SUCCESS', project_path, report_kind)
                P.info("[%s] Done" % project_name)
            else:
                P.write_report('ERROR', project_path, report_kind)
                P.error("[%s] Skipping project" % project_name)
                success = False
        else:
            P.info("[%s] %s report found (%s). Skipping" % (project_name, title, report)) 
        return success

    def run_analysis_tool(project_name, project_path, tool_path, jvm_options):
        P.info("[%s] Analyzing semanticdb files..." % project_name)
        title = "Semanticdb file analysis"
        command = "java -jar %s %s ." % (jvm_options, tool_path)
        return analysis_command(project_path, project_name, title, command, "analyzer_report")

    def run_cleanup_tool(project_name, project_path, tool_path):
        P.info("[%s] Cleaning up the data..." % project_name)
        title = "Data cleanup"
        command = "python %s ." % (tool_path)
        return analysis_command(project_path, project_name, title, command, "cleanup_report")

    def upload_to_database(project_name, project_path, tool_path, commit_to_db):
        P.info("[%s] Uploading to database...(commit changes: %s)" % (project_name, commit_to_db))
        commit_option = "-y" if commit_to_db else "-n"
        command = "python %s %s ." % (tool_path, commit_option)
        title = "DB upload"
        success = analysis_command(project_path, project_name, title, command, "db_push_report")
        if success:
            if commit_to_db:
                P.write_report('SUCCESS', project_path, "db_push_report")
                P.info("[%s] Changes commited to the database" % project_name)
            else:
                P.write_report('UNCOMMITED', project_path, "db_push_report")
                P.info("[%s] DB changes NOT COMMITED" % project_name)
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

    continue_analysis = gen_sdb(project_path)
    if continue_analysis:
        continue_analysis = run_analysis_tool(project_name, project_path, analysis_tool_path, jvm_options)
    if continue_analysis:
        continue_analysis = run_cleanup_tool(project_name, project_path, cleanup_tool_path)
    if continue_analysis and push_to_db:
       continue_analysis = upload_to_database(project_name, project_path, db_tool_path, commit_to_db)
    P.info("[%s] Analysis concluded" % project_name)

def analyze_projects(config_file=None):
    cwd = os.getcwd()
    P = Pipeline().get(config_file)
    projects = P.config.get("projects_dest")
    projects_path = os.path.join(cwd, projects)
    for subdir in os.listdir(projects_path):
        project_path = os.path.join(projects_path, subdir)
        analyze(project_path, config_file)
    condense_reports(config_file)

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

def condense_reports(config_file=None):
    def write_header(report_file):
        report_file.write("Condensed analysis reports, %s\n" % datetime.datetime.now())

    def append_report(project_path, report_file, report_kind):
        status = P.get_report(project_path, report_kind)
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
            append_report(project_path, report_file, "compilation_report")
            append_report(project_path, report_file, "analyzer_report")
            append_report(project_path, report_file, "cleanup_report")
            append_report(project_path, report_file, "db_push_report")
