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

def log(msg, color='magenta'):
    timestamp = colored("{:%H:%M:%S.%f}".format(datetime.datetime.now()), 'yellow')
    label = colored("Runner", color)
    print("%s[%s]%s" % (timestamp, label, msg))

def error(msg):
    log(msg, color='red')

def log_process(process):
    while True:
        output = process.stdout.readline()
        if output == '' and process.poll() is not None:
            break
        if output:
            log(output.decode('ascii').strip())
        rc = process.poll()
    process.terminate()

def local_canfail(name, command, verbose=True, interactive=True):
    failed = False
    with settings(warn_only=True):
        result = local(command, capture=(not verbose))
        if result.failed:
            if not interactive:
                failed = True
            else:
                if confirm("%s failed. Skip project?" % name):
                    failed = True
                else:
                    abort("Aborting at user request.")
    return failed


def parse_cli_args():
    parser = argparse.ArgumentParser(description='Collect data from sbt projects and push it into a database')
    parser.add_argument('--config', help='Path to the .collector.json config file', type=str)
    parser.add_argument('--cleanup', action='store_true', help='Invoke in cleanup mode. If invoked this way, only cleanup will be made')
    return parser.parse_args()

def cleanup():
    cwd = os.getcwd()
    log("Removing subdirs...")
    for item in os.listdir(cwd):
        if os.path.isdir(item):
            log("  %s" % item)
            shutil.rmtree(item)

def baseconfig():
    base_path = os.path.join(os.getcwd(), ".baseconfig.json")
    if not os.path.exists(base_path):
        log("Downloading default config")
        local("wget -O .baseconfig.json https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/implicit-context/scripts/.collector.json", capture=True)
    with open(os.path.join(base_path)) as base_file:
        return json.load(base_file)

def override_config(base, userconfig):
    config = base
    for field in userconfig:
        config[field] = userconfig[field]
    return config

def load_config(file):
    config = baseconfig()
    with open(file) as data_file:
        userconfig = json.load(data_file)
        return override_config(config, userconfig)

def importp(source, dest="./projects"):
    log("[Import] Importing projects into %s..." % dest)
    for subdir in os.listdir(source):
        srcpath = os.path.join(source, subdir)
        destpath = os.path.join(dest, subdir)
        if os.path.isdir(srcpath) and os.path.exists(os.path.join(srcpath, "build.sbt")):
            try:
                log("  %s" % str(srcpath))
                if os.path.exists(destpath):
                    log("[Import]   Already imported")
                else:
                    shutil.copytree(srcpath, destpath)
                    log("[Import]   Done!")
            except RuntimeError as error:
                print(error)


def analyze(subdir, config={}):
    def checkout_latest_tag(project_name):
        log("[%s]     Checkout latest tag..." % project_name)
        local("git fetch --tags", capture=True)
        local_canfail("Load latest tag", "latestTag=$( git describe --tags `git rev-list --tags --max-count=1` )", False, False)
        local("git checkout $latestTag", capture=True)

    def create_project_info(name, path):
        def count_locs():
            local("cloc --out=cloc_report.csv --csv .")
            with open(os.path.join(path, "cloc_report.csv")) as csv_file:
                scala_lines = 0
                total_lines = 0
                reader = csv.DictReader(csv_file)
                for line in reader:
                    lines = int(line["code"])
                    total_lines += lines
                    if line["language"] == "Scala":
                        scala_lines += lines
            return {"total": total_lines, "scala": scala_lines}

        version = local("git describe --always", capture=True)
        commit = local("git rev-parse HEAD", capture=True)
        url = local("git config --get remote.origin.url", capture=True)
        sloc = count_locs()
        project_info = {"path": str(name), "name": str(name), "version": version, "last_commit": commit, "url": url, "total_loc": sloc["total"], "scala_loc": sloc["scala"]}
        log("[%s]     Capture project info: %s" % (name, project_info))
        with open(os.path.join(path, "project.csv"), "w") as csv_file:
            writer = csv.writer(csv_file, delimiter=',')
            writer.writerow(project_info.keys())
            writer.writerow(project_info.values())
        return project_info

    def download_sbt_plugin(plugin_url, dest_folder):
        if not os.path.exists(os.path.join(dest_folder, "SemanticdbConfigure.scala")):
            with lcd(dest_folder):
                local("wget -O SemanticdbConfigure.scala %s" % plugin_url, capture=True)

    def generate_semanticdb_files(cwd, project_name, project_path, plugin_url):
        log("[%s] Looking for compilation report..." % project_name)
        continue_analysis = True
        if not os.path.exists(os.path.join(project_path, "SEMANTICDB_COMPLILATION_COMPLETE.TXT")):
            log("[%s]   Not found. Recompiling..." % project_name)
            with lcd(project_path):
                checkout_latest_tag(project_name)
                project_info = create_project_info(project_name, project_path)
                download_sbt_plugin(plugin_url, "./project/")
                failed = local_canfail("Generate semanticdb", "sbt semanticdb compile", False)
                if not failed:
                    local("echo %s >> SEMANTICDB_COMPLILATION_COMPLETE.TXT" % project_info["version"])
                else:
                    error("[%s]   Skipping project" % project_name)
                    continue_analysis = False
            lcd(cwd)
        else:
            log("[%s]   Compilation report found. Skipping" % project_name)
        return continue_analysis

    def run_analysis_tool(project_name, project_path, tool_path, jvm_options):
        log("[%s] Analyzing semanticdb files..." % project_name)
        continue_analysis = True
        if not os.path.exists(os.path.join(project_path, "SEMANTICDB_ANALYSIS_COMPLETE.TXT")):
            failed = local_canfail("Semanticdb file analysis", "java -jar %s %s %s" % (jvm_options, tool_path, project_path))
            if not failed:
                local("cat %s/SEMANTICDB_COMPLILATION_COMPLETE.TXT >> %s/SEMANTICDB_ANALYSIS_COMPLETE.TXT" % (project_path, project_path))
                log("[%s]   Done" % project_name)
            else:
                error("[%s]   Skipping project" % project_name)
                continue_analysis = False
        else:
            log("[%s]   Analysis report found. Skipping" % project_name)
        return continue_analysis

    def run_cleanup_tool(project_name, project_path, tool_path):
        log("[%s] Cleaning up the data..." % project_name)
        continue_analysis = True
        if not os.path.exists(os.path.join(project_path, "DATA_CLEANUP_COMPLETE.TXT")):
            failed = local_canfail("Data cleanup", "python %s %s" % (tool_path, project_path))
            if not failed:
                local("cat %s/SEMANTICDB_ANALYSIS_COMPLETE.TXT >> %s/DATA_CLEANUP_COMPLETE.TXT" % (project_path, project_path))
                log("[%s]   Done" % project_name)
            else:
                log("[%s]   Skipping project" % project_name)
                continue_analysis = False
        else:
            log("[%s]   Cleanup report found. Skipping" % project_name)
        return continue_analysis

    def upload_to_database(project_name, project_path, tool_path, commit_to_db):
        log("[%s] Uploading to database...(commit changes: %s)" % (project_name, commit_to_db))
        continue_analysis = True
        commit_option = "-y" if commit_to_db else "-n"
        if not os.path.exists(os.path.join(project_path, "DB_UPLOAD_COMPLETE.TXT")):
            failed = local_canfail("DB upload", "python %s %s %s" % (tool_path, commit_option, project_path), False)
            if not failed:
                local("cat %s/DATA_CLEANUP_COMPLETE.TXT >> %s/DB_UPLOAD_COMPLETE.TXT" % (project_path, project_path))
                if commit_to_db:
                    log("[%s]   Changes commited to the database" % project_name, color='green')
                else:
                    log("[%s]   DB changes NOT COMMITED" % project_name, color='yellow')
            else:
                log("[%s]   Skipping project" % project_name)
                continue_analysis = False
        else:
            log("[%s]   Upload report found. Skipping" % project_name)
        return continue_analysis

    cwd = os.getcwd()
    config = override_config(baseconfig(), config)

    projects = config["projects_dest"]
    plugin_url = config["semanticdb_plugin_url"]
    jvm_options = config["analyzer_jvm_options"]
    analysis_tool_path = os.path.join(cwd, config["tools_dir"], config["analyzer_name"])
    cleanup_tool_path = os.path.join(cwd, config["tools_dir"], config["cleanup_tool_name"])
    db_tool_path = os.path.join(cwd, config["tools_dir"], config["db_push_tool_name"])
    projects_path = os.path.join(cwd, projects)

    log("[%s]" % subdir)
    subdir_path = os.path.join(projects_path, subdir)
    continue_analysis = generate_semanticdb_files(projects_path, subdir, subdir_path, plugin_url)
    if continue_analysis:
        continue_analysis = run_analysis_tool(subdir, subdir_path, analysis_tool_path, jvm_options)
    if continue_analysis:
        continue_analysis = run_cleanup_tool(subdir, subdir_path, cleanup_tool_path)
    if continue_analysis:
        continue_analysis = upload_to_database(subdir,subdir_path, db_tool_path, config["commit_to_db"])

def analyze_projects(cwd, config):
    projects = config["projects_dest"]
    projects_path = os.path.join(cwd, projects)
    for subdir in os.listdir(projects_path):
        analyze(subdir)

def setup(config={}):
    cwd = os.getcwd()
    config = override_config(baseconfig(), config)
    def download_tool(title, name, url):
        log("[Setup] %s" % title)
        tool_path = os.path.join(tools_dir, name)
        if not os.path.exists(tool_path):
            log("[Setup]   Not found. Dowloading...")
            local("wget -O %s %s" % (name, url), capture=True)
        else:
            log("[Setup]   Already downloaded")

    log("Downloading tools...")
    tools_dir = os.path.join(cwd, config["tools_dir"])
    if not os.path.exists(tools_dir):
        os.mkdir(tools_dir)
    with lcd(tools_dir):
        download_tool("Semanticdb Analyzer", config["analyzer_name"], config["analyzer_url"])
        download_tool("Data cleanup tool", config["cleanup_tool_name"], config["cleanup_tool_url"])
        download_tool("Data cleanup library", config["cleanup_library_name"], config["cleanup_library_url"])
        download_tool("Database upload tool", config["db_push_tool_name"], config["db_push_tool_url"])


def run(cwd, config):
    importp(config["sbt_projects"], config["projects_dest"])
    setup(config)
    analyze_projects(cwd, config)
    log("All done!", color="green")

def main():
    cwd = os.getcwd()
    args = parse_cli_args()
    if args.cleanup:
        cleanup(cwd)
        sys.exit(0)
    if args.config:
        config = load_config(args.config)
        print(config["db_push_tool_name"])
        run(cwd, config)
    else:
        log("No config provided. Loading default...")
        config = baseconfig()
        run(cwd, config)
