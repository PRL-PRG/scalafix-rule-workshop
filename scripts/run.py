#!/usr/bin/python

import argparse
import json
import os
import shutil
import sys
from fabric.api import local, abort, lcd, get
from fabric.contrib.console import confirm
from fabric.contrib import files
import csv
import subprocess
from termcolor import colored

def log(msg, color='green'):
    label = colored("Runner", color)
    print("[%s] %s" % (label, msg))

def error(msg):
    log(msg)

def log_process(process):
    while True:
        output = process.stdout.readline()
        if output == '' and process.poll() is not None:
            break
        if output:
            log(output.decode('ascii').strip())
        rc = process.poll()
    process.terminate()

def handle_subprocess_error(name, result):
    if result.failed and not confirm("%s failed. Continue anyway?" % name):
        abort("Aborting at user request.")           

def parse_cli_args():
    parser = argparse.ArgumentParser(description='Collect data from sbt projects and push it into a database')
    parser.add_argument('--config', help='Path to the .collector.json config file', type=str)
    parser.add_argument('--cleanup', action='store_true', help='Invoke in cleanup mode. If invoked this way, only cleanup will be made')
    return parser.parse_args()

def cleanup(cwd):
    for subdir in os.listdir(cwd):
        if os.path.isdir(subdir):
            shutil.rmtree(subdir)

def load_config(file):
    with open(file) as data_file:    
        return json.load(data_file)

def import_projects(source, dest):
    log("Importing projects into %s..." % dest)
    for subdir in os.listdir(source):  
        srcpath = os.path.join(source, subdir)
        destpath = os.path.join(dest, subdir)
        if os.path.isdir(srcpath) and os.path.exists(os.path.join(srcpath, "build.sbt")):
            try:
                log("  %s" % str(srcpath))
                if os.path.exists(destpath):
                    log("    Already imported")
                else:
                    shutil.copytree(srcpath, destpath)                
            except RuntimeError as error:
                print(error)   

def checkout_latest_tag():
    log("    Checkout latest tag...")
    local("git fetch --tags", capture=True)
    local("latestTag=$( git describe --tags `git rev-list --tags --max-count=1` )", capture=True)
    local("git checkout $latestTag", capture=True)

def create_project_info(name, path):
    version = local("git describe --always", capture=True)
    commit = local("git rev-parse HEAD", capture=True)
    url = local("git config --get remote.origin.url", capture=True)
    project_info = {"path": str(name), "name": str(name), "version": version, "last_commit": commit, "url": url}
    log("  Capture project info: %s" % project_info)
    with open(os.path.join(path, "project.csv"), "w") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        writer.writerow(project_info.keys())
        writer.writerow(project_info.values())
    return project_info

def download_sbt_plugin(plugin_url, dest_folder):    
    if not os.path.exists(os.path.join(dest_folder, "SemanticdbConfigure.scala")):
        with lcd(dest_folder):
            local("wget -O SemanticdbConfigure.scala %s" % plugin_url, capture=True)

def compile_project():
    result = local("sbt semanticdb compile", capture=True)
    handle_subprocess_error("Generate semanticdb", result)

def generate_semanticdb_files(cwd, projects, plugin_url):
    log("Generating semanticdb files...")
    projects_path = os.path.join(cwd, projects)
    for subdir in os.listdir(projects_path):
        log("  %s" % subdir)
        subdir_path = os.path.join(projects_path, subdir)
        if not os.path.exists(os.path.join(subdir_path, "SEMANTICDB_REPORT.TXT")):
            with lcd(subdir_path):
                checkout_latest_tag()
                project_info = create_project_info(subdir, subdir_path)
                download_sbt_plugin(plugin_url, "./project/")
                compile_project()                
                local("echo %s >> SEMANTICDB_REPORT.TXT" % project_info["version"])
            lcd(projects_path)
        else:
            log("    Project info found. Skipping")

def run(cwd, config):
    download_sbt_plugin(config["semanticdb_plugin_url"], config["local_sbt_folder"])
    import_projects(config["sbt_projects"], config["projects_dest"])
    generate_semanticdb_files(cwd, config["projects_dest"], config["semanticdb_plugin_url"])

def main():
    cwd = os.getcwd()
    args = parse_cli_args()
    if args.cleanup:
        cleanup(cwd)
        sys.exit(0)
    if args.config:        
        config = load_config(args.config)
        run(cwd, config)
    else:
        log("No config provided.")
        sys.exit(0)
  
main()

    