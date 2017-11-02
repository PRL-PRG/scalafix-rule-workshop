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

def local_canfail(name, command):
    failed = False
    with settings(warn_only=True):
        result = local(command)
        if result.failed:
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

def cleanup(cwd):
    log("Removing subdirs...")
    for item in os.listdir(cwd):
        if os.path.isdir(item):
            log("  %s" % item)
            shutil.rmtree(item)

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
    log("  Checkout latest tag...")
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

def generate_semanticdb_files(cwd, project_name, project_path, plugin_url):
    log("  Looking for compilation report...")
    continue_analysis = True
    if not os.path.exists(os.path.join(project_path, "SEMANTICDB_COMPLILATION_COMPLETE.TXT")):
        log("    Not found. Recompiling...")
        with lcd(project_path):
            checkout_latest_tag()
            project_info = create_project_info(project_name, project_path)
            download_sbt_plugin(plugin_url, "./project/")
            failed = local_canfail("Generate semanticdb", "sbt semanticdb compile")   
            if not failed: 
                local("echo %s >> SEMANTICDB_COMPLILATION_COMPLETE.TXT" % project_info["version"])
            else:
                log("    Skipping project")
                continue_analysis = False
        lcd(cwd)
    else:
        log("    Compilation report found. Skipping")
    return continue_analysis

def run_analysis_tool(project_path, tool_path):    
    log("  Analyzing semanticdb files...")
    continue_analysis = True
    if not os.path.exists(os.path.join(project_path, "SEMANTICDB_ANALYSIS_COMPLETE.TXT")):        
        failed = local_canfail("Analyze semanticdb files", "java -jar %s %s" % (tool_path, project_path))
        if not failed:
            local("cat %s/SEMANTICDB_COMPLILATION_COMPLETE.TXT >> %s/SEMANTICDB_ANALYSIS_COMPLETE.TXT" % (project_path, project_path))
            log("    Done")
        else:
            log("    Skipping project")
            continue_analysis = False
    else:
        log("    Analysis report found. Skipping")
    return continue_analysis

def analyze_projects(cwd, config):        
    projects = config["projects_dest"]
    plugin_url = config["semanticdb_plugin_url"]
    analysis_tool_path = os.path.join(cwd, config["tools_dir"], config["analyzer_name"])
    projects_path = os.path.join(cwd, projects)
    for subdir in os.listdir(projects_path):
        log("%s" % subdir)
        subdir_path = os.path.join(projects_path, subdir)
        continue_analysis = generate_semanticdb_files(projects_path, subdir, subdir_path, plugin_url)
        if continue_analysis: continue_analysis = run_analysis_tool(subdir_path, analysis_tool_path)

def download_extraction_tools(cwd, config):
    def download_tool(title, name, url):
        log("  %s" % title)
        tool_path = os.path.join(tools_dir, name)
        if not os.path.exists(tool_path):
            log("    Not found. Dowloading...")
            local("wget -O %s %s" % (name, url), capture=True)
        else:
            log("    Already downloaded")
        
    log("Downloading tools...")
    tools_dir = os.path.join(cwd, config["tools_dir"])
    if not os.path.exists(tools_dir):
        os.mkdir(tools_dir)
    with lcd(tools_dir):
        download_tool("Semanticdb Analyzer", config["analyzer_name"], config["analyzer_url"])
        download_tool("Data cleanup tool", config["cleanup_tool_name"], config["cleanup_tool_url"])
        download_tool("Database upload tool", config["db_push_tool_name"], config["db_push_tool_url"])


def run(cwd, config):
    import_projects(config["sbt_projects"], config["projects_dest"])
    download_extraction_tools(cwd, config)
    analyze_projects(cwd, config)

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

    