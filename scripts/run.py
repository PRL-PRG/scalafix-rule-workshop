#!/usr/bin/python3

import argparse
import json
import os
import shutil
import sys
import subprocess

def log(msg):
    print("[Runner] %s" % msg)

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

def handle_subprocess_error(name, process):
    print(process)
    if process.returncode:
        error("Subprocess %s ended unsuccessfully." % name)
        error("  (q)uit? (i)gnore (s)how?")
        option = input("")
        if option == "q":
            log("  Oki-doke")
            sys.exit(1)
        elif option == "s":
            log(process.decode('ascii').strip())
        elif option == "i":
            log("  Ignoring")
        else:
            log("Wrong option. Ignoring by default")            

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
        if os.path.isdir(srcpath):
            try:
                log("  %s" % str(srcpath))
                shutil.copytree(srcpath, destpath)
            except RuntimeError as error:
                print(error)   

def generate_semanticdb_files(cwd, projects):
    log("Generating semanticdb files...")
    projects_path = os.path.join(cwd, projects)
    for subdir in os.listdir(projects_path):
        log("  %s" % subdir)
        subdir_path = os.path.join(projects_path, subdir)
        os.chdir(subdir_path)
        sbt_subprocess = subprocess.Popen(["sbt", "semanticdb", "compile"], stdout=subprocess.).communicate()
        handle_subprocess_error("sbt", sbt_subprocess)
        os.chdir(projects_path)

def run(cwd, config):
    import_projects(config["sbt_projects"], config["projects_dest"])
    generate_semanticdb_files(cwd, config["projects_dest"])

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

    