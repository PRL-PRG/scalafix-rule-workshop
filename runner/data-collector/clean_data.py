import os
import subprocess
import csv
import sys
from Fixer import Fixer

fully_qualified_name_fixes = ["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"]


def get_project_info(cwd):
    # Assuming cwd is the project
    path = os.path.abspath(cwd)
    name = os.path.relpath(cwd, cwd + "/..")
    os.chdir(name)
    version = subprocess.check_output(["git", "describe", "--always"]).decode('ascii').strip()
    commit = subprocess.check_output(["git", "rev-parse", "HEAD"]).decode('ascii').strip()
    url = subprocess.check_output(["git", "config", "--get", "remote.origin.url"]).decode('ascii').strip()
    os.chdir("..")
    return {"path": path, "name": name, "version": version, "last_commit": commit, "url": url}

def clean_param_row(row, fixer):
    row["id"] = fixer.fix(fully_qualified_name_fixes, row["id"])
    row["clazz"] = fixer.fix(fully_qualified_name_fixes, row["clazz"])
    row["kind"] = fixer.fix(["replace_unknown_kinds"], row["kind"])
    return row

def clean_funs_row(row, fixer):
    row["symbol"] = fixer.fix(fully_qualified_name_fixes, row["symbol"])
    return row

def clean_links_row(row, fixer):
    row["from"] = fixer.fix(fully_qualified_name_fixes, row["from"])
    return row

def clean_file(basepath, fixer, clean_function):
    with open(basepath+".csv", "r") as original:  
        with open(basepath+".clean.csv", "w") as clean_file:  
            reader = csv.DictReader(original)
            writer = csv.DictWriter(clean_file, fieldnames = reader.fieldnames)    
            writer.writeheader()
            for row in reader:
                new_values = clean_function(row, fixer)
                writer.writerow(new_values)

def save_project_data(dir, project_info):
     with open(dir+"/project.csv", "w", newline='') as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        writer.writerow(project_info.keys())
        writer.writerow(project_info.values())

def clean_project(dir):    
    info = get_project_info(dir)
    fixer = Fixer(info)
    
    params_path = dir+"/params"
    funs_path = dir+"/funs"
    params_funs_path = dir+"/params-funs"

    if os.path.exists(params_path+".csv") and os.path.exists(funs_path+".csv") and os.path.exists(params_funs_path+".csv"):
        save_project_data(dir, info)
        clean_file(params_path, fixer, clean_param_row)
        clean_file(funs_path, fixer, clean_funs_row)
        clean_file(params_funs_path, fixer, clean_links_row)
    else: 
        print("csv files not found")

def main():
    # Assume CWD is the codebases/ folder
    root = os.getcwd()

    if len(sys.argv) >= 2:
        if sys.argv[1] == "--all":
            for subdir in os.listdir(root):  
                if os.path.isdir(os.path.join(root, subdir)):  
                    clean_project(subdir)
        else:
            for arg in range(1, len(sys.argv)):
                clean_project(sys.argv[arg])        
    else:
        print ("No arguments provided")


main()