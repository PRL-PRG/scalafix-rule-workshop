import os
import subprocess
import csv
import sys
from Fixer import Fixer

fully_qualified_name_fixes = [
    "remove_leading_root",
    "remove_trailing_dot",
    "extract_function_name"
    "remove_L_notation",
    "remove_hashtags",
]

def get_project_info(cwd):
    with open(os.path.join(cwd, "project.csv"), "r") as projfile:  
        reader = csv.reader(projfile)
        names = next(reader)
        data = next(reader)
        info = dict(zip(names, data))
        print(info)
        return info

def clean_param_row(row, fixer):
    row["id"] = fixer.fix(fully_qualified_name_fixes, row["id"])
    row["clazz"] = fixer.fix(fully_qualified_name_fixes, row["clazz"])
    row["kind"] = fixer.fix(["replace_unknown_kinds"], row["kind"])
    return row

def clean_funs_row(row, fixer):
    row["symbol"] = fixer.fix(["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"], row["symbol"])
    row["fun_name"] = fixer.fix(["extract_function_name"], row["symbol"])
    row["parameters"] = fixer.fix(["extract_parameter_list"], row["symbol"])
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

def clean_project(directory):    
    info = get_project_info(directory)
    fixer = Fixer(info)
    
    params_path = directory+"/params"
    funs_path = directory+"/funs"
    params_funs_path = directory+"/params-funs"

    if os.path.exists(params_path+".csv") and os.path.exists(funs_path+".csv") and os.path.exists(params_funs_path+".csv"):
        print("Cleaning %s" % info["name"])

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
                    try:
                      clean_project(subdir)
                    except (ValueError, TypeError):
                      print("Error cleaning %s" % subdir)
        else:
            for arg in range(1, len(sys.argv)):
                clean_project(sys.argv[arg])        
    else:
        print ("No arguments provided")


main()
