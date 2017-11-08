import os
import subprocess
import csv
import sys
from Fixer import Fixer

fully_qualified_name_fixes = [
    "remove_leading_root",
    "remove_trailing_dot",
    "extract_function_name",
    "remove_L_notation",
    "remove_hashtags",
]

def get_project_info(cwd):
    with open(os.path.join(cwd, "project.csv"), "r") as projfile:  
        reader = csv.reader(projfile)
        names = next(reader)
        data = next(reader)
        info = dict(zip(names, data))
        return info

def clean_param_row(row, fixer):
    row["fqn"] = fixer.fix(fully_qualified_name_fixes, row["fqn"])
    row["class"] = fixer.fix(fully_qualified_name_fixes, row["class"])
    row["kind"] = fixer.fix(["replace_unknown_kinds"], row["kind"])
    return row

def clean_funs_row(row, fixer):
    row["symbol"] = fixer.fix(["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"], row["symbol"])
    row["fqfn"] = fixer.fix(["extract_function_name"], row["symbol"])
    row["fqparamlist"] = fixer.fix(["extract_parameter_list"], row["symbol"])
    return row

def clean_links_row(row, fixer):
    row["from"] = fixer.fix(fully_qualified_name_fixes, row["from"])
    return row

def clean_declared_implicits_row(row, fixer):
    try:
        row["fqn"] = fixer.fix(["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"], row["fqn"])
        row["kind"] = fixer.fix(["replace_unknown_kinds"], row["kind"])
        row["class"] = fixer.fix(fully_qualified_name_fixes, row["class"])
    except AttributeError as error:
        print(row)
        print(error)
        sys.exit(1)
    return row

def clean_file(directory, filename, fixer, clean_function):
    basepath = directory + "/" + filename
    filepath = basepath+'.csv'
    if not os.path.exists(filepath):
        print("File %s not found. Skipping" % filepath)
        return

    with open(filepath, "r") as original:
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
    print("Cleaning %s" % info["name"])

    clean_file(directory, "params", fixer, clean_param_row)
    clean_file(directory, "funs", fixer, clean_funs_row)
    clean_file(directory, "params-funs", fixer, clean_links_row)
    clean_file(directory, "declared-implicits", fixer, clean_declared_implicits_row)


def main():
    # Assume CWD is the codebases/ folder
    root = os.getcwd()

    if len(sys.argv) >= 2:        
        for arg in range(1, len(sys.argv)):
            clean_project(sys.argv[arg])        
    else:
        print ("No arguments provided")
        sys.exit(1)


main()
