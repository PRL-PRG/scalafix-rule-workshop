import os
import sys
import csv
import subprocess
import mysql.connector as sql

def get_project_info(cwd):
    # Assuming cwd is the project
    with open(os.path.join(cwd, "project.csv"), "r") as projfile:  
        reader = csv.reader(projfile)
        names = next(reader)
        data = next(reader)
        info = dict(zip(names, data))
        return info

def dump_params(full_path, project_id):
    csv_file = open(full_path, 'rb')
    csv_file.readline() # Skip headers
    reader = csv.reader(csv_file)
    ids = {}
    for param in reader: 
        print "Insert [%s, %s] into params" % (project_id, param)
        try:                
            cursor.execute("INSERT INTO params (project, name, fqn, type, fqtn, kind)"
                            "VALUES (%s, %s, %s, %s, %s, %s)",
                            (project_id, param[4], param[0], param[2], param[1], param[3]))
        except sql.Error as error:
            print error
            sys.exit()

        rowid = cursor.lastrowid
        ids[param[0]] = rowid # FIXME: This is an ugly way to accidentaly sidestep duplication            
    csv_file.close()
    return ids

def dump_funs(full_path, project_id):
    csv_file = open(full_path, 'rb')
    csv_file.readline() # Skip headers
    reader = csv.reader(csv_file)
    ids = {}
    for fun in reader:
        print "Insert [%s, %s] into funs" % (project_id, fun)
        try:
            cursor.execute("INSERT INTO funs (project, path, line, col, name, fqfn, nargs)"
                            "VALUES (%s, %s, %s, %s, %s, %s, %s)", 
                            (project_id, fun[1], fun[2], fun[3], fun[4], fun[5], fun[6]))

        except sql.Error as error:
            print error
            sys.exit()

        rowid = cursor.lastrowid
        ids[fun[0]] = rowid # FIXME: This is an ugly way to accidentaly sidestep duplication
             
    csv_file.close()
    return ids

def dump_declared_implicits(full_path, project_id):
    with open(full_path, 'rb') as file:
        reader = csv.DictReader(file)
        for line in reader:
            print "Insert [%s, %s] into declared_implicits" % (project_id, line)
            try:
                cursor.execute("INSERT INTO declared_implicits (project, sourcelink, path, line, col, name, fqn, class, type, kind)"
                               "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
                               (project_id, line["id"], line["path"], line["line"], line["col"], line["name"], line["fqn"], line["class"], line["type"], line["kind"]))
            except sql.Error as error:
                print error
                sys.exit()

def insert_project_data(dir, project_id):
    params_path = dir+"/params.clean.csv"
    funs_path = dir+"/funs.clean.csv"
    params_funs_path = dir+"/params-funs.clean.csv"
    declared_implicits_path = dir+"/declared-implicits.clean.csv"
    if os.path.exists(params_path) \
            and os.path.exists(funs_path) \
            and os.path.exists(params_funs_path)\
            and os.path.exists(declared_implicits_path):

        dump_declared_implicits(declared_implicits_path, project_id)
        param_ids = dump_params(params_path, project_id)
        funs_ids = dump_funs(funs_path, project_id)
        
        link_file = open(params_funs_path, 'rb')
        link_file.readline() # Skip headers
        link_reader = csv.reader(link_file)

        links = []
        for link in link_reader:
            links.append((param_ids[link[0]], funs_ids[link[1]]))

        link_file.close()

        link_set = set(links)
        for link in link_set:
            print "Insert (%s, %s) into params_funs" % (link[0], link[1])
            try:
                cursor.execute("INSERT INTO params_funs (param, fun)"
                                "VALUES (%s, %s)", 
                                (link[0], link[1]))
            except sql.Error as error:
                print error
                sys.exit()

    else: 
        print "csv files not found not found"

def insert_project_into_db(dir):
    projectfile = dir+"/project.csv"
    if not os.path.exists(projectfile):
        raise RuntimeError("No project info found")

    info = get_project_info(dir)    
    # Push into database
    print("Inserting %s into db" % info["name"])
    try:
        cursor.execute("INSERT INTO projects (name, version, path, url)"
                        "VALUES (%s, %s, %s, %s)", 
                        (info["name"], info["version"], info["path"], info["url"]))
        project_id = cursor.lastrowid
        print("Project ID is %s" % project_id)
        insert_project_data(dir, project_id)

    except sql.Error as error:
        print("Error: {}".format(error))
        sys.exit()
   


db = sql.connect(host="127.0.0.1",
                port="3306",
                user="scala",
                passwd="scala",
                database="scala")
cursor = db.cursor()

# Assume CWD is the codebases/ folder
root = os.getcwd()

if len(sys.argv) >= 2:
    if sys.argv[1] == "--all":
         for subdir in os.listdir(root):  
            if os.path.isdir(os.path.join(root, subdir)):
                try:
                    insert_project_into_db(subdir)
                except RuntimeError as error:
                    print error
    else:
        for arg in sys.argv[1:]:
            if arg != "-y":
                try:
                    insert_project_into_db(arg)
                except RuntimeError as error:
                        print error
    
    commit = False
    if len(sys.argv) >= 3:
        if sys.argv[1] == "-y":
            commit = True
        if sys.argv[1] == "-n":
            commit = False
    else:
        commit = raw_input("Commit changes to the database? (y/n) ") == "y"
    
    if commit:
        db.commit()
    else:
        print "Oki-doke"
else:
    print "No arguments provided"

db.close()  
