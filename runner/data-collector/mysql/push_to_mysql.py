import os
import sys
import csv
import subprocess
import mysql.connector as sql

def get_project_info(cwd):
    # Assuming cwd is the project
    path = os.path.abspath(cwd)
    name = os.path.relpath(cwd, cwd + "/..")
    os.chdir(name)
    version = subprocess.check_output(["git", "describe", "--always"]).strip()
    url = subprocess.check_output(["git", "config", "--get", "remote.origin.url"]).strip()
    os.chdir("..")
    return {"path": path, "name": name, "version": version, "url": url}

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

def insert_project_data(dir, project_id):
    params_path = dir+"/params.csv"
    funs_path = dir+"/funs.csv"
    params_funs_path = dir+"/params-funs.csv"
    if os.path.exists(params_path) and os.path.exists(funs_path) and os.path.exists(params_funs_path):

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
    info = get_project_info(dir)
    proceed = raw_input("Wish to insert %s in the database? (y/n) " % info)
    if proceed == "y":
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
    else:
        print("Oki-doke")


db = sql.connect(host="127.0.0.1",
                port="6612",
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
                insert_project_into_db(subdir)
    else:
	for arg in range(1, len(sys.argv)):
	     insert_project_into_db(sys.argv[arg])
    
    proceed = raw_input("Commit changes to the database? (y/n) ")
    if proceed == "y":
        db.commit()
    else:
        print "Oki-doke"
else:
    print "No arguments provided"

db.close()  
