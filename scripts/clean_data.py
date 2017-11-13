import os
import subprocess
import csv
import sys
import re
from Fixer import Fixer

unknow_kind_pattern = r".*Denotation\(([A-Z]*( \| [A-Z]*)*).*"
L_notation_pattern = re.compile(r"L([a-zA-Z-_$]*\/)*[a-zA-Z-_$]*;")

def remove_leading_root(text):
        return text.replace("_root_.", "")

def remove_trailing_dot(text):
    return text[:-1] if text[-1] == '.' else text

def clean_L_notation_instance(text):
    text = text[1:] if text[0] == "L" else text
    text = text.replace("/", ".").replace(";", ",")
    return text

def remove_L_notation(text):
    clean = L_notation_pattern.sub(lambda m: clean_L_notation_instance(m.group(0)), text)
    corner_cases = clean.replace(",)", ")").replace(",.", ".")
    no_trail = corner_cases[:-1] if corner_cases[-1] == "," else corner_cases
    return no_trail

def remove_hashtags(text):
    return (text if text[-1] != "#" else text[:-1]).replace("#", ".")

def replace_unknown_kinds(text):
    match = re.search(unknow_kind_pattern, text)
    if match:
        return match.group(1)
    else:
        return text

def extract_function_name(text):
    index = text.find('(')
    return text[:index] if index != -1 else text

def extract_parameter_list(text):
    index = text.find('(')
    return text[index:] if index != -1 else ""

fixes = {
    "remove_L_notation": remove_L_notation,
    "remove_leading_root": remove_leading_root,
    "remove_trailing_dot": remove_trailing_dot,
    "extract_function_name": extract_function_name,
    "extract_parameter_list": extract_parameter_list,
    "replace_unknown_kinds": replace_unknown_kinds,
    "remove_hashtags": remove_hashtags
}

def fix(transforms, text):
    for transform in transforms:
        text = fixes[transform](text)
    return text

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

def clean_param_row(row):
    row["fqn"] = fix(fully_qualified_name_fixes, row["fqn"])
    row["class"] = fix(fully_qualified_name_fixes, row["class"])
    row["kind"] = fix(["replace_unknown_kinds"], row["kind"])
    return row

def clean_funs_row(row):
    row["symbol"] = fix(["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"], row["symbol"])
    row["fqfn"] = fix(["extract_function_name"], row["symbol"])
    row["fqparamlist"] = fix(["extract_parameter_list"], row["symbol"])
    return row

def clean_links_row(row):
    row["from"] = fix(fully_qualified_name_fixes, row["from"])
    return row

def clean_declared_implicits_row(row):
    try:
        row["fqn"] = fix(["remove_leading_root", "remove_trailing_dot", "remove_L_notation", "remove_hashtags"], row["fqn"])
        row["kind"] = fix(["replace_unknown_kinds"], row["kind"])
        row["class"] = fix(fully_qualified_name_fixes, row["class"])
    except AttributeError as error:
        print(row)
        print(error)
        sys.exit(1)
    return row

def clean_file(directory, filename, clean_function):
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
                new_values = clean_function(row)
                writer.writerow(new_values)

def clean_project(directory):
    info = get_project_info(directory)
    print("Cleaning %s" % info["name"])

    clean_file(directory, "params", clean_param_row)
    clean_file(directory, "funs", clean_funs_row)
    clean_file(directory, "params-funs", clean_links_row)
    clean_file(directory, "declared-implicits", clean_declared_implicits_row)

def test():
    assert(fix(["remove_leading_root", "remove_trailing_dot"], "_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
            == "implicits.ComplexArgument#WriterLike.WriterStringInt")
    assert(fix(["remove_L_notation"], "_root_.org.scalacheck.util.Pretty.prettyAny(Ljava/lang/Object;)Lorg/scalacheck/util/Pretty;.")
            == "_root_.org.scalacheck.util.Pretty.prettyAny(java.lang.Object)org.scalacheck.util.Pretty.")
    assert(fix(["replace_unknown_kinds"], "<unknown: Denotation(IMPLICIT | MACRO, 'methodFor', '[T] => (f: Function0[T]): ScalaMethodCall[T]')>")
            == "IMPLICIT | MACRO")
    assert(fix(["extract_function_name"], "com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
            == "com.twitter.algebird.Monad.operators")
    assert(fix(["extract_parameter_list"], "com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
            == "(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
    assert(fix(["extract_function_name"], "_root_.org.scalacheck.util.Pretty.prettyAny")
            == "_root_.org.scalacheck.util.Pretty.prettyAny")
    assert(fix(["remove_L_notation"], "_root_.akka.actor.Scheduler#schedule(Lscala/concurrent/duration/FiniteDuration;Lscala/concurrent/duration/FiniteDuration;Lakka/actor/ActorRef;Ljava/lang/Object;Lscala/concurrent/ExecutionContext;Lakka/actor/ActorRef;)Lakka/actor/Cancellable;.")
            == "_root_.akka.actor.Scheduler#schedule(scala.concurrent.duration.FiniteDuration,scala.concurrent.duration.FiniteDuration,akka.actor.ActorRef,java.lang.Object,scala.concurrent.ExecutionContext,akka.actor.ActorRef)akka.actor.Cancellable.")
    print("All tests passed!")

def main():
    # Assume CWD is the codebases/ folder
    test()
    root = os.getcwd()

    if len(sys.argv) >= 2:
        for arg in range(1, len(sys.argv)):
            clean_project(sys.argv[arg])
    else:
        print ("No arguments provided")
        sys.exit(1)

main()