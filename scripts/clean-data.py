import os
import subprocess
import csv
import sys
import re
import functools
import doctest

unknow_kind_pattern = r".*Denotation\(([A-Z]*( \| [A-Z]*)*).*"
L_notation_pattern = re.compile(r"L([\w\-$]*\/)*[\w\-$]*;")

def remove_leading_root(text):
    '''
    Removes leading _root_. in semanticdb symbols

    >>> remove_leading_root("_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
    'implicits.ComplexArgument#WriterLike.WriterStringInt.'
    '''
    return text.replace("_root_.", "")

def remove_trailing_dot(text):
    '''
    Removes trailing dot (.) in semanticdb symbols

    >>> remove_trailing_dot("_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
    '_root_.implicits.ComplexArgument#WriterLike.WriterStringInt'
    '''
    return text[:-1] if text[-1] == '.' else text


def remove_L_notation(text):
    def clean_L_notation_instance(text):
        '''
        Clean an L-notation instance - Remove the L, substitute the dashes (/) with dots (.) and the semicolns with commas

        >>> clean_L_notation_instance("Ljava/lang/Object;")
        'java.lang.Object,'
        >>> clean_L_notation_instance("Lorg/scalacheck/util/Pretty;")
        'org.scalacheck.util.Pretty,'
        >>> clean_L_notation_instance("Lorg/1s2c3a4l_a/_util_/Pretty;")
        'org.1s2c3a4l_a.util.Pretty,'
        '''
        text = text[1:] if text[0] == "L" else text
        text = text.replace("/", ".").replace(";", ",")
        return text

    '''
    Find all instances of L-notation and clean them up. Then, adjust for edge cases (e.g. the final comma in a parameter list)

    >>> remove_L_notation("_root_.org.scalacheck.util.Pretty.prettyAny(Ljava/lang/Object;)Lorg/scalacheck/util/Pretty;.")
    '_root_.org.scalacheck.util.Pretty.prettyAny(java.lang.Object)org.scalacheck.util.Pretty.'
    >>> remove_L_notation("_root_.akka.actor.Scheduler#schedule(Lscala/concurrent/duration/FiniteDuration;Lscala/concurrent/duration/FiniteDuration;Lakka/actor/ActorRef;Ljava/lang/Object;Lscala/concurrent/ExecutionContext;Lakka/actor/ActorRef;)Lakka/actor/Cancellable;.")
    '_root_.akka.actor.Scheduler#schedule(scala.concurrent.duration.FiniteDuration,scala.concurrent.duration.FiniteDuration,akka.actor.ActorRef,java.lang.Object,scala.concurrent.ExecutionContext,akka.actor.ActorRef)akka.actor.Cancellable.'
    >>> remove_L_notation("_root_.a12a.actor.Sched12uler#schedule(Lscala/concurr3nt/duration/FiniteDu2_3ation;Lscala/concurrent/duration/FiniteDuration;Lakka/actor/ActorRef;Ljava/lang/Object;Lscala/concurrent/ExecutionContext;Lakka/actor/ActorRef;)Lakka/actor/Cancellable;.")
    '_root_.a12a.actor.Sched12uler#schedule(scala.concurr3nt.duration.FiniteDu2_3ation,scala.concurrent.duration.FiniteDuration,akka.actor.ActorRef,java.lang.Object,scala.concurrent.ExecutionContext,akka.actor.ActorRef)akka.actor.Cancellable.'
    >>> remove_L_notation("scala.collection.generic.GenericTraversableTemplate.unzip(Lscala/Function1;)Lscala/Tuple2;")
    'scala.collection.GenericTraversableTemplate.unzip(scala.Function1)scala.Tuple2'
    '''
    clean = L_notation_pattern.sub(lambda m: clean_L_notation_instance(m.group(0)), text)
    corner_cases = clean.replace(",)", ")").replace(",.", ".")
    no_trail = corner_cases[:-1] if corner_cases[-1] == "," else corner_cases
    return no_trail

def remove_hashtags(text):
    '''
    Remove the hashtags.
    - A trailing hashtag can appear in the end of a fully qualified name,
        for example in extracting the name of a class.
        This must be removed, since it's not part of the fqn of the class.
    - Other hastags in fqns reamrk that whatever is on their right hand side
        is a private member of the left hand side. Since this is not important for now,
        it can be substituted with dots to unify how we represent fqns.

    >>> remove_hashtags("_root_.lila.db.dsl#$id(Ljava/lang/Object;Lreactivemongo/bson/BSONWriter;)Lreactivemongo/bson/BSONDocument;.")
    '_root_.lila.db.dsl.$id(Ljava/lang/Object;Lreactivemongo/bson/BSONWriter;)Lreactivemongo/bson/BSONDocument;.'
    '''
    return (text if text[-1] != "#" else text[:-1]).replace("#", ".")

def replace_unknown_kinds(text):
    '''
    Sometimes there are unknown kinds, of implicits. When that happens, the extraction tool logs the structure, which contains the flags.
    Since the flags are sufficient to classify by kind, a good approximation to kind is to extract the flags.

    >>> replace_unknown_kinds("<unknown: Denotation(IMPLICIT | MACRO, 'methodFor', '[T] => (f: Function0[T]): ScalaMethodCall[T]')>")
    'IMPLICIT | MACRO'
    '''
    match = re.search(unknow_kind_pattern, text)
    return match.group(1) if match else text

def extract_function_name(text):
    '''
    For some fields that can be functions (such as the fqns of implicit parameters),
    we are only interested in the name of the function being passed, not the parameter list.

    This function extracts the name of a function by returning everything before the first '('.
    If no '(' is found, it returns the original string.

    >>> extract_function_name("com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
    'com.twitter.algebird.Monad.operators'
    >>> extract_function_name("_root_.org.scalacheck.util.Pretty.prettyAny")
    '_root_.org.scalacheck.util.Pretty.prettyAny'
    '''
    index = text.find('(')
    return text[:index] if index != -1 else text

def extract_parameter_list(text):
    '''
    The reverse of extract_function_name, it returns everything after the first '(' found,
    or the empty string if it is not found.

    >>> extract_parameter_list("com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
    '(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]'
    '''
    index = text.find('(')
    return text[index:] if index != -1 else ""

def compose(*fs):
    return functools.reduce(lambda f, g: lambda x: f(g(x)), fs, lambda x: x)

clean_fqn = compose(
    remove_leading_root,
    remove_trailing_dot,
    remove_L_notation,
    remove_hashtags
)
def clean_fqn_test():
    '''
    An empty function to trigger doctest.
    clean_fqn agglomerates various cleaning functions, which should be comon to all symbols in semanticdb.

    >>> clean_fqn('_root_.fastparse.Api#P(Lscala/Functi_on0;Lsourcecode/Name;)Lfastparse/core/Parser;.')
    'fastparse.Api.P(scala.Functi_on0,sourcecode.Name)fastparse.core.Parser'
    '''

def clean_param_row(row):
    row["fqn"] = clean_fqn(row["fqn"])
    row["fqfn"] = extract_function_name(row["fqn"])
    row["fqparamlist"] = extract_parameter_list(row["fqn"])
    row["fqtn"] = extract_function_name(clean_fqn(row["fqtn"]))
    row["kind"] = replace_unknown_kinds(row["kind"])
    return row

def clean_funs_row(row):
    row["symbol"] = clean_fqn(row["symbol"])
    row["fqfn"] = extract_function_name(row["symbol"])
    row["fqparamlist"] = extract_parameter_list(row["symbol"])
    return row

def clean_links_row(row):
    row["from"] = clean_fqn(row["from"])
    return row

def clean_declared_implicits_row(row):
    row["fqn"] = clean_fqn(row["fqn"])
    row["kind"] = replace_unknown_kinds(row["kind"])
    row["fqtn"] = extract_function_name(clean_fqn(row["fqtn"]))
    return row

def clean_file(directory, filename, clean_function):
    basepath = os.path.join(directory, filename)
    filepath = basepath + '.csv'
    if not os.path.exists(filepath):
        print("File %s not found. Skipping" % filepath)
        return

    with open(filepath, "r") as original:
        with open(basepath + ".clean.csv", "w") as clean_file:
            reader = csv.DictReader(original)
            writer = csv.DictWriter(clean_file, fieldnames = reader.fieldnames)
            writer.writeheader()
            for row in reader:
                new_values = clean_function(row)
                writer.writerow(new_values)

def clean_project(directory):
    print("Cleaning %s" % directory)

    clean_file(directory, "params", clean_param_row)
    clean_file(directory, "funs", clean_funs_row)
    clean_file(directory, "params-funs", clean_links_row)
    clean_file(directory, "declared-implicits", clean_declared_implicits_row)

def main():
    doctest.testmod()
    if len(sys.argv) >= 2:
        for arg in range(1, len(sys.argv)):
            clean_project(sys.argv[arg])
    else:
        print ("No arguments provided")
        sys.exit(1)

main()