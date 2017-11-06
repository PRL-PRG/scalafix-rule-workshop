import re
import unittest

def remove_leading_root(text):
        return text.replace("_root_.", "")

def remove_trailing_dot(text):
    return text[:-1] if text[-1] == '.' else text

def clean_L_notation_instance(text):
    text = text[1:] if text[0] == "L" else text 
    text = text.replace("/", ".").replace(";", ",")
    return text

def remove_L_notation(text):
    pattern = re.compile(r"L([a-zA-Z-_$]*\/)*[a-zA-Z-_$]*;")
    clean = pattern.sub(lambda m: clean_L_notation_instance(m.group(0)), text)
    corner_cases = clean.replace(",)", ")").replace(",.", ".")
    no_trail = corner_cases[:-1] if corner_cases[-1] == "," else corner_cases
    return no_trail

def remove_hashtags(text):
    return (text if text[-1] != "#" else text[:-1]).replace("#", ".")

def replace_unknown_kinds(text):
    flag_pattern = r".*Denotation\(([A-Z]*( \| [A-Z]*)*).*"
    match = re.search(flag_pattern, text)
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

class Fixer:
    def __init__(self, project_info):
        self.project = project_info
        self.fixes = {
            "remove_L_notation": remove_L_notation,
            "remove_leading_root": remove_leading_root,
            "remove_trailing_dot": remove_trailing_dot,
            "extract_function_name": extract_function_name,
            "extract_parameter_list": extract_parameter_list,
            "replace_unknown_kinds": replace_unknown_kinds,
            "remove_hashtags": remove_hashtags
        }

    def fix(self, transforms, text):
        for transform in transforms:
            text = self.fixes[transform](text)
        return text

def main():
    def test():
            fixer = Fixer({})
            assert(fixer.fix(["remove_leading_root", "remove_trailing_dot"], "_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
                    == "implicits.ComplexArgument#WriterLike.WriterStringInt")
            assert(fixer.fix(["remove_L_notation"], "_root_.org.scalacheck.util.Pretty.prettyAny(Ljava/lang/Object;)Lorg/scalacheck/util/Pretty;.")
                    == "_root_.org.scalacheck.util.Pretty.prettyAny(java.lang.Object)org.scalacheck.util.Pretty.")
            assert(fixer.fix(["replace_unknown_kinds"], "<unknown: Denotation(IMPLICIT | MACRO, 'methodFor', '[T] => (f: Function0[T]): ScalaMethodCall[T]')>")
                    == "IMPLICIT | MACRO")
            assert(fixer.fix(["extract_function_name"], "com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
                    == "com.twitter.algebird.Monad.operators")
            assert(fixer.fix(["extract_parameter_list"], "com.twitter.algebird.Monad.operators(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
                    == "(java.lang.Objectcom.twitter.algebird.Monad)com.twitter.algebird.MonadOperators.[M]")
            assert(fixer.fix(["extract_function_name"], "_root_.org.scalacheck.util.Pretty.prettyAny")
                    == "_root_.org.scalacheck.util.Pretty.prettyAny")
            assert(fixer.fix(["remove_L_notation"], "_root_.akka.actor.Scheduler#schedule(Lscala/concurrent/duration/FiniteDuration;Lscala/concurrent/duration/FiniteDuration;Lakka/actor/ActorRef;Ljava/lang/Object;Lscala/concurrent/ExecutionContext;Lakka/actor/ActorRef;)Lakka/actor/Cancellable;.")
                    == "_root_.akka.actor.Scheduler#schedule(scala.concurrent.duration.FiniteDuration,scala.concurrent.duration.FiniteDuration,akka.actor.ActorRef,java.lang.Object,scala.concurrent.ExecutionContext,akka.actor.ActorRef)akka.actor.Cancellable.")
            print("All tests passed!")
    test()

main()