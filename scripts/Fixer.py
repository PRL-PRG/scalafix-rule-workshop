import re
import unittest

def remove_leading_root(text):
        return text.replace("_root_.", "")

def remove_trailing_dot(text):
    return text[:-1] if text[-1] == '.' else text

def clean_L_notation_instance(text):
    text = text[1:] if text[0] == "L" else text 
    text = text.replace("/", ".")
    text = text.replace(";", "")
    return text

def remove_L_notation(text):
    pattern = re.compile(r"L([a-zA-Z-_$]*\/)*[a-zA-Z-_$]*;")
    return pattern.sub(lambda m: clean_L_notation_instance(m.group(0)), text)

def remove_hashtags(text):
    return (text if text[-1] != "#" else text[:-1]).replace("#", ".")

def replace_unknown_kinds(text):
    flag_pattern = r".*Denotation\(([A-Z]*( \| [A-Z]*)*).*"
    match = re.search(flag_pattern, text)
    if match:
        return match.group(1)
    else:
        return text

class Fixer:
    def __init__(self, project_info):
        self.project = project_info
        self.fixes = {
            "remove_L_notation": remove_L_notation,
            "remove_leading_root": remove_leading_root,
            "remove_trailing_dot": remove_trailing_dot,
            "replace_unknown_kinds": replace_unknown_kinds,
            "remove_hashtags": remove_hashtags
        }
    
    def fix(self, transforms, text):
        if len(transforms) == 1 and transforms[0] == "all":
            transforms = self.fixes.keys()

        for transform in transforms:
            text = self.fixes[transform](text)
        return text

class FixerTest(unittest.TestCase):
    def test(self):
        fixer = Fixer({})
        assert(fixer.fix(["remove_leading_root", "remove_trailing_dot"], "_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
                == "implicits.ComplexArgument#WriterLike.WriterStringInt")
        assert(fixer.fix(["remove_L_notation"], "_root_.org.scalacheck.util.Pretty.prettyAny(Ljava/lang/Object;)Lorg/scalacheck/util/Pretty;.")
                == "_root_.org.scalacheck.util.Pretty.prettyAny(java.lang.Object)org.scalacheck.util.Pretty.")
        assert(fixer.fix(["all"], "_root_.scala.Predef.$conforms()Lscala/Predef/$less$colon$less;.")
                == "scala.Predef.$conforms()scala.Predef.$less$colon$less")
        assert(fixer.fix(["replace_unknown_kinds"], "<unknown: Denotation(IMPLICIT | MACRO, 'methodFor', '[T] => (f: Function0[T]): ScalaMethodCall[T]')>")
                == "IMPLICIT | MACRO")
        