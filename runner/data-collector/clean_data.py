import re

def Fix(transforms, text):
    def remove_leading_root(text):
        return text.replace("_root_.", "")

    def remove_trailing_dot(text):
        return text[:-1] if text[-1] == '.' else text

    def remove_L_notation(text):
        pattern = re.compile(r"L([a-zA-Z-_$]*\/)*[a-zA-Z-_$]*;")
        return pattern.sub(lambda m: clean_L_notation_instance(m.group(0)), text)

    def remove_hashtags(text):
        return (text if text[-1] != "#" else text[:-1]).replace("#", ".")

    def clean_L_notation_instance(text):
        text = text[1:] if text[0] == "L" else text 
        text = text.replace("/", ".")
        text = text.replace(";", "")
        return text

    def replace_unknown_kinds(text):
        flag_pattern = r".*Denotation\(([A-Z]*( \| [A-Z]*)*).*"
        match = re.search(flag_pattern, text)
        if match:
            return match.group(1)
        else:
            return text

    fixes = {
        "remove_L_notation": remove_L_notation,
        "remove_leading_root": remove_leading_root,
        "remove_trailing_dot": remove_trailing_dot,
        "replace_unknown_kinds": replace_unknown_kinds
    }

    if len(transforms) == 1 and transforms[0] == "all":
        transforms = fixes.keys()

    for transform in transforms:
        print("Running %s in \"%s\"" % (transform, text))
        text = fixes[transform](text)
    return text
   
def main():
    assert(Fix(["remove_leading_root", "remove_trailing_dot"], "_root_.implicits.ComplexArgument#WriterLike.WriterStringInt.")
            == "implicits.ComplexArgument#WriterLike.WriterStringInt")
    assert(Fix(["remove_L_notation"], "_root_.org.scalacheck.util.Pretty.prettyAny(Ljava/lang/Object;)Lorg/scalacheck/util/Pretty;.")
            == "_root_.org.scalacheck.util.Pretty.prettyAny(java.lang.Object)org.scalacheck.util.Pretty.")
    assert(Fix(["all"], "_root_.scala.Predef.$conforms()Lscala/Predef/$less$colon$less;.")
            == "scala.Predef.$conforms()scala.Predef.$less$colon$less")
    assert(Fix(["replace_unknown_kinds"], "<unknown: Denotation(IMPLICIT | MACRO, 'methodFor', '[T] => (f: Function0[T]): ScalaMethodCall[T]')>")
            == "IMPLICIT | MACRO")

    print("Done!")


main()