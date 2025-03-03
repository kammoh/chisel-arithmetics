#!/usr/bin/env python3
from pathlib import Path
import re
import argparse
from textwrap import indent

parser = argparse.ArgumentParser(description="Inline bind file")
parser.add_argument("file", help="SV file to convert", type=Path)
parser.add_argument("-b", "--bindfile", type=Path)
parser.add_argument("-o", "--output", help="Output file")
parser.add_argument("-i", "--inplace", help="Overwrite the input file", action="store_true")
args = parser.parse_args()

bindfile = args.bindfile or (args.file.parent / "bindfile.sv")


with open(bindfile, "r", encoding="utf8") as f:
    bind_content = f.read()
    instances: dict[str, list[str]] = dict()
    for m in re.findall(r"bind\s+(\w+)\s+([^;]+;)", bind_content, re.MULTILINE | re.DOTALL):
        assert m
        module_name = m[0]
        print(f"found bind for {module_name}")
        submodule_inst = m[1]
        sub_insts = instances.get(module_name, [])
        sub_insts.append(submodule_inst)
        instances[module_name] = sub_insts
with open(args.file, "r", encoding="utf8") as f:
    content = f.read()
    for module_name, submodule_instances in instances.items():
        inject = "\n\n".join(indent(si, "  ") for si in submodule_instances)
        inject = "  initial assume(reset);\n\n" + inject
        inject = (
            "`ifndef SYNTHESIS\n"
            # + "`ifdef FORMAL\n"
            + inject
            # + "\n`endif // FORMAL"
            + "\n`endif // not SYNTHESIS"
        )
        (content, n) = re.subn(
            r"(.*module\s+" + module_name + r"\s*\(.*)(endmodule)",
            rf"\1\n{inject}\n\2",
            content,
            flags=re.MULTILINE | re.DOTALL,
        )
        assert n == 1


if args.output:
    assert not args.inplace, "Cannot use -o and -i together"
    with open(args.output, "w", encoding="utf8") as f:
        f.write(content)
elif args.inplace:
    with open(args.file, "w", encoding="utf8") as f:
        f.write(content)
else:
    print(content)
