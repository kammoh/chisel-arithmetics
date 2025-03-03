#!/usr/bin/env python3
import argparse
from dataclasses import dataclass
import json
import os
from pathlib import Path
import random
import re
import shutil
import subprocess
import time
from typing import Literal, Optional, OrderedDict, Sequence, Union

import numpy as np
import matplotlib.pyplot as plt
import rich
import rich.text
import seaborn as sns
from quantiphy import Quantity
from rich.console import Console
from rich.table import Table
from rich.live import Live

console = Console()

# Synthesize RTL sources using yosys and then run PROLEAD

argparser = argparse.ArgumentParser(description="Run PROLEAD")

argparser.add_argument("source_files", nargs="*", default=[], type=Path, help="Source files")
argparser.add_argument(
    "--sources-list", default=None, type=Path, help="File containing list of source files"
)
argparser.add_argument("--netlist", type=Path, help="Netlist file")
argparser.add_argument("-t", "--top-module", default=None, help="Top module")
argparser.add_argument("--force-synth", help="Force synthesis.", action="store_true")
argparser.add_argument(
    "--quiet-synth",
    action=argparse.BooleanOptionalAction,
    type=bool,
    default=True,
    help="Supres synthesis output",
)
argparser.add_argument(
    "--prolead-root-dir", help="Path to PROLEAD source directory", type=Path, default=None
)
argparser.add_argument("--yosys-bin", help="Path to yosys binary", default="yosys")
argparser.add_argument(
    "--yosys-verilog-lib", help="Path to Verilog cell library", default=None, type=Path
)
argparser.add_argument("--yosys-lib", help="Path to .lib cell library", default=None, type=Path)
argparser.add_argument("--prolead-bin", help="Path to PROLEAD binary", default=None)
argparser.add_argument("--library-json", help="Path to library JSON file", type=Path, default=None)
argparser.add_argument("--library-name", help="Library name", type=str, default="custom")
argparser.add_argument("--random-seed", default=None, type=int, help="Random seed")
argparser.add_argument("-d", "--order", default=1, type=int, help="SCA order")
argparser.add_argument(
    "-N", "--num-simulations", default=Quantity("10 M"), type=Quantity, help="Number of simulations"
)
argparser.add_argument(
    "-c", "--sim-cycles", type=int, default=None, help="Number of simulation cycles"
)
argparser.add_argument(
    "--transitional",
    action=argparse.BooleanOptionalAction,
    type=bool,
    default=True,
    help="Enable transitional leakage",
)
argparser.add_argument(
    "--compact",
    action=argparse.BooleanOptionalAction,
    type=bool,
    default=False,
    help="""Use compact distributions. Only the Hamming weight of the observations is stored, 
            resulting in a more concise table. Reduces memory usage by storing less detailed data,
            which means that leakages can be overseen compared to normal mode.""",
)
argparser.add_argument(
    "--ports-json",
    type=Path,
    default=None,
    help="Path to json file containing port information",
)
argparser.add_argument(
    "--opt",
    help="Run optimizations during synthesis",
    default="none",
    choices=["full", "flatten", "none"],
)
argparser.add_argument(
    "--show-figure",
    action="store_true",
    help="Show figure",
)
argparser.add_argument(
    "--minimize-probing-sets",
    choices=["trivial", "aggressive", "no"],
    default="trivial",
    help="Minimize probing sets",
)
argparser.add_argument(
    "--simulations-per-step",
    type=Quantity,
    default=None,
    help="Number of simulations per step",
)
argparser.add_argument(
    "--pretty",
    action=argparse.BooleanOptionalAction,
    type=bool,
    default=True,
    help="Pretty print parsed output",
)
argparser.add_argument(
    "--probing-sets-per-step",
    type=Quantity,
    default=None,
    help="Specifies the number of probing sets PROLEAD should evaluate. In each step, PROLEAD assesses the specified number of probing sets across all simulations. Once evaluated, the probing sets will be deleted.",
)
argparser.add_argument(
    "--num-cores",
    type=str,
    default=None,
    help="Maximum number of CPU cores to use",
)
argparser.add_argument(
    "--prolead-config",
    type=Path,
    default=None,
    help="Path to PROLEAD config file. All other PROLEAD options will be ignored.",
)


def synthesize(
    yosys_bin: Union[Path, str],
    yosys_run_dir: Path,
    source_files: list[Path],
    top_module: Optional[str],
    verilog_lib: Path,
    liberty_lib: Path,
    verilog_netlist: Path,
    defines: Optional[dict[str, Optional[str]]] = None,
    parameters: Optional[dict[str, str]] = None,
    opt_flatten: bool = True,
    opt_full: bool = True,
    split_nets: bool = False,
    quiet: bool = True,
):
    yosys_run_dir.mkdir(parents=True, exist_ok=True)

    liberty_lib = liberty_lib.resolve()
    verilog_netlist = verilog_netlist.absolute()
    netlist_dir = verilog_netlist.parent

    # copy verilog_lib next to the netlist
    if verilog_lib:
        verilog_lib = verilog_lib.resolve()
        shutil.copyfile(verilog_lib, netlist_dir / verilog_lib.name, follow_symlinks=True)

    json_netlist = verilog_netlist.with_suffix(".json")

    yosys_script = []

    vhdl_files = []
    ghdl_args = ["--std=08"]

    if defines is None:
        defines = {}

    read_verilog_args = [
        "-noautowire",
        "-defer",
        "-noassert",
        "-noassume",
        "-nolatches",
    ]

    define_args = [f"-D{k}" if v is None else f"-D{k}={v}" for k, v in defines.items()]
    read_verilog_args += define_args
    slang_args = ["--extern-modules", "--best-effort-hierarchy"]
    slang_args += define_args

    sv_slang = True
    has_sv_files = any(f.suffix == ".sv" for f in source_files)

    for src in source_files:
        src = Path(src)
        if src.suffix == ".sv":
            if sv_slang:
                yosys_script.append(f"read_slang {' '.join(slang_args + [str(src)])}")
            else:
                yosys_script.append(
                    f"read_verilog {' '.join(read_verilog_args + ['-sv', str(src)])}"
                )
        elif src.suffix == ".v":
            yosys_script.append(f"read_verilog {' '.join(read_verilog_args + [str(src)])}")
        elif src.suffix in (".vhd", ".vhdl"):
            vhdl_files.append(src)
        else:
            raise ValueError(f"Unsupported file type: {src}")
    if vhdl_files:
        yosys_script += [
            f"ghdl {' '.join(ghdl_args)} {' '.join(map(str, vhdl_files))} -e",
        ]

    hierarchy_args = ["-check"]
    if top_module:
        hierarchy_args += ["-top", top_module]
    else:
        hierarchy_args.append("-auto-top")

    if parameters is not None:
        for k, v in parameters.items():
            hierarchy_args += ["-chparam", k, str(v)]

    yosys_script += [
        "hierarchy " + " ".join(hierarchy_args),
        "opt_clean -purge",
        "proc",
        "opt_clean -purge",
        "check -assert",
    ]

    yosys_script += [
        f"write_verilog -noattr {netlist_dir / 'yosys_rtl.v'}",
        f"write_json {netlist_dir / 'yosys_rtl.json'}",
    ]
    synth_args = [
        # "-noabc",
        # "-noshare",
        # "-nordff",
    ]

    if opt_flatten:
        synth_args.append("-flatten")
    yosys_script += [
        # "setattr -set keep_hierarchy 1",
        # f"read_verilog -lib {verilog_lib}",
        f"read_liberty -lib {liberty_lib}",
        f"synth {' '.join(synth_args)}",
        "opt_clean -purge",
    ]
    if opt_full:
        yosys_script.append("opt -full -purge")
    else:
        yosys_script.append("opt_clean -purge")

    abc_flags = ["-liberty", liberty_lib]

    # if opt_full:
    #     abc_flags += ["-dff"]
    # else:
    #     abc_flags += ["-keepff", "-fast"]
    # abc_flags += ["-fast"]
    # abc_flags += ["-script", "+strash;&ifraig,-x;scorr;dc2;strash;&get,-n;&dch,-f;&nf,{D};&put"]
    abc_flags += [
        "-script",
        "+strash;map,{D}",
    ]

    yosys_script += [
        "write_verilog pre_abc_dump.v",
        # "async2sync -nolower",
        f"dfflibmap -prepare -liberty {liberty_lib}",
        "opt_clean -purge",
        f"abc " + " ".join(str(e) for e in abc_flags),
        "opt_clean -purge",
        f"dfflibmap -liberty {liberty_lib}",
        "opt_clean -purge",
    ]

    if opt_full:
        yosys_script += [
            "opt -full -purge",
            "opt -full -fine -purge",
            "opt -full -fine -sat -purge",
            "opt -full -purge",
        ]

    yosys_script += [
        "setundef -zero",
        "opt -full -purge" if opt_full else "opt_clean -purge",
        "setattr -set keep_hierarchy 0",
        "opt_clean -purge",
        "flatten",
        "opt_clean -purge",
        # "check -assert -noinit -mapped",
    ]

    if opt_full:
        yosys_script += [
            "opt -full -purge",
            "opt -full -fine -purge",
            "opt -full -fine -sat -purge",
            "opt -full -purge",
        ]
    else:
        yosys_script.append("opt_clean -purge")

    if split_nets:
        yosys_script += ["splitnets -driver -format ___"]
        # yosys_script += ["splitnets -driver"]

    # if top_module:
    #     yosys_script.append(f"select {top_module}")

    if opt_full:
        yosys_script.append("opt -full -purge")
    else:
        yosys_script.append("opt_clean -purge")

    write_verilog_args = [
        "-noexpr",
        "-noattr",
        "-nodec",
        "-nostr",
        "-simple-lhs",
    ]
    write_verilog_args.append(str(verilog_netlist))

    yosys_script += [
        f"write_json {json_netlist}",
        # "check -assert -noinit -initdrv",
        f"stat -liberty {liberty_lib}",
        "check -mapped -noinit -initdrv",
        "check -assert -noinit -initdrv",
        f"write_verilog {' '.join(write_verilog_args)}",
    ]
    yosys_cmd = [yosys_bin, "-Q", "-T"]
    if quiet:
        yosys_cmd.append("-q")
        yosys_cmd += ["-l", "yosys.log"]
    # else:
    #     yosys_cmd.append("-g")
    if vhdl_files:
        yosys_cmd += ["-m", "ghdl"]
    if sv_slang and has_sv_files:
        yosys_cmd += ["-m", "slang"]

    # write yosys_script to file
    yosys_script_file = yosys_run_dir / "yosys_script.ys"
    with open(yosys_script_file, "w") as f:
        f.write("\n".join(yosys_script))
    # yosys_cmd += ["-p", "; ".join(yosys_script)]
    yosys_cmd += ["-s", yosys_script_file.relative_to(yosys_run_dir)]

    print("\n" + "=" * 20 + " YOSYS SYNTHESIS " + "=" * 20)
    yosys_cmd = [str(c) for c in yosys_cmd]
    print(f"** Running {' '.join(yosys_cmd)}\n")
    subprocess.run(
        yosys_cmd,
        cwd=yosys_run_dir,
        check=True,
    )
    assert verilog_netlist.exists(), f"Failed to generate netlist {verilog_netlist}"
    assert json_netlist.exists(), f"Failed to generate json netlist {json_netlist}"
    print(f"** Generated netlist: {verilog_netlist}\n")
    print("" + "=" * 56 + "\n")


NAME_FROM_PORT_SINGLE_REGEX = re.compile(r"^(?P<name>.*)\[(?P<start>\d+)\]$")
NAME_FROM_PORT_RANGE_REGEX = re.compile(r"^(?P<name>.*)\[(?P<end>\d+):(?P<start>\d+)\]$")


@dataclass
class Port:
    name: str
    width: Optional[int] = None  # none for scalar 1-bit ports?
    type: Optional[str] = None
    direction: Optional[str] = "input"
    value: Union[int, str, None] = None
    share_id: Optional[int] = None  # none for non-shared ports
    start_bit: Optional[int] = None

    @property
    def num_bits(self):
        if self.width is None:
            _, end, start = Port.range_from_name(self.name)
            return end - start + 1 if start is not None else 1
        return max(self.width, 1)

    @property
    def name_bits(self):
        if self.start_bit is None:
            name, end, start = Port.range_from_name(self.name)
            self.name = name
            self.start_bit = start
            if not self.width:
                self.width = end - start + 1 if start is not None else 1
        if self.start_bit is None and self.num_bits > 1:
            self.start_bit = 0
        if self.start_bit is not None:
            if self.width is None:
                self.width = 1
            return f"{self.name}[{self.start_bit + self.width - 1}:{self.start_bit}]"
        else:
            return self.name

    @property
    def is_input(self):
        return self.direction is None or self.direction == "input"

    @property
    def is_output(self):
        return self.direction == "output"

    @property
    def value_str(self):
        if self.value == "fixed":
            self.value = random.randint(0, self.num_bits - 1)
        elif isinstance(self.value, str) and self.value.isnumeric():
            self.value = int(self.value)
        if isinstance(self.value, int):
            # return f"{self.num_bits}'b{self.value:0{self.num_bits}b}"
            return verilog_value(self.value, self.width)
        return self.value

    @classmethod
    def range_from_name(cls, name: str) -> tuple[str, int, Optional[int]]:
        m = NAME_FROM_PORT_SINGLE_REGEX.match(name)
        if m:
            name = m.group("name")
            start = int(m.group("start"))
            return (name, start, start)
        m = NAME_FROM_PORT_RANGE_REGEX.match(name)
        if m:
            start = int(m.group("start"))
            end = int(m.group("end"))
            name = m.group("name")
            return (name, end, start)
        return (name, 0, None)


def get_top_module_and_ports(netlist: dict) -> tuple[Optional[str], list]:
    modules = netlist["modules"]
    assert modules and isinstance(modules, dict), "Failed to parse json netlist"
    top_name = None
    top_module = None
    ports = []
    output_ports = []
    for name, module in modules.items():
        if int(module.get("attributes", {}).get("top", "0")) == 1:
            top_name = name
            top_module = module
            break
    if top_module:
        ports_dict: dict = top_module.get("ports", {})
        for name, port in ports_dict.items():
            direction = port.get("direction")
            assert direction in ["input", "output"], f"Invalid direction: {direction}"
            width = len(port["bits"])
            p = {"name": name, "width": width, "direction": direction}
            ports.append(p)

    return top_name, ports


def parse_json_netlist(json_netlist_file: Path) -> tuple[Optional[str], list]:
    with open(json_netlist_file, "r") as f:
        netlist = json.load(f)
    return get_top_module_and_ports(netlist)


def format_time(seconds: float) -> str:
    seconds = int(seconds)
    return "{:2}:{:02}:{:02}".format(seconds // 3600, seconds % 3600 // 60, seconds % 60)


def run_prolead(
    prolead_bin: Union[str, Path],
    prolead_run_dir: Path,
    netlist_file: Path,
    top_module: str,
    library_name: str,
    library_json: Path,
    sca_config: dict,
    config_file: Path,
    show_figure: bool = False,
    pretty: bool = True,
    result_folder: Union[str, Path] = "results",
):

    assert netlist_file.exists(), f"Netlist file {netlist_file} does not exist"

    print(f"** Running PROLEAD in {prolead_run_dir.absolute()}")

    # config_file = config_file.relative_to(prolead_run_dir)
    config_file = config_file.absolute()

    library_json = library_json.absolute()

    if not netlist_file.is_absolute():
        netlist_file = netlist_file.resolve()  # .relative_to(prolead_run_dir)

    if isinstance(result_folder, Path):
        result_folder.mkdir(parents=True, exist_ok=True)
        if not result_folder.is_absolute():
            result_folder = result_folder.relative_to(prolead_run_dir)

    prolead_cmd = [
        prolead_bin.resolve() if isinstance(prolead_bin, Path) else prolead_bin,
        "--libraryfile",
        library_json,
        "--libraryname",
        library_name,
        "--designfile",
        netlist_file,
        "--configfile",
        config_file,
        "--resultfolder",
        result_folder,
    ]
    if top_module:
        prolead_cmd += ["--modulename", top_module]

    print(f"** Running {' '.join(map(str, prolead_cmd))}")
    proc = subprocess.Popen(
        prolead_cmd,
        bufsize=0,
        cwd=prolead_run_dir,
        stdout=subprocess.PIPE,
        text=True,
    )

    assert proc.stdout is not None, "stdout is None"

    result_line_regex = re.compile(
        r"^\s*\|\s*(?P<elapsed_time>\d+\.\d+)[s]\s*\|\s*(?P<ram_usage>\d+\.\d+)\s*(?P<ram_usage_unit>[A-Z]+)\s*\|\s*(?P<n_sim>\d+)(\s*\/\s*(?P<required_sims>\d+))?\s*\|\s*(\[(?P<signals>([^\(]+\(\d+\)(,\s)?)+)\])?\s*\|\s*(?P<p_log>(\d+\.\d+|inf))\s*\|\s*(?P<status>[A-Z]+)\s*\|\s*$",
    )

    first_result_line_regex = re.compile(
        r"\|\s*Elapsed Time\s*\|\s*.*\s*\|\s*[a-zA-Z\s]+\s*\|\s*[a-zA-Z\s]+\s*\|\s*-log10\(p\)\s*\|\s*Status\s*\|",
        re.IGNORECASE,
    )
    first_line_done = False

    data = []
    data_np = None

    write_every = 300

    prev_checkpoint = 0

    npy_file = prolead_run_dir / f"{top_module}_data.npz"

    def save_data():
        if data_np is not None:
            np.savez_compressed(npy_file, data_np)

    table = Table() if pretty else None

    leaking_signals = set()

    stop_when_required_sims_reached = False

    terminated = False

    def print_header(table: Table):
        table.add_column(rich.text.Text("Time", justify="center"), width=8, justify="right")
        table.add_column(rich.text.Text("Memory (GB)", justify="center"), width=6, justify="right")
        table.add_column(
            rich.text.Text("#Simulations", justify="center"),
            justify="right",
            width=20,
            max_width=26,
        )
        table.add_column(rich.text.Text("Highest Leakage", justify="center"), justify="left")
        table.add_column(rich.text.Text("-Log(p)", justify="center"), justify="right")
        table.add_column("Status", justify="center", width=8)

    def print_data_line(
        table: Table,
        leakage: bool,
        elapsed_time: float,
        ram_usage: float,
        ram_usage_unit: str,
        n_sim: int,
        required_sims: Optional[int],
        p_log: float,
        status: str,
        sigs: list[str],
    ):

        if leakage:
            stat_color = "red" if p_log > 7 else "yellow"
        elif p_log <= 0.0:
            stat_color = "yellow"
        else:
            stat_color = "green"
        table.add_row(
            format_time(elapsed_time),
            f"{ram_usage:.2f}{ram_usage_unit if ram_usage_unit != 'GB' else ''}",
            f"{n_sim:9,d} / {required_sims:6,d}" if required_sims else f"{n_sim:9,d}",
            f"{', '.join(sigs)}",
            f"{p_log:.2f}",
            f"[{stat_color}]{status}[/{stat_color}]",
        )

    # catch KeyboardInterrupt
    try:
        with Live(table, console=console, vertical_overflow="visible", auto_refresh=False) as live:
            for line in map(str.strip, proc.stdout):
                if not first_line_done and first_result_line_regex.fullmatch(line):
                    first_line_done = True
                    if table is not None:
                        print_header(table)
                        live.refresh()
                    else:
                        print(line)
                    continue
                m = result_line_regex.fullmatch(line)
                if m:
                    elapsed_time = float(m.group("elapsed_time"))
                    ram_usage = float(m.group("ram_usage"))
                    ram_usage_unit = m.group("ram_usage_unit")
                    n_sim = int(m.group("n_sim"))
                    required_sims = int(m.group("required_sims") or 0)
                    p_log = float(m.group("p_log"))
                    status = m.group("status")
                    leakage = status != "OKAY"
                    signals = m.group("signals")
                    sigs = [] if signals is None else signals.split(", ")
                    if leakage and sigs:
                        leaking_signals.update((s, n_sim, p_log) for s in sigs)
                    if table is not None:
                        print_data_line(
                            table,
                            leakage,
                            elapsed_time,
                            ram_usage,
                            ram_usage_unit,
                            n_sim,
                            required_sims,
                            p_log,
                            status,
                            sigs,
                        )
                        live.refresh()
                    else:
                        print(line)
                    data.append((n_sim, p_log))
                    if stop_when_required_sims_reached and required_sims and n_sim > required_sims:
                        print(f"** Required simulations reached: {n_sim}/{required_sims}")
                        terminated = True
                        proc.terminate()
                        break
                    if elapsed_time - prev_checkpoint >= write_every:
                        prev_checkpoint = elapsed_time
                        print(f"Writing checkpoint to {npy_file}")
                        data_np = np.array(data)
                        save_data()
                    # print(f"{n_sim}/{total_sim} {signals} {p_log} {status}")
                else:
                    print(line)
    except KeyboardInterrupt:
        print("*** Caught KeyboardInterrupt, terminating PROLEAD... ***")
        proc.terminate()
    finally:
        if proc.poll() is None:
            proc.wait()
        if data:
            data_np = np.array(data)
            print(f"** Writing data to {npy_file}")
            save_data()
        else:
            print("No data captured")
        if leaking_signals:
            print(f"** Leakage Detected!!!")
            cycles_signals = []
            for s, n_sim, p_log in leaking_signals:
                m = re.match(r"([^\(]+)\((\d+)\)", s)
                if m:
                    cycles_signals.append((int(m.group(2)), m.group(1), p_log))
                else:
                    print(f"** Unmatched signal / cycle format: {s}")
            cycles_signals = list(set(cycles_signals))
            cycles_signals.sort(key=lambda x: x[0])

            with open(prolead_run_dir / f"{top_module}_leaking_signals.csv", "w") as f:
                f.write("Cycle,Signal,Log(p)\n")
                for c, s, p_log in cycles_signals:
                    f.write(f"{c},{s},{p_log}\n")
            pr = "\n".join(f" {c:4d}: {s} [{p_log:3.2f}]" for c, s, p_log in cycles_signals)
            print(f"** Leaking signals:\n{pr}")

    ## https://github.com/ChairImpSec/PROLEAD/wiki/Results

    if data_np is not None:
        sns.set_theme(style="whitegrid", context="paper")
        plt.figure()

        x = data_np[:, 0]
        y = data_np[:, 1]  # The smallest p-value from the g-test in logarithmic form

        num_sims = np.max(x)
        max_p_log = np.max(y)

        if num_sims >= 1e10:
            x_scale = 1e9
        elif num_sims >= 1e7:
            x_scale = 1e6
        elif num_sims >= 1e4:
            x_scale = 1e3
        else:
            x_scale = 1

        if x_scale > 1:
            x = x / x_scale

        plot = sns.lineplot(
            x=x,
            y=y,
            # kind="line",
            label=r"$-\log_{10}(p)$"
            + " [glitch"
            + ("+transition" if sca_config.get("transitional_leakage") else "")
            + "]",
        )
        plot.axhline(y=max_p_log, linestyle="--", label="Minimum p-value", alpha=0.6)
        plot.axhline(y=5, color="r", linestyle="--", label="Threshold")
        plot.set_xlabel(
            "Number of Simulations" + (rf" ($\times${int(x_scale):,})" if x_scale > 1 else "")
        )
        plot.set_ylabel(r"$-\log_{10}(p)$")
        plot.legend(loc="best", fancybox=True, framealpha=0.9)
        plt.tight_layout()

        fig_file = npy_file.with_suffix(".png")

        print(f"Saving plot to {fig_file}")
        plt.savefig(fig_file, dpi=600)
        if show_figure:
            plt.show()

    if not terminated and proc.returncode:
        print(f"PROLEAD failed with return code {proc.returncode}")
        exit(1)


def div_ceil(a: int, b: int) -> int:
    return (a + b - 1) // b


def verilog_value(value: Union[int, str], width: Optional[int]) -> str:
    if width is None:
        assert isinstance(value, int), "Expected int value for width=None"
        width = value.bit_length()
    if width <= 64:
        base = "b"
        vbase = "b"
        n_digits = width
    else:
        base = "X"
        vbase = "h"
        n_digits = div_ceil(width, 4)
    prefix = f"{width}'{vbase}"

    if isinstance(value, int):
        v = f"{value:0{n_digits}{base}}"
    elif value == "$":
        v = "$" * n_digits
    else:
        v = str(value)
    return prefix + v


def generate_config(
    config_file: Path,
    ports: list[Port],
    sca_config: dict,
    sim_config: dict,
    perf_config: dict,
):

    # print(f" ports: {ports}")
    input_ports: list[Port] = [p for p in ports if p.is_input]
    # print(f" input_ports: {input_ports}")
    output_ports: list[Port] = [p for p in ports if p.is_output]

    # print(f" output_ports: {output_ports}")

    shared_inputs = [p for p in input_ports if p.share_id is not None]
    print(f"  shared_inputs: {', '.join(f'{p.name_bits}::{p.share_id}' for p in shared_inputs)}")
    # shared_outputs = [Output(**p) for p in output_ports if p.get("share_id") is not None]
    rand_inputs = [p for p in input_ports if p.type == "random"]
    print(f"  rand_inputs: {', '.join(p.name_bits for p in rand_inputs)}")
    clocks = [p for p in input_ports if p.type == "clock"]
    print(f"  clocks: {', '.join(p.name_bits for p in clocks)}")
    resets = [p for p in input_ports if p.type == "reset"]
    print(f"  resets: {', '.join(p.name_bits for p in resets)}")
    reset_signal = resets[0] if resets else None

    end_signals = [
        p
        for p in output_ports
        if p.share_id is None and p.type == "end" and (p.value is not None or p.num_bits == 1)
    ]
    for p in end_signals:
        if p.value is None:
            p.value = 1
    print(f"  end_signals: {', '.join(f'{p.name_bits} -> {p.value}' for p in end_signals)}")

    if reset_signal is not None:
        if reset_signal.value is None:
            reset_signal.value = 1
        sim_cycles = sim_config["number_of_clock_cycles"]
        if sca_config.get("clock_cycles") is None:
            assert sim_cycles > 1
            # Note: range is exclusive w.r.t. the end value
            sca_config["clock_cycles"] = [f"1-{sim_cycles}"]

    assert len(clocks) <= 1, "Expected at most one clock signal"
    assert len(resets) <= 1, "Expected at most one reset signal"
    clock_signal = clocks[0].name if clocks else None
    # sca_order = 2

    total_input_bits = max(1, sum(p.num_bits for p in shared_inputs if p.share_id == 0))

    groups = []
    fixed_group_value = random.randint(0, 2**total_input_bits - 1)

    # random group
    groups.append(verilog_value("$", total_input_bits))
    # fixed group
    groups.append(verilog_value(fixed_group_value, total_input_bits))

    end_cycles: Optional[int] = None

    sim_vcd = sim_config.pop("vcd", False)

    number_of_simulations: int = sim_config.pop("number_of_simulations", None)

    number_of_simulations_per_step = sim_config.get("number_of_simulations_per_step", 256)

    number_of_sim_steps = div_ceil(number_of_simulations, number_of_simulations_per_step)

    assert number_of_sim_steps, "number_of_sim_steps must be specified"

    assert (
        number_of_simulations_per_step % 64 == 0
    ), "number_of_simulations_per_step must be a multiple of 64"

    if sim_vcd:
        sim_config["waveform_simulation"] = True
        print("** VCD waveform generation is enabled!")
        print("** Number of simulations was set to 64!")
        number_of_sim_steps = 1
        number_of_simulations_per_step = 64

    number_of_simulations = number_of_simulations_per_step * number_of_sim_steps

    print(f"** Total number of simulations: {number_of_simulations:,}")
    print(f"** Number of simulations per step: {number_of_simulations_per_step:,}")

    input_sequence = []

    start_bits = {}

    end_condition = {}

    if end_cycles:
        end_condition["clock_cycles"] = end_cycles
    if end_signals:
        end_condition["signals"] = [
            {
                "name": p.name_bits,  ## FIXME??? p.name?
                "value": (
                    p.value
                    if isinstance(p.value, str)
                    else verilog_value(p.value if p.value is not None else 1, p.num_bits)
                ),
            }
            for p in end_signals
        ]

    sim_config["number_of_simulations_per_step"] = number_of_simulations_per_step
    sim_config["number_of_simulations"] = number_of_simulations

    sim_config["groups"] = groups
    sim_config["always_random_inputs"] = [p.name_bits for p in rand_inputs]
    sim_config["input_sequence"] = input_sequence
    sim_config["end_condition"] = end_condition

    def assign_fresh_values(inputs: Sequence[Port]) -> list[dict[str, str]]:
        signals = []
        for p in inputs:
            value = p.value_str

            if value is None and p.share_id is not None:
                start_bit = start_bits.get(p.share_id, 0)
                value = "group_in" + (
                    f"{p.share_id}[{p.num_bits + start_bit - 1}:{start_bit}]"
                    if p.num_bits > 1
                    else f"{p.share_id}[{start_bit}]"
                )
                start_bits[p.share_id] = start_bit + p.num_bits
            if value is not None:
                signals.append(
                    {
                        "name": p.name_bits,
                        "value": value,
                    }
                )
        return signals

    initial_signals = assign_fresh_values(
        [p for p in input_ports if p.share_id is not None or p.type is None]
    )
    if reset_signal:
        assert reset_signal.value_str is not None, "Expected reset value at this point"
        initial_signals.append({"name": reset_signal.name, "value": reset_signal.value_str})

    input_sequence += [
        {
            "signals": initial_signals,
            "hold_for_cycles": 1,
        },
    ]
    if reset_signal:
        not_reset = (
            (~reset_signal.value) & ((1 << (reset_signal.num_bits or 1)) - 1)
            if isinstance(reset_signal.value, int)
            else 1 if reset_signal.value in ("1'b0", "1'h0", "0") else 0
        )
        print(f"** Reset signal: {reset_signal.name} = {reset_signal.value} -> {not_reset}")
        input_sequence += [
            {
                "signals": [
                    {
                        "name": reset_signal.name,
                        "value": verilog_value(not_reset, 1),
                    },
                ],
                "hold_for_cycles": 1,
            },
        ]

    hardware = {}

    if clock_signal:
        hardware["clock_signal_name"] = clock_signal

    config = {
        "performance": perf_config,
        "simulation": sim_config,
        "hardware": hardware,
        "side_channel_analysis": sca_config,
    }
    print(f"Writing config to {config_file.absolute()}")

    with open(config_file, "w") as f:
        f.write(json.dumps(config, indent=2))


if __name__ == "__main__":
    args = argparser.parse_args()

    if not args.source_files and not args.sources_list:
        if not args.netlist:
            print("No source files specified")
            exit(1)
        if not args.top_module:
            print("Top module not specified")
            exit(1)
    else:
        if args.netlist:
            print("Either specify source files or netlist file, not both")
            exit(1)

    if args.sources_list:
        with open(args.sources_list, "r") as f:
            args.source_files = [Path(l.strip()) for l in f]

    prolead_root_dir = args.prolead_root_dir or os.environ.get("PROLEAD_ROOT_DIR")

    if prolead_root_dir is None and args.prolead_bin:
        prolead_root_dir = Path(args.prolead_bin).parent.parent

    if prolead_root_dir:
        prolead_root_dir = Path(prolead_root_dir).resolve()
        if args.prolead_bin is None:
            args.prolead_bin = prolead_root_dir / "release" / "PROLEAD"

    if args.netlist is None:
        verilog_lib = args.yosys_verilog_lib
        liberty_lib = args.yosys_lib
        
        if liberty_lib is None:
            if prolead_root_dir is None:
                print("Neither --yosys-verilog-lib/--yosys-lib nor --prolead-root-dir where specified")
                exit(1)
            assert isinstance(prolead_root_dir, Path)
            LIBRARY_PATH = prolead_root_dir / "yosys" / "lib"
            liberty_lib = LIBRARY_PATH / "custom_cells.lib"

            if verilog_lib is None:
                verilog_lib = LIBRARY_PATH / "custom_cells.v"

        if verilog_lib:
            assert verilog_lib.exists(), f"Verilog library {verilog_lib} does not exist"

        assert liberty_lib, f"Liberty library not specified"
        assert liberty_lib.exists(), f"Liberty library {liberty_lib} does not exist"

    prolead_run_dir: Path = Path("prolead_run") / (args.top_module or "top")

    if not prolead_run_dir.exists():
        prolead_run_dir.mkdir(parents=True)

    yosys_run_dir = prolead_run_dir

    if not yosys_run_dir.exists():
        yosys_run_dir.mkdir(parents=True)

    for f in args.source_files:
        assert isinstance(f, Path), f"Expected Path, got {f}"
        assert f.exists(), f"File {f} does not exist"

    assert isinstance(args.source_files, list)

    assert all(isinstance(f, Path) for f in args.source_files)

    args.source_files = [Path(f).absolute() for f in args.source_files]

    if args.netlist:
        netlist_file = args.netlist
        run_synth = False
    else:
        netlist_file = None

        if args.top_module:
            netlist_file = prolead_run_dir / "netlist.v"

        run_synth = args.force_synth

        if not run_synth:
            # Check if the netlist file exists
            if not netlist_file or not netlist_file.exists():
                run_synth = True
            else:
                # check if modification time of the netlist file is older than the source files
                netlist_mtime = netlist_file.stat().st_mtime
                if any(netlist_mtime < Path(f).stat().st_mtime for f in args.source_files):
                    run_synth = True
        netlist_file = prolead_run_dir / "netlist.v"

    if run_synth:
        synthesize(
            args.yosys_bin,
            yosys_run_dir,
            args.source_files,
            args.top_module,
            verilog_lib=verilog_lib,
            liberty_lib=liberty_lib,
            verilog_netlist=netlist_file,
            opt_flatten=args.opt == "flatten" or args.opt == "full",
            opt_full=args.opt == "full",
            split_nets=True,
            quiet=args.quiet_synth,
        )
    else:
        print(f"** Using existing netlist: {netlist_file}")

    if not args.netlist:
        json_netlist = netlist_file.with_suffix(".json")

        print(f"** Parsing {json_netlist}")
        top, ports = parse_json_netlist(json_netlist)

        if not args.top_module:
            print(f"** Detected top module: {top}")
            args.top_module = top
            assert args.top_module, "Failed to detect top module"
            # new_name = prolead_run_dir.parent / args.top_module
            # print(f"Renaming {prolead_run_dir} to {new_name}")
            # prolead_run_dir.rename(new_name)
            # prolead_run_dir = new_name
            # netlist_file = prolead_run_dir / "netlist.v"
    else:
        ports = []

    assert netlist_file

    exclude_signals_regex = ""

    probe_placement = {
        "include": {"signals": ".*", "paths": ".*"},
        "exclude": {
            "signals": exclude_signals_regex if exclude_signals_regex else "(?!)",
            "paths": "(?!)",
        },
    }

    sca_config = {
        "order": args.order,
        "transitional_leakage": args.transitional,
        "effect_size": 0.1,
    }

    if probe_placement:
        sca_config["probe_placement"] = probe_placement

    num_simulations = int(args.num_simulations)

    if args.simulations_per_step:
        number_of_simulations_per_step = int(args.simulations_per_step)
    else:
        number_of_simulations_per_step = min(16, div_ceil(num_simulations, 1_000_000) * 2) * 1024

    ports_map = {p["name"]: p for p in ports}

    jports_map = OrderedDict()

    if args.ports_json:
        jports = []  # merge?
        with open(args.ports_json, "r") as f:
            j = json.load(f)
            p = j.get("ports", {})
            shares_maps = p.get("regex")
            if isinstance(shares_maps, dict):
                shares_maps = [(k, v) for k, v in shares_maps.items()]
            elif shares_maps is None:
                shares_maps = []
            for k, v in p.get("inputs", p.get("input", {})).items():
                v["direction"] = "input"
                jports.append({"name": k, **v})
            for k, v in p.get("outputs", p.get("output", {})).items():
                v["direction"] = "output"
                jports.append({"name": k, **v})
        for p in jports:
            name = p["name"]
            width = p.get("width")
            direction = p.get("direction")
            name, end, start = Port.range_from_name(name)
            if not width and start is not None:
                width = end - start + 1
            port_from_yosys = ports_map.get(name)
            if port_from_yosys:
                assert isinstance(port_from_yosys, dict), "Expected dict"
                if not width:
                    width = port_from_yosys.get("width")
                if not direction:
                    direction = port_from_yosys.get("direction")
            elif ports:
                print(f"** [WARNING] Port {name} not found in yosys netlist!!!")
            if width:
                p["width"] = int(width)
            if direction:
                p["direction"] = direction
            p["name"] = name
            jports_map[name] = p
        if not ports:
            ports = jports
            ports_map = {p["name"]: p for p in ports}
    else:
        shares_maps = [
            (r"^(io_)?rand.*", {"type": "random"}),
            (r"\w+_(?P<share_id>\d+)$", {}),
            (r"^(clk|clock)", {"type": "clock"}),
            (r"^(rst_n|reset_n)", {"type": "reset", "value": 0}),
            (r"^(rst|reset)", {"type": "reset", "value": 1}),
        ]

    for p_name, p in ports_map.items():
        for regex, d in shares_maps:
            m = re.match(regex, p_name)
            if m:
                p.update(**d)
                p.update(**m.groupdict())
                break
        share_id = p.get("share_id")
        if not p.get("width"):
            name, end, start = Port.range_from_name(name)
            p["width"] = end - start + 1 if start is not None else 1
            p["name"] = name

        if share_id is None:
            if (
                jports_map
                and p_name not in jports_map
                and p.get("direction") == "input"
                and p.get("width", 0) > 1
            ):
                print(
                    f"** [WARNING] Input port {p_name} with width {p.get('width')} does not have a share_id!!!"
                )
        else:
            p["share_id"] = int(share_id)

    # print(f"** Ports Map: {ports_map}")
    # print(f"** JSON Ports Map: {jports_map}")

    for k, v in jports_map.items():
        if k not in ports_map:
            ports_map[k] = v
        elif isinstance(ports_map[k], dict):
            ports_map[k].update(v)
        else:
            print(f"** [WARNING] Port {k} already exists in ports_map!!!")

    if args.library_json is None:
        assert isinstance(prolead_root_dir, Path)
        library_json = prolead_root_dir / "library.json"
    else:
        library_json = args.library_json

    library_json = library_json.resolve()

    assert library_json.exists(), f"Library JSON file {library_json} does not exist"

    if args.minimize_probing_sets in (False, 0, "false", "none"):
        args.minimize_probing_sets = "no"

    if not args.num_cores:
        args.num_cores = "half"

    if args.prolead_config:
        print(
            f"** Using existing config file: {args.prolead_config}. All other prolead configuration arguments are ignored!"
        )
        config_file = Path(args.prolead_config)
    else:
        random_seed = (
            args.random_seed if args.random_seed is not None else random.randint(0, 2**64 - 1)
        )
        print(f"Using random seed: {random_seed}")
        random.seed(random_seed)

        config_file = prolead_run_dir / "config.json"
        ports = [Port(**p) for p in ports_map.values()]

        if not args.sim_cycles:
            print(f"** Number of simulation cycles (--sim-cycles) must be specified!")
            exit(1)

        sim_config = {
            "number_of_simulations": num_simulations,
            "number_of_simulations_per_step": number_of_simulations_per_step,
            # "end_wait_cycles": 0,
            "number_of_clock_cycles": args.sim_cycles,
            "number_of_simulations_per_write": 1024 * number_of_simulations_per_step,
        }

        perf_config = {
            # "max_number_of_threads": "half",  ### half of the available cores
            "max_number_of_threads": args.num_cores,
            # "minimize_probing_sets": "aggressive", # "trivial" ,"aggressive", "no"
            "minimize_probing_sets": args.minimize_probing_sets,
            "compact_distributions": args.compact,
        }

        if args.probing_sets_per_step:
            perf_config["number_of_probing_sets_per_step"] = int(args.probing_sets_per_step)

        generate_config(config_file, ports, sca_config, sim_config, perf_config)

    run_prolead(
        args.prolead_bin,
        prolead_run_dir,
        netlist_file,
        args.top_module,
        library_name=args.library_name,
        library_json=library_json,
        sca_config=sca_config,
        config_file=config_file,
        show_figure=args.show_figure,
        result_folder="results",
        pretty=args.pretty,
    )
