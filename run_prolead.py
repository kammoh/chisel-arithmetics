#!/usr/bin/env python3
import argparse
from dataclasses import dataclass
import json
import os
from pathlib import Path
import random
import re
import subprocess
from typing import Any

import numpy as np
import matplotlib.pyplot as plt

# Synthesize RTL sources using yosys and then run PROLEAD

argparser = argparse.ArgumentParser(description="Run PROLEAD")

argparser.add_argument("source_files", nargs="+", type=Path, help="Source files")
argparser.add_argument("-t", "--top-module", default=None, help="Top module")
argparser.add_argument("--force-synth", help="Force synthesis.", action="store_true", default=False)
argparser.add_argument("--yosys-bin", help="Path to yosys binary", default="yosys")
argparser.add_argument(
    "--prolead-root-dir", help="Path to PROLEAD source directory", type=Path, default=None
)
argparser.add_argument("--prolead-bin", help="Path to PROLEAD binary", default=None)
argparser.add_argument("--random-seed", default=None, type=int, help="Random seed")

args = argparser.parse_args()

PROLEAD_ROOT_ROOT = args.prolead_root_dir or Path(
    os.environ.get("PROLEAD_ROOT_ROOT", "/Volumes/src/PROLEAD")
)

if PROLEAD_ROOT_ROOT and args.prolead_bin is None:
    args.prolead_bin = PROLEAD_ROOT_ROOT / "release" / "PROLEAD"

LIBRARY_PATH = PROLEAD_ROOT_ROOT / "yosys" / "lib"

verilog_lib = LIBRARY_PATH / "custom_cells.v"
liberty_lib = LIBRARY_PATH / "custom_cells.lib"


def synthesize(
    yosys_bin: Path | str,
    yosys_run_dir: Path,
    source_files: list[Path],
    top_module: str | None,
    verilog_lib: Path,
    liberty_lib: Path,
    verilog_netlist: Path,
    opt_flatten: bool = True,
    opt_full: bool = True,
    quiet: bool = True,
):
    yosys_run_dir.mkdir(parents=True, exist_ok=True)

    verilog_lib = verilog_lib.resolve()
    liberty_lib = liberty_lib.resolve()
    verilog_netlist = verilog_netlist.absolute()

    json_netlist = verilog_netlist.with_suffix(".json")

    yosys_script = []

    for src in source_files:
        src = Path(src)
        if src.suffix == ".sv":
            yosys_script.append(f"read_verilog -sv {src}")
        elif src.suffix == ".v":
            yosys_script.append(f"read_verilog {src}")
        else:
            raise ValueError(f"Unsupported file type: {src}")

    yosys_script += [
        f"read_verilog -lib {verilog_lib}",
    ]

    synth_prep_args = f"-top {top_module}" if top_module else "-auto-top"

    # yosys_script.append("setattr -set keep_hierarchy 1")
    if opt_flatten:
        synth_prep_args += " -flatten"

    yosys_script += [
        "prep " + synth_prep_args,
        "opt_clean -purge",
        "setattr -set keep_hierarchy 1",
        "synth " + synth_prep_args,
    ]
    if opt_full:
        yosys_script.append("opt -full")
    else:
        yosys_script.append("opt_clean -purge")
    yosys_script += [
        f"dfflibmap -liberty {liberty_lib}",
    ]

    if opt_full:
        yosys_script.append("opt -full")
    else:
        yosys_script.append("opt_clean -purge")
    yosys_script += [
        f"abc -liberty {liberty_lib}",
    ]

    if opt_full:
        yosys_script.append("opt -full")
    else:
        yosys_script.append("opt_clean -purge")

    yosys_script += [
        "setattr -set keep_hierarchy 0",
        "flatten",
    ]

    if opt_full:
        yosys_script.append("opt -full")
    else:
        yosys_script.append("opt_clean -purge")

    if top_module:
        yosys_script.append(f"select {top_module}")

    if opt_full:
        yosys_script.append("opt -full")

    yosys_script += [
        "opt_clean -purge",
        f"stat -liberty {liberty_lib}",
        f"write_verilog -noattr -selected {verilog_netlist}",
        f"write_json {json_netlist}",
    ]
    yosys_cmd = [yosys_bin, "-Q", "-T"]
    if quiet:
        yosys_cmd.append("-q")
    yosys_cmd += ["-p", "; ".join(yosys_script)]

    print("\n" + "=" * 20 + " YOSYS SYNTHESIS " + "=" * 20)

    print(f"** Running {' '.join(map(str, yosys_cmd))}\n")
    subprocess.run(
        yosys_cmd,
        cwd=yosys_run_dir,
        check=True,
    )
    assert verilog_netlist.exists(), f"Failed to generate netlist {verilog_netlist}"
    assert json_netlist.exists(), f"Failed to generate json netlist {json_netlist}"
    print(f"** Generated netlist: {verilog_netlist}\n")
    print("" + "=" * 56 + "\n")


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

netlist_file = None

if args.top_module:
    netlist_file = prolead_run_dir / "netlist.v"

run_synth = args.force_synth or (netlist_file is None)

if run_synth is None and netlist_file:
    # Check if the netlist file exists
    if not netlist_file.exists():
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
    )
else:
    print(f"** Using existing netlist: {netlist_file}")


@dataclass
class Port:
    name: str
    width: int | None  # none for scalar 1-bit ports?
    type: str | None = None

    @property
    def num_bits(self):
        return self.width if self.width else 1

    @property
    def name_bits(self):
        if self.num_bits > 1:
            return f"{self.name}[{self.num_bits - 1}:0]"
        return self.name


@dataclass
class SharedPort(Port):
    share_id: int | None = None  # none for non-shared ports


@dataclass
class Input(SharedPort):
    pass


@dataclass
class Random(Port):
    pass


@dataclass
class Output(SharedPort):
    pass


@dataclass
class ClockPort(Port):
    width = None


def get_top_module_and_ports(netlist: dict) -> tuple[str | None, list, list]:
    modules = netlist["modules"]
    assert modules and isinstance(modules, dict), "Failed to parse json netlist"
    top_name = None
    top_module = None
    input_ports = []
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
            width = len(port["bits"])
            p = {"name": name, "width": width}
            if direction == "input":
                input_ports.append(p)
            elif direction == "output":
                output_ports.append(p)

    return top_name, input_ports, output_ports


def parse_json_netlist(json_netlist_file: Path) -> tuple[str | None, list, list]:
    with open(json_netlist_file, "r") as f:
        netlist = json.load(f)
    return get_top_module_and_ports(netlist)


json_netlist = netlist_file.with_suffix(".json")

print(f"** Parsing {json_netlist}")
top, input_ports, output_ports = parse_json_netlist(json_netlist)

if not args.top_module:
    print(f"** Detected top module: {top}")
    args.top_module = top
    assert args.top_module, "Failed to detect top module"
    # new_name = prolead_run_dir.parent / args.top_module
    # print(f"Renaming {prolead_run_dir} to {new_name}")
    # prolead_run_dir.rename(new_name)
    # prolead_run_dir = new_name
    # netlist_file = prolead_run_dir / "netlist.v"


def run_prolead(
    prolead_bin: str | Path,
    netlist_file: Path,
    top_module: str,
    library_name: str,
    library_json: Path,
    result_folder: str | Path = "results",
):

    assert netlist_file.exists(), f"Netlist file {netlist_file} does not exist"

    config_file = prolead_run_dir / "config.json"
    generate_config(config_file, input_ports, output_ports)

    print(f"** Running PROLEAD in {prolead_run_dir.absolute()}")

    config_file = config_file.relative_to(prolead_run_dir)

    if not library_json.is_absolute():
        library_json = library_json.relative_to(prolead_run_dir)

    if not netlist_file.is_absolute():
        netlist_file = netlist_file.relative_to(prolead_run_dir)

    if isinstance(result_folder, Path):
        result_folder.mkdir(parents=True, exist_ok=True)
        if not result_folder.is_absolute():
            result_folder = result_folder.relative_to(prolead_run_dir)

    prolead_cmd = [
        prolead_bin,
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
        r"^\s*\|\s*(?P<elapsed_time>\d+\.\d+)[smh]\s*\|\s*(?P<ram_usage>\d+\.\d+[A-Z]*)\s*\|\s*(?P<n_sim>\d+)(\s*\/\s*(?P<total_sim>\d+))?\s*\|\s*\[(?P<signals>([\w\$\[\]]+\(\d+\)(,\s)?)+)\]\s*\|\s*(?P<p_log>(\d+\.\d+|inf))\s*\|\s*(?P<status>[A-Z]+)\s*\|\s*$",
        # r"^\s*\|\s*(?P<elapsed_time>\d+\.\d+)[smh]\s*\|\s*(?P<ram_usage>\d+\.\d+[A-Z]*)\s*\|\s*(?P<n_sim>\d+)(\s*\/\s*(?P<total_sim>\d+))?\s*\|\s*\[(?P<signals>.*)\]\s*\|\s*(?P<p_log>(\d+\.\d+|inf))\s*\|\s*(?P<status>[A-Z]+)\s*\|\s*$",
        re.DOTALL,
        # | re.IGNORECASE
    )

    data = []
    data_np = None

    write_every = 2_000_000

    prev_checkpoint = 0

    npy_file = prolead_run_dir / f"{top_module}_data.npy"

    leaking_signals = set()

    # catch KeyboardInterrupt
    try:
        for line in map(str.strip, proc.stdout):
            m = result_line_regex.fullmatch(line)
            if m:
                print("> " + line)
                # print(f"matches: {m.groups()}")
                n_sim = int(m.group("n_sim"))
                # total_sim = int(m.group("total_sim"))
                signals = m.group("signals")
                p_log = float(m.group("p_log"))
                status = m.group("status")
                leakage = status != "OKAY"
                if leakage and signals:
                    leaking_signals.update((s, n_sim, p_log) for s in signals.split(", "))
                # leakage = status == "LEAKAGE"
                data.append((n_sim, p_log))
                if write_every > 0 and (n_sim - prev_checkpoint >= write_every):
                    print(f"Writing checkpoint to {npy_file}")
                    prev_checkpoint = n_sim
                    data_np = np.array(data)
                    np.save(npy_file, data_np)
                # print(f"{n_sim}/{total_sim} {signals} {p_log} {status}")
            else:
                print(line)
        proc.wait()
    except KeyboardInterrupt:
        print("*** Caught KeyboardInterrupt, terminating PROLEAD... ***")
        proc.terminate()
    finally:
        if data:
            data_np = np.array(data)
            print(f"** Writing data to {npy_file}")
            np.save(npy_file, data_np)
        else:
            print("No data captured")
        if leaking_signals:
            print(f"** Leakage Detected!!!")
            cycles_signals = []
            for s, n_sim, p_log in leaking_signals:
                m = re.match(r"([\$\w\[\]]+)\((\d+)\)", s)
                if m:
                    cycles_signals.append((int(m.group(2)), m.group(1), p_log))
                else:
                    print(f"** Unmatched signal / cycle format: {s}")
            cycles_signals = list(set(cycles_signals))
            cycles_signals.sort(key=lambda x: x[0])
            print(
                f"** Leaking signals:\n{'\n'.join(f" {c:4d}: {s} [{p_log:3.2f}]" for c, s, p_log in cycles_signals)}"
            )

    if proc.returncode:
        print(f"PROLEAD failed with return code {proc.returncode}")
        exit(1)

    if data_np is not None:
        # Plot
        plt.figure()

        x = data_np[:, 0]
        y = data_np[:, 1]

        num_sims = np.max(x)
        max_p_log = np.max(y)

        if num_sims > 10_000_000_000:
            x_scale = 1_000_000_000
        elif num_sims > 10_000_000:
            x_scale = 1_000_000
        elif num_sims > 10_000:
            x_scale = 1_000
        else:
            x_scale = 1

        if x_scale > 1:
            x = x / x_scale

        plt.plot(x, y, label="glitch+transition")
        plt.axhline(y=6, color="r", linestyle="--")
        plt.axhline(y=max_p_log, color="blue", linestyle="--")
        plt.xlabel("Number of Simulations" + (f" (x{int(x_scale):,})" if x_scale > 1 else ""))
        plt.ylabel(r"$-\log_{10}(p)$")

        fig_file = npy_file.with_suffix(".png")

        print(f"Saving plot to {fig_file}")
        plt.savefig(fig_file, dpi=600)


def div_ceil(a: int, b: int) -> int:
    return (a + b - 1) // b


def generate_config(
    config_file: Path, input_ports: list[dict[str, Any]], output_ports, sca_order=1
):

    shares_maps = [
        (r"^rand.*", {"type": "random"}),
        (r"\w+_(?P<share_id>\d+)$", {}),
        (r"^(clk|clock)", {"type": "clock"}),
        (r"^(rst|reset)", {"type": "reset"}),
    ]
    for p in input_ports:
        for regex, d in shares_maps:
            m = re.match(regex, p["name"])
            if m:
                p.update(**d)
                p.update(**m.groupdict())
                break
        share_id = p.get("share_id")
        if share_id is not None:
            p["share_id"] = int(share_id)
    for p in output_ports:
        for regex, d in shares_maps:
            m = re.match(regex, p["name"])
            if m:
                p.update(**d)
                p.update(**m.groupdict())
                break
        share_id = p.get("share_id")
        if share_id is not None:
            p["share_id"] = int(share_id)

    shared_inputs = [Input(**p) for p in input_ports if p.get("share_id") is not None]
    print(f"  shared_inputs: {shared_inputs}")
    shared_outputs = [Output(**p) for p in output_ports if p.get("share_id") is not None]
    rand_inputs = [Random(**p) for p in input_ports if p.get("type") == "random"]
    clocks = [ClockPort(**p) for p in input_ports if p.get("type") == "clock"]
    print(f"  clocks: {clocks}")
    resets = [Input(**p) for p in input_ports if p.get("type") == "reset"]

    assert len(clocks) <= 1, "Expected at most one clock signal"
    assert len(resets) <= 1, "Expected at most one reset signal"
    clock_signal = clocks[0].name if clocks else None
    reset_signal = resets[0].name if resets else None
    # sca_order = 2

    total_input_bits = sum(p.num_bits for p in shared_inputs)

    groups = []
    fixed_group_value = random.randint(0, 2**total_input_bits - 1)

    # random group
    groups.append(f"{total_input_bits}'b{'$' * total_input_bits}")
    # fixed group
    groups.append(f"{total_input_bits}'b{fixed_group_value:0{total_input_bits}b}")

    # simulation configuration
    simulation_cycles: int = 14

    end_cycles: int | None = 14
    end_signal: str | None = None
    end_signal_value: str | int = 1

    sim_vcd = False

    # number_of_simulations = 50_000_000
    number_of_simulations = 10_000_000

    number_of_simulations_per_step = 16 * 1024

    number_of_sim_steps = div_ceil(number_of_simulations, number_of_simulations_per_step)

    assert (
        number_of_simulations_per_step % 64 == 0
    ), "number_of_simulations_per_step must be a multiple of 64"

    if sim_vcd:
        number_of_sim_steps = 1
        number_of_simulations_per_step = 64

    number_of_simulations = number_of_simulations_per_step * number_of_sim_steps

    print(f"Total number of simulations: {number_of_simulations:,}")

    performance = {
        # "max_number_of_threads": "half",  ### half of the available cores
        "max_number_of_threads": "8",
        # "minimize_probing_sets": "aggressive",
        "minimize_probing_sets": "trivial",
        "compact_distributions": True,
    }

    input_sequence = []

    start_bits = {}

    def assign_fresh_values(shared_inputs: list[Input]) -> list[dict[str, str]]:
        signals = []
        for p in shared_inputs:
            start_bit = start_bits.get(p.share_id, 0)
            signals.append(
                {
                    "name": p.name_bits,
                    "value": "group_in"
                    + (
                        f"{p.share_id}[{p.num_bits + start_bit - 1}:{start_bit}]"
                        if p.num_bits > 1
                        else f"{p.share_id}[{start_bit}]"
                    ),
                }
            )
            start_bits[p.share_id] = start_bit + p.num_bits
        return signals

    input_sequence += [
        {
            "signals": [
                *assign_fresh_values(shared_inputs),
                {"name": reset_signal, "value": "1'b1"},
            ],
            "hold_for_cycles": 1,
        },
        {
            "signals": [
                {"name": reset_signal, "value": "1'b0"},
            ],
            "hold_for_cycles": 1,
        },
    ]

    end_condition = {}

    if end_cycles:
        end_condition["clock_cycles"] = end_cycles
    if end_signal:
        if isinstance(end_signal_value, (int, bool)):
            end_signal_value = f"1'b{int(end_signal_value):b}"
        end_condition = {"signals": {[{"name": end_signal, "value": f"{end_signal_value}"}]}}

    simulation = {
        "groups": groups,
        "always_random_inputs": [p.name_bits for p in rand_inputs],
        "input_sequence": input_sequence,
        "end_condition": end_condition,
        "number_of_simulations": number_of_simulations,
        "number_of_simulations_per_step": number_of_simulations_per_step,
        "end_wait_cycles": 0,
        "number_of_clock_cycles": simulation_cycles,
    }
    if sim_vcd:
        simulation["waveform_simulation"] = True

    hardware = {}

    probe_placement = {
        # "include": {"signals": ".*", "paths": ".*"},
        # "exclude": {"signals": "_g_WIRE(_1)?"},
    }

    sca = {
        "order": sca_order,
        # "transitional_leakage": True,
        "transitional_leakage": False,
        # "clock_cycles": ["1-2"],
    }

    if probe_placement:
        sca["probe_placement"] = probe_placement

    if clock_signal:
        hardware["clock_signal_name"] = clock_signal

    config = {
        "performance": performance,
        "simulation": simulation,
        "hardware": hardware,
        "side_channel_analysis": sca,
    }
    print(f"Writing config to {config_file.absolute()}")

    with open(config_file, "w") as f:
        f.write(json.dumps(config, indent=2))


assert netlist_file

random_seed = args.random_seed if args.random_seed is not None else random.randint(0, 2**64 - 1)

print(f"Using random seed: {random_seed}")
random.seed(random_seed)

run_prolead(
    args.prolead_bin,
    netlist_file,
    args.top_module,
    library_name="custom",
    library_json=PROLEAD_ROOT_ROOT / "library.json",
    result_folder="results",
)
