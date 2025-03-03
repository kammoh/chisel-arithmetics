#!/usr/bin/env python3
import argparse
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


from rich.console import Console
from rich.table import Table
from rich.live import Live
import time


argparser = argparse.ArgumentParser(description="Plot ProLead")
argparser.add_argument("data", help="Data file", type=Path)
argparser.add_argument("--output", default=None, help="Output file", type=Path)
argparser.add_argument("--dpi", default=600, help="DPI of the output image.", type=int)

args = argparser.parse_args()

console = Console()

data_file = args.data
assert data_file and isinstance(data_file, Path)

sns.set_theme(style="whitegrid", context="paper")
plt.figure()

table = Table()
table.add_column("ID")
table.add_column("Value")




with Live(table, console=console, refresh_per_second=4) as live:
    for i in range(10):
        table.add_row(str(i), str(i * 2))
        time.sleep(1)

with np.load(data_file) as data:
    if isinstance(data, np.lib.npyio.NpzFile):
        key = data.files[0]
        data_np = data[key]
    else:
        data_np = data

    print(f"Loaded data {data_np.shape}")

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

    plot = sns.lineplot(
        x=x,
        y=y,
        # kind="line",
        label=r"$-\log_{10}(p)$",
    )
    plot.axhline(y=max_p_log, linestyle="--", label="Minimum p-value", alpha=0.5)
    plot.axhline(y=5, color="r", linestyle="--", label="Threshold")
    plot.set_xlabel("Number of Simulations" + (fr" ($\times${int(x_scale):,})" if x_scale > 1 else ""))
    plot.set_ylabel(r"$-\log_{10}(p)$")
    plot.legend(loc="best", fancybox=True, framealpha=0.9)
    plt.tight_layout()

    fig_file = args.output or data_file.with_suffix(".png")

    print(f"Saving plot to {fig_file}")
    plt.savefig(fig_file, dpi=args.dpi)
    plt.show()
