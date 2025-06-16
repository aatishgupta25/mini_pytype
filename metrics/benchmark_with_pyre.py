
import subprocess
import time
import csv

"""
Benchmark mini-pytype per-file versus a single Pyre run over the project.
Outputs bench_results.csv with total and per-file metrics.
"""

# Inputs & outputs\INPUT_LIST = "all_py_files.txt"
INPUT_LIST = "metrics/.all_py_files.txt"
CSV_OUTPUT = "metrics/benchmark_with_pyre_summary.csv"

def run_mini_pytype(file_list):
    """
    Run mini-pytype on each file individually, return total duration.
    """
    start = time.perf_counter()
    for fn in file_list:
        fn = fn.strip()
        if not fn:
            continue
        subprocess.run(["dune", "exec", "--", "mini-pytype", fn],
                       stdout=subprocess.DEVNULL,
                       stderr=subprocess.DEVNULL)
    total = time.perf_counter() - start
    return total


def run_pyre():
    """
    Run Pyre type-check once on the project, return duration.
    Assumes `pyre` is configured (e.g., .pyre_configuration).
    """
    start = time.perf_counter()
    subprocess.run(["pyre", "check", "--noninteractive"],
                   stdout=subprocess.DEVNULL,
                   stderr=subprocess.DEVNULL)
    total = time.perf_counter() - start
    return total


def main():
    # Read file list
    with open(INPUT_LIST) as f:
        files = [line.strip() for line in f if line.strip()]

    # Measure mini-pytype
    mini_time = run_mini_pytype(files)
    mini_avg  = mini_time / len(files)

    # Measure Pyre once
    pyre_time = run_pyre()
    pyre_avg  = pyre_time / len(files)

    # Write summary CSV
    with open(CSV_OUTPUT, "w", newline="") as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(["tool", "total_s", "avg_per_file_s"])
        writer.writerow(["mini-pytype", f"{mini_time:.4f}", f"{mini_avg:.4f}"])
        writer.writerow(["pyre",       f"{pyre_time:.4f}", f"{pyre_avg:.4f}"])

    # Print results
    print(f"mini-pytype: total {mini_time:.2f}s, avg {mini_avg:.4f}s/file")
    print(f"pyre:       total {pyre_time:.2f}s, avg {pyre_avg:.4f}s/file")

if __name__ == "__main__":
    main()
