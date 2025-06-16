import os
import subprocess
import time
import csv

# List of Python files to benchmark and output CSV
INPUT_LIST = "metrics/.all_py_files.txt"
BENCH_CSV  = "metrics/metrics_test.csv"

def run_tool(cmd):
    """
    Invoke `mini-pytype` via Dune, measure elapsed time, and return (duration_s, exit_code).
    """
    # Prefix with `dune exec --` so the executable is found in the build
    full_cmd = ["dune", "exec", "--"] + cmd
    start = time.perf_counter()
    proc  = subprocess.run(full_cmd, capture_output=True, text=True)
    elapsed = time.perf_counter() - start
    return elapsed, proc.returncode


def main():
    with open(INPUT_LIST) as f, open(BENCH_CSV, "w", newline="") as out:
        writer = csv.writer(out)
        # filename, runtime seconds, file size in bytes, lines of code, exit code
        writer.writerow(["filename", "duration_s", "bytes", "loc", "exit_code"])

        for line in f:
            fn = line.strip()
            if not fn:
                continue

            # Measure file size and line count
            try:
                size = os.path.getsize(fn)
                loc  = sum(1 for _ in open(fn, errors="ignore"))
            except OSError as e:
                print(f"Skipping {fn}: {e}")
                continue

            # Run the type-checker
            duration, exit_code = run_tool(["mini-pytype", fn])

            # Write results
            writer.writerow([fn, f"{duration:.4f}", size, loc, exit_code])
            print(f"{fn}: {duration:.2f}s, size={size} bytes, loc={loc}, exit={exit_code}")

if __name__ == "__main__":
    main()
