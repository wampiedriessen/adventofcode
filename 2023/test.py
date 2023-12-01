from pathlib import Path
import subprocess
import os

dirs = [ f for f in os.scandir('.') if f.is_dir() and f.name.startswith('day') ]

results = dict()

for daydir in dirs:
    dayfiles = os.scandir(f'{daydir.path}/files')
    inputs = [ f for f in dayfiles if f.name.endswith('.in') ]

    results[daydir.name] = dict()

    for inp in inputs:
        inputValue = Path(inp.path).read_text()

        for n in ["1","2"]:
            outfile = f'{inp.path[:-3]}.out{n}'
            if os.path.exists(outfile):
                p = subprocess.run([f'{daydir.path}/start', n], input=inputValue, capture_output=True, text=True)
                expectedOutput = Path(outfile).read_text()

                results[daydir.name][f'p{n} - {inp.name[:-3]}'] = {
                    'real': p.stdout,
                    'test': expectedOutput,
                    'check': p.stdout == expectedOutput
                }

passed_all = True

for day in results:
    print(f'==> {day.upper()}', end=" ")
    passed = True
    for name,run in results[day].items():
        if not run['check']:
            if passed:
                print()
            print(f'    FAILED: {day} -> {name}')
            print(f'    Expected: {run["test"]}\nGot: {run["real"]}')
            passed = False
    if passed:
        print(f'✓')
    else:
        passed_all = False

assert passed_all
