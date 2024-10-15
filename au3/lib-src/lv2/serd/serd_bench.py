#!/usr/bin/env python

import csv
import itertools
import math
import matplotlib
import optparse
import os
import subprocess
import sys


class WorkingDirectory:
    "Scoped context for changing working directory"
    def __init__(self, working_dir):
        self.original_dir = os.getcwd()
        self.working_dir = working_dir

    def __enter__(self):
        sys.stderr.write("Entering directory `%s'\n" % self.working_dir)
        os.chdir(self.working_dir)
        return self

    def __exit__(self, type, value, traceback):
        sys.stderr.write("Leaving directory `%s'\n" % self.working_dir)
        os.chdir(self.original_dir)


def filename(n):
    "Filename for a generated file with n statements"
    return 'gen%d.ttl' % n


def gen(sp2b_dir, n_min, n_max, step):
    "Generate files with n_min ... n_max statements if they are not present"
    with WorkingDirectory(sp2b_dir) as dir:
        for n in range(n_min, n_max + step, step):
            out_path = os.path.join(dir.original_dir, 'build', filename(n))
            if not os.path.exists(out_path):
                subprocess.call(['./sp2b_gen', '-t', str(n), out_path])


def write_header(results, progs):
    "Write the header line for TSV output"
    results.write('n')
    for prog in progs:
        results.write('\t' + os.path.basename(prog.split()[0]))
    results.write('\n')


def parse_time(report):
    "Return user time and max RSS from a /usr/bin/time -v report"
    time = memory = None
    for line in report.split('\n'):
        if line.startswith('\tUser time'):
            time = float(line[line.find(':') + 1:])
        elif line.startswith('\tMaximum resident set'):
            memory = float(line[line.find(':') + 1:]) * 1024

    return (time, memory)


def get_dashes():
    "Generator for plot line dash patterns"
    dash = 2.0
    space = dot = 0.75

    yield []             # Solid
    yield [dash, space]  # Dashed
    yield [dot, space]   # Dotted

    # Dash-dots, with increasing number of dots for each line
    for i in itertools.count(2):
        yield [dash, space] + [dot, space] * (i - 1)


def plot(in_file, out_filename, x_label, y_label, y_max=None):
    "Plot a TSV file as SVG"

    matplotlib.use('agg')
    import matplotlib.pyplot as plt

    fig_height = 4.0
    dashes = get_dashes()
    markers = itertools.cycle(['o', 's', 'v', 'D', '*', 'p', 'P', 'h', 'X'])

    reader = csv.reader(in_file, delimiter='\t')
    header = next(reader)
    cols = [x for x in zip(*list(reader))]

    plt.clf()
    fig = plt.figure(figsize=(fig_height * math.sqrt(2), fig_height))
    ax = fig.add_subplot(111)

    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)

    if y_max is not None:
        ax.set_ylim([0.0, y_max])

    ax.grid(linewidth=0.25, linestyle=':', color='0', dashes=[0.2, 1.6])
    ax.ticklabel_format(style='sci', scilimits=(4, 0), useMathText=True)
    ax.tick_params(axis='both', width=0.75)

    x = list(map(float, cols[0]))
    for i, y in enumerate(cols[1::]):
        ax.plot(x,
                list(map(float, y)),
                label=header[i + 1],
                marker=next(markers),
                dashes=next(dashes),
                markersize=3.0,
                linewidth=1.0)

    plt.legend()
    plt.savefig(out_filename, bbox_inches='tight', pad_inches=0.025)
    plt.close()
    sys.stderr.write('wrote {}\n'.format(out_filename))


def run(progs, n_min, n_max, step):
    "Benchmark each program with n_min ... n_max statements"
    with WorkingDirectory('build'):
        results = {'time':       open('serdi-time.txt', 'w'),
                   'throughput': open('serdi-throughput.txt', 'w'),
                   'memory':     open('serdi-memory.txt', 'w')}

        # Write TSV header for all output files
        for name, f in results.items():
            write_header(f, progs)

        for n in range(n_min, n_max + step, step):
            # Add first column (n) to rows
            rows = {}
            for name, _ in results.items():
                rows[name] = [str(n)]

            # Run each program and fill rows with measurements
            for prog in progs:
                cmd = '/usr/bin/time -v ' + prog + ' ' + filename(n)
                with open(filename(n) + '.out', 'w') as out:
                    sys.stderr.write(cmd + '\n')
                    proc = subprocess.Popen(
                        cmd.split(), stdout=out, stderr=subprocess.PIPE)

                    time, memory = parse_time(proc.communicate()[1].decode())
                    rows['time'] += ['%.07f' % time]
                    rows['throughput'] += ['%d' % (n / time)]
                    rows['memory'] += [str(memory)]

            # Write rows to output files
            for name, f in results.items():
                f.write('\t'.join(rows[name]) + '\n')

        for name, f in results.items():
            tsv_filename = 'serdi-%s.txt' % name
            sys.stderr.write('wrote %s\n' % tsv_filename)


def plot_results():
    "Plot all benchmark results"
    with WorkingDirectory('build'):
        plot(open('serdi-time.txt', 'r'), 'serdi-time.svg',
             'Statements', 'Time (s)')
        plot(open('serdi-throughput.txt', 'r'), 'serdi-throughput.svg',
             'Statements', 'Statements / s')
        plot(open('serdi-memory.txt', 'r'), 'serdi-memory.svg',
             'Statements', 'Bytes')


if __name__ == "__main__":
    class OptParser(optparse.OptionParser):
        def format_epilog(self, formatter):
            return self.expand_prog_name(self.epilog)

    opt = OptParser(
        usage='%prog [OPTION]... SP2B_DIR',
        description='Benchmark RDF reading and writing commands\n',
        epilog='''
Example:
  %prog --max 100000 \\
      --run 'rapper -i turtle -o turtle' \\
      --run 'riot --output=ttl' \\
      --run 'rdfpipe -i turtle -o turtle' /path/to/sp2b/src/
''')

    opt.add_option('--max', type='int', default=1000000,
                   help='maximum triple count')
    opt.add_option('--run', type='string', action='append', default=[],
                   help='additional command to run (input file is appended)')
    opt.add_option('--no-generate', action='store_true',
                   help='do not generate data')
    opt.add_option('--no-execute', action='store_true',
                   help='do not run benchmarks')
    opt.add_option('--no-plot', action='store_true',
                   help='do not plot benchmarks')

    (options, args) = opt.parse_args()
    if len(args) != 1:
        opt.print_usage()
        sys.exit(1)

    progs = ['serdi -b -f -i turtle -o turtle'] + options.run
    min_n = int(options.max / 10)
    max_n = options.max
    step = min_n

    if not options.no_generate:
        gen(str(args[0]), min_n, max_n, step)
    if not options.no_execute:
        run(progs, min_n, max_n, step)
    if not options.no_plot:
        plot_results()
