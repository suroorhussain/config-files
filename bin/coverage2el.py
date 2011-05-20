#!/usr/bin/env python

from coverage import coverage, summary, misc
from coverage.config import CoverageConfig


class ElispReporter(summary.SummaryReporter):
    def report(self, morfs, outfile=None, config=None):
        conf = CoverageConfig()
        conf.include = ["/pluto/pycloud/*"]
        conf.omit = ["*.txt", "*.xml", '*.tmpl']
        self.find_code_units(None, conf)

        out = open(".coverage.el", "w")

        out.write("(let ((results (make-hash-table :test 'equal)))\n")
        for cu in self.code_units:
            f = cu.filename
            try:
                (fn, executable, missing, mf) = self.coverage.analysis(cu)
            except misc.NoSource:
                continue
            code_linenumbers = executable
            uncovered_code = missing
            covered_linenumbers = sorted(set(executable) - set(missing))
            out.write(
                " (puthash \"%s\" '((%s) (%s) (%s)) results)\n"
                % (f,
                   " ".join([str(ln) for ln in sorted(code_linenumbers)]),
                   " ".join([str(ln) for ln in sorted(covered_linenumbers)]),
                   " ".join([str(ln) for ln in sorted(uncovered_code)])))
        out.write(" results)\n")
        out.close()


def main():
    c = coverage()
    c.load()
    ElispReporter(c).report(None)

if __name__ == '__main__':
    main()
