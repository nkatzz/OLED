"""
This is a test to see if the increase in memory over long-time
runs (that repeatedly call clingo via Jep) is due to Jep or
the Gringo python module.
"""

from itertools import islice, tee, izip
import gringo
import locale
import objgraph


fraud_data = "/media/storage_/SPEEDD-FRAUD-TEST-DATA/data.lp"
bk = "/home/nkatz/dev/ILED/pyscripts/fraud-bk.lp"
write_to = "/home/nkatz/Desktop/kernell.txt"


class ASPHandler:
    
    def __init__(self, aspFile, solveMode, task):
        # args = dict(x.split('=',1) for x in sys.argv[1:])
        # command-line args may be passed using the 'get_const'
        # method, see the incqueens example
        self.aspfile = aspFile
        self.solveMode = solveMode  # should be '' if none is given
        self.task = task
        self.models = []
        self.result = ''
        self.grndPrgSize = 0
        self.solvingTime = 0

    # @profile
    def __on_model(self, model):
        self.models.append(model.atoms())

    # @profile
    def solve(self):
        locale.setlocale(locale.LC_ALL, 'C')
        ctl = gringo.Control(['-Wno-atom-undefined', '-t4'])
        ctl.load(self.aspfile)
        ctl.conf.solve.models = 0 if self.solveMode in ["all", "optN"] else self.solveMode
        if self.solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])])
        self.result = ctl.solve(assumptions=None, on_model=self.__on_model)


def run(aspFile, solveMode, task):
    asp = ASPHandler(aspFile, solveMode, task)
    asp.solve()
    return asp.models


def sliding(o, size, skip=1):
    return islice(izip(*(islice(j, i, None) for i, j in enumerate(tee(o, size)))), 0, None, skip)


def write_to_sap_file(current_example, asp_file, previous_inference_results):
    f = open(asp_file,'w')
    f.write('#include \"%s\".\n\n'%bk)
    (first,second) = (current_example[0], current_example[1])
    f.write('%s\n%s\n\n'%(first,second))
    z = map(lambda x: str(x)+".", previous_inference_results)
    z1 = '\n'.join(z)
    f.write('%s\n' % z1)
    f.close()


# @profile
def preprocess(f1):
    previous_inference_results = []
    i = 0
    with open(fraud_data) as infile:
        for x in islice(sliding(infile, 2), 1000000):
            write_to_sap_file(x, f1, previous_inference_results)
            result = run(f1, 'all', 'inference')[0]
            print(i, result)
            i = i + 1
            previous_inference_results = result

objgraph.show_growth()
preprocess(write_to)
# objgraph.show_most_common_types()
objgraph.show_growth()
roots = objgraph.get_leaking_objects()
print(len(roots))









