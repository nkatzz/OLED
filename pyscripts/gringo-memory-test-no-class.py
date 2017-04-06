"""
This is a test to see if the increase in memory over long-time
runs (that repeatedly call clingo via Jep) is due to Jep or
the Gringo python module.
"""

from itertools import islice, tee, izip
import gringo
import locale


fraud_data = "/media/storage_/SPEEDD-FRAUD-TEST-DATA/fraud-data"
bk_preprocess = "/home/nkatz/dev/ILED/datasets/Fraud/bk-preprocess.lp"
bk = "/home/nkatz/dev/ILED/datasets/Fraud/bk.lp"
write_to = "/home/nkatz/Desktop/kernel.txt"


def run(aspFile, solveMode, task):

    models = []

    def __on_model(model):
        models.append(model.atoms())

    @profile
    def solve():
        locale.setlocale(locale.LC_ALL, 'C')
        ctl = gringo.Control(['-Wno-atom-undefined', '-t4'])
        ctl.load(aspFile)
        ctl.conf.solve.models = 0 if solveMode in ["all", "optN"] else solveMode
        if solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])])
        result = ctl.solve(assumptions=None, on_model=__on_model)
        return models

    return solve()


def sliding(o, size, skip=1):
    return islice(izip(*(islice(j, i, None) for i, j in enumerate(tee(o, size)))), 0, None, skip)


def write_to_sap_file(current_example, asp_file, previous_inference_results):
    f = open(asp_file,'w')
    f.write('#include \"%s\".\n\n'%bk_preprocess)
    (first,second) = (current_example[0], current_example[1])
    f.write('%s\n%s\n\n'%(first,second))
    z = map(lambda x: str(x)+".", previous_inference_results)
    z1 = '\n'.join(z)
    f.write('%s\n' % z1)
    f.close()


@profile
def preprocess(f1):
    previous_inference_results = []
    i = 0
    with open(fraud_data) as infile:
        for x in sliding(infile, 2):
            write_to_sap_file(x, f1, previous_inference_results)
            result = run(f1, 'all', 'inference')[0]
            print(i, result)
            i = i + 1
            previous_inference_results = result


preprocess(write_to)