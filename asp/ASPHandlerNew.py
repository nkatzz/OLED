import gringo
import sys
from java.util import ArrayList, HashMap
import locale

def run(aspFile, solveMode, task):

    models = []

    def __on_model(model):
        models.append(model.atoms())

    def solve(aspFile, solveMode, task):
        print('test')
        solvingTime = 0.0
        grndPrgSize = 0.0
        locale.setlocale(locale.LC_ALL, 'C')
        ctl = gringo.Control(['-Wno-atom-undefined','-t8'])
        ctl.load(aspFile)
        ctl.conf.solve.models = 0 if solveMode in ["all","optN"] else solveMode
        if solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])])
        result = ctl.solve(assumptions = None, on_model = __on_model)
        if task == 'score_rules':
            solvingTime = ctl.stats['time_total']
            grndPrgSize = ctl.stats['lp']['atoms']
            print(solvingTime)
        return (result,solvingTime,grndPrgSize)


    (result,solvingTime,grndPrgSize) = solve(aspFile, solveMode, task)
    r1 = result
    r2 = [y for y in [" ".join(map(lambda x: str(x),model)) for model in models]]

    m2 = ArrayList()
    for x in r2:
        m2.add(x)

    m3 = ArrayList()
    m3.add(str(r1))
    resultsMap = HashMap()
    resultsMap.put("status", m3)
    resultsMap.put("models", m2)
    #del models
    return resultsMap