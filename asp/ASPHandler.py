#!/usr/bin/env python


import gringo
import sys
from java.util import ArrayList, HashMap
import locale


class ASPHandler:

#-Wno-atom-undefined

    models = []
    result = ''

    def __init__(self, aspFile, solveMode, task):
        #args = dict(x.split('=',1) for x in sys.argv[1:])
        # command-line args may be passed using the 'get_const'
        # method, see the incqueens example
        self.aspfile = aspFile
        self.solveMode = solveMode # should be '' if none is given

        
    def __on_model(self, model):
        self.models.append(model.atoms())
    
    """
    def __on_finish(self, result, canceled):
        print(result)
        return self.models
    """
    
    def solve(self):
        #import multiprocessing
        #cores = multiprocessing.cpu_count()
        # -t8: run in parallel using 8 threads
        #ctl = gringo.Control(['-Wno-atom-undefined','-t8']) if cores >= 8 else gringo.Control(['-Wno-atom-undefined'])
        #ctl = gringo.Control(['-Wno-atom-undefined'])
        locale.setlocale(locale.LC_ALL, 'C')
        ctl = gringo.Control(['-Wno-atom-undefined','-t8'])
        #ctl = gringo.Control(['-Wno-atom-undefined'])
        #ctl = gringo.Control()
        ctl.load(self.aspfile)
        ctl.conf.solve.models = 0 if self.solveMode in ["all","optN"] else self.solveMode
        if self.solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])])
        #f = ctl.solve_async(assumptions = None, on_model = self.__on_model, on_finish = self.__on_finish)
        #f.wait()
        self.result = ctl.solve(assumptions = None, on_model = self.__on_model)


def run(aspFile, solveMode, task):
    asp = ASPHandler(aspFile, solveMode, task)
    asp.solve()
    r1 = asp.result
    r2 = [y for y in [" ".join(map(lambda x: str(x),model)) for model in asp.models]]

    m2 = ArrayList()
    for x in r2:
        m2.add(x)

    m3 = ArrayList()
    m3.add(str(r1))
    resultsMap = HashMap()
    resultsMap.put("status", m3)
    resultsMap.put("models", m2)

    return resultsMap



"""
def run(aspFile, solveMode, task):
    m3 = ArrayList()
    m3.add('SAT')
    resultsMap = HashMap()
    m2 = ArrayList()
    resultsMap.put("status", m3)
    resultsMap.put("models", m2)
    return resultsMap
"""


"""
if __name__ == "__main__":
    asp = ASPHandler()
    asp.solve()
    print(asp.result)
    for x in [" ".join(map(lambda x: str(x),model)) for model in asp.models]: print(x)
    #run()
"""
