#!/usr/bin/env python

import gringo
import sys
sys.path.append('/home/nkatz/dev/OLED/iled/datasets/Fraud/py-utils')
#sys.path.append('/home/nkatz/dev/OLED/iled/datasets/Fraud/py-utils')
import functions
from java.util import ArrayList, HashMap
import locale



class ASPHandler:

    def __init__(self, aspFile, solveMode, task):
        #args = dict(x.split('=',1) for x in sys.argv[1:])
        # command-line args may be passed using the 'get_const'
        # method, see the incqueens example
        self.aspfile = aspFile
        self.solveMode = solveMode # should be '' if none is given
        self.task = task
        self.models = []
        self.result = ''
        self.grndPrgSize = 0
        self.solvingTime = 0

    def __on_model(self, model):
        self.models.append(model.atoms())
        #pass

    #def getCores(self):
    #    return multiprocessing.cpu_count()
    
    """
    def __on_finish(self, result, canceled):
        print(result)
        return self.models
    """
    
    def solve(self):
        #cores = '-t%d'%(self.getCores())
        locale.setlocale(locale.LC_ALL, 'C')
        ctl = gringo.Control(['-Wno-atom-undefined','-t4'])
        ctl.load(self.aspfile)
        ctl.conf.solve.models = 0 if self.solveMode in ["all","optN"] else self.solveMode
        if self.solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])], functions)
        #f = ctl.solve_async(assumptions = None, on_model = self.__on_model, on_finish = self.__on_finish)
        #f.wait()
        self.result = ctl.solve(assumptions = None, on_model = self.__on_model)

        #print(ctl.stats.keys())
        #for k in ctl.stats.keys():
        #    print(k,ctl.stats[k])

        if self.task == 'score_rules':
            self.solvingTime = ctl.stats['time_total']
            self.grndPrgSize = ctl.stats['lp']['atoms']
            #print(self.grndPrgSize)


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
    ## Get statistics for the ground program
    #if task == 'score_rules':
    #    m4 = ArrayList()
    #    m4.add(str(asp.grndPrgSize))
    #    resultsMap.put("grnd", m4)
    resultsMap.put("status", m3)
    resultsMap.put("models", m2)
    return resultsMap





#if __name__ == "__main__":
#    asp = ASPHandler()
#    asp.solve()
#    print(asp.result)
#    for x in [" ".join(map(lambda x: str(x),model)) for model in asp.models]: print(x)
#    #run()

