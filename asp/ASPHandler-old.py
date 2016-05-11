#!/usr/bin/env python

# This is a backup of the old ASP handler
# (the new one is used along with JEPP)

import gringo
import sys

class ASPHandler:

    #-Wno-atom-undefined

    models = []
    result = ''

    def __init__(self):
        args = dict(x.split('=',1) for x in sys.argv[1:])
        # command-line args may be passed using the 'get_const'
        # method, see the incqueens example
        self.aspfile = args['aspfile']
        self.solveMode = args['solveMode'] if 'solveMode' in args else ''


    def __on_model(self, model):
        self.models.append(model.atoms())

    """
    def __on_finish(self, result, canceled):
        print(result)
        return self.models
    """

    def solve(self):
        # There is a very strange behaviour with parallel mode (-t): When running
        # experiments in parallel, there were cases when unnecessary use3 atoms
        # were included in an answer set. This happened only with -t on. The
        # unnecessary atoms lead to NoSuchElement exceptions (when trying to retrieve)
        # the corresponding literal from the map, or to wrong rules.
        # This happened only when running experiments in parallel, when running ILED in
        # a single core the -t option works fine and its ok to use it (it also speeds
        # up the application in cases that you'd have to wait for hours -- at best--
        # to get an answer).

        # ctl = gringo.Control(['-Wno-atom-undefined','-t8']) # -t8: run in parallel using 8 threads
        ctl = gringo.Control(['-Wno-atom-undefined'])
        #ctl = gringo.Control()
        ctl.load(self.aspfile)
        ctl.conf.solve.models = 0
        if self.solveMode == 'optN':
            ctl.conf.solve.opt_mode = 'optN'
        ctl.ground([("base", [])])
        #f = ctl.solve_async(assumptions = None, on_model = self.__on_model, on_finish = self.__on_finish)
        #f.wait()
        self.result = ctl.solve(assumptions = None, on_model = self.__on_model)

def run():
    asp = ASPHandler()
    asp.solve()
    r1 = asp.result
    r2 = [y for y in [" ".join(map(lambda x: str(x),model)) for model in asp.models]]
    return (r1,r2)

if __name__ == "__main__":
    asp = ASPHandler()
    asp.solve()
    print(asp.result)
    for x in [" ".join(map(lambda x: str(x),model)) for model in asp.models]: print(x)
    #run()

