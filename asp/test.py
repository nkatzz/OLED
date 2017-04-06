import multiprocessing

cores = multiprocessing.cpu_count()
_cores = '-t%d'%(cores)

print(_cores)