from gringo import Fun

"""
This is a workaround to get globals if they are deleted by shutting down jep
(this is not a problem anymore).

builtins = [x for x in (1).__class__.__base__.__subclasses__() if x.__name__ == 'catch_warnings'][0]()._module.__builtins__
__import__ = builtins['__import__']
float = builtins['float']
int = builtins['int']
gringo = __import__('gringo')
Fun = gringo.Fun
"""

def quantize_amount(amount):
    x = float(amount)
    if x < 1:
        return Fun('tiny_amount')
    elif x < 50:
        return Fun('very_small_amount')
    elif x < 100:
        return Fun('small_amount')
    elif x < 200:
        return Fun('medium_amount')
    elif x < 300:
        return Fun('high_amount')
    elif x < 400:
        return Fun('very_high_amount')
    elif x < 200000:
        return Fun('massive_amount')


def predicate_quantize_amount(amount):
    x = float(amount)
    if x < 1:
        return Fun( 'tiny_amount(%s)' % (amount) )
    elif x < 50:
        return Fun('very_small_amount(%s)' % (amount))
    elif x < 100:
        return Fun('small_amount(%s)' % (amount) )
    elif x < 200:
        return Fun('medium_amount(%s)' % (amount))
    elif x < 300:
        return Fun('high_amount(%s)' % (amount))
    elif x < 400:
        return Fun('very_high_amount(%s)' % (amount))
    elif x < 200000:
        return Fun('massive_amount(%s)' % (amount))


def less_than(x,y):
    return float(x) < float(y)

def greater_than(x,y):
    return float(x) > float(y)

def equal(x,y):
    return float(x) == float(y)

def less_than_int(x,y):
    return int(x) < int(y)

def greater_than_int(x,y):
    return int(x) > int(y)

def less_than_int_or_eq(x,y):
    return int(x) <= int(y)

def compare_win_length(start, end, span):
    x = int(start)
    y = int(end)
    z = (y - x)/60000.0
    return int(z) <= int(span)