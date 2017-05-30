# OLED: Online Learning of Event Definitions


``OLED`` is an online ('single-pass') Inductive Logic Programming system for learning logical theories from data streams. It has been designed having in mind the construction of knowledge bases for event recognition applications, in the form of domain-specific axioms in the Event Calculus, i.e. rules that specify the conditions under which simple, low-level events initiate or terminate complex event. However, `OLED` can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure).

## Related paper/more info

Please consult the paper that comes with the source (``iclp-2016.pdf``) to get a grasp on the theory behind ``OLED``. The paper can also be downloaded from [here](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/online-learning-of-event-definitions/B1244B019AF03F6172DC92B57896544D)

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## installation

Clone the source to some local directory, let's refer to that by `/oledhome` in what follows for convenience. The following tools and software are necessary to use ``OLED``:

* Scala 2.11 & SBT with Java > 7 (preferably Java 8).
* Python 2.7.
* Clingo with Python, Lua and multithreading (TBB) support. To install:
  Download the Clingo source from [here](https://sourceforge.net/projects/potassco/files/clingo/) (**version 4.5.4 of the Clingo solver**, source code, e.g. `clingo-4.5.4-source.tar.gz`) and follow the instructions that come with the source (the INSTALL manual) to install. TBB is typically installed on Linux by `apt-get install libtbb-dev`. When you're done you should add the Clingo executable that will be generated to your `PATH` variable. The Clingo executable will be located in `/clingohome/build/release`, where `/clingohome` is the location where you unzipped the clingo source code.
* Mongodb (just follow the instructions from the Mongodb website to install).
* Jep (Java Embedded Python). To install:
  Clone the source from [here](https://github.com/mrj0/jep) to a location (let's refer to that by `/jephome` in what follows), and do `python setup.py install --home=/jephome`. Afterwards, add `/jephome/lib/python` to your `PYTHONPATH` variable and copy the generated jar file from  `/jephome/lib/python/jep/jep-x.x.x.jar` to `/oledhome/lib` (generate a `/lib` folder under `oledhome` if not present).
* When you're done, download [this patch](http://users.iit.demokritos.gr/~nkatz/clingo-patch/solve-multi.patch.0), put it inside `clingohome` (the folder where where you unzipped the clingo source code) and run it as follows:
```
patch -p0 < solve-multi.patch.0
```
from within `clingohome`. Then recompile the python module:
```
scons --build-dir=release pyclingo
```

## Datasets

A dataset on which ``OLED`` has been evaluated is the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. More datasets and instructions on how to try them out will be uploaded soon. In the meantime, I'd be happy to offer assistance in using `OLED` with other datasets (formulate background knowledge, import the data in a mongodb etc). Please contact me at ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``.

## Usage

#### Run on the CAVIAR dataset

A `json` dump (extracted from a mongodb) of the `CAVIAR` dataset (`caviar.json`) may be downloaded from [this link](http://users.iit.demokritos.gr/~nkatz/OLED-data/caviar.json.tar.gz). To load the dataset to your local mongodb instance, unzip the downloaded file, cd into that folder and do ``mongoimport --db yourDBname --collection examples --file caviar.json``, where `yourDBname` is the name of your database (it can be anything, but do not change the name of the collection -- `examples`). Next, to run `OLED` with `CAVIAR` data, generate a jar file from the source code:

* Start SBT from within `/oledhome`
* Type `assembly`
* Move the generated file `/oledhome/target/scala-2.11/oled.jar` to `/oledhome`
* Run it as follows:

```
java -cp oled.jar:/lib/jep-x.x.x.jar -Djava.library.path=/jephome/lib/python/jep app.OLEDRunner inpath=/oledhome/datasets/Caviar/meeting delta=0.00001 prune=0.7 db=yourDBname wjep=true chunksize=10 hle=meeting
```

A list of available command line args follows :

```
Input parameters:
-------------------
inpath=<path>: (mandatory) 'path' The path to a particular application (containing the background knowledge and mode declarations).
db=<dbname>: (mandatory) 'dbname' is a name of a mongodb that stores the data to learn from.
hle=<event>: (mandatory) 'event' is the name of a target complex event (e.g. either meeting or moving for the CAVIAR experiments).
delta=<double>: (optional, default is 10^-5). 'double' is the δ parameter for the Hoeffding bound test.
prune=<double>: (optional, default is 0.0). 'double' is a rule quality threshold (rules with score lower than that will be pruned.)
minseen=<integer>: (optional, default is 1000). 'integer' is a minimum number of examples on which each clause must be evaluated.
spdepth=<integer>: (optional, default is 1). 'integer' is the specialization depth parameter (see the the iclp-2016.pdf paper that accompanies the source).
repfor=<integer>: (optional, default is 1). Train by passing several times ('integer'-many) over the data.
wjep=<true|false>: (optional, default is false). Use/not use Jep to communicate with the Clingo solver.
chunksize=<integer>: (optional, default is false). Pass the data in chunks of size 'integer'.
ties=<double>: (optional, default is 0.005). Tie-breaking threshold.
evalth=<true|false>: (optional, default is false). If false. learn from the data, else evaluate an existing theory on the data.
```

## Distributed version of `OLED`

Description & instructions coming soon.











