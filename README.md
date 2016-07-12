# OLED: Online Learning of Event Definitions


``OLED`` is an online Inductive Logic Programming system for learning logical theories from data streams. ``OLED`` has been designed having in mind the construction of knowledge for event recognition applications, in particular, learning Event Calculus theories, but it can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure). Please consult the paper that comes with the source (``oled.pdf``) to get a grasp on the theory behind ``OLED``.

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## installation

Clone the source to some local directory, let's call it `/oledhome`. The following is necessary to use ``OLED``:

* Scala 2.11 & SBT with Java > 7
* Python 2.7
* Clingo with Python and multithreading (TBB) support. To install:
  Download the Clingo source from [here](http://potassco.sourceforge.net/) and follow the instructions that come with the source to install. Aftewards you should add the clingo executable that will be generated to your `PATH` variable. The path to the executable is: `/clingohome/build/release`, where `/clingohome` is the location where you unzipped the clingo source code.
* Mongodb
* Jep (Java Embedded Python). To install:
  Clone the source from [here](https://github.com/mrj0/jep) to a location (call that `/jephome`), and do `python setup.py install --home=/jephome`. Afterwards, add `/jephome/lib/python` to your `PYTHONPATH` variable and copy the generated jar file from  `/jephome/lib/python/jep/jep-x.x.x.jar` to `/oledhome/lib`.

## Datasets

``OLED`` has been evaluated on the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. A more generic version for using it with your own datasets will be uploaded here soon. In the meantime, I'd be happy to offer assistance in using it with other datasets (please contact ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``). 

## Usage

#### Run on the CAVIAR dataset

A `json` dump (extracted from a mongodb) of the `CAVIAR` dataset (`caviar.json`) may be found in the `\data` folder. To load the dataset in your local mongodb instance do ``mongoimport --db yourDBname --collection examples --file caviar.json`` where `yourDBname`. Next, to run `OLED` with `CAVIAR` data, generate a jar file from the source code:

* Edit the `build.sbt` making sure that the corresponding lines look as follows:
  * `jarName in assembly := "oled.jar"`
  * `mainClass in (Compile, packageBin) := Some("all.core.oled.Runner")`
* Start SBT from within `\oledhome`
* Type `assembly`
* Move the generated file `/oledhome/target/scala-2.11/oled.jar` to `\oledhome`
* Run ir as follows:

`java -cp oled.jar:/lib/jep-x.x.x.jar -Djava.library.path=/jephome/lib/python/jep all.core.oled.Runner <command line args>`

where `<command line args>` is the following list of runtime parameters for `OLED`: 

```
Input parameters:
-------------------
db=dbname: (mandatory) 'dbname' is a name of a mongodb to learn from.
target=event: (mandatory) 'event' is the name of a target complex event (either meeting or moving).
dnum=integer: (mandatory). 'integer' is the number of a training set (the code contains 10 pairs of training/test sets of the CAVIAR dataset -- see  all.core.oled.MeetingTrainingData).
delta=double: (optional, default is 10^-5). 'double' is the δ parameter (significance) for the Hoeffding bound.
prune=double: (optional, default is 0.0). 'double' is a clause quality threshold (clauses with score lower than that will be pruned.)
nmin=integer: (optional, default is 1000). 'integer' is a minimum number of examples on which each clause must be evaluated.
sdepth=integer: (optional, default is 1). 'integer' is the specialization depth.
```


#### Run on a Fragment of CAVIAR

A fragment of the CAVIAR dataset used for experiments in 

* Skarlatidis A., Paliouras G., Artikis A., and Vouros G. Probabilistic Event Calculus for Event Recognition, ACM Transactions on Computational Logic, 16(2):1-37, 2015.

This dataset has been used for evaluating ``OLED``, see the paper that comes with source (``oled.pdf``) for details. Follow [this link](http://users.iit.demokritos.gr/~nkatz/OLED-data/CAVIAR_MLN) to download the data.











