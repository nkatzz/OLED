# OLED: Online Learning of Event Definitions


``OLED`` is an online Inductive Logic Programming system for learning logical theories from data streams. ``OLED`` has been designed having in mind the construction of knowledge for event recognition applications, in particular, learning Event Calculus theories, but it can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure).

## Related paper

Please consult the paper that comes with the source (``iclp-2016.pdf``) to get a grasp on the theory behind ``OLED``.

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## installation

Clone the source to some local directory, let's refer to that by `/oledhome` in what follows for convenience. The following tools and software are necessary to use ``OLED``:

* Scala 2.11 & SBT with Java > 7 (preferably Java 8).
* Python 2.7.
* Clingo with Python, Lua and multithreading (TBB) support. To install:
  Download the Clingo source from [here](http://potassco.sourceforge.net/) (version 4.5.4 of the Clingo solver) and follow the instructions that come with the source (the INSTALL manual) to install. TBB is typically installed on Linux by `apt-get install libtbb-dev`. When you're done you should add the Clingo executable that will be generated to your `PATH` variable. The Clingo executable will be located in `/clingohome/build/release`, where `/clingohome` is the location where you unzipped the clingo source code.
* Mongodb (just follow the instructions from the Mongodb website to install).
* Jep (Java Embedded Python). To install:
  Clone the source from [here](https://github.com/mrj0/jep) to a location (let's refer to that by `/jephome` in what follows), and do `python setup.py install --home=/jephome`. Afterwards, add `/jephome/lib/python` to your `PYTHONPATH` variable and copy the generated jar file from  `/jephome/lib/python/jep/jep-x.x.x.jar` to `/oledhome/lib` (generate a `/lib` folder under `oledhome` if not present).

## Datasets

A dataset on which ``OLED`` has been evaluated on is the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. I'd be happy to offer assistance in using it with other datasets (formulate background knowledge, import the data in a mongodb etc). Please contact me at ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``. 

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

To see a description of available command-line parameters with `OLED` type:

```
...
```

#### The distributed version of `OLED`

Description & instructions coming soon.











