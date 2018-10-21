# OLED: Online Learning of Event Definitions


``OLED`` is an online ('single-pass') Inductive Logic Programming system for learning logical theories from data streams. It has been designed having in mind the construction of knowledge bases for event recognition applications, in the form of domain-specific axioms in the Event Calculus, i.e. rules that specify the conditions under which simple, low-level events initiate or terminate complex event. However, `OLED` can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure).

## Related paper/more info

Please consult the paper that comes with the source (``iclp-2016.pdf``) to get a grasp on the theory behind ``OLED``. The paper can also be downloaded from [here](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/online-learning-of-event-definitions/B1244B019AF03F6172DC92B57896544D)

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## Installation

The manual that comes with the source code contains detailed instructions. In sort:

* Clone of download the source code. Open a terminal and navigate to the source code location. In what follows we'll refer to this directory by `/oledhome`
* `cp -r OLED/install-scripts .`
* `cd install-scripts`
* `./install.sh`

Please update your `PATH`, `PYTHONPATH` and `LD_LIBRARY_PATH` as per the instructions in the manual. 

## Test Run

Detailed instructions on how to perform a test run with ``OLED`` are provided in the manual. Please refer to the manual for details on the data and the learning task we'll use for this test run. In sort:

* Install MongoDB.
* Download some data and some background knowledge: 
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-data.zip`
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-data.bk`
* `unzip caviar-data.zip`
* `unzip caviar-bk.zip`
* `cd caviar-data`
* Import the data into Mongo:
   * mongoimport --db caviar-train --collection examples --file caviar-train.json
   * mongoimport --db caviar-test --collection examples --file caviar-test.json
* Make sure everything is ok (after axecuting the `show dbs` command you should see the newly-created dbs 'caviar-train' and 'caviar test'):
   * `mongo`
   * `show dbs`
* Run ``OLED``. From within `/oledhome` do:
   * `java -cp oled.jar app.runners.OLEDDefaultRunner --inpath=/home/nkatz/oledhome/caviar-bk --delta=0.00001 --prune=0.8 --showrefs=false --target=meeting --db=caviar-train --saveto=/home/nkatz/oledhome/theory.lp`
   



## Datasets

A dataset on which ``OLED`` has been evaluated is the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. More datasets and instructions on how to try them out will be uploaded soon. In the meantime, I'd be happy to offer assistance in using `OLED` with other datasets (formulate background knowledge, import the data in a mongodb etc). Please contact me at ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``.

## Usage

#### Run on the CAVIAR dataset

A `json` dump (extracted from a mongodb) of the `CAVIAR` dataset (`caviar.json`) may be downloaded from [this link](http://users.iit.demokritos.gr/~nkatz/OLED-data/caviar.json.tar.gz). Also, a larger version of CAVIAR, synthetically augmented with 10 times more domain constants amy be found [here](http://users.iit.demokritos.gr/~nkatz/data/caviarx10.zip). To load the data to your local mongodb instance, unzip the downloaded file, cd into that folder and do ``mongoimport --db yourDBname --collection examples --file caviar.json``, where `yourDBname` is the name of your database (it can be anything, but do not change the name of the collection -- `examples`). Next, to run `OLED` with `CAVIAR` data, generate a jar file from the source code:

* Start SBT from within `/oledhome`
* Type `assembly`
* Move the generated file `/oledhome/target/scala-2.11/oled.jar` to `/oledhome`
* Run it as follows:

```
java -cp oled.jar:/lib/jep-x.x.x.jar -Djava.library.path=/jephome/lib/python/jep app.runners.OLEDDefaultRunner --inpath=/oledhome/datasets/Caviar/meeting --delta=0.00001 --prune=0.7 --db=yourDBname --wjep=true --chunksize=10 --hle=meeting
```

Type 


```
java -cp oled.jar app.runners.OLEDDefaultRunner --help 
```

to see the list of cmd args

## Distributed version of `OLED`

Description & instructions coming soon.











