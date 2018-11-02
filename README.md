# OLED: Online Learning of Event Definitions


``OLED`` is an online ('single-pass') Inductive Logic Programming system for learning logical theories from data streams. It has been designed having in mind the construction of knowledge bases for event recognition applications, in the form of domain-specific axioms in the Event Calculus, i.e. rules that specify the conditions under which simple, low-level events initiate or terminate complex event. However, `OLED` can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure).

## Related paper/more info

Please consult the paper that comes with the source (``OLED/manual/paper``) to get a grasp on the theory behind ``OLED``. The paper can also be downloaded from [here](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/online-learning-of-event-definitions/B1244B019AF03F6172DC92B57896544D). 

## License

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## Installation

You'll need to have Scala (> 2.11) with SBT (Scala Build Tool), Java 8, and Python 2.7 installed on your machine.

* Clone or download the source code:
  * `git clone https://github.com/nkatzz/OLED.git`
* `cd OLED/install-scripts`
* `./install.sh`
* Update your `PATH`, `PYTHONPATH` and `LD_LIBRARY_PATH`. Append the following lines to your `~/.profile` (or `~/.bashrc`) file:
  * `<PATH TO CLONED SOURCE>/OLED/external_dependencies/lpsolve55:$LD_LIBRARY_PATH`  
  * `export LD_LIBRARY_PATH`
  * `export PATH=$PATH:<PATH TO CLONED SOURCE>/OLED/external_dependencies/LoMRF/target/universal/LoMRF-0.5.5-SNAPSHOT/bin:<PATH TO CLONED SOURCE>/OLED/external_dependencies/clingo/clingo-4.5.4-source/build/release`
  * `export PYTHONPATH=$PYTHONPATH:<PATH TO CLONED SOURCE>/OLED/external_dependencies/clingo/clingo-4.5.4-source/build/release/python`

## Test Run

Detailed instructions on how to perform a test run with ``OLED`` are provided in the manual. Please refer to the manual for details on the data and the learning task we'll use for this test run. In sort:

* Install MongoDB.
* Download some data and some background knowledge at some location: 
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-data.zip`
   * `wget http://users.iit.demokritos.gr/~nkatz/oled/caviar-bk.zip`
* `unzip caviar-data.zip`
* `unzip caviar-bk.zip`
* `cd caviar-data`
* Import the data into Mongo:
   * `mongoimport --db caviar-train --collection examples --file caviar-train.json`
   * `mongoimport --db caviar-test --collection examples --file caviar-test.json`
* Make sure everything is ok (after axecuting the `show dbs` command you should see the newly-created dbs 'caviar-train' and 'caviar test'):
   * `mongo`
   * `show dbs`
* Run ``OLED``:
   <!--
   * `java -cp oled.jar app.runners.OLEDDefaultRunner \`  <br/>
     ` --inpath=/oledhome/caviar-bk \` <br/>
     `--delta=0.00001 \` <br/>
     `--prune=0.8 \` <br/>
     `--target=meeting \` <br/>
     `--train=caviar-train \` <br/>
     `--saveto=/oledhome/theory.lp`
   -->
   * `java -cp oled-0.1.jar app.runners.OLEDDefaultRunner --inpath=<PATH TO THE caviar-bk FOLDER> --delta=0.00001 --prune=0.8 --db=caviar-train --saveto=<PATH TO SOME LOCATION>/theory.lp`
* After learning terminates the learnt hypothesis will be written in `theory.lp`. You can evaluate this theory on the test set as follows:   
   <!--
   * `java -cp oled.jar app.runners.OLEDDefaultRunner \`  <br/>
         ` --inpath=/oledhome/caviar-bk \` <br/>
         `--target=meeting \` <br/>
         `--test=caviar-test \` <br/>
         `--evalth=/home/nkatz/oledhome/theory.lp`
   -->
   * `java -cp oled-0.1.jar app.runners.OLEDDefaultRunner --inpath=<PATH TO THE caviar-bk FOLDER> --db=caviar-test --evalth=--inpath=<PATH TO theory.lp FILE>` 
* You may see all available cmd args with `java -cp oled-0.1.jar -help`

## Datasets

A dataset on which ``OLED`` has been evaluated is the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition (which was also used for the test run above). See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. More datasets and instructions on how to try them out will be uploaded soon. In the meantime, I'd be happy to offer assistance in using `OLED` with other datasets (formulate background knowledge, import the data in a mongodb etc). Please contact me at ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``.

## Online Learning on Markov Logic Rules with `OLED`

Instructions coming soon. In the meantime, please read the related paper:

* Katzouris N., Michelioudakis E., Artikis A. and Paliouras Georgios, [Online Learning of Weighted Relational Rules for Complex Event Recognition](http://www.ecmlpkdd2018.org/wp-content/uploads/2018/09/154.pdf), ECML-PKDD 2018.

## Parallel/Distributed version of `OLED`

Instructions coming soon. In the meantime, please read the related paper:

* Katzouris N., Artikis A. and Paliouras Georgios, [Parallel Online Learning of Event Definitions](https://link.springer.com/chapter/10.1007/978-3-319-78090-0_6), ILP 2017.











