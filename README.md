# OLED: Online Learning of Event Definitions


``OLED`` is an online Inductive Logic Programming system for learning logical theories from data streams. ``OLED`` has been designed having in mind the construction of knowledge for event recognition applications, in particular, learning Event Calculus theories, but it can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure). Please consult the paper that comes with the source (``oled.pdf``) to get a grasp on the theory behind ``OLED``.

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## installation

Clone the source to some local directory. From now one we'll refer to this directory as `/oled`. The following is necessary to use ``OLED``:

* Scala 2.11 & SBT with Java > 7
* Python 2.7
* Clingo with Python and multithreading (TBB) support. Follow the instructions from [here](http://potassco.sourceforge.net/) to install.
* Mongodb
* Jep (Java Embedded Python). To install, clone the source from [here](https://github.com/mrj0/jep), and do `python setup.py install --home=</your/path>`. Afterwards, add `/your/path/lib/python` to your PYTHONPATH variable and copy the generated jar file from `/your/path/lib/python/jep/jep-x.x.x.jar` to `/oled/lib`.

## Datasets

``OLED`` has been evaluated on the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. See [here](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. A more generic version for using it with your own datasets will be uploaded here soon. In the meantime, I'd be happy to offer assistance in using it with other datasets (please contact ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``).

Links to download the CAVIAR dataset and instructions on how to use it with with ``OLED`` follow. 

###### CAVIAR Fragment

A fragment of the CAVIAR dataset used for experiments in 

* Skarlatidis A., Paliouras G., Artikis A., and Vouros G. Probabilistic Event Calculus for Event Recognition, ACM Transactions on Computational Logic, 16(2):1-37, 2015.

This dataset has been used for evaluating ``OLED``, see the paper that comes with source (``oled.pdf``) for details. Follow [this link](http://users.iit.demokritos.gr/~nkatz/OLED-data/CAVIAR_MLN) to download the data.

###### Whole CAVIAR

The entire CAVIAR dataset, also used for evaluating ``OLED`` -- see the paper that comes with source (``oled.pdf``) for details, is available from [this link](http://users.iit.demokritos.gr/~nkatz/OLED-data/caviar.json) in the form of a mongodb json dumb. To load the dataset in your local mongodb instance do ``mongoimport --db yourDBname --collection examples --file caviar.json``









