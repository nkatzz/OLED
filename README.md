# ILED: Online Learning of Event Definitions


``OLED`` is an online Inductive Logic Programming system for learning logical theories from data streams. ``OLED`` has been designed having in mind the construction of knowledge for event recognition applications, in particular, learning Event Calculus theories, but it can practically be used within any domain where ILP is applicable (preferably, large volumes of sequential data with a time-like structure). Please consult the paper that comes with the source (oled.pdf) to get a grasp on the theory behind ``OLED``.

## Licence

This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under certain conditions; See the GNU General Public License v3 for more details.

## installation

Clone the source to some local directory. From now one we'll refer to this directory as `/oled`. The following is necessary to use ``OLED``:

* Scala 2.11 & SBT with Java > 7
* Python 2.7
* Clingo with Python and multithreading (TBB) support. Follow the instructions from [here](http://potassco.sourceforge.net/) to install.
* Mongodb
* Jep (Java Embedded Python). To install, clone the source from [here](https://github.com/mrj0/jep), and do `python setup.py install --home=</your/path>`. Afterwards, add `/your/path/lib/python` to your PYTHONPATH variable and copy the generated jar file from `/your/path/lib/python/jep/jep-x.x.x.jar` to `/oled/lib`.

## Usage

``OLED`` has been evaluated on the [CAVIAR dataset](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for activity recognition. A more generic version for using it with your own datasets will be uploaded here soon. In the meantime, I'd be happy to offer assistance in using it with other datasets (please contact ``nkatz`` ``at`` ``iit`` ``dot`` ``demokritos`` ``dot`` ``gr``).

#### Datasets

test

###### CAVIAR Fragment

test

###### Whole CAVIAR

test



#### Activity Recognition (CAVIAR)

See [this link](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. You can also find more information on the dataset itself and the way it has been used for learning with ``ILED`` in [ILED's technical report](http://arxiv.org/pdf/1402.5988v2.pdf). You can download data plus instructions on how to perform learning with ``ILED`` from [this link](http://users.iit.demokritos.gr/~nkatz/ILED-data/CAVIAR.tar.gz).







