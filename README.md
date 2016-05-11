# ILED: Online Learning of Event Definitions


``OLED`` is an online Inductive Logic Programming system for learning logical theories from streams of sequential data with a time-like structure. ``OLED`` has been designed having in mind the construction of knowledge for event recognition applications, in particular, learning Event Calculus theories, but it can practically be used within any domain where ILP is applicable. It is implemented in the Scala programming language and uses an ASP solver (``Clingo``) as the main reasoning Component. Please consult the paper that comes with the source (oled.pdf) to get a grasp on the theory behind ``OLED``. For information on Answer Set Programming (ASP) and the input language of ``OLED``, please consult the material from the [Potsdam Answer Set Solving Team](http://potassco.sourceforge.net/).

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

To use ``ILED`` you need to provide the following with the input:

1) Some background knowledge in ASP format. To do so, edit the `/knowledge/bk.lp` file with your own input. The background knowledge may consist of ground facts, as well as non-ground rules that model some domain knowledge.

2) A set of training examples. To do so, edit the `/knowledge/examples.pl`. Examples must be provided in the form of ground atoms, wrapped inside an `example` predicate. For instance `example(father(john,peter)).` Examples inside the ``example`` predicate are positive examples. It should be noted that ILED makes a Closed World Assumption. This means that if a predicate ``p`` is used for the representation of the examples, then each ground instantion of ``p`` that is not wrapped inside ``example`` predicate is considered as negative example during learning. Operating on an open world (where negative examples will have to be explicitly declared) will be supported in a future version of ``ILED``.

3) A set of mode declarations. To do so, edit the `/knowledge/modes.pl` file with your own input. Mode declarations are a widely used language bias in ILP, specifying the syntax of target rules. ``ILED`` uses mode declarations in the same way as well-known ILP systems like [``ALEPH``](http://www.cs.ox.ac.uk/activities/machlearn/Aleph/aleph.html) and [``PROGOL``](http://www.doc.ic.ac.uk/~shm/Papers/InvEnt.pdf) do. Please consult these resources, as well as the the reference papers mentioned above for more information on how to define and use mode declarations for your own learning tasks.

4) A set of example patterns: Unlike most ILP systems, ``ILED`` can learn definitions for target predicates that are different from the predicates used to record the training examples. Such a learning setting is called non-Onservational Predicate Learning (non-OPL) in ILP terminology and handling it is useful in general, but also necessary in some cases. For example, when learning Event Calculus programs, the examples (observartions in time) are recorded using a ``holdsAt`` predicate, while one often is interested in learning conditions under which events are initiated or terminated by the occurrence of other events. Thus target predicates are ``initiatedAt`` and ``terminatedAt`` predicates (the basics of the Event Calculus are covered in [ILED's technical report](http://arxiv.org/pdf/1402.5988v2.pdf)).

Given the general non-OPL setting that ``ILED`` assumes, the predicate structure used for representing the examples must also be provided with the input. These predicates may be more than one and they are "declared" as abstractions of actual examples, i.e. non-ground logical atoms. These atoms are declared in the mode declarations file, wrapped in a `examplePattern` predicate. For instance if the examples are of the form `example(father(john,peter))` then add the following atom in the mode declarations file: `examplePattern(father(X,Y))`. 

Once you have prepared the input, you can run the /src/main.py script, indicating that you want to perform "batch" learning, i.e. learn from all the examples at once:

`/src$ python main.py mode=batch`

The output hypothesis is written in ``/theory.lp``. This is a simple way to use ``ILED`` on toy examples or small domains. Actually, when run in batch mode ``ILED`` is an implementation of the ``XHAIL`` algorithm (`eXtended Hybrid Abductive Inductive Learning` -- see the second paper mentioned above for details). The directory ``/knowledge/ecoli`` contains background knowledge, mode declarations and training examples for running ILED in batch mode on an example provided in 

* O. Ray. Nonmonotonic Abductive Inductive Learning. Journal of Applied Logic 7(3): 329-340, 2009.

related to the metabolism of the bacterium E. coli. Please consult the above paper for details on this case study. To run the example:

1) Copy the contents of ``/knowledge/ecoli/bk.lp``, ``/knowledge/ecoli/modes.pl`` and ``/knowledge/ecoli/examples.lp`` into the corresponding files under the ``/knowledge`` directory of ``ILED``'s source code.

2) Add the following to modes.pl (if not there): examplePattern(holdsAt(available(S),T)).
 
3) Run `/src$ python main.py mode=batch`. 

## Using ILED (incremental mode)

``ILED`` has been designed for scalable learning from large volumes of data, in cases where processing all data at once is intractable, or where data arrive over time. It thus extends the ``XHAIL`` functionality and it operates also in an incremental mode, constructing a hypothesis from the first available piece of data, and revising it as long as new data arrives, while revisions account for the whole set of accumulated expirience. To implement this functionality, ``ILED`` relies on an external database (``mongodb``) the serves as a historical memory. So you can set up your data storage in a ``mongodb`` instance and run ``ILED`` to learn incrementally.

The best way to see how this works is to run ``ILED`` on the data provided here. ``ILED`` has been evaluated on two large-scale event recognition applications: Activity Recognition and City Transport Management. For both these domains, data and necessary knowledge (background knowledge and mode declarations) are provided. To use ``ILED`` incrementally on your own datasets, you will have to import your data in a ``mongodb`` instance. Hopefully, the instructions provided in the links below should be sufficient for that. Please contact me if you try to use ``ILED`` (either in batch or incremental mode) and face difficulties. 

#### City Transport Management (CTM)

The CTM dataset was created in the context of the [PRONTO project](http://www.ict-pronto.org/). For information of this dataset, please consult [ILED's technical report](http://arxiv.org/pdf/1402.5988v2.pdf). You can download the dataset, along with information on how to set-up ``mongodb``, import the data from the provided JSON dumps and run ``ILED`` from [this link](http://users.iit.demokritos.gr/~nkatz/ILED-data/CTM.tar.gz).

#### Activity Recognition (CAVIAR)

See [this link](http://homepages.inf.ed.ac.uk/rbf/CAVIARDATA1/) for information on the CAVIAR dataset. You can also find more information on the dataset itself and the way it has been used for learning with ``ILED`` in [ILED's technical report](http://arxiv.org/pdf/1402.5988v2.pdf). You can download data plus instructions on how to perform learning with ``ILED`` from [this link](http://users.iit.demokritos.gr/~nkatz/ILED-data/CAVIAR.tar.gz).




## Related Software

* [RTEC](https://github.com/aartikis/RTEC): Run-Time Event Calculus. RTEC is an extension of the Event Calculus. It is a highly-scalable logic programming language for representing and reasoning about events and their effects.

* [LoMRF](https://github.com/anskarl/LoMRF): Logical Markov Random Fields: LoMRF is a library for Markov Logic Networks that supports Event Calculus reasoning under uncertainty.


