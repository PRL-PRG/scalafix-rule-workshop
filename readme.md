# implicit-analyzer: Tools for examining the use of implicits in large Scala codebases

This repository is a toolbelt to analyze the uses of implicits in Scala projects. It features utilites to:
- Extract and store information about the uses of implicits in a project, using our analyzer built on top of [`SemanticDB`]() and [`scala-reflect`]().
- Extract and store information about the uses of implicits in thousands of projects, in parallel, using our extraction pipeline.
- Make predicate-style queries to that data, using our representation schema and query DSL (under construction).

This toolbelt is being developed as part of a larger project, and therefore we haven't had the time to make it stage-ready yet.
However, we keep actively maintaining it and we expect an official release sooner rather than later. 

In the meantime, here is a short tutorial.

## Short tutorial

*NOTE:* As mentioned above, this toolbelt is quite far from stable. In fact, we would very much like to revamp it to include only Scala code, as opposed to the Python pipeline currently in place. Enter at your own peril.

### 1: Install the system dependencies

We assume that you have Scala tools like `sbt` installed.
The pipeline still relies on Python, so you'll have to download a few dependencies:

#### Unix (Debian, Ubuntu)

  1. Install dependency managers:  `$ sudo apt-get install python python-pip`

  2. Install Python dependencies:  `$ sudo pip install fabric termcolor`

### 2: Download the pipeline file

Next, you need to download the Python scipt that will make everything work, and place it at the root of your workspace as with the name `fabfile.py`.

  `$ cd /path/to/workbench`

  `$ curl https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/master/scripts/run.py > fabfile.py`

### 3: Follow the instructions

Now we will refer you to the guide that we use internally for people that come into the project. [The guide](https://raw.githubusercontent.com/PRL-PRG/scalafix-rule-workshop/master/scripts/run.py).
