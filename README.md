kconfigreader
===========

[![Build Status](https://travis-ci.org/ckaestne/kconfigreader.svg?branch=master)](https://travis-ci.org/ckaestne/kconfigreader)
[![Coverage](https://coveralls.io/repos/ckaestne/kconfigreader/badge.png?branch=master)](https://coveralls.io/github/ckaestne/kconfigreader)

tooling to read kconfig files and convert them into 
formulas for further reasoning (primarily for the
TypeChef infrastructure, but may be used elsewhere).

Build with `sbt`


To extract the raw data from kconfig files, this tool
relies on a utility `dumpconf`, inspired by Untertaker, that
builds on on top of Linux's kconfig infrastructure to dump
the internal representation in XML format. The file
`dumpconf/dumpconf.c` must be compiled against the 
Linux source tree that is to be analyzed.
A binary version compiled on ubuntu 12.04 for Linux 2.3.33.6 
is available in the `binary` folder for convenience.


Instrunctions
=====


With `sbt mkrun` you can create a `run.sh` file that configures
all dependencies correctly.

To extract information from a model run `./run.sh de.fosd.typechef.kconfig.KConfigReader [kconfig] [out]`
where `kconfig` refers to the kconfig file that should be analyzed and `out` points to the base name of the
files that should be written (the tool will create multiple output files with
different extensions).

By default, the tool will create two files: `out.rsf` contains the output of `dumpconf`, a raw
dump of the kconfig information in a intermediate format and `out.model` contains the boolean constraints
sorted by the feature they belong to.

The feature names in this file have the following encoding: Feature names in quotes occur undefined
in the kconfig model and are hence dead. A tristate feature X is represented by two variables X and
X_MODULE that are mutually exclusive, just as used in Linux. Nonboolean options are represented by
multiple variables, one for each value explicitly mentioned in the kconfig files (X=n means the variable
is deactived, X=1 means it has value 1, and so on).

Additional options:

  * `--fast` skips the consistency check. This speeds up extraction significantly and uses
    much less memory, since the SAT solver is not involved. May silently produce unsatisfiable
    models though. Omit `--fast` for checking consistency and debugging inconsistent models.

  * `--dumpconf [file]` provide the path to the dumpconf tool to be called from within
    this tool

  * `--writeDimacs` writes a `out.dimacs` file that can be used with any SAT solver or
    directly as TypeChef feature model (e.g. through `FeatureExprFactory.dflt.featureModelFactory.createFromDimacsFile`).
    Comments in the beginning of the dimacs file provide a mapping to the option names (including variables
    for nonboolean options).
    The dimacs file contains additional variables to avoid explosion of the transformation into CNF
    (the transformation is equisatisfiable, but not equivalent; and equivalent transformation into CNF
    is possible for small models, but not for the larger constraints in Linux; changes are easily possible
    in the source code by changing the parameter to `DimacsWriter.writeAsDimacs2`).

  * `--writeCompletedConf` writes `out.completed.h` and `out.open` files. It checks for every option
    whether it can be activated. If it is activated in all configurations, it is defined as macro in
    the .h file, if it is deactivated in all configurations, it is undefined in the .h file. If it is
    activated in some and deactivated in other valid configurations, it is included in the .open file.
    Those files are used as input for TypeChef to reduce the search space (using TypeChef's `--include`
    and `--openFeat` parameters). Since it requires two SAT calls for every option, it is expensive to compute.
    It requires to write a .dimacs file for reasonable performance.


  * `--writeNonBoolean` writes a `out.nonbool.h` file that defines all nonboolean options to
     their defaults using #define directives. Additionally, #ifdef directives are used if
     different defaults are defined in different configurations or defaults are not available
     for all configurations.

  * `--reduceConstraints` eliminates all redundant constraints (i.e., constraints that are
     implied already by previous constraints) before writing the .dimacs file. This is a very expensive
     operation, that however can reduce the size of the .dimacs file by a few percent.




For an example of how to use this, see `genFMs.sh` in https://github.com/ckaestne/TypeChef-LinuxAnalysis


Comments and Limitations
=====

Tristate and boolean options (with prompts and without) are accurately handled as far
as we know. If you find a mistake, please provide a small kconfig file as test case
where our extraction differs from the default kconfig behavior (the test infrastructure
tests all combinations of those files in a brute-force fashion, there are many examples
of such files in the test directory).

Select statements (and depends) are potentially order dependent and may trigger kconfig to produce
otherwise invalid configurations. Kconfig issues a warning when this happens and it is rather unlikely.
Our tool does not model this extreme behavior correctly. It would be worth writing an extension
which detects potential issues to report them for to the Linux maintainers (as they try to avoid these
cases as well).

The precise handling of tristate and nonprompt options often lead to large constraints. This is
unavoidable unless imprecise approximations are desired. (Those could be added on top of our
infrastructure easily).

A different behavior for nonboolean options would be possible. Here a finite abstraction of
an infinite domain is necessary. The current encoding represents precisely what is enforced
within kconfig, but limited to the values mentioned in the kconfig file as defaults or
in constraints. A different behavior would be possibly by changing the implementation.

Range expressions currently may not depend on other configuration values, but only on constants.
There is only a single case in Linux-x86 where range expressions are ignored due to this limitation.

We currently create constraints for each option separately. Select statements are listed under the
selected statement, not the selecting statement. It would be an straightforward extension to additionally distinguish
the kind of constraints further and maintain traceability information back to the .rsf file, if desired.

The MODULES option (if used in the model) must be named MODULES. It is matched by name, not
by the additional Kconfig attribute. (could be changed by modifying both this tool and dumpconf)


Testing
====

The semantics of Kconfig are nontrivial, not only with regard to tristate options, but also
with regard to nonboolean functions, items and choices with and without prompts, and so forth.

We invested significant effort in a testing infrastructure, to ensure that the Kconfig behavior
is correctly captured. The idea is to use a differential testing approach. We use kconfig
itself (more precisely the tool `conf --olddefconfig`) to check whether a specific
configuration is valid. This tool will modify values in an invalid configuration to a valid
one; that is, it provides a means to establish ground truth which configurations are valid.

On small kconfig models we apply a brute-force strategy where we execute kconfig on all
possible valid and invalid configurations (or a sampled subset of values for nonboolean options). We
compare for every configuration whether our propositional abstraction yields the same
result as kconfig.

On large models as the Linux kernel model, a brute-force strategy obviously does not work.
Instead, we can provide a partial configuration or a subset of configuration options that
should be explored in a brute-force way. Our testing infrastructure completes the configuration
with the abstracted model and checks whether the configuration validity agrees between our
abstraction and the kconfig behavior.

Note that this requires a releatively recent version of Kconfig's conf tool that is part of
the Linux kernel in which the option `--olddefconfig` is available. (after 2011)

For all bug reports, please provide a failing test case in terms of a small kconfig file
where the brute-force analysis finds differences between the kconfig behavior and our
abstraction.


Notes
====

tristate to CONFIG_x translation:

  ```
  x=y
  => #define CONFIG_x
  => #undef CONFIG_x_MODULE

  x=m
  => #undef CONFIG_x
  => #define CONFIG_x_MODULE

  x=n
  => #undef CONFIG_x
  => #undef CONFIG_x_MODULE
  ```


Credits and Support
====

This tool was developed by Christian Kaestner at Carnegie Mellon University. Please
contact him in terms of questions.


It builds on the dumpconf infrastructure of the Undertaker project and reuses some test cases
from that project. It was inspired by Undertaker and the LVAT infrastructure (which is
unfortunately no longer maintained).
