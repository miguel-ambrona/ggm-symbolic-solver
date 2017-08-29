# GGM Symbolic Solver

This solver allows to analyze symbolic experiments to prove security of cryptographic constructions in the generic group model.

## Installation

*1*. Install [Opam](https://opam.ocaml.org/).

 * In Ubuntu,

~~~~~
apt-get install -y ocaml ocaml-native-compilers opam libtool libtool-bin libgmp-dev libffi-dev m4 libz-dev libssl-dev camlp4-extra
~~~~~

 * In OS X, use homebrew,

~~~~~
brew install opam
~~~~~

*2*. Install the right compiler and the right libraries (replace DIRECTORY_TO_THE_CODE by the actual
path to the folder where you downloaded the code).

~~~~~
opam pin add symbolic-solver DIRECTORY_TO_THE_CODE -n
opam install symbolic-solver --deps-only
~~~~~

*3*. Set the path variable:

~~~~~
export GGM_PATH=DIRECTORY_TO_THE_CODE
~~~~~

You may want to add the above line to your .bashrc file.


*4*. To compile the tool use *make* from the main directory.

*5*. To install the web interface, go to the web folder *cd web/* and run *make*. This will download styles from bootstrap, the ace editor and katex.

*6*. To run the tool, execute *./websolver* and then open the file *web/index.html* from the browser (you may need to enable websockets).

*7*. To reproduce our experiments, run *./run_examples*.
