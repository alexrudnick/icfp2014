icfp2014
========

Our ICFP Contest 2014 entry for Team "They Are On A Team"!

Team "They Are On A Team" is:

  * [Martin Robinson](http://abandonedwig.info/)
  * [Alex Rudnick](http://alexr.cc)

Languages used: Python, JavaScript, Lisp [0] ...

[0] ... the tiny Common Lisp-esque dialect that our compiler can compile to the
General Compute Coprocessor instruction set...

## running the code
  * use the included script to set up a Python 3 virtual environment
  * in your shell, do "source venv/bin/activate" to get in the venv
  * run compiler.py on a lisp source file, get GCC assembly to stdout!
  * ghost-compiler.py and ghost AIs are still under development...
  * compiler.py may be the first compiler that depends on NLTK
  * see also the included command-line tool run-lambdaman.js, which uses PhantomJS to run the GCC vm in a headless browser

## and check out all that Lisp code too...
  * our Lambda-Man AIs are written in a super-minimal dialect of Lisp
  * in which there is no lambda special form
  * https://twitter.com/lindsey/status/493112453970948096
  * you want a function, you can defun one like a frickin' adult
  * Lisp code in the examples/ directory -- we submitted the compiled version of ids-with-tree.lisp
  * there's some data-structure cleverness in there

This was fun, thanks! :D

-- 
-- Team "They Are On A Team", The Bay Area, July 2014
