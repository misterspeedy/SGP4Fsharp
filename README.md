SGP4Fsharp
==========

An F# port of the SGP4 satellite orbit tracker.

Background
==========

This repo is an F# port of SGP4, described here:

http://www.centerforspace.com/downloads/files/pubs/AIAA-2008-6770.pdf

The port is based on the C++ code downloaded from here:

http://www.centerforspace.com/downloads/

Project Aims
============

The aim of this repo is to explore the strengths and weakness of F# when applied to a
calculation-intensive scientific domain.

Project Stages
==============

1) A naive, line-by-line port of the C++ code, warts and all.  

The first version of this repo represents this step.  The results produced by running
this version are the same as those produced by the C++ version  (entering the parameters 
'a', 'v' and '72'), except for some rounding differences in the last digit.

This version also includes a project (SGP4RegressionTest) which generates a new set of 
output and compares it with the output produced by the first version.  This means that
we can refactor the code with the confidence that results are unchanged.

2) Refactoring. 

This step will involve making the code idiomatic.  Use of mutability will be reduced or
elminated, conventional F# naming will be used, large functions will be broken up, repetition
will be eliminated, and F# features will be used more fully.

3) UI.

The model will be linked to a UI which will allow visualisation of satellite orbits.
