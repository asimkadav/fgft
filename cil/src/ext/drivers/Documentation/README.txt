Author: Vinod Ganapathy.
Date started: August 1, 2006.
Description: CIL module to analyze device drivers.

We want to do a local analysis of each driver subdirectory, and propagate
annotations for that driver, rather than do a whole program analysis with
the entire kernel. Thus, we need to exclude the rest of the kernel from the
build, and only selectively include certain drivers in our build process.
Doing a whole-program analysis on the kernel is not advisable for two reasons:
* We will propagate annotations to kernel functions that we don't ultimately
  want to relegate to user-space.
* It will be prohibitively expensive and will involve a significant engineering
  effort (with CIL).
However, this does mean that our analysis will be incomplete. Thus, for
instance, we will have to terminate our analysis at the "boundaries". That is,
we will have to terminate our analysis at calls to functions that are not
implemented within the code-base that we are analyzing.

===============================================================================

NOTES:

[1] ANALYZING LINUX-2.6 (2.6.17)
--------------------------------
We want to configure the build process of Linux-2.6 so that we can build
drivers in isolation. Here are the changes to make to the build process to
achieve this. 

* Several one-time changes in header files to remove parse-errors.

To compile drivers in separate directories (e.g., e1000):
1. Modify the Makefile in the directory that you are interested in, e.g.,
   $(TOP_LEVEL)/drivers/net/e1000/Makefile, to redefine $CC, $LD and $AS
   with cilly.

2. To make a particular driver in a directory, e.g., the e1000 driver, 
   use the command 'make V=1 drivers/net/e1000/' from the command-line 
   at $(TOP_LEVEL). 
   The trailing '/' is very important! Without the trailing '/', you will
   get the response "make: Nothing to be done for `drivers/net/e1000'"

To compile standalone drivers (e.g., pcnet32 and e100):
* To make a particular driver in a single file, just include that 
  driver's name in the $(TOP_LEVEL)/drivers/net/Makefile file. Comment
  out the rest, and use 'make V=1 drivers/net/' from $(TOP_LEVEL)

-----------------------------------------------------------------

8 March 07: Methodology for annotations:

* NULLTERM annotations. These cannot be infered automatically. They
  must be provided by the user.
  
* ARITH annotations. These can be infered automatically to a large
  extent. Run the analysis, print out the points-to-graph, and observe
  fields that have "arith" annotations. Apply ARITH annotations to those
  fields and run the analysis again. This can be automated too, but we 
  currently don't automate it (just an infrastructure issue).
  
* RECURSE annotations. These can be infered automatically by looking
  at cycles in data structures. To infer these, run the analysis and 
  see where infinite loops happen. This will provide insight into where
  there are recursive accesses of data structures. Mark these fields of
  data structures RECURSE and run the analysis again.


12 June 2007: Our first microdriver actually works! A simple version of
pcnet32 with pcnet32_get_drvinfo in the slave.
