Team Members
============
Alejandro Frias
Ravi Kumar


DSHW4
=====

Version: 2014-03-03
Last Update: 2014-03-07

We set the cookie to "PHILOSOPHER" so that the external controller can be anyone using that cookie. Make sure to set your cookie to "PHILOSOPHER" or your messages will be disallowed.

How to Set cookie: erlang:set_cookie(node(), 'philosopher')

We also have multiple files to compile:
	dsutils contains some handy utility functions such as timestamped logging
	philosopher.erl has our philosopher code
	controller.erl has some testing code in it

To compile all three, run the "make" script with ./make


Philosophers assumed node names for controller.erl:

a@amazonia
b@arden
c@ash

Overall we didn't have too much trouble with the assignment. We first sat down and hashed out the algorithm, and didn't really run into any major issues while coding or testing it, as we thought it out quite extensively before we started coding. 

