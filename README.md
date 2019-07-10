# plumes-hs
Tool to parse and plot simulation data exported from Visual Plumes 

## Overview

	Simulation:

	       Depth        Amb-cur       Far-dir  Disprsn    P-dia  V-angle  P-depth  Polutnt   Dilutn  CL-diln   x-posn   y-posn
		
    Step    (m)           (m/s)        (deg)  (m0.67/s2) (m)     (deg)      (m)    (kg/kg)    ()       ()      (m)      (m)
	 0     200.0      0.0 2.142E+8      0.0      1.0     60.0    200.0      1.0      1.0      1.0      0.0      0.0; stream limit reached;
	 1     200.0      0.0 2.142E+8      0.0     1.01    59.96    200.0     0.98     1.02      1.0   0.0246      0.0; bottom hit;
	 2     199.9      0.0 2.142E+8      0.0    1.031    59.91    200.0    0.961    1.041      1.0   0.0501      0.0;
     3     199.9      0.0 2.142E+8      0.0    1.052    59.87    200.0    0.942    1.062      1.0   0.0761      0.0;
     4     199.8      0.0 2.142E+8      0.0    1.074    59.82    200.0    0.924    1.083      1.0    0.103      0.0;
     5     199.8      0.0 2.142E+8      0.0    1.096    59.76    200.0    0.906    1.105      1.0     0.13      0.0;
     6     199.7      0.0 2.142E+8      0.0    1.119    59.71    200.0    0.888    1.127      1.0    0.158      0.0;
     7     199.7      0.0 2.142E+8      0.0    1.143    59.65    200.0    0.871    1.149      1.0    0.186      0.0;
     8     199.6      0.0 2.142E+8      0.0    1.167     59.6    200.0    0.853    1.173      1.0    0.215      0.0;
     9     199.6      0.0 2.142E+8      0.0    1.191    59.53    200.0    0.837    1.196      1.0    0.245      0.0;
    10     199.5      0.0 2.142E+8      0.0    1.216    59.47    200.0     0.82     1.22      1.0    0.275      0.0;

## Usage
``` bash
$ git clone https://github.com/dalpd/plumes-hs.git
$ cabal run plumes-hs
```
