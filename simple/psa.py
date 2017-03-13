# Install varsens
# Install ghalton

import os
from varsens import * 
import scipy.stats as ss
from csv import *
import numpy as np

import tempfile

def beta(x, a, b): return ss.beta.ppf(x, a, b)

def scale1(x):
  return  np.array( [ beta(x[0], 100, 900),
                      scale.linear(x[1], 0.01, 0.09),
                      scale.linear(x[2], 0.20, 0.25),
                      scale.linear(x[3], 0.45, 0.55),
                      scale.linear(x[4], 0.07, 0.13),
                      beta(x[5], 500, 500)
                    ])

def simple_scale(x):
  return np.apply_along_axis(scale1, 1, x) 

# Load sampled space, transformed latin hypercube (15,000 points)
points = np.loadtxt('test-cube.csv',delimiter=',',skiprows=1)

# Pair down to 7500 points in hypercube sample, which gives a run size of 105000
# Load Latin hypercube
#sample = Sample(6, 7500, raw=points[0:15000,...])
# Try Halton
n = 15000
sample = Sample(6, n, scaling=simple_scale, verbose=True)
np.corrcoef(sample.M_1[:,0], sample.M_2[:,0])[0,1]
np.corrcoef(sample.M_1[:,1], sample.M_2[:,1])[0,1]
np.corrcoef(sample.M_1[:,2], sample.M_2[:,2])[0,1]
np.corrcoef(sample.M_1[:,3], sample.M_2[:,3])[0,1]
np.corrcoef(sample.M_1[:,4], sample.M_2[:,4])[0,1]
np.corrcoef(sample.M_1[:,5], sample.M_2[:,5])[0,1]

sampleFilename = 'saltelli-cube.csv'

# Have to write to tmp file due to numpy.savetxt stupidly editing header when writing
tmpFile = tempfile.NamedTemporaryFile()
np.savetxt(tmpFile.name, sample.flat(), delimiter=',')
with open(sampleFilename, "w") as f: # Write header properly to actual file
    f.write('vRiskA,vFatalA,A_survive,vRiskB,B,vRR\n')

os.system("cat "+tmpFile.name+" >> "+sampleFilename) # Then append tmp file results
tmpFile.close()

# At command prompt, execute julia
os.system("julia simple-numerical.jl "+ sampleFilename +" > psa.out")

# Load function results
objectives = np.loadtxt('psa.out',delimiter=',',skiprows=1)

# Let's look at the first result
o = Objective(6, n, objective_vals=objectives[:,[0]], verbose=True)

v = Varsens(o, sample=sample, k=6, n=n, verbose=True)
