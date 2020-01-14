RIGHT - Rational Integration of Genomic Healthcare Testing
===================================

Simulation code used for publications on the [RIGHT](https://rightsim.org) project.

This project explores the development of rational cost-effective
strategies for integrating multiplexed genomic information
into clinical practice.

Simulations of a patient population are done using the R package
simmer, with a variety of testing scenarios.

For questions regarding this repository of code, please contact [Shawn Garbett](mailto:shawn.garbett@vumc.org), at Biostatistics, Vanderbilt University Medical Center.

1/14/2020: Updated for current version of simmer


Creating Events Checklist
-------------------------
1. Add event to event registry in main simulation file.
2. Create a time to event in the events file.
3. Create an event function in the events file
4. Add the initial relative risk (=1) in the patient attributes file.  
5. If event is downstream of another event (e.g., drug initiation), add the new event to the main function for the upstream event. 



All code Copyright (c) 2016, Vanderbilt University Medical Center

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
