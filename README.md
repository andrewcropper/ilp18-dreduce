Code and experimental data for the paper:

A. Cropper and S. Tourret. Derivation reduction of metarules in meta-interpretive learning. ILP 2018.
http://andrewcropper.com/pubs/ilp18-dreduce.pdf

data are in the folder 'data'
learned programs are in the folder 'programs'
results of the tests are in the folder 'results'

- To reproduce the results in figure 4, run 'python results.py'
- To rerun the testing step, run 'bash test.sh'
- To rerun the learning step, run 'bash learn.sh'
- To generate new data, run 'bash gen-data.sh'

Through combinations of the above you can reproduce the experimental results with either the same data or new data.

Note that when running the tests, if Metagol fails to learn a program then the classification decision is random, so the exact results may fluctuate slightly.