# NE_matter_arising
Materials to reproduce the results of robustness analysis based on Network Entanglement

## Dismantling
The data and the code to reproduce the experiments of the Matter Arising paper are based on the [Network Dismantling repository](https://github.com/NetworkDismantling/review/) repository.
Please follow the installation instructions there, and then run the following command to reproduce the results of the paper:

```bash
cd network_dismantling # go to the network dismantling folder

# Run the dismantling experiments selecting the heuristics to use (or a subset of them),
#  the location of the dataset, selecting the output file and the dismantling threshold to use
python network_dismantling/dismantler.py --heuristics network_entanglement_large network_entanglement_mid network_entanglement_small vertex_entanglement vertex_entanglement_reinsertion network_entanglement_large_reinsertion network_entanglement_small_reinsertion network_entanglement_mid_reinsertion --location <DATASET_LOCATION> -o out/df/heuristics_entanglement.csv --threshold 0.1 -v DEBUG

```

Then, you can run the following command to reproduce the figures of the paper:

```bash

# First, convert the output file to a format that can be used by the R plotting scripts
python ne_matter_arising/convert_output.py out/df/heuristics_entanglement.csv
```

This will generate a file with an "_extended" suffix, containing a row for each removal step of each heuristic.
This file can be used to generate the figures of the paper.

```bash
# Run the R scripts to generate the figures of the paper
Rscript ne_matter_arising/plot_results.R out/df/heuristics_entanglement_extended.csv
```
