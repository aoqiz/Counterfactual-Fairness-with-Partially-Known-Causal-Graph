#!/bin/bash
for((j=10; j<=40; j+=10))
do
    # Generate synthetic data and the corresponding counterfactual data.
    python data_generator.py $j $((2*j))
    for((i=0; i<100; i++))
    do
        # Fitting models
        Rscript main.R $j $((2*j)) $i
    done
    # Obtain Table 1. Numeric results for RMSE and Unfairness.
    Rscript Simu_results_filter_and_processor.R $j $((2*j))
done
# Obtain Figure 1. Boxplot for RMSE and Unfairness.
Rscript Simu_filtered_Boxplot.R     