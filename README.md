This repository contains the data and source code for the following paper:

-   M. Corsi and J. Urbano, "[The Treatment of Ties in Rank-Biased Overlap](http://julian-urbano.info/files/publications/068-treatment-ties-rank-biased-overlap.pdf)", *International ACM SIGIR Conference on Research and Development in Information Retrieval*, 2024.

A [single ZIP file](https://github.com/julian-urbano/sigir2024rbo/archive/master.zip) can be downloaded as well.

## Implementation of RBO

File `rbo/rbo.R` contains a standalone implementation of all RBO coefficients presented in the paper ($RBO_{EXT}$, $RBO_{MIN}$, $RBO_{MAX}$ and $RBO_{RES}$) in all three tie-aware variants ($RBO^w$, $RBO^a$ and $RBO^b$).

### What RBO variant should you use?

-   When a tie represents equality, so that tied items *really* occur at the same rank, you should compute $RBO^w$.

-   When a tie represents uncertainty, so that it is not known which item appears first:

    -   Ties should not be broken deterministically, such as by doc ID, because it inflates $RBO$ scores.
    -   Ties should not be broken at random because it introduces noise. $RBO^a$ should be used instead, as it precisely computes the expected $RBO$ when breaking ties at random.
    -   If the measured overlap should be corrected by the amount of information lost due to ties, $RBO^b$ should be used. This ensures $RBO^b(X,X)=1$, and implies $RBO^a\leq RBO^b$.

For more details, please refer to Section 1.1 of the paper.

## How to reproduce the results in the paper

This is the project structure:

-   `data/` Input data files.
-   `output/` Generated output files.
-   `rbo/` RBO implementation.
-   `src/` Source code in R to reproduce results from the paper.
-   `scratch/` Temporary files generated in the process.

All code is written in [R](https://www.r-project.org). You will need the following packages installed from CRAN: `dplyr`, `extraDistr`, `future.apply`, `ggplot2`, `glue`, `latex2exp`, `mvtnorm` and `rio`.

The source files in `src/` need to be run in order. You can run each file individually by executing `Rscript src/<file>.R`. They will store intermediate data in `scratch/` and the final data in `output/`.

**It is important that you always run from the base directory**.

1.  `src/01-trec-download.R`: download TREC runs (you will need password and username; see <https://trec.nist.gov/results.html>). Store in `scratch/01-trec-download`.
2.  `src/02-trec-stats.R`: compute statistics about TREC runs. Store in `output/trec-stats`.
3.  `src/11-rbo-trec.R`: compute RBO scores between pairs of TREC runs. Store in `output/rbo-trec`.
4.  `src/12-rbo-synthetic.R`: simulate synthetic data and compute RBO scores. Store in `output/rbo-synthetic`.
5.  `src/99-paper.R`: generates tables and figures. Store in `output/figures`.

## License

-   Databases and their contents are distributed under the terms of the [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).
-   Software is distributed under the terms of the [MIT License](https://opensource.org/licenses/MIT).

When using this archive, please cite this paper:

    @inproceedings{corsi2024treatment,
      author = {Corsi, Matteo and Urbano, Juli\'{a}n},
      booktitle = {International ACM SIGIR Conference on Research and Development in Information Retrieval},
      title = {{The Treatment of Ties in Rank-Biased Overlap}},
      year = {2024},
      pages = {xx--xx}
    }
