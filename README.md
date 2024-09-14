This repository is the companion code to the papers *Primordial Stochastic Gravitational Wave Backgrounds from a Sharp Feature in Three-field Inflation* ([I](https://arxiv.org/abs/2304.00065) and II), by Vikas Aragam, Sonia Paban, and Robert Rosati.


# Quick start

## Mathematica

The Bogoliubov coefficients for two and three fields are stored in the `*.wl` files. The files with `*Small*` in the name correspond to the branch of solutions with $` {\cal M}_{ss,0}, {\cal M}_{bb,0} \leq 9/4 `$ (cf. Eq (2.11) of paper II), while the other files are the branch with larger constant masses. Each file contains two variables

  - `regIIIcoeffsSimp`: these are the Bogoliubov coefficients of the sharp feature's excited state
  - `αsols`: the WKB exponents during the feature are $` \pm \sqrt{\alpha_i} `$. 

Example usage and plots reproducing many of the figures of our work are available in `bog_plotting_clean.nb`.

## Julia


# Citations
If these results are useful to you, please cite the corresponding papers :)

```bibtex
@article{Aragam:2023adu,
    author = "Aragam, Vikas and Paban, Sonia and Rosati, Robert",
    title = "{Primordial stochastic gravitational wave backgrounds from a sharp feature in three-field inflation. Part~I. The radiation era}",
    eprint = "2304.00065",
    archivePrefix = "arXiv",
    primaryClass = "astro-ph.CO",
    reportNumber = "UTWI-9-2023",
    doi = "10.1088/1475-7516/2023/11/014",
    journal = "JCAP",
    volume = "11",
    pages = "014",
    year = "2023"
}
```
