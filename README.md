This repository is the companion code to the papers *Primordial Stochastic Gravitational Wave Backgrounds from a Sharp Feature in Three-field Inflation* ([I](https://arxiv.org/abs/2304.00065) and [II](https://arxiv.org/abs/2409.09023)), by Vikas Aragam, Sonia Paban, and Robert Rosati.


# Quick start

First clone the repo.

## Mathematica

Example usage and plots reproducing many of the figures of our work are available in `bog_plotting_clean.nb`, just clone the repo and get started.

The Bogoliubov coefficients for two and three fields are stored in the `*.wl` files. The files with `*Small*` in the name correspond to the branch of solutions with $` {\cal M}_{ss,0}, {\cal M}_{bb,0} \leq 9/4 `$ (cf. Eq (2.11) of paper II), while the other files are the branch with larger constant masses. Each file contains two variables

  - `regIIIcoeffsSimp`: these are the Bogoliubov coefficients of the sharp feature's excited state
  - `αsols`: the WKB exponents during the feature are $` \pm \sqrt{\alpha_i} `$.

Not discussed in the papers but also included here are the two-field Bogoliubov coefficients including a constant contribution to the entropic mass. These should match the $` \tau \rightarrow 0 `$ limit of the $`(\zeta, s)`$ 2$`\times`$2 block of the three-field coefficients.

## Julia


### Installing Julia

First [install Julia](https://julialang.org/downloads/), then clone this repo.
Open a terminal in the cloned repo's folder.

As a small aside, the Julia workflow is a little different than python -- because Julia uses just-in-time compilation, you'll avoid excessive recompilation if you leave a julia REPL running between editing and rerunning code.

### Activating the environment

Next we'll want to activate the environment in this folder
```julia-repl
julia> import Pkg; Pkg.activate(".")
```
(as a shortcut, you can also press the `]` key and type `activate .`).

### Instantiating the environment
The first time you run this, you'll also need install the dependencies and precompile them (you only have to do this once):
```julia-repl
julia> Pkg.instantiate()
```
This will take a little while.

Then you'll be able to run the codes here.

### How to run
Each time you re-open Julia, you'll need to [reactivate the environment](#activating-the-environment) in this folder before running any of the codes.
Then you can load the codes with

```julia-repl
julia> include("bog_plotting_minimal.jl")
```
to generate a few plots interactively.
The code is organized around handling the description of the synthetic background in a `SyntheticModelspec`, defined as
```julia
	@with_kw struct SyntheticModelspec
		d::Int = 3
		feature_at::Float64 =23.0 # e-folds before the end of inflation
		feature_fwhm::Float64=0.25
		efolds::Float64 = 80.0
		ϵ::Float64=0.01
		Mss_ratio::Float64 =-3.0 # Mss/(Ω^2+τ^2)
		Mss_const::Float64=  0.0 # H^2
		Ω::Float64=1.0
		τ::Float64=1.0
		Msb_ratio::Float64= 0.0  # Msb/τ
		Msb_const::Float64= 0.0  # H^2
		Mbb_ratio::Float64= 1.0  # Mbb/τ^2
		Mbb_const::Float64= 0.0 # H^2
		Hinit::Float64 = 1e-4
	end
```
The functions `get_all_bogoliubov_mass(κ,sms::SyntheticModelspec)`, `analyticPz(κ,sms::SyntheticModelspec)`, etc, all take a `SyntheticModelspec` and return the quantities of interest. Please see the examples in `bog_plotting_minimal.jl`. For now, all of the Bogoliubov coefficients in Julia are the branch that allows for $`0 < M_0 < 9/4`$. They are source-to-source translations of the Mathematica results using https://github.com/rjrosati/mathematica2julia

Alternatively, you can start a notebook-style interface with
```julia-repl
julia> import Pluto; Pluto.run()
```
and selecting `bog_plotting_pluto.jl` in your browser.

Pluto is a little different than Jupyter notebooks or Mathematica in that it is *stateless*, so changing a variable in one part of the notebook changes it in all parts of the notebook, and the order of the cells doesn't matter.

See https://github.com/fonsp/Pluto.jl/wiki/%F0%9F%94%8E-Basic-Commands-in-Pluto for some basic how-to's with Pluto notebooks.

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

@article{Aragam:2024,
    author = "Aragam, Vikas and Paban, Sonia and Rosati, Robert",
    title = "{Primordial stochastic gravitational wave backgrounds from a sharp feature in three-field inflation. Part~II. The inflationary era}",
    eprint = "2409.09023",
    archivePrefix = "arXiv",
    primaryClass = "astro-ph.CO",
    year = "2024"
}
```
