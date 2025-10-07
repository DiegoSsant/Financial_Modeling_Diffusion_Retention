# Financial_Modeling_Diffusion_Retention
Code for empirical validation of the article "Modeling Asset Price Dynamics with Investor Inertia: Advection-Diffusion Model and Fourth-Order Extension". A preprint is available at: https://arxiv.org/abs/2509.18488.

**Authors:** Diego da Silva Santos, Luiz Gustavo Bastos Pinho

This repository contains the R code for the paper titled "Modeling Asset Price Dynamics with Investor Inertia: From Diffusion with Advection to a Fourth-Order Extension". The scripts allow for the replication of all calibrations, simulations, and figures presented in the manuscript.

---

## Abstract

Standard models of asset price dynamics, such as geometric Brownian motion, do not formally incorporate investor inertia. This paper presents a two-stage framework for modelling this behaviour. First, we establish a microfoundation for the classic diffusion-with-advection model by representing the asset's log price as a three-state random walk (up, down or neutral). While this derivation offers a clear behavioural origin for drift and volatility, it is ultimately limited by its Gaussian nature and fails to capture the heavy tails (leptokurtosis) observed in financial markets. To address this issue, we introduce and apply a fourth-order extension inspired by diffusion-with-retention models, where a more complex representation of inertia generates non-Gaussian dynamics. Through an empirical application using Brazilian PETR4.SA data, we demonstrate that this extended model significantly outperforms the original in fitting the real distribution of returns. Our findings suggest that investor inertia is a dual concept capable of explaining both standard market trends and extreme events.

---

## Repository Contents

* `Financial_Modeling_Diffusion_Retention.R`: The main R script that performs all necessary tasks: data download, model calibration, simulations, and generation of all figures and plots shown in the paper.
* `README.md`: This file.
* `LICENSE`: MIT License.

---

## System Requirements

* [R programming language](https://www.r-project.org/) (version 4.0 or later recommended).
* R packages: `quantmod`, `dplyr`, `ggplot2`, `moments`.

---

## Installation & Usage

To replicate the results presented in the paper, follow these steps:

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/seu-usuario/nome-do-repositorio.git](https://github.com/seu-usuario/nome-do-repositorio.git)
    cd nome-do-repositorio
    ```

2.  **Install R dependencies:**
    Open an R session and run the following command to install the required packages:
    ```R
    install.packages(c("quantmod", "dplyr", "ggplot2", "moments"))
    ```

3.  **Run the analysis:**
    Open the `Financial_Modeling_Diffusion_Retention.R` file in R or RStudio and execute the entire script. The script is self-contained and will automatically download the necessary financial data for PETR4.SA from Yahoo Finance.

### Expected Output

Running the script will:
1.  Print the calibrated parameters for both the advection-diffusion and the diffusion-with-retention models to the console.
2.  Generate and display the comparative plots as seen in the paper:
    * Time-series plots of the real price vs. multiple simulations for each model.
    * The histogram of log-returns overlaid with the theoretical density curves.
    * The comparative Q-Q plots for model fit assessment.

---

## How to Cite

If you use this code or the methods presented in our paper in your research, please cite our work.

