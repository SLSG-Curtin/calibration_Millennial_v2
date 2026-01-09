# Calibration of Millennial Model v2 in Australian Rangelands

> A comprehensive guide to the calibration workflow, model structure, and parameters for the Millennial Model v2 in Australian Rangelands.

---

## 📚 Table of Contents

- [General Principle](#-general-principle)
- [Calibration Workflow](#-calibration-workflow)
- [Global Sensitivity Analysis](#-1-global-sensitivity-analysis)
- [Methods to Reduce Uncertainty](#-2-methods-to-reduce-uncertainty)
- [Calibration Strategies](#-3-calibration-strategies)
- [Data Summary](#-data-summary)
- [References](#-references)
- [Appendix](#-appendix)

---

## 💡 General Principle: Parameter Tuning in Soil C Models

The calibration process follows a logical sequence analogous to parameter estimation in regression models, adapted for complex process-based parameters tuning.

| Step | Ordinary Least Squares (Linear Regression) | Parameter Tuning (Soil C Model) |
| :--- | :--- | :--- |
| **1. Data Collection** | Collect data of $X$ and $Y$ | Collect observational datasets (e.g., soil C observations, environmental drivers) |
| **2. Model Selection** | Select an equation $Y = \beta_0 + \beta_1 X$ | Select the process-based model (e.g., Millennial v2) |
| **3. Cost Function & Constraints** | Measure the mismatch $(y - \hat{y})$ (typically unconstrained) | Define a **cost function** to minimize mismatch, incorporating **constraints** (penalties) for invalid states |
| **4. Optimization** | Minimize $\sum (y - \hat{y})^2$ to optimize $\beta_0$ and $\beta_1$ | Apply **global optimization** algorithms to minimize the cost function |
| **5. Parameter Estimation** | Obtain the optimized $\beta_0$ and $\beta_1$ values | Obtain the optimized or estimated model parameter values |
| **6. Prediction** | Use the optimized equation to predict | Use the parameterized model to predict spatial or temporal soil C dynamics |


## 🌍 Calibration Workflow

```text
  GLOBAL SENSITIVITY ANALYSIS            METHODS TO REDUCE UNCERTAINTY
 ═════════════════════════════          ═══════════════════════════════

                                     ┌───────────────────────────────────┐
      [Millennial v2 Model]          │   Integrating different types     │
               │                     │        of uncertainties           │
               ▼                     └─────────────────┬─────────────────┘
      [TOC and C Fractions]                            │
               │                                       │ (Splits into 4 streams)
               ▼                            ⎧          │
 [Multi-output Sensitivity Analysis]        │          ▼
               │                            │  ┌────────────────────────┐       ┌──────────────────────────┐
               ▼                            │  │     Initial states     │──────►│   Measured C fractions   │
          ((Screening))                     │  │      uncertainty       │       │ for states initialization│
               │                            │  └────────────────────────┘       └─────────────────────┬────┘
               ▼                            │                                                         │
     [Influential Parameters] ──────────────┤  ┌────────────────────────┐       ┌─────────────────────┴────┐
                                            │  │        Forcing         │──────►│    Using high-quality    │
                                            │  │      uncertainty       │       │ climate and soil drivers │
                                            │  └────────────────────────┘       └─────────────────────┬────┘
                                            │                                                         │
                                            │  ┌────────────────────────┐       ┌─────────────────────┴────┐
                                            │  │  Estimation of model   │──────►│  Refining function for   │
                                            │  │ parameters uncertainty │       │ max sorption capacity    │
                                            │  └────────────────────────┘       └─────────────────────┬────┘
                                            │                                                         │
                                            │  ┌────────────────────────┐       ┌─────────────────────┴────┐
                                            │  │ Parameters optimisation│──────►│    Bound-constrained     │
                                            │  │      uncertainty       │       │        optimiser         │
                                            │  └────────────────────────┘       └─────────────────────┬────┘
                                            ⎩                                                         │
                                                                                                      │
                                                                                                      │
                                               CALIBRATION STRATEGIES                                 │
                                            ══════════════════════════                                │
                                                                                                      │
                                        ⎧                                                             │
                                        │   ┌──────────────────────────────────────────────────────┐  │
                                        │   │   Alignment of model simulations and observations    │◄─┘
                                        │   │             (Scale-aware calibration)                │
                                        │   └──────────────────────────┬───────────────────────────┘
                                        │                              │
                                        │                              │
                                        │                   ⎧          ▼
                                        │                   │   (Global Calibration)
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │   (Bioregional Calibration)
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │
                                        │                   │   (Site-by-site Calibration)
                                        ⎩                   ⎩
```

This document outlines the systematic approach for the calibration of the Millennial model v2, specifically tailored for application within the Australian Rangelands. The process integrates global sensitivity analysis, uncertainty reduction techniques, and multi-scale calibration strategies.

---

## 🔍 1. Global Sensitivity Analysis

The initial phase focuses on understanding model behavior and identifying the most influential drivers of Carbon (C) dynamics using the PAWN sensitivity analysis method (`SAFER` package).

### ⚙️ Sensitivity Analysis Workflow
- **Script**: `d01_src/sensitivity_PAWN_hybrid.R`
- **Key Packages**: `FME`, `deSolve`, `rootSolve`, `SAFER`.
- **Purpose**: To run the Millennial v2 model across a sampled parameter space to evaluate the sensitivity of Total Soil Organic Matter (SOM).

#### 1. Parameter Sampling
A Latin Hypercube Sampling (LHS) strategy is used to explore the 24-dimensional parameter space.
- **Samples ($N$)**: 3,000 to 5,000.
- **Strategy**: `AAT_sampling` with `lhs` strategy.
- **Parameters**: 24 influential parameters (Uniform distribution) including $P_i$, $P_a$, $K_{pl}$, $\alpha_{pl}$, $E\alpha_{pl}$, etc.
- **Forcing Input**: Global average data.
  - **File**: `d02_data/globalaverage.txt`
  - **Structure**: 365-day (1 year) daily time-series.
  - **Columns & Units**:
    - `forc_st`: Soil Temperature (°C)
    - `forc_sw`: Soil Moisture (m³ m⁻³)
    - `forc_npp`: Net Primary Productivity (g C m⁻² d⁻¹)

#### 2. Model Execution
The model is executed for each parameter set to reach steady state and then run for a simulation period.
- **Initialization**: Solve for steady state (`stode`) using global average forcing data and constant initial C fractions.
- **Simulation**: With the steady state solution, run the model (`ode` from `deSolve`) with 4th-order Runge-Kutta (`rk4`) method over 100 years.
- **Output**: Extraction of state variables (POM, LMWC, AGG, MIC, MAOM) to compute Total SOM.

### 📊 Multi-output Sensitivity Analysis
- **Script**: `d01_src/sensitivity_PAWN_multihybrid.R`
- **Key Packages**: `SAFER`, `ggplot2`.
- **Purpose**: To calculate the PAWN sensitivity indices for multiple model outputs and visualize the influential parameters.

#### 1. Calculating PAWN Indices
PAWN indices are calculated for each C pool to understand which parameters drive specific carbon dynamics:
- **Primary Outputs**: SOM, MAOM, POM, LMWC, AGG, and MIC.
- **Method**: `pawn_indices` with bootstrapping ($N_{boot}=1000$ or $3000$, tuning parameter $n=8, 9, 10$) to estimate mean sensitivity and confidence intervals.
- **Thresholding**: A dummy parameter is included to establish a threshold for identifying non-influential input factors.

#### 2. Visualizing Sensitivity Matrices
A comprehensive sensitivity heatmap is generated to categorize parameter influences:
- **Temperature Effects**: $\alpha_{pl}$, $E\alpha_{pl}$, $\alpha_{lb}$, $E\alpha_{lb}$, $CUE_{ref}$, $CUE_T$, $T_{ae-ref}$.
- **Water Effects**: $P_a$, $K_{pa}$, $K_b$, $K_l$, $K_{ma}$, $K_{a,min}$, $\lambda$, Porosity, $\phi$.
- **Langmuir Model**: $P_1$, $P_2$, $K_{ld}$.
- **Reaction Kinetics**: $K_{pl}$, $K_{lb}$.
- **Other**: $P_i$, $P_b$, $K_{bd}$.

---

## 📉 2. Methods to Reduce Uncertainty

To improve the reliability of model predictions, we integrate different types of uncertainties into the workflow. This process is divided into four main streams:

### Stream A: Initial States Uncertainty
*   **Measured C fractions for states initialization**: Reducing uncertainty in the starting conditions by using high-quality measurements of carbon fractions to initialize the model states.

### Stream B: Forcing Uncertainty
*   **Using high-quality climate and soil drivers**: Enhancing the precision of external drivers (climate data and soil properties) to ensure the model is "forced" by accurate environmental conditions.

### Stream C: Estimation of Model Parameters Uncertainty
*   **Refining function for max sorption capacity**: Improving the mathematical representations within the model, specifically the functions governing maximum sorption capacity, to better capture biochemical processes.

### Stream D: Parameters Optimisation Uncertainty
*   **Bound-constrained optimiser**: Utilizing robust optimization algorithms with defined parameter bounds to find the most effective parameter sets while minimizing uncertainty in the optimization process.

---

## 🎯 3. Calibration Strategies

The final phase involves the actual alignment of model simulations with real-world observations through a "Scale-aware calibration" approach.

### Alignment of model simulations and observations
The calibration is conducted across three distinct spatial scales to ensure the model is robust at various levels of granularity:

### 3.1 Global Calibration/Optimization
This initial step optimizes the model parameters to capture the general trends across all 993 rangeland sites.

#### ⚙️ Workflow
- **Script**: `d01_src/rangelands_calibration_global.R`
- **Key Packages**: `FME`, `deSolve`, `rtop`.
- **Method**: Global optimization using `FME::modFit`.
- **Initialization**:
    1.  **Steady State**: `stode` calculates the equilibrium state for each site.
    2.  **Transient Simulation**: `ode` runs the model forward to align with the Observation timestamp.

#### 🎯 Optimization Setup
- **Algorithms**:
    - `Marquardt` (Levenberg-Marquardt) for robust local convergence.
    - `L-BFGS-B` (quasi-Newton) for bound-constrained optimization.
- **Cost Function**: Minimizes RSS between modeled and observed C pools using `FME::modCost`.
    - **Model Output**: `Objective.pools()` or `Objective.states()` functions return simulated C pools (POM, LMWC, AGG, MIC, MAOM, SOM, POM.AGG)
    - **Observations**: Compares against observed SOM, MAOM, and POM.AGG values
    - **RSS Calculation**: `modCost(out, Obs.pools, x="id", weight = "none")`
    - **Penalty**: Invalid parameter sets return large penalty values via `modCost(df_inf, Obs.pools, x="id", weight = "none")`
- **Outcome**: A single "global" parameter set that minimizes the error across all sites simultaneously.

#### Technical Implementation Details
- **Cost Function**: Minimizes RSS using `FME::modCost` for C pools:
  - SOM deviation: `|SOM_sim - SOM_obs|²`
  - MAOM deviation: `|MAOM_sim - MAOM_obs|²`
  - POM.AGG deviation: `|POM.AGG_sim - POM.AGG_obs|²`
  - Total cost: `RSS = Σ(SOM_dev² + MAOM_dev² + POM.AGG_dev²)`
- **Constraints**: 
  - LMWC/SOM ≤ 0.03 (`rate_lw > 0.03` triggers penalty)
  - MIC/SOM ≤ 0.03 (`rate_mc > 0.03` triggers penalty)
- **Equilibrium**: Uses `stode` solver for steady-state; fallback to `ode` if needed
- **Parallel Processing**: Single batch optimization across all 993 sites
- **Penalty Handling**: Invalid parameters return `modCost(df_inf, Obs.pools)` where `df_inf` contains 100s

### 3.2 Bioregional Calibration
This step refines the model by optimizing parameters for specific environmental clusters or bioregions.

#### ⚙️ Workflow
- **Script**: `d01_src/rangelands_calibration_clusters.R`
- **Key Packages**: `foreach`, `doParallel`, `FME`, `deSolve`.
- **Method**: Parallel optimization for bioregional clusters.
- **Input Data**:
  - **Cluster Membership**: `d02_data/bioclimatic_clusters_new.txt`
  - **Dynamic Forcing**: `d02_data/rangeland_daily_driving/` (time-varying site-specific data)
  - **Static Forcing**: `d02_data/rangeland_avg_driving/` (time-invariant site-specific data)

#### 🎯 Optimization Setup
- **Sampling Strategy**: Large clusters (e.g., Cluster 25) are split into smaller sub-samples (5 sets of ~39 sites) to ensure computational efficiency and representativeness.
- **Initialization**: Parameters are initialized using the **globally optimized** values (`parToFit.VR.opt`).
- **Parallel Processing**: Uses `makeCluster` to calibrate clusters simultaneously.
- **Algorithm**: `L-BFGS-B` for bound-constrained optimization.
- **Cost Function**: Minimizes RSS for SOM, MAOM, and carbon fractions using `FME::modCost`.
    - **Model Output**: `Objective.clusters()` or `Objective.clusters.drive()` functions return simulated C pools and fractions
    - **Output Columns**: POM, LMWC, AGG, MIC, MAOM, SOM, POM.AGG, rate_ma, rate_lw, rate_mc
    - **Observations**: Compares columns 2-5 (id, SOM, MAOM, POM.AGG) of observed data
    - **RSS Calculation**: `modCost(out[,c(2:11)], Obs.cluster[,c(2:5)], x="id", weight = "none")`
    - **Penalty**: Invalid parameter sets return large penalty matrix (100s for all variables)

#### Technical Implementation Details
- **Cost Function**: Minimizes RSS using `FME::modCost` for C pools:
  - SOM deviation: `|SOM_sim - SOM_obs|²`
  - MAOM deviation: `|MAOM_sim - MAOM_obs|²`
  - POM.AGG deviation: `|POM.AGG_sim - POM.AGG_obs|²`
  - Total cost: `RSS = Σ(SOM_dev² + MAOM_dev² + POM.AGG_dev²)` per cluster
- **Constraints**: 
  - MAOM/SOM ≥ 0.04 (`rate_ma < 0.04` triggers penalty)
  - LMWC/SOM ≤ 0.03 (`rate_lw > 0.03` triggers penalty)
  - MIC/SOM ≤ 0.03 (`rate_mc > 0.03` triggers penalty)
- **Equilibrium**: Uses `stode` solver for steady-state; fallback to `ode` for 10,000 steps if needed
- **Parallel Processing**: Use `foreach` with `doParallel` for cluster-wise optimization (39-43 clusters)
- **Penalty Handling**: Invalid parameters return penalty matrix with 100s for all C pool columns

#### 🧪 Calibration Sub-scenarios
The bioregional calibration is evaluated under three distinct initialization and forcing scenarios to assess model performance under different assumptions about initial conditions and environmental drivers:

- **Static initialisation**: uniform initial C fractions across all sites but different time-invariant, site-specific forcing inputs, with steady-state solutions via stode.
- **Measured initialisation**: site-specific measured C fractions and time-invariant forcing inputs.
- **Dynamic simulation**: site-specific measured C fractions with time-varying forcing inputs.

For sub-scenarios 2 and 3, where steady-state could not be achieved using \text{stode}, we employed the ordinary differential equation (ode) solver for up to 10\,000 times to approximate equilibrium. The `L-BFGS-B' algorithm was used in all sub-scenarios to minimise the SSE of the TOC and C fractions.

### 📋 Evaluation Instructions

To evaluate the three calibration sub-scenarios, follow these steps using the R script /'d01_src/rangelands_calibration_ode.R':

#### 1. **Static Initialisation Scenario 1**
   - **Initialization**: Use uniform initial C fractions across all sites:
     - POM  = 1 gC m⁻² (fixed)
     - LMWC = 1 gC m⁻² (fixed)
     - AGG  = 1 gC m⁻² (fixed) 
     - MIC  = 1 gC m⁻² (fixed)
     - MAOM = 1 gC m⁻² (fixed)
   - **Forcing Inputs**: Time-invariant, site-specific values from the input data:
     - Soil temperature (`SoilTMP.C`)
     - Soil moisture (`SoilMoi.m3m3`)
     - Net primary productivity (`NPP.gC.m2.d`)
   - **Steady-State Solution**: Use `SS.Model.clusters()` function with `stode` solver
   - **Fallback**: If `stode` fails to achieve steady-state, use `ode` solver for 10,000 time steps with constant forcing
   - **Optimization**: Minimize SSE using `Objective.clusters()` with L-BFGS-B algorithm
   - **Output**: Optimized parameters saved to `./parsets/{cluster}_parset.csv`

#### 2. **Measured Initialisation Scenario 2**
   - **Initialization**: Use site-specific measured C fractions:
     - POM = observed POM.AGG value for each site
     - LMWC = 6 gC m⁻² (fixed)
     - AGG = 6 gC m⁻² (fixed)
     - MIC = 6 gC m⁻² (fixed)
     - MAOM = observed MAOM value for each site
   - **Forcing Inputs**: Time-invariant, site-specific values (same as Scenario 1)
   - **Steady-State Solution**: Use `SS.Model.sets()` function with `stode` solver
   - **Fallback**: If `stode` fails, use `ode` solver for 10,000 time steps with constant forcing
   - **Optimization**: Minimize SSE using `Objective.clusters()` with L-BFGS-B algorithm
   - **Output**: Optimized parameters and simulated C pools saved to respective cluster directories

#### 3. **Dynamic Simulation Scenario 3**
   - **Initialization**: Use site-specific measured C fractions (same as Scenario 2)
   - **Forcing Inputs**: Time-varying climate data from daily driving files:
     - Load dynamic forcing using `store.cluster.climate()` function
     - Soil temperature, moisture, and NPP vary over time for each site
   - **Steady-State Solution**: Use `SS.Model.clusters.drive()` or `SS.Model.sets.drive()` functions
   - **Fallback**: If `stode` fails, use `ode` solver with dynamic forcing repeated over multiple years until equilibrium
   - **Optimization**: Minimize SSE using `Objective.clusters.drive()` with L-BFGS-B algorithm
   - **Output**: Optimized parameters considering temporal variability in forcing inputs

### 🔧 Technical Implementation Details (Sub-scenarios)

- **Parameter Bounds**: All scenarios use the same parameter bounds defined in `parToFit.lower.opt` and `parToFit.upper.opt`
- **Cost Function**: RSS minimization using `FME::modCost` for C pools:
  - SOM deviation: `|SOM_sim - SOM_obs|²`
  - MAOM deviation: `|MAOM_sim - MAOM_obs|²`
  - POM.AGG deviation: `|POM.AGG_sim - POM.AGG_obs|²`
  - Total cost: `RSS = Σ(SOM_dev² + MAOM_dev² + POM.AGG_dev²)` per cluster
- **Constraints**:
  - MAOM/SOM ≥ 0.04 (`rate_ma < 0.04` triggers penalty)
  - LMWC/SOM ≤ 0.03 (`rate_lw > 0.03` triggers penalty)  
  - MIC/SOM ≤ 0.03 (`rate_mc > 0.03` triggers penalty)
- **Penalty Handling**: Invalid parameters return penalty matrix with 100s for all C pool columns
- **Parallel Processing**: Use `foreach` with `doParallel` for cluster-wise optimization (39-43 clusters)
- **Model Validation**: Check simulated C pools against observed values for each scenario
- **File Outputs**: 
  - Parameter sets: `./{cluster}_parset.csv`
  - Simulated pools: `./{cluster}_optim_sets.csv`

### 3.3 Site-specific Calibration
This final calibration step optimizes parameters for each individual site to achieve the highest possible accuracy at the finest spatial scale.

#### ⚙️ Workflow
- **Script**: `d01_src/rangelands_calibration_site.R`
- **Key Packages**: `rtop`, `deSolve`, `FME`, `foreach`, `doParallel`, `zoo`.
- **Method**: Individual site optimization using SCE-UA algorithm.
- **Input Data**:
  - **Site Configuration**: `d02_data/continental_input_sites_update.txt`
  - **Daily Forcing**: `d02_data/rangeland_daily_driving/` (365-day averaged cycles)

#### 🎯 Optimization Setup
- **Algorithm**: SCE-UA (Shuffled Complex Evolution - University of Arizona) for global optimization.
- **Parameters**: 13 key parameters from the `important_params_set6` set:
  - `param_pa`, `rate_pa`, `rate_break`, `rate_leach`, `rate_ma`, `matpot`, `eact_pl`, `eact_lb`, `cue_ref`, `tae_ref`, `kaff_des`, `param_p1`, `param_pi`
- **Initialization**: Site-specific measured C fractions:
  - POM = observed POM.AGG value
  - LMWC = 6 gC m⁻² (fixed)
  - AGG = 6 gC m⁻² (fixed)
  - MIC = 6 gC m⁻² (fixed)
  - MAOM = observed MAOM value
- **Forcing**: 100-year simulation using repeated 365-day averaged climate cycles from daily driving data.

#### Technical Implementation Details
- **Cost Function**: Minimizes absolute deviations for C pools:
  - POM+AGG deviation: `|POM_sim + AGG_sim - POM.AGG_obs|`
  - MAOM deviation: `|MAOM_sim - MAOM_obs|`
  - SOM deviation: `|SOM_sim - SOM_obs|`
  - Total cost: `J = POMAGG_dev + MAOM_dev + SOM_dev`
- **Constraints**: 
  - MAOM/SOM ≥ 0.4
  - LMWC/SOM ≤ 0.03
  - MIC/SOM ≤ 0.03
- **Equilibrium**: Uses `ode` solver with 100-year spin-up (36,500 days) to reach steady-state
- **Parallel Processing**: HPC cluster with 100 cores for simultaneous site optimization
- **Maximum Iterations**: 1,000 SCE-UA iterations per site

#### 📋 Evaluation Instructions

To perform site-specific calibration:

1. **Data Preparation**:
   - Load site configuration from `d02_data/continental_input_sites_update.txt`
   - Process daily driving data into 365-day averaged cycles
   - Extract site-specific parameters (`qmax`, `pH`) for each location

2. **Model Initialization**:
   - Set initial C pools using observed POM.AGG and MAOM values
   - Fix LMWC, AGG, MIC at 6 gC m⁻² each
   - Configure site-specific soil parameters

3. **Forcing Setup**:
   - Create 100-year forcing by repeating 365-day averaged cycles
   - Use `approxfun()` to create continuous forcing functions
   - Apply `rk4` integration method for numerical stability

4. **Optimization Execution**:
   - Run SCE-UA algorithm with parameter bounds from `selected_params`
   - Monitor convergence for each of the 993 sites
   - Handle optimization failures with penalty values

5. **Output Processing**:
   - Save optimized parameters and objective function values
   - Store results in RDS format: `sitebysite_benchmark_pars_outs_ode_100y_0526_init.rds`
   - Validate against observed C pools and fractions


### 📂 Input Data Structure

The following data source table summarizes the soil C fractions, forcing inputs, and soil properties used in the calibration process.

| Variable name | Description | Unit | Source |
| :--- | :--- | :--- | :--- |
| **Soil C Fractions** | | | |
| TOC | Total organic carbon | Mg C ha⁻¹ | Viscarra Rossel et al. (2019) |
| POC | Particulate organic carbon (50–2000 µm) | Mg C ha⁻¹ | Viscarra Rossel et al. (2019) |
| MAOC | Mineral-associated organic carbon (<50 µm) | Mg C ha⁻¹ | Viscarra Rossel et al. (2019) |
| **Forcing Inputs** | | | |
| $\theta$ | Soil moisture (0–7 cm, 7–28 cm) | m³ m⁻³ | Hersbach et al. (2020) (ERA5) |
| $T_{soil}$ | Soil temperature (0–7 cm, 7–28 cm) | °C | Hersbach et al. (2020) (ERA5) |
| NPP | Net primary productivity (aggregated to daily) | g C m⁻² day⁻¹ | NPP (2020) |
| **Soil Properties** | | | |
| pH | Soil pH | --- | Viscarra Rossel et al. (2015) |
| Clay | Clay content | % | Viscarra Rossel et al. (2015) |
| Silt | Silt content | % | Viscarra Rossel et al. (2015) |
| BD | Bulk density | kg m⁻³ | Viscarra Rossel et al. (2015) |
| AWC | Available water capacity | mm m⁻¹ | Viscarra Rossel et al. (2015) |
| $Q_{max}$ (frontier) | MAOC sorption capacity (new approach) | g C m⁻² | Viscarra Rossel et al. (2024) |

The model requires two types of input files: a site configuration file and daily driving variable files.

#### 1. Site Configuration File
- **File**: `d02_data/continental_input_sites_update.txt`
- **Purpose**: Provides initialization data and site-specific soil properties for each of the 993 rangeland sites.
- **Columns**:
  - `site`: Unique identifier for the rangeland location.
  - `x`, `y`: Longitude and Latitude coordinates.
  - `SOM`: Total Soil Organic Matter (mg C cm⁻³).
  - `MAOM`, `POM.AGG`: Initial carbon pool states.
  - `SoilTMP-C`, `SoilMoi-m3m3`: Reference soil temperature and moisture.
  - `NPP.gC.m2.d`: Reference Net Primary Productivity.
  - `qmax.gC.m2`: Maximum sorption capacity ($Q_{max}$).
  - `pH_CaCl2`: Soil pH level.
  - `BD.mg.cm3`: Bulk Density.
  - `depth`: Soil depth (m).

#### 2. Daily Driving Data
- **Location**: `d02_data/rangeland_daily_driving/`
- **Files**: Named by site ID (e.g., `DECCW0061.txt`).
- **Purpose**: Provides daily time-series forcing data for model simulation.
- **Columns**:
  - `Year`, `Month`, `Day`: Temporal identifiers.
  - `forc_npp`: Daily Net Primary Productivity ($g C m^{-2} day^{-1}$).
  - `forc_sw`: Daily Soil Moisture ($m^3 m^{-3}$).
  - `forc_st`: Daily Soil Temperature ($°C$).

> [!NOTE]
> **Study sites**: 993 locations across Australian rangelands, which are a subset of Lee et al. (2021).


---

## 🔎 References

- Abramoff, Rose Z., et al. "Improved global-scale predictions of soil carbon stocks with Millennial Version 2." Soil Biology and Biochemistry 164 (2022): 108466.
- Pianosi, F., & Wagener, T. (2018). "Distribution-based sensitivity analysis from a generic input-output sample." Environmental Modelling & Software. [Website](https://safetoolbox.github.io/Pawn.html)
- Skøien et al. (2014) "rtop: An R package for interpolation of data with a variable spatial support." Computers & Geosciences. [CRAN](https://cran.r-project.org/web/packages/rtop/index.html) doi:10.1016/j.cageo.2014.02.009.
- Viscarra Rossel, R. A., Webster, R., Zhang, M., & Shen, Z. (2024). "How much organic carbon could the soil store? The carbon sequestration potential of Australian soil." Global Change Biology.

---

## 📂 Appendix

### 🏗️ Millennial Model Version 2 (MV2)

#### Daily Soil C Turnover

```text
══════════════════════════════════════════════════════════════════════════════════
              MILLENNIAL MODEL Version 2 (MV2): DAILY SOIL C TURNOVER
══════════════════════════════════════════════════════════════════════════════════

C INPUT
┌─────────────────────────────┐
│ NPP (forc_npp)              │
└──────────────┬──────────────┘
               │
     ┌─────────┴─────────┐
     │                   │
param_pi             (1 - param_pi)
(to POM)                (to LMWC)
     │                   │
     ▼                   ▼
┌───────────────┐   ┌───────────────┐
│      POM      │   │      LMWC     │
└───────┬───────┘   └───────┬───────┘
        │                   │
        │ f_PO_AG           │ f_LM_MA          f_LM_leach
        │ (agg. form)       │ (adsorb)         (loss)
        ▼                   │                  ▼
┌───────────────┐   ┌───────┴───────┐   ┌───────────────┐
│      AGG      │   │     MAOM      │   │   LEACHING    │
└───┬─────┬─────┘   └───────┬───────┘   └───────────────┘
    │     │ f_MA_AG         │ f_MA_LM
    │     │ (agg. form)     │ (desorb)
    │     │                 │
    │ f_AG_break×param_pa   │
    │ (to POM)              │
    │                       │
    │ f_AG_break×(1-param_pa)
    │ (to MAOM)             │
    │                       │
    ▼                       ▼
           ┌──────────────────────────┐
           │           MIC            │
           │    (microbial biomass)   │
           └───────────┬──────────────┘
                       │
        ┌──────────────┼──────────────┐
        │ f_MB_turn×(1-param_pb)      │ f_MB_atm = f_LM_MB × (1 - CUE)
        │ (to LMWC)                   │ (respiration → CO₂)
        │                             │
        ▼                             ▼
   ┌───────────────┐           ┌───────────────┐
   │      LMWC     │           │      CO₂      │
   └───────────────┘           └───────────────┘
                 │
                 │ f_MB_turn×param_pb
                 │ (necromass → MAOM)
                 ▼
          ┌───────────────┐
          │      MAOM     │
          └───────────────┘

══════════════════════════════════════════════════════════════════════════════════
ENVIRONMENTAL MODIFIERS
- scalar_wd = (forc_sw / porosity)^0.5                      (moisture)
- scalar_wb = exp(λ·-matpot)·(kamin + (1-kamin)·((porosity - forc_sw)/porosity)^0.5)·scalar_wd
- vmax_pl = alpha_pl·exp(-eact_pl / (R·(T+273.15)))         (POM → LMWC)
- vmax_lb = alpha_lb·exp(-eact_lb / (R·(T+273.15)))         (LMWC → MIC)
- CUE = cue_ref - cue_t·(forc_st - tae_ref)                 (growth efficiency)

══════════════════════════════════════════════════════════════════════════════════
KEY FLUX DEFINITIONS
- f_PO_LM    = vmax_pl·scalar_wd·POM·MIC / (kaff_pl + MIC)
- f_PO_AG    = rate_pa·scalar_wd·POM
- f_AG_break = rate_break·scalar_wd·AGG
- f_LM_leach = rate_leach·scalar_wd·LMWC
- kaff_lm    = exp(-p1·pH - p2)·kaff_des   (sorption affinity)
- f_LM_MA    = scalar_wd·kaff_lm·LMWC·(1 - MAOM/param_qmax)
- f_MA_LM    = kaff_des·MAOM/param_qmax
- f_LM_MB    = vmax_lb·scalar_wb·MIC·LMWC / (kaff_lb + LMWC)
- f_MB_turn  = rate_bd·MIC²
- f_MA_AG    = rate_ma·scalar_wd·MAOM
- f_MB_atm   = f_LM_MB·(1 - CUE)

══════════════════════════════════════════════════════════════════════════════════
POOL UPDATE EQUATIONS (daily)
- dPOM  = NPP·param_pi + f_AG_break·param_pa - f_PO_AG - f_PO_LM
- dLMWC = NPP·(1-param_pi) + f_PO_LM + f_MA_LM + f_MB_turn·(1-param_pb)
          - f_LM_leach - f_LM_MA - f_LM_MB
- dAGG  = f_PO_AG + f_MA_AG - f_AG_break
- dMIC  = f_LM_MB - f_MB_turn - f_MB_atm
- dMAOM = f_LM_MA + f_MB_turn·param_pb + f_AG_break·(1-param_pa)
          - f_MA_LM - f_MA_AG

══════════════════════════════════════════════════════════════════════════════════
NOTES / CHECKS
- State vector: [POM, LMWC, AGG, MIC, MAOM]
- Stops if any |Δ| > 100 or any updated pool < 0 (mass-balance sanity checks).
- CO₂ loss = f_MB_atm (plus dissolved C lost via f_LM_leach).
══════════════════════════════════════════════════════════════════════════════════
```

### ⚙️ Model Parameters

The table below lists the initial parameters used in the Millennial v2 model, including their definitions, units, and ranges (lower and upper bounds).

| ID | Variable | Definition | Units | Default values | Lower | Upper |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 1 | $P_i$ | Proportion of C input allocated to POM | --- | 0.66 | 0 | 1 |
| 2 | $P_a$ | Proportion of aggregate-C breakdown allocated to POM | --- | 0.40 | 0 | 1 |
| 3 | $K_{pl}$ | Half-saturation constant of POM decomposition to LMWC | g C m⁻² | 8617 | 5000 | 15000 |
| 4 | $\alpha_{pl}$ | Pre-exponential constant for maximum rate of POM decomposition | g C m⁻² | 2.60E+12 | 1.25E+12 | 3.75E+12 |
| 5 | $E\alpha_{pl}$ | Activation energy for maximum rate of POM decomposition | J mol⁻¹ | 63339 | 50000 | 70000 |
| 6 | Porosity | Total porosity | mm³ mm⁻³ | 0.62 | 0.3 | 0.9 |
| 7 | $K_{pa}$ | Rate of aggregate formation from POM | d⁻¹ | 0.012 | 0 | 0.85 |
| 8 | $K_b$ | Breakdown rate of macroaggreate | d⁻¹ | 0.026 | 0 | 0.1 |
| 9 | $P_b$ | Partitioning of necromass to MAOM and LMWC | --- | 0.52 | 0 | 1 |
| 10 | $K_l$ | Leaching rate of LMWC | d⁻¹ | 0.0015 | 0.00075 | 0.00225 |
| 11 | $K_{ld}$ | Desorption rate for LMWC and MIC | mg C L⁻¹ d⁻¹ | 0.02 | 0.012 | 0.036 |
| 12 | $P_1$ | Coefficient for estimating the binding affinity for LMWC sorption | --- | 0.078 | 0.05 | 0.5 |
| 13 | $P_2$ | Coefficient for estimating the binding affinity for LMWC sorption | --- | 0.216 | 0.108 | 0.324 |
| 14 | $K_{lb}$ | Half saturation constant for microbial uptake | g C m⁻² | 710.8 | 100 | 1000 |
| 15 | $\alpha_{lb}$ | Pre-exponential constant for maximum rate of microbial uptake | g C m⁻² | 1.20E+12 | 1.00E+12 | 3.90E+12 |
| 16 | $E\alpha_{lb}$ | Activation energy for the potential LMWC uptake rate | J mol⁻¹ | 60428 | 50000 | 75000 |
| 17 | $\lambda$ | Dependence of respiration rate on matric potential | kPa⁻¹ | 2.10E-04 | 1.05E-4 | 3.15E-4 |
| 18 | $K_{a,min}$ | Minimum relative rate in saturated soil | --- | 0.2 | 0.1 | 0.3 |
| 19 | $\phi$ | Matric potential | kPa | 15 | 7.5 | 22.5 |
| 20 | $K_{bd}$ | Microbial death rate | m² g C⁻¹ d⁻¹ | 0.0044 | 0.0018 | 0.0054 |
| 21 | $K_{ma}$ | Rate of aggregate formation from MAOM | d⁻¹ | 0.0052 | 0.001 | 0.1 |
| 22 | $CUE_{ref}$ | Reference carbon use efficiency | --- | 0.53 | 0.3 | 0.9 |
| 23 | $CUE_T$ | Carbon use efficiency dependence on temperature | °C⁻¹ | 0.012 | 0.006 | 0.018 |
| 24 | $T_{ae-ref}$ | Reference temperature for temperature control on CUE | °C | 15 | 7.5 | 22.5 |
