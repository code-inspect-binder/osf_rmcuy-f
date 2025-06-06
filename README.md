# Executable Environment for OSF Project [rmcuy](https://osf.io/rmcuy/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Strategies for integrating disparate social information

**Project Description:**
> Social information use is widespread in the animal kingdom, helping individuals rapidly acquire useful knowledge and adjust to novel circumstances. In humans, the highly interconnected world provides ample opportunities to benefit from social information but also requires navigating complex social environments with people holding disparate or conflicting views. It is, however, still largely unclear how people integrate information from multiple social sources that (dis)agree with them, and among each other. We address this issue in three steps. First, we present a judgment task in which participants could adjust their judgments after observing the judgments of three peers. We experimentally varied the distribution of this social information, systematically manipulating its variance (extent of agreement among peers) and its skewness (peer judgments clustering either near or far from the participant’s). As expected, higher variance among peers reduced their impact on behaviour. Importantly, observing a single peer confirming an individual’s judgment markedly decreased the influence of other—more distant—peers. Second, we develop a framework for modelling the cognitive processes underlying the integration of disparate social information, combining Bayesian updating with simple heuristics. Our model accurately accounts for observed adjustment strategies and reveals that people particularly heed social information that confirms personal judgments. Moreover, the model exposes strong inter-individual differences in strategy use. Third, using simulations, we explore the possible implications of identified strategies for belief updating more broadly. They show how confirmation-based weighting can hamper the influence of disparate social information, exacerbate filter bubble effects and deepen group polarization. Overall, our results clarify what aspects of the social environment are, and are not, conducive to changing people’s minds.

**Original OSF Page:** [https://osf.io/rmcuy/](https://osf.io/rmcuy/)

---

**Important Note:** The contents of the `rmcuy_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_rmcuy-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_rmcuy-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `rmcuy_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-rmcuy-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-rmcuy-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_rmcuy](https://github.com/code-inspect-binder/osf_rmcuy)

