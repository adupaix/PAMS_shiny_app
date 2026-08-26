# syntax=docker/dockerfile:1

FROM rocker/geospatial:4.4.1
# NOTE: rocker/geospatial (built on rocker/verse) ships precompiled sf,
# terra, stars and their GDAL/GEOS/PROJ system libraries — avoids compiling
# sf from source, which is by far the slowest step in a naive spatial build.

# ── 1. Additional system libraries not covered by rocker/geospatial ─────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libglpk-dev \
    libicu-dev \
    git \
    && rm -rf /var/lib/apt/lists/*
# libglpk-dev  : required for igraph (SLRtools dependency) — see Notes above
# libicu-dev   : required for stringi (SLRtools dependency)
# git          : safety net for devtools::install_github() source installs

# ── 2. Shiny + app-level CRAN packages ───────────────────────────────────────
# NOTE: rocker/geospatial does not include the shiny package itself.
RUN R -e "install.packages('shiny', repos = 'https://cloud.r-project.org')"

# ── 3. SLRtools' full CRAN dependency tree (from its DESCRIPTION Imports) ───
# NOTE: installed explicitly rather than assuming rocker/verse already has
# them — several (httr, igraph, osmdata, readxl, RColorBrewer, stringdist,
# here, arrow, jsonlite, magrittr, brio) are not part of core tidyverse.
RUN R -e "install.packages(c( \
    'arrow', 'brio', 'here', 'httr', 'igraph', 'jsonlite', 'magrittr', \
    'maps', 'osmdata', 'RColorBrewer', 'readxl', 'stringdist', 'stringi', \
    'bs4Dash', 'ggVennDiagram', 'devtools' \
    ), repos = 'https://cloud.r-project.org')"
# NOTE: dplyr, ggplot2, purrr, sf, stringr, tibble, tidyr already present
# in rocker/geospatial's tidyverse + spatial base layers.

# ── 4. GitHub-only packages (SLRtools' own Imports + app-level extras) ──────
# NOTE: built into the image, not re-installed at runtime as global.R's
# `if (!'X' %in% installed.packages())` check would otherwise trigger.
RUN R -e "devtools::install_github('yutannihilation/ggsflabel')" \
 && R -e "devtools::install_github('FRBCesab/rbibtools')" \
 && R -e "devtools::install_github('ricardo-bion/ggradar')"

RUN --mount=type=secret,id=PAT_GITHUB,env=PAT_GITHUB \
    R -e "devtools::install_github('adupaix/SLRtools')"


# ── 5. Copy app code (secrets are NOT copied — see Space "Secrets" setting) ─
WORKDIR /srv/shiny-app
COPY . .

# ── 6. Hugging Face Spaces networking requirements ───────────────────────────
# NOTE: HF Spaces requires the container to listen on 0.0.0.0:7860, not
# Shiny's local default of 127.0.0.1:3838.
EXPOSE 7860
USER shiny

# ── 7. Launch ─────────────────────────────────────────────────────────────
CMD ["R", "-e", "shiny::runApp(appDir = '/srv/shiny-app', host = '0.0.0.0', port = 7860)"]

