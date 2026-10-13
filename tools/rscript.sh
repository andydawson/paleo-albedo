#!/usr/bin/env bash
# OPTIONAL, Linux only: run an R script with the environment of the anchored runs.
#
# Not needed to run the pipeline, which needs only `Rscript` from the repository root on
# any system. This helper matters only for the byte-for-byte regression checks of the
# single-config migration, on this machine (micromamba environment paleo-rf).
#
# Why: calling the env's Rscript binary directly leaves PROJ_DATA/GDAL_DATA unset (they are
# only set by `micromamba activate`), which breaks script 1's elevation lookup and the sf
# map helpers; and predict.gam reproduces byte for byte only at the BLAS thread count of
# the anchored runs (8). This wrapper fixes both, so comparison runs are like for like.
#
# Usage: tools/rscript.sh <script.R> [args...]
#   THREADS=8 by default; CORES (a taskset list, e.g. 0-7) optional.
set -euo pipefail
ENV=${PALEO_RF_ENV:-/home/expert/micromamba/envs/paleo-rf}
THREADS=${THREADS:-8}
export PROJ_DATA="$ENV/share/proj" GDAL_DATA="$ENV/share/gdal"
export OPENBLAS_NUM_THREADS=$THREADS OMP_NUM_THREADS=$THREADS MKL_NUM_THREADS=$THREADS
export RUN_CORES=${CORES:-"(unpinned)"}
if [[ -n "${CORES:-}" ]]; then
  exec taskset -c "$CORES" "$ENV/bin/Rscript" "$@"
else
  exec "$ENV/bin/Rscript" "$@"
fi
