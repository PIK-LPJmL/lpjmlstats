#!/usr/bin/env bash

# Usage: ilamb_run_cmd.sh <ilamb_dir>

ILAMB_DIR="$1"

module use --append /p/projects/poem/davidho/ilamb/modulefile
module load ILAMB

cd "$ILAMB_DIR"
export ILAMB_ROOT=$PWD

# Run ILAMB
srun --qos=priority --mem=64G ilamb-run --config sample.cfg --model_root "$ILAMB_ROOT/MODELS/" --regions global
