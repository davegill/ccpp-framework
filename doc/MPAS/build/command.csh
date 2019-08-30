#!/bin/csh

../../../scripts/ccpp_capgen.py \
     --host-files ../mpas_host.meta,../mpas_mod.meta \
     --scheme-files ../bl_ysu.meta \
     --suites ../mpas_suite.xml \
     --generate-host-cap \
     --host-name mpas


