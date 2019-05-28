#
# Makefile for WRF and MPAS example for a simple physics scheme
#

FC              = gfortran

scheme		= kessler
case		= wm_example
exec		= kess_test

# My dirs
BLDDIR := $(shell pwd -P)
MKFILE_PATH := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
ROOT = $(shell dirname $(shell dirname $(MKFILE_PATH)))
CAPGEN := $(ROOT)/scripts/ccpp_capgen.py
CAPGEN_ARGS := --host-files $(MKFILE_PATH)/$(case)_host.meta,$(MKFILE_PATH)/$(case)_mod.meta
CAPGEN_ARGS += --scheme-files $(MKFILE_PATH)/$(scheme).meta
CAPGEN_ARGS += --suites $(MKFILE_PATH)/$(case)_suite.xml
CAPGEN_ARGS += --generate-host-cap --host-name $(exec)
CAPGEN_ARGS += --verbose --verbose
SRCS_META := $(MKFILE_PATH)/$(case)_host.meta
SRCS_META += $(MKFILE_PATH)/$(case)_mod.meta
SRCS_META += $(MKFILE_PATH)/$(scheme).meta
SRCS_XML := $(MKFILE_PATH)/$(case)_suite.xml

INCFLAG = -I
INCPATH += $(INCFLAG)$(BLDDIR) $(INCFLAG)$(MKFILE_PATH)
FCFLAGS += -g

# SOURCE FILES
SRCS_F90 = $(MKFILE_PATH)/$(scheme).F90 $(MKFILE_PATH)/$(case)_host.F90
SRCS_F90 += $(MKFILE_PATH)/$(case)_mod.F90
OBJS_F90 = $(scheme).o $(case)_mod.o $(case)_host.o

# Do we have generated CAP files?
$(eval CAPOBJS=$(shell if [ -f caplocal.txt ]; then cat caplocal.txt; fi))

.PHONY: all
all: $(exec)

# Generate CCPP cap files
capfiles.txt: $(SRCS_META) $(SRCS_XML)
	$(CAPGEN) $(CAPGEN_ARGS)

.PHONY: capobjs
capobjs: capfiles.txt
	$(shell $(MKFILE_PATH)/mkcaplocal.sh $(BLDDIR)/capfiles.txt)
	$(eval CAPOBJS=$(shell cat caplocal.txt))

# Human written
$(exec): capobjs $(CAPOBJS) $(OBJS_F90)
	$(FC) $(FCFLAGS) -o $@ $(CAPOBJS) $(OBJS_F90)

$(scheme).o: ccpp_kinds.o $(MKFILE_PATH)/$(scheme).F90
	$(FC) -c $(INCPATH) $(FCFLAGS) $(MKFILE_PATH)/$(scheme).F90

$(case)_mod.o: ccpp_kinds.o $(MKFILE_PATH)/$(case)_mod.F90
	$(FC) -c $(INCPATH) $(FCFLAGS) $(MKFILE_PATH)/$(case)_mod.F90

$(case)_host.o: $(exec)_ccpp_cap.o ccpp_kinds.o
$(case)_host.o: $(MKFILE_PATH)/$(case)_host.F90
	$(FC) -c $(INCPATH) $(FCFLAGS) $(MKFILE_PATH)/$(case)_host.F90

# CCPP generated
ccpp_kinds.F90: capobjs

$(exec)_ccpp_cap.F90: capobjs

ccpp_$(case)_suite_cap.F90: capobjs

ccpp_kinds.o: ccpp_kinds.F90
	$(FC) -c $(INCPATH) $(FCFLAGS) ccpp_kinds.F90

$(exec)_ccpp_cap.o: ccpp_kinds.o
$(exec)_ccpp_cap.o: $(case)_mod.o ccpp_$(case)_suite_cap.o
$(exec)_ccpp_cap.o: $(exec)_ccpp_cap.F90
	$(FC) -c $(INCPATH) $(FCFLAGS) $(exec)_ccpp_cap.F90

ccpp_$(case)_suite_cap.o: ccpp_kinds.o ccpp_$(case)_suite_cap.F90
	$(FC) -c $(INCPATH) $(FCFLAGS) ccpp_$(case)_suite_cap.F90

.PHONY: clean
clean:
	rm -f *.o *.mod $(exec) capfiles.txt caplocal.txt \
              $(shell if [ -f capfiles.txt ]; then cat capfiles.txt; fi)
