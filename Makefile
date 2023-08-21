include Makefile.SRCS


###################################################

F90 = ifort
DEBUG_FLAGS = -init=snan,arrays -check all -check noarg_temp_created -debug-parameters all \
              -traceback -ftrapuv -g -fpe0 -O0
#F90FLAGS = -qopenmp -O3 -r8 -g -ftz $(shell nf-config --cflags)
#F90FLAGS = -O3 -r8 -g -ftz $(shell nf-config --fflags)
F90FLAGS = -O3 -g -ftz $(shell nf-config --fflags)

LLIB = $(shell nf-config --flibs) 
LDFLAGS =  $(F90FLAGS) $(LLIB)

#PROG = NORTRIP_multiroad_combined_v2-el7
PROG = NORTRIP_multiroad_combined_v2-r8

NILU_DIR = NORTRIP_multiroad/NILU
NORTRIP_DIR = NORTRIP
NORTRIPMULTI_DIR = NORTRIP_multiroad
OSPM_DIR = NORTRIP/OSPM
UEMEP_DIR = NORTRIP_multiroad/uEMEP

%.o: $(NILU_DIR)/%.for
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: $(NILU_DIR)/%.f90
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: $(NORTRIP_DIR)/%.f90
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: $(NORTRIPMULTI_DIR)/%.f90
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: $(OSPM_DIR)/%.f90
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: $(UEMEP_DIR)/%.f90
	$(F90) $(F90FLAGS) -c $< -o $@

%.o: %.f90
	$(F90) $(F90FLAGS) -c $< -o $@
###	$(F90) $(F90FLAGS) $(DEBUG_FLAGS) -c $< -o $@

all:  $(PROG)

$(PROG): dependencies

include dependencies

$(PROG): $(FOBJ)
	$(F90) -o $@ $(FOBJ) $(LDFLAGS)

all:  $(PROG)

clean: 
	rm -f $(PROG) $(FOBJ)
