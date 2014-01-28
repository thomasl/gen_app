## Gen_app makefile [for examples]
##
## - this is basically a copy of src/Makefile
##   except with other modules

GEN_APP_INC_DIR=../include
GEN_APP_SRC_DIR=../examples

## UNFINISHED
## - weird paths, should be fixed

BASEDIR=$(HOME)/git
UTIL_DIR=$(BASEDIR)/smart_exceptions/devel/ebin

# NB: We currently export everything, to simplify debugging

ERLC=erlc
ERLC_OPTS= \
	+debug_info \
	+nowarn_unused_vars \
	+strict_record_tests \
	+warn_export_vars \
	-pa ../ebin -pa $(UTIL_DIR) \
	-I../ebin \
	-I$(UTIL_DIR) \
	-I$(GEN_APP_INC_DIR) \
	-o ../ebin \
	+'{parse_transform, smart_exceptions}' \
	+export_all

#

LISTIFY=perl ../bin/listify.pl
BIGSUBST=perl ../bin/bigsubst.pl

# rules to make every beam file
# $< is the name of the .erl file

../ebin/%.beam:	%.erl
	$(ERLC) $(ERLC_OPTS) $<

HRL_SRC= \

BEHAVIOUR_SRC= \

GEN_APP_MODS= \
	ex_ssl

GEN_APP_SRC := $(patsubst %, %.erl, $(GEN_APP_MODS))

# OBJS are all .beam files

GEN_APP_OBJS := $(patsubst %.erl,../ebin/%.beam,$(GEN_APP_SRC))
BEHAV_OBJS := $(patsubst %.erl,../ebin/%.beam,$(BEHAVIOUR_SRC))

all:	behaviours objs

behaviours: $(BEHAV_OBJS)

objs:	$(GEN_APP_OBJS)

clean: clean_bin 

clean_bin: clean_beam clean_app

clean_beam:
	rm -f ../ebin/*.beam

clean_app:
	rm -f ../ebin/*.app

