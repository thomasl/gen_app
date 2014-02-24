## Gen_app makefile
##
## - this has been adapted from a generic makefile,
##   so it's a bit more elaborate than needed
##
## We assume gnumake here
## 
## See ERLC_OPTS for some choices:
## - debug_info
## - export_all
## - uses smart_exceptions parse transform

GEN_APP_INC_DIR=../include
GEN_APP_SRC_DIR=../src

UTIL_DIR=../smart_exceptions/devel/ebin

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
	gen_app \
	gen_sup \
	gen_sup_bridge \
	gen_table_owner

GEN_APP_SRC := $(patsubst %, %.erl, $(GEN_APP_MODS))
OM_SRC=$(GEN_APP_SRC)
ERL_SRC= $(BEHAVIOUR_SRC) $(OM_SRC)

# OBJS are all .beam files

GEN_APP_OBJS := $(patsubst %.erl,../ebin/%.beam,$(GEN_APP_SRC))
BEHAV_OBJS := $(patsubst %.erl,../ebin/%.beam,$(BEHAVIOUR_SRC))

all:	behaviours objs ../ebin/gen_app.app

behaviours: $(BEHAV_OBJS)

objs:	$(GEN_APP_OBJS)

../ebin/gen_app.app:
	$(LISTIFY) $(GEN_APP_MODS) > ../ebin/gen-app.tmp
	$(BIGSUBST) \
	  -x %GEN_APP_MODS% ../ebin/gen-app.tmp \
	  -e %GEN_APP_VSN% $(shell cat ../vsn.mk) \
	  -e %GEN_APP_DEPS% $(shell $(LISTIFY) $(GEN_APP_DEPS)) \
	  -e %GEN_APP_PROCS% $(shell $(LISTIFY) $(GEN_APP_PROCS)) \
	  -i gen_app.app.src \
	  > ../ebin/gen_app.app
	rm -f ../ebin/gen-app-mods.tmp

clean: clean_bin 

clean_bin: clean_beam clean_app

clean_beam:
	rm -f ../ebin/*.beam

clean_app:
	rm -f ../ebin/*.app

