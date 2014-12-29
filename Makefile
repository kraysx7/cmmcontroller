#  File:	 GNU Makefile
#  Author:	 Ilya Troshkov
#  Created:	 08.05.2014 13:14:34

VSN = 1.0

INSTALL_DIR=/srv/cmmcontroller

######################################################################

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl, ebin/%.beam, $(ERL_FILES))

######################################################################

all: $(BEAM_FILES)

install: all
	@[ -n "$(INSTALL_DIR)" ] || (echo "Set DESTDIR before running the install target."; false)
	install -d $(INSTALL_DIR)/ebin
	install -d $(INSTALL_DIR)/priv
	install -m 644 src/*.app ebin/
	install -m 644 ebin/* $(INSTALL_DIR)/ebin


ebin/%.beam: src/%.erl ebin
	erlc -o ebin $<

ebin:
	mkdir -p ebin

priv/bin : 
	mkdir -p priv/bin

clean:
	rm -f $(BEAM_FILES)

echo-version:
	@echo $(VSN)
