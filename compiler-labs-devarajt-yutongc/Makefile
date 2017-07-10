# Copyright 2011 Stefan Nilsson. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

VERSION = 1.3

DIR = se/kth/csc

NICDIR := $(DIR)/nic
NICFILES = $(shell find $(NICDIR) | grep -E "\.java$$")
NICCLASSES = nicclasses
NICMAIN = se.kth.csc.nic.NIC
NICJAR = nic.jar

NASDIR := $(DIR)/nas
NASFILES = $(shell find $(NASDIR) | grep -E "\.java$$")
NASCLASSES = nasclasses
NASMAIN = se.kth.csc.nas.NAS
NASJAR = nas.jar

all: nic nas

$(NICCLASSES)/nicman.mf:
	mkdir -p $(NICCLASSES)
	printf "%s\n" "Implementation-Version: $(VERSION)" > $(NICCLASSES)/nicman.mf
	printf "%s\n" "Specification-Version: $(VERSION)" >> $(NICCLASSES)/nicman.mf
	printf "%s\n" "Main-Class: se.kth.csc.nic.NIC" >> $(NICCLASSES)/nicman.mf

nic: nic.jar
nic.jar: $(NICFILES) $(NICCLASSES)/nicman.mf
	mkdir -p $(NICCLASSES)
	javac -d $(NICCLASSES) $(NICFILES)
	cd $(NICCLASSES); jar cfm $(NICJAR) nicman.mf se; mv $(NICJAR) ..


$(NASCLASSES)/nasman.mf:
	mkdir -p $(NASCLASSES)
	printf "%s\n" "Implementation-Version: $(VERSION)" > $(NASCLASSES)/nasman.mf
	printf "%s\n" "Specification-Version: $(VERSION)" >> $(NASCLASSES)/nasman.mf
	printf "%s\n" "Main-Class: se.kth.csc.nas.NAS" >> $(NASCLASSES)/nasman.mf

nas: nas.jar
nas.jar: $(NASFILES) $(NASCLASSES)/nasman.mf
	mkdir -p $(NASCLASSES)
	javac -d $(NASCLASSES) $(NASFILES)
	cd $(NASCLASSES); jar cfm $(NASJAR) nasman.mf se; mv $(NASJAR) ..

check: all
	java -classpath nasclasses se.kth.csc.nas.test.TestNAS

# Remove all compiled files.
clean:
	rm -f *.jar
	rm -rf $(NICCLASSES) $(NICDIR)/*.class $(NICDIR)/*/*.class
	rm -rf $(NASCLASSES) $(NASDIR)/*.class $(NASDIR)/*/*.class

totalclean: clean
	cd doc/manual; make clean
	find . -name "*~" -delete


