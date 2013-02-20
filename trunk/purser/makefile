# Makefile

.PHONY: all post-build

all:
	cd base/Debug && $(MAKE) all
	cd daemon/Debug && $(MAKE) all
	cd protocol/Debug && $(MAKE) all
	cd receiver/Debug && $(MAKE) all
	cd responder/Debug && $(MAKE) all
	cd sender/Debug && $(MAKE) all

post-build:
	cd base/Debug && $(MAKE) post-build
	cd daemon/Debug && $(MAKE) post-build
	cd protocol/Debug && $(MAKE) post-build
	cd receiver/Debug && $(MAKE) post-build
	cd responder/Debug && $(MAKE) post-build
	cd sender/Debug && $(MAKE) post-build
	
clean:
	cd base/Debug && $(MAKE) clean		
	cd daemon/Debug && $(MAKE) clean
	cd protocol/Debug && $(MAKE) clean
	cd receiver/Debug && $(MAKE) clean
	cd responder/Debug && $(MAKE) clean
	cd sender/Debug && $(MAKE) clean
