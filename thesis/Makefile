REFS = references.bib

.PHONY: all clean

all: thesis.html thesis.pdf

clean:
	$(RM) thesis.html thesis.pdf

thesis.html: thesis.md thesis_01.md thesis_02.md thesis_03.md thesis_04.md thesis_05.md thesis_06.md thesis_07.md thesis_a1.md thesis_a2.md thesis_r.md

WD := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
include ~/git/dogx/template/settings.mk
