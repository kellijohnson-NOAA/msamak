#
# Makefile for notes
#

all : notes.pdf

notes.pdf : notes.md
	pandoc notes.md --filter pandoc-citeproc -o $@
