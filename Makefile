################################################################################
# (c) Andreas Rossberg 2000-2013
#
# Makefile for building HaMLet. Supports (in alphabetical order):
# - Alice ML            (make with-alice)
# - ML Kit              (make with-mlkit)
# - MLton               (make with-mlton)
# - Moscow ML           (make with-mosml)
# - Poly/ML             (make with-poly)
# - SML of New Jersey   (make with-smlnj)
# - SML#                (make with-smlsharp)
#
# A generic file containing all modules for compilation with other systems
# can be generated with:
#   make hamlet-bundle.sml
################################################################################

################################################################################
# Configuration (you might want to edit here)

# Set the installation directory (Unix only):
INSTALLDIR = /usr/local/hamlet

# For Poly/ML, you might want to set the library path if it is not in std place:
POLY_LIBDIR = /usr/local/lib/

# For DOS-based systems with Cygwin, comment out the following lines:
# EXE = .exe
# BAT = .bat

# For portability...
ECHO = /bin/echo

################################################################################
# System-specific definitions

EXT_alice = alc
EXT_mlkit = sml
EXT_mlton = sml
EXT_mosml = ui
EXT_poly  = sml
EXT_smlnj = sml
EXT_smlsharp = sml

FIXES_alice = 
FIXES_mlkit = String Real
FIXES_mlton = 
FIXES_mosml = Real  # for Moscow < 2.10: Large Word Char CharVector String Real ListPair TextIO OS
FIXES_poly  = 
FIXES_smlnj = Word Char String ListPair TextIO OS
FIXES_smlsharp = Word CommandLine

with-alice:  SYSTEM = alice
with-alice+: SYSTEM = alice
with-mlkit:  SYSTEM = mlkit
with-mlkit+: SYSTEM = mlkit
with-mlton:  SYSTEM = mlton
with-mlton+: SYSTEM = mlton
with-mosml:  SYSTEM = mosml
with-mosml+: SYSTEM = mosml
with-poly:   SYSTEM = poly
with-poly+:  SYSTEM = poly
with-smlnj:  SYSTEM = smlnj
with-smlnj+: SYSTEM = smlnj
with-smlsharp: SYSTEM = smlsharp
with-smlsharp+: SYSTEM = smlsharp

EXT   = ${EXT_${SYSTEM}}
FIXES = ${FIXES_${SYSTEM}}

################################################################################
# Default target

usage:
	@${ECHO}
	@${ECHO} "Please use one of the following targets to build HaMLet:"
	@${ECHO} "  make with-alice    (for Alice ML 1.4+)"
	@${ECHO} "  make with-mlkit    (for ML Kit 4.3+)"
	@${ECHO} "  make with-mlton    (for MLton 20010706+)"
	@${ECHO} "  make with-mosml    (for Moscow ML 2.10+)"
	@${ECHO} "  make with-poly     (for Poly/ML 5.0+)"
	@${ECHO} "  make with-smlnj    (for SML/NJ 110+)"
	@${ECHO} "  make with-smlsharp (for SML# 0.20+)"
	@${ECHO}

################################################################################
# Make dependencies

Makefile.depend: fix/Makefile.depend smlnj-lib/Makefile.depend syntax/Makefile.depend infrastructure/Makefile.depend parse/Makefile.depend elab/Makefile.depend eval/Makefile.depend exec/Makefile.depend lib/Makefile.depend compile-js/Makefile.depend main/Makefile.depend
	${ECHO} "# DO NOT EDIT!" >$@
	${ECHO} "# Generated from */Makefile.depend.in (${shell date -u})" >>$@
	cat $^ >>$@

%/Makefile.depend: %/Makefile.depend.in
	sed "s|^\([^#]\)|$*/\1|g;s| \([^.:]\)| $*/\1|g;s|[.][.]/||g" $< >$@

Makefile-%.depend: Makefile.depend
	sed "s/[$$][(]EXT[)]/${EXT_$*}/g;s/[$$][(]SYSTEM[)]/$*/g" $< >>$@

################################################################################
# Source files

FIXFILES = ${FIXES:%=fix/%-${SYSTEM}.sml}
LIBFILES := $(shell grep -h '[.]sml' smlnj-lib/smlnj-lib.cm smlnj-lib/ml-yacc-lib.cm | grep "^ ")
SRCFILES := $(shell grep -h '[.]sml' sources.cm | grep -v wrap- | grep -v fix/)
FILES = ${FIXFILES} ${LIBFILES:%=smlnj-lib/%} ${SRCFILES}
BOOTSTRAPFILES = ${filter-out main/Main.sml main/MAIN-sig.sml,${FILES}}
BASISFILES = $(shell grep -h '^use ' basis/all.sml | sed 's/use //g' | sed 's/[\"\;]//g')
BUNDLEFILE = hamlet-bundle.sml

hamlet.sml: ${BOOTSTRAPFILES} sources.cm
	@${ECHO} Generating $@
	@${ECHO} "(* DO NOT EDIT! *)" >$@
	@${ECHO} "(* Generated from sources.cm (${shell date}) *)" >>$@
	@${ECHO} >>$@
	@${ECHO} '${BOOTSTRAPFILES:%=\use "%";}' | tr "\\" "\n" >>$@

hamlet.mlb:
	@${ECHO} Generating $@
	@${ECHO} "(* DO NOT EDIT! *)" >$@
	@${ECHO} "(* Generated from sources.cm (${shell date}) *)" >>$@
	@${ECHO} >>$@
	@${ECHO} '$$(SML_LIB)/basis/basis.mlb${FILES:%=\\%}\main/wrap-${SYSTEM}.sml' | tr "\\" "\n" >>$@

################################################################################
# Create single file

.PHONY: ${BUNDLEFILE}-init

${BUNDLEFILE}: ${BUNDLEFILE}-init main/wrap-generic.dummy

${BUNDLEFILE}-init:
	@${ECHO} Generating $@
	@${ECHO} "(* DO NOT EDIT! *)" >${BUNDLEFILE}
	@${ECHO} "(* Generated from */Makefile.depend.in (${shell date}) *)" >>${BUNDLEFILE}

%.dummy: %.sml
	@${ECHO} >>${BUNDLEFILE}
	@${ECHO} "(****" $< "****)" >>${BUNDLEFILE}
	@${ECHO} >>${BUNDLEFILE}
	@cat $< >>${BUNDLEFILE}

Makefile-dummy.depend: Makefile.depend
	@${ECHO} "# DO NOT EDIT!" >$@
	@${ECHO} "# Generated from sources.cm (${shell date})" >>$@
	@sed "s/[$$][(]EXT[)]/dummy/g;s/[$$][(]SYSTEM[)]/generic/g" $< >>$@

-include Makefile-dummy.depend

################################################################################
# Alice ML

.PHONY: with-alice with-alice-rec with-alice+ with-alice-all clean-alice

with-alice: with-alice-rec with-alice+

with-alice-rec: ${FILES} Makefile-alice.depend
	alicec --no-warn-conventions --no-warn-unused-imports --dependency-file Makefile-alice.depend --recursive-compilation -c main/wrap-alice.sml -o main/wrap-alice.alc

# use this target when you want to build incrementally after changes
with-alice+: Makefile-alice.depend main/wrap-alice.alc hamlet-alice${BAT}
	alicelink --include ${PWD} main/wrap-alice -o hamlet.alc
	ln -f hamlet-alice${BAT} hamlet${BAT}

# use this target when you want to compile as a single component
with-alice-all: hamlet-all.sml hamlet-alice${BAT}
	alicec --no-warn-conventions -c $< -o hamlet.alc
	ln -f hamlet-alice${BAT} hamlet${BAT}

hamlet-alice${BAT}: sh/hamlet-alice.sh${BAT}
	${ECHO} "# DO NOT EDIT!" >$@
	${ECHO} "# Generated from $< (${shell date})" >>$@
	cat $< >>$@
	chmod +x $@

%.alc: %.sml
	alicec --no-warn-conventions --no-warn-unused-imports --dependency-file Makefile-alice.depend -c $< -o $@

-include Makefile-alice.depend

clean-alice:
	rm -f hamlet-alice
	rm -f *.alc */*.alc
	rm -f *.ozf */*.ozf

################################################################################
# ML Kit

.PHONY: with-mlkit with-mlkit+ clean-mlkit

with-mlkit: hamlet-mlkit${EXE}
	ln -f hamlet-mlkit${EXE} hamlet${EXE}

# ML Kit always builds incrementally
with-mlkit+: with-mlkit

hamlet-mlkit${EXE}: ${FIXES_mlkit:%=fix/%-mlkit.sml} ${FILES} main/wrap-mlkit.sml hamlet.mlb
	mlkit hamlet.mlb
	mv run hamlet-mlkit${EXE}

fix/%-mlkit.sml: fix/%.sml
	${ECHO} "(* DO NOT EDIT! *)" >$@
	${ECHO} "(* Generated from $< (${shell date}) *)" >>$@
	cat $< >>$@

clean-mlkit:
	rm -f hamlet-mlkit
	rm -f hamlet.mlb
	rm -rf MLB */MLB

################################################################################
# MLton

.PHONY: with-mlton with-mlton+ clean-mlton

with-mlton: hamlet-mlton${EXE}
	ln -f hamlet-mlton${EXE} hamlet${EXE}

# MLton does not support incremental build
with-mlton+: with-mlton

hamlet-mlton${EXE}: hamlet.mlb main/wrap-mlton.sml ${FILES}
	mlton -output hamlet-mlton${EXE} hamlet.mlb

clean-mlton:
	rm -f hamlet-mlton
	rm -f hamlet.mlb

################################################################################
# Moscow ML

.PHONY: with-mosml with-mosml+ clean-mosml

with-mosml: hamlet-mosml${EXE}
	ln -f hamlet-mosml${EXE} hamlet${EXE}

# use this target when you want to build incrementally after changes
with-mosml+: Makefile-mosml.depend main/wrap-mosml.ui
	mosmlc ${FILES:%.sml=%.uo} main/wrap-mosml.uo -o hamlet-mosml${EXE}
	ln -f hamlet-mosml${EXE} hamlet${EXE}

hamlet-mosml${EXE}: ${FIXES_mosml:%=fix/%-mosml.sml} ${FILES} main/wrap-mosml.sml
	mosmlc -toplevel ${FILES} main/wrap-mosml.sml -o hamlet-mosml${EXE}

fix/%-mosml.sml: fix/%.sml
	${ECHO} "(* DO NOT EDIT! *)" >$@
	${ECHO} "(* Generated from $< (${shell date}) *)" >>$@
	cat $< >>$@

fix/%.ui: fix/%.sml Makefile-mosml.depend
	DEPENDS=`grep "^$@ :" Makefile-mosml.depend` ;\
	mosmlc -toplevel $${DEPENDS/* [:]/} -c $<

%.ui: %.sml ${FIXES_mosml:%=fix/%-mosml.ui} Makefile-mosml.depend
	DEPENDS=`grep "^$@ :" Makefile-mosml.depend` ;\
	mosmlc -toplevel ${FIXFILES:%.sml=%.ui} $${DEPENDS/* [:]/} -c $<

-include Makefile-mosml.depend

clean-mosml:
	rm -f hamlet-mosml
	rm -f *.u[oi] */*u[oi]

################################################################################
# Poly/ML

.PHONY: with-poly with-poly+ clean-poly

with-poly: hamlet-poly${EXE}
	ln -f hamlet-poly${EXE} hamlet${EXE}

# Poly/ML does not support incremental build
with-poly+: with-poly

hamlet-poly${EXE}: main/wrap-poly.sml hamlet.sml ${FILES}
	poly <main/wrap-poly.sml
	polyc -o $@ hamlet.o

clean-poly:
	rm -f hamlet-poly
	rm -f hamlet.o

################################################################################
# SML of New Jersey

.PHONY: with-smlnj with-smlnj+ clean-smlnj

with-smlnj: hamlet-smlnj${BAT}
	ln -f hamlet-smlnj${BAT} hamlet${BAT}

# SML/NJ always builds incrementally
with-smlnj+: with-smlnj

hamlet-smlnj${BAT}: sh/hamlet-smlnj.sh${BAT} main/wrap-smlnj.sml ${FILES} sources.cm
	sml${BAT} <main/wrap-smlnj.sml && \
	${ECHO} "# DO NOT EDIT!" >$@ && \
	${ECHO} "# Generated from $< (${shell date})" >>$@ && \
	cat $< >>$@
	chmod +x $@

clean-smlnj:
	rm -f hamlet-smlnj
	rm -rf CM */CM
	rm -rf .cm */.cm

################################################################################
# SML#

.PHONY: with-smlsharp with-smlsharp+ clean-smlsharp

with-smlsharp: hamlet-smlsharp${EXE}
	ln -f hamlet-smlsharp${EXE} hamlet${EXE}

# SML# does not support incremental built
with-smlsharp+: with-smlsharp

hamlet-smlsharp${EXE}: main/wrap-smlsharp.sml hamlet.sml ${FIXES_smlsharp:%=fix/%-smlsharp.sml} ${FILES}
	smlsharp2exe.sh --xVMHeapSize=10240000 -I ${PWD} main/wrap-smlsharp.sml -o $@

fix/%-smlsharp.sml: fix/%.sml
	${ECHO} "(* DO NOT EDIT! *)" >$@
	${ECHO} "(* Generated from $< (${shell date}) *)" >>$@
	cat $< >>$@

clean-smlsharp:
	rm -f hamlet-smlsharp
	rm -f *.sme

################################################################################
# Installation

.PHONY: install

install:
	mkdir -p ${INSTALLDIR}
	mkdir -p ${INSTALLDIR}/basis
	install -m 444 basis/*.sml ${INSTALLDIR}/basis
	if ls hamlet-image.* ;\
	then \
	    install -m 444 hamlet-image.* ${INSTALLDIR} ;\
	fi
	install -m 555 hamlet ${INSTALLDIR}

################################################################################
# JavaScript compilation

.PHONY: clean-js distclean-js

js: basis.js

basis.js: compile-js/runtime.js ${BASISFILES:%=basis/%} hamlet
	@${ECHO} Generating $@
	@${ECHO} "// DO NOT EDIT!" >$@
	@${ECHO} "// Generated from compile-js/runtime.js and basis/*.sml (${shell date}) *)" >>$@
	@${ECHO} >>$@
	@${ECHO} "//// runtime.js ////" >>$@
	@${ECHO} >>$@
	@cat compile-js/runtime.js >>$@
	@${ECHO} >>$@
	@${ECHO} "//// Compiled basis/*.sml ////" >>$@
	@${ECHO} >>$@
	./hamlet -b - -j ${BASISFILES:%=basis/%} >>$@ || rm $@

hamlet.js: basis.js ${BUNDLEFILE} ${BASISFILES:%=basis/%} hamlet
	@${ECHO} Generating $@
	@cat basis.js >$@
	@${ECHO} >>$@
	@${ECHO} "//// Embedded file system for basis/* ////" >>$@
	@${ECHO} >>$@
	@${ECHO} "_SML.OS.FileSys.mkDir(\"basis\");" >>$@
	@${ECHO} >>$@
	@for FILE in all.sml ${BASISFILES} ;\
	do \
	  ${ECHO} "_SML.file(\"basis/$$FILE\", " >>$@ ;\
	  cat basis/$$FILE | sed 's/\\/\\\\/g' | sed 's/\"/\\\"/g' | sed 's/^/\"/g' | sed 's/$$/\\n\" +/g' | sed -E 's/[[:blank:]]/ /g' >>$@ ;\
	  ${ECHO} "\"\");" >>$@ ;\
	  ${ECHO} >>$@ ;\
	done
	@${ECHO} >>$@
	@${ECHO} "//// Compiled" ${BUNDLEFILE} "////" >>$@
	@${ECHO} >>$@
	./hamlet -j ${BUNDLEFILE} >>$@ || rm $@

clean-js:

distclean-js: clean-js
	rm -f basis.js hamlet.js

################################################################################
# Clean-up

.PHONY: clean distclean

clean: clean-alice clean-mlkit clean-mlton clean-mosml clean-poly clean-smlnj clean-smlsharp clean-js
	rm -f Makefile.depend */Makefile.depend
	rm -f Makefile-*.depend
	rm -f make.bat.*
	for FILE in `ls fix/*-*.sml` ;\
	do \
	  if [ -f `${ECHO} $$FILE | sed 's/-.*/.sml/'` ] ;\
	  then \
	    rm -f $${FILE} ;\
	  fi ;\
	done

distclean: clean distclean-js
	rm -f hamlet hamlet-image.* hamlet.bat *.exe
	rm -f hamlet.sml ${BUNDLEFILE}
	rm -f make.bat

################################################################################
# Generate a batch file for Windows

make.bat: sh/make.bat.in make.bat.alice make.bat.mlkit make.bat.mlton make.bat.mosml make.bat.poly make.bat.smlnj
	${ECHO} "rem DO NOT EDIT!" >$@
	${ECHO} "rem Generated from $< (${shell date})" >>$@
	cat $< >>$@

	mv $@ $@.tmp
	${ECHO} ${FILES} \
	| tr " " "\n" \
	| sed "s/^/@set files=%files% /g" >$@.files
	sed '/^set files/r $@.files' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:alice/r $@.alice' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:mlkit/r $@.mlkit' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:mlton/r $@.mlton' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:mosml/r $@.mosml' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:poly/r $@.poly' $@.tmp >$@

	mv $@ $@.tmp
	sed '/^:smlnj/r $@.smlnj' $@.tmp >$@

	rm $@.*

make.bat.%:
	${ECHO} "@set fixfiles=" >$@
	${ECHO} ${FIXES_$*:%=fix/%-$*.sml} \
	| tr " " "\n" \
	| sed "s/^/@set fixfiles=%fixfiles% /g" >>$@
	${ECHO} "@set files=%fixfiles% %files%" >>$@
	for FILE in ${FIXES_$*:%=fix/%.sml} ;\
	do \
	  if [ -f $${FILE} ] ;\
	  then \
	    ${ECHO} "copy $${FILE} $${FILE/.sml/-$*.sml}" >>$@ ;\
	  fi ;\
	done

################################################################################
# Hacks for parser and lexer

# To avoid conflicts due to incompatible file naming conventions,
# we have to rename the files generated by ML-Yacc for Moscow ML.
# Since Alice does not fully handle sharing constraints, we also
# have to do some trivial rewriting in the parser signature.

parse/Parser-sig.sml: parse/Parser.grm.sig
	(cat $< ; ${ECHO} ';') \
	| sed -e '/structure Tokens : Parser_TOKENS/{x; N; G;}' >$@

parse/Parser.sml: parse/Parser.grm.sml
	(cat $< ; ${ECHO} ';') >$@

# Newer versions of MLton seem to fix the old ML-Lex position bug.
# Unfortunately, that breaks our lexer, so we have to revert the fix.
parse/Lexer.sml: parse/Lexer.lex.sml
	(cat $< ; ${ECHO} ';') >$@.tmp
	if [ "${SYSTEM}" == "mlton" ] ;\
	then \
	    sed -e 's/val yygone0:int= ~1/val yygone0:int=1/g' $@.tmp >$@ ;\
	    rm $@.tmp ;\
	else \
	    mv $@.tmp $@ ;\
	fi

parse/Parser.grm.sig: parse/Parser.grm
	ml-yacc${BAT} $<

parse/Parser.grm.sml: parse/Parser.grm
	ml-yacc${BAT} $<

# Newer versions of SML/NJ generate calls to Unsafe.*, which is NJ-specific.
parse/Lexer.lex.sml: parse/Lexer.lex
	ml-lex${BAT} $<
	sed "s/Unsafe[.]//g" $@ >$@.tmp
	mv -f $@.tmp $@
