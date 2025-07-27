NAME = hamlet-succ
VERSION = 1.3.2S6
PACKAGE = ${NAME}-${VERSION}
BUILDDIR = build/${PACKAGE}
INSTALLDIR = /home/ps/httpd/html/hamlet
SRCDIRS = fix smlnj-lib infrastructure syntax parse elab eval exec lib main
DIRS = ${SRCDIRS} sh basis doc doc/examples test

.PHONY:	all tar install clean

all:	tar

tar:
	(cd ..; make distclean)
	(cd ../doc; make)

	rm -rf ${BUILDDIR}
	mkdir -p ${BUILDDIR}

	for DIR in ${DIRS} ;\
	do \
	  mkdir ${BUILDDIR}/$$DIR ;\
	  if [ $$DIR != doc ] ;\
	  then \
	    cp ../$$DIR/* ${BUILDDIR}/$$DIR ;\
	  fi ;\
	done
	cp ../doc/*.1 ${BUILDDIR}
	cp ../doc/*.txt ${BUILDDIR}
	cp ../doc/Manual.pdf ${BUILDDIR}/doc/Hamlet-S-${VERSION}.pdf

	(cd ..; make make.bat)
	(cd ..; make hamlet.sml)
	cp ../Makefile ${BUILDDIR}
	cp ../make.bat ${BUILDDIR}
	cp ../sources.cm ${BUILDDIR}
	cp ../hamlet.sml ${BUILDDIR}
	cp -f ../parse/Lexer.* ${BUILDDIR}/parse
	cp -f ../parse/Parser.* ${BUILDDIR}/parse

	(cd ${BUILDDIR}/..; tar -czf ../${PACKAGE}.tar.gz ${PACKAGE}/*)
	(cd ${BUILDDIR}/..; rm -rf ../${PACKAGE}.zip; zip -r ../${PACKAGE}.zip ${PACKAGE}/*)

	rm -rf build

install:	tar
	cp ${PACKAGE}.tar.gz  ${INSTALLDIR}
	cp ${PACKAGE}.zip     ${INSTALLDIR}
	cp ../doc/HaMLet.html ${INSTALLDIR}/index.html
	cp ../doc/Manual.ps   ${INSTALLDIR}/${PACKAGE}.ps
	cp ../doc/Manual.pdf  ${INSTALLDIR}/${PACKAGE}.pdf
	cp ../doc/README.txt  ${INSTALLDIR}/README-succ.txt
	cp ../doc/CHANGES.txt ${INSTALLDIR}/CHANGES-succ.txt
	cp ../doc/examples/*.sml ${INSTALLDIR}/examples/

clean:
	rm -rf build
