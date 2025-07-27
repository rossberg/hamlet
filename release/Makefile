NAME = hamlet-succ
VERSION = 1.3.2S6
PACKAGE = ${NAME}-${VERSION}
BUILDDIR = build/${PACKAGE}
UPLOADURL = rossberg@contact.mpi-sws.org:homepage/hamlet/
SRCDIRS = fix smlnj-lib infrastructure syntax parse elab eval exec lib main
DIRS = ${SRCDIRS} sh basis doc doc/examples test

.PHONY:	all tar upload clean

all:	tar

tar:
	(cd ..; make distclean) && \
	(cd ../doc; make) && \
	\
	rm -rf ${BUILDDIR} && \
	mkdir -p ${BUILDDIR} && \
	\
	for DIR in ${DIRS} ;\
	do \
	  mkdir ${BUILDDIR}/$$DIR ;\
	  if [ $$DIR != doc ] ;\
	  then \
	    cp ../$$DIR/* ${BUILDDIR}/$$DIR ;\
	  fi ;\
	done && \
	cp ../doc/*.1 ${BUILDDIR}
	cp ../doc/*.txt ${BUILDDIR} && \
	cp ../doc/Manual.pdf ${BUILDDIR}/doc/Hamlet-S-${VERSION}.pdf && \
	\
	(cd ..; make make.bat) && \
	(cd ..; make hamlet.sml) && \
	cp ../Makefile ${BUILDDIR} && \
	cp ../make.bat ${BUILDDIR} && \
	cp ../sources.cm ${BUILDDIR} && \
	cp ../hamlet.sml ${BUILDDIR} && \
	cp -f ../parse/Lexer.* ${BUILDDIR}/parse && \
	cp -f ../parse/Parser.* ${BUILDDIR}/parse && \
	\
	(cd ${BUILDDIR}/..; COPYFILE_DISABLE=1 tar -czf ../${PACKAGE}.tar.gz ${PACKAGE}/*) && \
	(cd ${BUILDDIR}/..; rm -rf ../${PACKAGE}.zip; zip -r ../${PACKAGE}.zip ${PACKAGE}/*) && \
	\
	rm -rf build

upload:	tar
	rm -rf upload && \
	mkdir upload && \
	cp ${PACKAGE}.tar.gz   upload && \
	cp ${PACKAGE}.zip      upload && \
	cp ../doc/Manual.ps    upload/${PACKAGE}.ps && \
	cp ../doc/Manual.pdf   upload/${PACKAGE}.pdf && \
	cp ../doc/README.txt   upload/README-succ.txt && \
	cp ../doc/CHANGES.txt  upload/CHANGES-succ.txt && \
	cp -r ../doc/examples  upload/examples-succ && \
	scp -r upload/* ${UPLOADURL} && \
	rm -rf upload

clean:
	rm -rf build upload
