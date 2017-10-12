NAME = hamlet
VERSION = 2.0.0
PACKAGE = ${NAME}-${VERSION}
BUILDDIR = build/${PACKAGE}
UPLOADURL = rossberg@contact.mpi-sws.org:homepage/${NAME}/
SRCDIRS = fix smlnj-lib infrastructure syntax parse elab eval exec lib compile-js main
DIRS = ${SRCDIRS} sh basis doc test

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
	cp ../doc/Manual.pdf ${BUILDDIR}/doc/Hamlet-${VERSION}.pdf && \
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
	mkdir upload && \
	cp ${PACKAGE}.tar.gz   upload && \
	cp ${PACKAGE}.zip      upload && \
	cp ../doc/HaMLet.html  upload/index.html && \
	cp ../doc/Manual.ps    upload/${PACKAGE}.ps && \
	cp ../doc/Manual.pdf   upload/${PACKAGE}.pdf && \
	cp ../doc/Mistakes.ps  upload/defects.ps && \
	cp ../doc/Mistakes.pdf upload/defects.pdf && \
	cp ../doc/CHANGES.txt  upload && \
	scp upload/* ${UPLOADURL} && \
	rm -rf upload

clean:
	rm -rf build upload
