include Makefile.inc

PATH := $(JAVADIR)/bin:$(PATH)

ARCHIVE = rtb.jar

.SUFFIXES: .java

.java.class:
	javac -d nhctracer $<

all:	${ARCHIVE}

classfiles:
	javac nhctracer/*.java

${ARCHIVE} jar:	classfiles
	jar cf ${ARCHIVE} nhctracer/*.class

docs:
	javadoc -private -d doc nhctracer

install: ${ARCHIVE}
	${INSTALL} ${ARCHIVE} ${LIBDIR}

clean:
	rm -f ${ARCHIVE} nhctracer/*.class
 
