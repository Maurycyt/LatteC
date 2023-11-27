MAIN_CLASSPATH=target/scala-3.3.1/classes/
INCLUDE_CLASSPATH=-cp ${MAIN_CLASSPATH} -cp `cat dependencies.cp`

MAIN_ARGS= # To be supplied by the calling script

ARCHIVE_NAME=mw429680

${MAIN_CLASSPATH}main.built: src/main/resources/Latte.g4 $(shell find src/main/scala -type f -name "*.scala")
	make build
	echo "#!/bin/bash\nscala ${INCLUDE_CLASSPATH}" 'main "$$1" false' > latc
	cat latc > latc_llvm
	chmod +x latc latc_llvm
	touch $@

.PHONY: build
build: build.sbt
	sbt compile

.PHONY: clean
clean:
	sbt clean
	rm -rf latc latc_llvm dependencies.cp "${ARCHIVE_NAME}.tgz"

.PHONY: test
test: ${MAIN_CLASSPATH}main.built
	bash test.sh

.PHONY: archive
archive:
	make clean
	rm -rf "${ARCHIVE_NAME}.tmp"
	mkdir -p "${ARCHIVE_NAME}.tmp/project"
	cp project/build.properties "${ARCHIVE_NAME}.tmp/project/"
	cp build.sbt Makefile README.md test.sh "${ARCHIVE_NAME}.tmp"
	cp -r src "${ARCHIVE_NAME}.tmp/"
	# What follows is some magic from https://stackoverflow.com/a/39530409/14406682
	find ${ARCHIVE_NAME}.tmp/ -printf "%P\n" | tar -cvzf ${ARCHIVE_NAME}.tgz --no-recursion -C ${ARCHIVE_NAME}.tmp/ -T -
	rm -rf "${ARCHIVE_NAME}.tmp"
