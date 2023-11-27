MAIN_CLASSPATH=target/scala-3.3.1/classes/
INCLUDE_CLASSPATH=-cp ${MAIN_CLASSPATH} -cp `cat dependencies.cp`

MAIN_ARGS= # To be supplied by the calling script

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
	rm -rf latc latc_llvm

.PHONY: test
test: ${MAIN_CLASSPATH}main.built
	bash test.sh
