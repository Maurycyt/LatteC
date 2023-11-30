testPath="src/test/resources/"

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0

runOn() {
  ./latc "$1" &> /dev/null
}

runOnAllInDirectory () {
  echo "Running on all files in $1"
  for file in "${testPath}$1"/*.lat
  do
    TESTS_RUN=$((TESTS_RUN + 1))
    runOn "${file}"
    if (( $? == $2 ))
    then
      echo -e "\t${file}: ${GREEN}OK${NC}"
      TESTS_PASSED=$((TESTS_PASSED + 1))
    else
      echo -e "\t${file}: ${RED}ERR${NC}"
    fi
  done
}

runOnGoodCore () {
  runOnAllInDirectory "good/provided/core" 0
}

runOnGoodExt () {
  runOnAllInDirectory "good/provided/extensions/arrays1" 0
  runOnAllInDirectory "good/provided/extensions/struct" 0
  runOnAllInDirectory "good/provided/extensions/objects1" 0
  runOnAllInDirectory "good/provided/extensions/objects2" 0
}

runOnBad () {
  runOnAllInDirectory "bad/provided" 1
  runOnAllInDirectory "bad/custom" 1
}

runOnGoodCore
runOnGoodExt
runOnBad

if ((TESTS_RUN == TESTS_PASSED))
then
  echo -e "\n${GREEN}--------------------\nALL ${TESTS_RUN} TESTS PASSED!\n--------------------${NC}\n"
else
  echo -e "\n${RED}---------------------------------\nTHERE WERE ERRORS! ($((TESTS_RUN - TESTS_PASSED)) OUT OF ${TESTS_RUN})\n---------------------------------${NC}\n"
fi
