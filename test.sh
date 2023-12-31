testPath="src/test/resources/"

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0

SUCCESS_STATUS=0
FAILURE_STATUS=42

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
  runOnAllInDirectory "good/provided/core" $SUCCESS_STATUS
}

runOnGoodExt () {
  runOnAllInDirectory "good/provided/extensions/arrays1" $SUCCESS_STATUS
  runOnAllInDirectory "good/provided/extensions/struct" $SUCCESS_STATUS
  runOnAllInDirectory "good/provided/extensions/objects1" $SUCCESS_STATUS
  runOnAllInDirectory "good/provided/extensions/objects2" $SUCCESS_STATUS
}

runOnBad () {
  runOnAllInDirectory "bad/provided" $FAILURE_STATUS
  runOnAllInDirectory "bad/custom" $FAILURE_STATUS
}

runOnGoodCore
runOnGoodExt
runOnBad

if ((TESTS_RUN == TESTS_PASSED))
then
  echo -e "\n${GREEN}--------------------\nALL ${TESTS_RUN} TESTS PASSED!\n--------------------${NC}\n"
  exit 0
else
  echo -e "\n${RED}---------------------------------\nTHERE WERE ERRORS! ($((TESTS_RUN - TESTS_PASSED)) OUT OF ${TESTS_RUN})\n---------------------------------${NC}\n"
  exit 1
fi
