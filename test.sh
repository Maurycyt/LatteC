testPath="src/test/resources/"

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0

SUCCESS_STATUS=0
FAILURE_STATUS=42

runOn () {( set -e
  exec_path="${1%.lat}"
  bc_path="${exec_path}.bc"
  input_path="${exec_path}.input"
  output_path="${exec_path}.output"
  test_output_path="${exec_path}.test_output"

  if [ ! -e "${input_path}" ]; then
    input_path=/dev/null
  fi

  ./latc "$1" &> /dev/null

  lli "${bc_path}" < "${input_path}" > "${test_output_path}" || true

  if [ -e "${output_path}" ]; then
    diff "${output_path}" "${test_output_path}"
  fi
)}

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
  runOnAllInDirectory "good/custom" $SUCCESS_STATUS
}

runOnGoodExt () {
  runOnAllInDirectory "good/provided/extensions/arrays1" $SUCCESS_STATUS
  runOnAllInDirectory "good/provided/extensions/struct" $SUCCESS_STATUS
#  runOnAllInDirectory "good/provided/extensions/objects1" $SUCCESS_STATUS
#  runOnAllInDirectory "good/provided/extensions/objects2" $SUCCESS_STATUS
}

runOnBad () {
  runOnAllInDirectory "bad/provided" $FAILURE_STATUS
  runOnAllInDirectory "bad/custom" $FAILURE_STATUS
}

runOnGoodCore
runOnGoodExt
runOnBad
#runOnAllInDirectory "../../../../mimuw-mrjp-tests/good" $SUCCESS_STATUS
#runOnAllInDirectory "../../../../mimuw-mrjp-tests/bad" $FAILURE_STATUS

if ((TESTS_RUN == TESTS_PASSED))
then
  echo -e "\n${GREEN}--------------------\n${GREEN}ALL ${TESTS_RUN} TESTS PASSED!\n${GREEN}--------------------${NC}\n"
  exit 0
else
  echo -e "\n${RED}---------------------------------\n${RED}THERE WERE ERRORS! ($((TESTS_RUN - TESTS_PASSED)) OUT OF ${TESTS_RUN})\n${RED}---------------------------------${NC}\n"
  exit 1
fi
