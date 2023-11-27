testPath="src/test/resources/"

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m'

SUCCESS=true

runOn() {
  sbt "run $1 true" > /dev/null
}

runOnAllInDirectory () {
  echo "Running $1"
  for file in "${testPath}$1"/*.lat
  do
    runOn "${file}"
    if (( $? == $2 ))
    then
      echo -e "\t${file}: ${GREEN}OK${NC}"
    else
      echo -e "\t${file}: ${RED}ERR${NC}"
      SUCCESS=false
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

if $SUCCESS
then
  echo -e "\n${GREEN}-----------------\nALL TESTS PASSED!\n-----------------${NC}\n"
else
  echo -e "\n${RED}------------------\nTHERE WERE ERRORS!\n------------------${NC}\n"
fi
