#!/bin/bash
set -e
CUR_DIR=$( cd "$( dirname ${BASH_SOURCE[0]})" > /dev/null 2>&1 && pwd -P)

pushd .

cd $CUR_DIR

while [[ $# -gt 0 ]]; do
    case $1 in
        clean)
            CLEAN=TRUE
            shift
            ;;
        clean-build)
            CLEAN_BUILD=TRUE
            shift
            ;;
    esac
done

if [[ $CLEAN == "TRUE" ]]; then
    echo "Cleaning src/Language/Dts"
    rm -rf src/Language/Dts
    exit 0
fi


if [[ $CLEAN_BUILD == "TRUE" ]]; then
    echo "Cleaning src/Language/Dts"
    rm -rf src/Language/Dts
    stack clean --full 
fi

bnfc --haskell -p Language.Dts ./grammar/dts.cf -o src
cd src/Language/Dts
alex LexDts.x
happy ParDts.y
mv TestDts.hs TestDts.txt
cd ../../../
stack build
popd