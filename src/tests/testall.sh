#!/bin/bash

# START HERE:
# hook up your Goat executable (e.g. "./Goat") and sample dir below

GOAT="./Goat"
SAMPLE_ROOT="Tests/samples"

# Script constants and settings:

FAIL='\033[0;31mFAIL\033[0m'
WARN='\033[0;35mWARN\033[0m'
shopt -s globstar

# RUN THE TESTS!

# Run GOAT to prettify all the goat programs (*.gt) under SAMPLE_ROOT,
# and compare your pretty output to the existing pretty output (*.gt.pp)
echo "TEST: parser should correctly print well-formed goat programs..."
for testin in "$SAMPLE_ROOT"/**/*.gt; do
    # running goat:
    "$GOAT" -p "$testin" > ".temp.gt" 2>&1
    
    if [ $? == 0 ]; then
        # if it succeeded, compare the output:
        if [ -f "$testin.pp" ]; then
            diff "$testin.pp" ".temp.gt" > /dev/null
            if [ $? == 1 ]; then
                echo -e "${FAIL}: $testin output different:"
                diff -y "$testin.pp" ".temp.gt" | sed 's/^/  /'
            fi
        else
            echo -e "${WARN}: $testin.pp does not exist!"
        fi
    else
        # if it failed, probably there was a parse error
        echo -e "${FAIL}: $testin caused us a parse error:"
        cat ".temp.gt" | sed 's/^/  /'
    fi
done
printf "DONE! num tests:"; ls -1 "$SAMPLE_ROOT"/**/*.gt | wc -l

# Run GOAT to parse all the baad goat programs (*.gt.bad) under SAMPLE_ROOT,
# and ensure your parser rejects them (and sends the right error code)
echo "TEST: parser should reject any ill-formed goat programs..."
for testin in "$SAMPLE_ROOT"/**/*.gt.bad; do
    # running goat:
    "$GOAT" -p "$testin" > ".temp.gt" 2>&1
    
    if [ $? == 0 ]; then
        # if it succeeded, we have a problem! There should have been an error
        echo -e "${FAIL}: parsing $testin succeeded, but should have rejected:"
        if [ -f "$testin.out" ]; then
            cat "$testin.out" | sed 's/^/  /'
        else
            echo "  ($testin.out missing---no example error message)"
        fi
    fi
    # if it failed, that's good! the error message is not specified, though, so
    # we need not compare it.
done
printf "DONE! num tests:"; ls -1 "$SAMPLE_ROOT"/**/*.gt.bad | wc -l

# clean up temporary files:
rm ".temp.gt"

echo "ALL DONE! (note: no output means all tests passed)."