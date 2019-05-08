#!/bin/sh

# 1. hook up your Goat executable (e.g. "./Goat") and sample dir:
GOAT="./Goat"
SAMPLE_ROOT="Tests/samples"

# Run GOAT to prettify all the goat programs (*.gt) under SAMPLE_ROOT,
# and compare your pretty output to the existing pretty output (*.gt.pp)
FAIL='\033[0;31mFAIL\033[0m'
WARN='\033[0;35mWARN\033[0m'
shopt -s globstar

# Testing good goat programs: compare output in *.gt to *.gt.pp
echo "TEST: parser can correctly pretty-print all well-formed goat programs..."
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

echo "TEST: parser should reject all ill-formed goat programs..."
for testin in "$SAMPLE_ROOT"/**/*.gt.bad; do
    # running goat:
    "$GOAT" -p "$testin" > ".temp.gt" 2>&1
    
    if [ $? == 0 ]; then
        # if it succeeded, we have a problem! There should have been an error
        echo -e "${FAIL}: $testin parse succeeds, but should have errored"
        if [ -f "$testin.out" ]; then
            echo "here's the intended error and message:"
            cat "$testin.out" | sed 's/^/  /'
        else
            echo "(NOTE: $testin.out missing---no example error message)"
        fi
    fi
    # if it failed, that's good! the error message is not specified, though, so
    # we need not compare it.
done


# clean up temporary files:
rm ".temp.gt"

echo "DONE: see output above (no output means all tests passed)."