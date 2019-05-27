#!/bin/bash

# START HERE:
# hook up your Goat executable (e.g. "./Goat") and sample dir below

GOAT="./Goat"
SAMPLE_ROOT="Tests/samples"
OZ="../oz/oz"

# Script constants and settings:

if [ -t 1 ]; then
    FAIL='\033[0;31mFAIL\033[0m'
    WARN='\033[0;35mWARN\033[0m'
else
    FAIL="FAIL"
    WARN="WARN"
fi
shopt -s globstar

# RUN THE TESTS!

# First, check for missing test outputs

# Run GOAT to compile all the goat programs (*.gt) under SAMPLE_ROOT,
# run the emulator on resulting Oz code and compare output to that expected.
echo "Scan for any missing test cases..."
for testin in "$SAMPLE_ROOT"/**/*.gt; do
    if ! [ -f "$testin.out" ] && ! [ -f "$testin.pp" ]; then
        echo -e "${WARN}: $testin missing .out or .pp file; won't be tested!"
    fi
done


# Run GOAT to prettify all the goat programs (*.gt) under SAMPLE_ROOT,
# and compare your pretty output to the existing pretty output (*.gt.pp)
echo "TEST: parser should correctly print well-formed goat programs..."
tests=0
failed=0
for testin in "$SAMPLE_ROOT"/**/*.gt; do
    if [ -f "$testin.pp" ]; then
        tests=$((tests+1))
        # running goat:
        "$GOAT" -p "$testin" > .temp.gt 2>&1

        if [ $? == 0 ]; then
            # if it succeeded, compare the output:
                diff -y "$testin.pp" ".temp.gt" > .temp.diff
                if [ $? == 1 ]; then
                    failed=$((failed+1))
                    echo -e "${FAIL}: $testin output different:"
                    sed 's/^/  /' .temp.diff
                fi
        else
            # if it failed, probably there was a parse error
            failed=$((failed+1))
            echo -e "${FAIL}: $testin caused us a parse error?"
            sed 's/^/  /' .temp.gt
        fi
    fi
done
printf "DONE! num tests: $tests, num failed: $failed\n";

# Run GOAT to parse all the baad goat programs (*.gt.bad) under SAMPLE_ROOT,
# and ensure your parser rejects them (and sends the right error code)
echo "TEST: parser should reject any SYNTACTICALLY ill-formed goat programs..."
tests=0
failed=0
for testin in "$SAMPLE_ROOT"/**/*.gt.bad; do
    tests=$((tests+1))
    # running goat:
    "$GOAT" -p "$testin" > ".temp.gt" 2>&1

    if [ $? == 0 ]; then
        # if it succeeded, we have a problem! There should have been an error
        failed=$((failed+1))
        echo -e "${FAIL}: parsing $testin succeeded, but should have rejected:"
        if [ -f "$testin.out" ]; then
            sed 's/^/  /' "$testin.out"
        else
            echo "  ($testin.out missing---no example error message)"
        fi
    fi
    # if it failed, that's good! the error message is not specified, though, so
    # we need not compare it.
done
printf "DONE! num tests: $tests, num failed: $failed\n";


# Run GOAT to analyse all the baad (semantic) goat programs
# (*.gt.semantic-error) under SAMPLE_ROOT, and ensure your compiler rejects
# them (and sends the right error code)
echo "TEST: parser should reject any SEMANTICALLY ill-formed goat programs..."
tests=0
failed=0
for testin in "$SAMPLE_ROOT"/**/*.gt.semantic-error; do
    tests=$((tests+1))
    # running goat:
    "$GOAT" -x "$testin" > ".temp.gt" 2>&1

    case $? in
    0)
        # if it succeeded, we have a problem! There should have been an error
        failed=$((failed+1))
        echo -e "${FAIL}: compiling $testin succeeded, but should have rejected:"
        if [ -f "$testin.out" ]; then
            sed 's/^/  /' "$testin.out" 
        else
            echo "  ($testin.out missing---no example error message)"
        fi
    ;;
    1)
        # seems like a haskell runtime error. uh oh!
        failed=$((failed+1))
        echo -e "${FAIL}: compiling $testin broke the compiler:"
        cat ".temp.gt" | sed 's/^/  /'
    ;;
    *)
        # any other error code is okay, right?
    ;;
    esac
    # if it failed, that's good! the error message is not specified, though, so
    # we need not compare it.
done
printf "DONE! num tests: $tests, num failed: $failed\n";


# Run GOAT to compile all the goat programs (*.gt) under SAMPLE_ROOT,
# run the emulator on resulting Oz code and compare output to that expected.
echo "TEST: parser should correctly compile well-formed goat programs..."
tests=0
failed=0
for testin in "$SAMPLE_ROOT"/**/*.gt; do
    if [ -f "$testin.out" ]; then
        tests=$((tests+1))
        # running goat:
        "$GOAT" -x "$testin" > .temp.oz 2>&1

        if [ $? == 0 ]; then
            # if it succeeded, run the emulator on the compiled Oz code:
            touch "$testin.in" # in case it doesn't exist yet, create a blank
                               # input file.
            "$OZ" .temp.oz < "$testin.in" > .temp.out 2>&1

            diff "$testin.out" .temp.out > .temp.diff
            if [ $? == 1 ]; then
                failed=$((failed+1))
                echo -e "${FAIL}: $testin output different:"
                sed 's/^/  /' .temp.diff
                echo
            fi
        else
            # if it failed, probably there was a parse error
            failed=$((failed+1))
            echo -e "${FAIL}: $testin caused us a compile error:"
            sed 's/^/  /' .temp.oz
        fi
    fi
done
printf "DONE! num tests: $tests, num failed: $failed\n";


# clean up temporary files:
rm .temp.*

echo "ALL DONE! (note: no output means all tests passed)."
