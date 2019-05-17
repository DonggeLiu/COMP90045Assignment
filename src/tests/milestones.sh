#!/bin/bash

echo "===================================="
echo "         Milestone Tests            "
echo "===================================="

OZ="../oz/oz"
MILES_DIR="tests/samples/stage3-miles/"

for n in {1..6}; do
    echo "--- Compiling mile$n.gt ---"
    ./Goat "$MILES_DIR/mile$n.gt" || break
    echo "--- Emulating mile$n.oz ---"    
    ./Goat "$MILES_DIR/mile$n.gt" > "mile$n.oz"
    $OZ -s "mile$n.oz" || echo "ERROR (did you make the emulator?)"
    # TODO: Diff output with 'expected' (where do we get expected output?)
done
echo "--- Removing tempfiles ---"
rm -f mile*.oz
