#!/bin/sh
#
# This script can be executed with "make test" in the parent directory (../)

cwd=`pwd`
cd ../src
make
status=$?
if [ $status -ne 0 ]; then
    echo "ERROR in make in src/  Aborted." >&2
    exit $status
fi
cd $cwd

make
status=$?
if [ $status -ne 0 ]; then
    echo "ERROR in make in test/  Aborted." >&2
    exit $status
fi

for com in f90test1 asmtest; do
  echo "## Running $com (Unit tests)..."
  ./$com
  status=$?
  if [ $status -ne 0 ]; then
      echo "ERROR($com) Status=$status  Aborted." >&2
      exit $status
  fi
done

### Integration test

cd ../test
echo '--- Performing integration tests...'
exec python3 -m unittest integ_test.py

######################################################
exit  # Temporary
######################################################

echo "% cd .."
cd ../src
echo "PWD="`pwd`

sample_dir='../../ginga_samples'
com = "./asmdump $sample_dir/ginga_sirius_R199006190617.fits $sample_dir/FR880428.S0220.fits $sample_dir/asmdump_test_out.fits"
echo '## Running asmdump... (Integrated tests; creates ~/scratch/out.fits)'
echo "[`pwd`]% $com"
com

status=$?
if [ $status -ne 0 ]; then
    echo "ERROR(asmdump) Status=$status " >&2
    exit $status
fi
