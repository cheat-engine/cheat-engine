#!/bin/sh

TESTSUITE_PATH=$HOME/gcc/gcc-3.2/gcc/testsuite/gcc.c-torture
TCC="./tcc -B. -I. -DNO_TRAMPOLINES"
rm -f tcc.sum tcc.fail
nb_ok="0"
nb_failed="0"
nb_exe_failed="0"

for src in $TESTSUITE_PATH/compile/*.c ; do
  echo $TCC -o /tmp/tst.o -c $src 
  $TCC -o /tmp/tst.o -c $src >> tcc.fail 2>&1
  if [ "$?" = "0" ] ; then
    result="PASS"
    nb_ok=$(( $nb_ok + 1 ))
  else
    result="FAIL"
    nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> tcc.sum
done

for src in $TESTSUITE_PATH/execute/*.c ; do
  echo $TCC $src -o /tmp/tst -lm
  $TCC $src -o /tmp/tst -lm >> tcc.fail 2>&1
  if [ "$?" = "0" ] ; then
    result="PASS"
    if /tmp/tst >> tcc.fail 2>&1
    then
      result="PASS"
      nb_ok=$(( $nb_ok + 1 ))
    else
      result="FAILEXE"
      nb_exe_failed=$(( $nb_exe_failed + 1 ))
    fi
  else
    result="FAIL"
    nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> tcc.sum
done

echo "$nb_ok test(s) ok." >> tcc.sum
echo "$nb_ok test(s) ok."
echo "$nb_failed test(s) failed." >> tcc.sum
echo "$nb_failed test(s) failed."
echo "$nb_exe_failed test(s) exe failed." >> tcc.sum
echo "$nb_exe_failed test(s) exe failed."
