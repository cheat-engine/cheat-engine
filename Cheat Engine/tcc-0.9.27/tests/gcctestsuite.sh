#!/bin/sh

TESTSUITE_PATH=$HOME/gcc/gcc-3.2/gcc/testsuite/gcc.c-torture
TCC="./tcc -B. -I. -DNO_TRAMPOLINES" 
rm -f tcc.sum tcc.log
nb_failed="0"

for src in $TESTSUITE_PATH/compile/*.c ; do
  echo $TCC -o /tmp/test.o -c $src 
  $TCC -o /tmp/test.o -c $src >> tcc.log 2>&1
  if [ "$?" = "0" ] ; then
     result="PASS"
  else
     result="FAIL"
     nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> tcc.sum
done

for src in $TESTSUITE_PATH/execute/*.c ; do
  echo $TCC $src 
  $TCC $src >> tcc.log 2>&1
  if [ "$?" = "0" ] ; then
     result="PASS"
  else
     result="FAIL"
     nb_failed=$(( $nb_failed + 1 ))
  fi
  echo "$result: $src"  >> tcc.sum
done

echo "$nb_failed test(s) failed." >> tcc.sum
echo "$nb_failed test(s) failed."
