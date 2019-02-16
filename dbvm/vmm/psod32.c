/*
 * psod32.c
 *
 *  Created on: Feb 27, 2018
 *      Author: eric heijnen
 *
 *  A small test of mixing 32-bit code in with the 64-bit DBVM
 */

//todo: instead of using reboot implement a call32() stub that switches to 32-bit, calls the function, and then return to the caller in 64-bit
int PSOD32BitHandler()
{
  int PMInfoBlock=0;
  volatile unsigned char *biossource=(volatile unsigned char *)0xc0000;
  volatile unsigned char *bioscopy=(volatile unsigned char *)0x80000;
  int i;
  for (i=0; i<65536; i++)
  {
    bioscopy[i]=biossource[i];
    //PMID
    if ((biossource[i]=='P') && (biossource[i]=='M') &&  (biossource[i]=='I') && (biossource[i]=='D'))
      PMInfoBlock=i;
  }

  if (PMInfoBlock==0x6678)
    return 13;

  return 12;
}

