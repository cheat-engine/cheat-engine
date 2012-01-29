#include "neward.h"
#include "common.h"
#include "main.h"

PARD fakeARD;

void sendARD(void)
{
	int i;
	unsigned long long tempbase;
	unsigned long long templength;

	if (!loadedOS)
	{
    for (i=0; fakeARD[i].Type != 255 ;i++)
    {
      tempbase=((unsigned long long)fakeARD[i].BaseAddrHigh << 32)+fakeARD[i].BaseAddrLow;
      templength=((unsigned long long)fakeARD[i].LengthHigh << 32)+fakeARD[i].LengthLow;
      //sendstringf("i=%d : BaseAddress=%6, Length=%6, Type=%d \n\r",i, tempbase, templength, fakeARD[i].Type);
      displayline("i=%d : BaseAddress=%6, Length=%6, Type=%d \n",i, tempbase, templength, fakeARD[i].Type);
    }
	}
}

void initARDcount(void)
{
  int i;
  ARDcount=0;

  if (!loadedOS)
  {
    for (i=0; fakeARD[i].Type != 255 ;i++)
    {
      ARDcount++;
    }
  }
}
