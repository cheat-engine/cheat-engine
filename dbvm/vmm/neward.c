#include "neward.h"
#include "common.h"
#include "main.h"

PARD fakeARD;

void setARDAddress(PARD entry, QWORD address)
{
  entry->BaseAddrLow=(DWORD)address;
  entry->BaseAddrHigh=address << 32;
}

QWORD getARDAddress(PARD entry)
{
  return entry->BaseAddrLow+(((QWORD)(entry->BaseAddrHigh))<<32);
}

void setARDSize(PARD entry, QWORD size)
{
  entry->LengthLow=(DWORD)size;
  entry->LengthHigh=size << 32;
}

QWORD getARDSize(PARD entry)
{
  return entry->LengthLow+(((QWORD)entry->LengthHigh)<<32);
}

void fakeARD_insertAtPos(int index, PARD ard)
{
  //shift entries after index by one and then place ARD at the free spot
  int length;
  int i;
  if (loadedOS)
    return;
  for (length=index; fakeARD[length].Type != 255; length++);

  for (i=length+1; i>index; i--)
    fakeARD[i]=fakeARD[i-1];

  fakeARD[index]=*ard;
}

void fakeARD_InsertRange(QWORD address, QWORD size, DWORD type)
//inserts the given range into the list inside an existing type 1 region. If type matches the previous or next item, merges it instead of add
//requirement: range must be within an existing ARD entry
{
  int i;
  //find the entry that matches this address
  if (loadedOS)
    return;


  for (i=0; fakeARD[i].Type != 255 ;i++)
  {
    PARD e=&fakeARD[i];
    QWORD a_address=getARDAddress(e);
    QWORD a_size=getARDSize(e);

    if ((address>=a_address) && (address<a_address+a_size))
    {
      //found the entry
      if (e->Type==type)
        return; //it's the same type, no need to add

      if (address==a_address) //start of the range
      {
        //check if the previous type can be merged
        if ((i>0) && (fakeARD[i-1].Type==type) && (getARDAddress(&fakeARD[i-1])==address))
        {
          PARD e2=&fakeARD[i-1];

          //same type as this, merge it instead of insert
          setARDSize(e2, getARDSize(e2)+size); //grow the one in front
          setARDAddress(e, getARDAddress(e)+size); //shift and shrink this one
          setARDSize(e, getARDAddress(e)-size);
          return;
        }
      }

      if ((address+size)==a_address+a_size)
      {
        //it's at the end
        //check if the next type can be merged
        PARD e2=&fakeARD[i+1];
        if ((e2->Type==type) && (getARDAddress(e2)==address+size)) //check if start is here (in case of gap/unordered)
        {
          //same type as this, merge it instead of insert
          setARDSize(e, getARDSize(e)-size); //shrink this one
          setARDAddress(e2, getARDAddress(e2)-size); //shift the one in front and grow it
          setARDSize(e2, getARDSize(e2)+size);
          return;
        }
      }

      //still here so no merge, insert instead

      int j;
      ARD newentry;
      setARDAddress(&newentry, address);
      setARDSize(&newentry, size);

      ARD before=*e;
      ARD after=*e;

      //adjust before and after to the new state
      setARDSize(&before, address-a_address);

      setARDAddress(&after, address+size);
      setARDSize(&after, (a_address+a_size)-getARDAddress(&after));

      if ((getARDSize(&before)==0) && (getARDSize(&after)==0))
      {
        //encompasses the whole block
        e->Type=type;
        return;
      }

      j=i;
      if (getARDSize(&before))
      {
        *e=before;
        j++;
      }

      fakeARD_insertAtPos(j, &newentry);
      j++;

      if (getARDSize(&after))
        fakeARD_insertAtPos(j, &after);
    }

  }
}

int fakeARD_getConventionalMemory()
{
  //return the total contiguos memory between 0 and 0xfffff
  int i;
  int size=0;
  if (!loadedOS)
  {
    for (i=0; fakeARD[i].Type != 255 ;i++)
    {
      if (getARDAddress(&fakeARD[i])==0)
        size=max(size, getARDSize(&fakeARD[i]));

    }
  }

  return size;
}

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
