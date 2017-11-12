/*
 * pci.c
 *
 *  Created on: Jun 8, 2009
 *      Author: erich
 */

#include "common.h"
#include "main.h"
#include "pci.h"

DWORD pciConfigReadDWord (unsigned short bus, unsigned short slot, unsigned short func, unsigned short offset)
{
   unsigned long address;
   unsigned long lbus = (unsigned long)bus;
   unsigned long lslot = (unsigned long)slot;
   unsigned long lfunc = (unsigned long)func;

   /* create configuration address as per Figure 1 */
   address = (unsigned long)((lbus << 16) | (lslot << 11) |
             (lfunc << 8) | (offset & 0xfc) | ((UINT32)0x80000000));

   /* write out the address */
   outportd(0xCF8, address);
   /* read in the data */
   return inportd(0xCFC);
}

WORD pciConfigReadWord (unsigned short bus, unsigned short slot, unsigned short func, unsigned short offset)
{
   unsigned long address;
   unsigned long lbus = (unsigned long)bus;
   unsigned long lslot = (unsigned long)slot;
   unsigned long lfunc = (unsigned long)func;

   /* create configuration address as per Figure 1 */
   address = (unsigned long)((lbus << 16) | (lslot << 11) |
             (lfunc << 8) | (offset & 0xfc) | ((UINT32)0x80000000));

   /* write out the address */
   outportd(0xCF8, address);
   /* read in the data */
   return (unsigned short)((inportd (0xCFC) >> ((offset & 2) * 8)) & 0xffff);
}

BYTE pciConfigReadByte (unsigned short bus, unsigned short slot, unsigned short func, unsigned short offset)
{
   ULONG dw=pciConfigReadDWord(bus,slot,func,offset);
   int index=offset & 3;

   return (dw >> (8*index)) & 0xff;



}

void pciConfigEnumPci(void)
{
  int i,j;
  for (i = 0 ; i < 256 ; i++ )
  for (j = 0 ; j < 32 ; j++ )
  {
    if (pciConfigReadWord(i,j,0,0)!=0xffff)
    {
      int k;

      for (k=0; k<8; k++)
      {
        if (pciConfigReadWord(i,j,k,0)!=0xffff)
        {
          //found one
          WORD VendorID, DeviceID;
          BYTE Header;


          VendorID=pciConfigReadWord(i,j,k,0);
          DeviceID=pciConfigReadWord(i,j,k,2);
          Header=pciConfigReadByte(i,j,k,0xe);


          //known: 9710-9865 =  serial port
          if ((VendorID==0x9710) || (VendorID==0x1415))  //1415 as well
          {
            int l;
            DWORD BAR[6];
            BAR[0]=pciConfigReadDWord(i,j,k,0x10);
            BAR[1]=pciConfigReadDWord(i,j,k,0x14);
            BAR[2]=pciConfigReadDWord(i,j,k,0x18);
            BAR[3]=pciConfigReadDWord(i,j,k,0x1c);
            BAR[4]=pciConfigReadDWord(i,j,k,0x20);
            BAR[5]=pciConfigReadDWord(i,j,k,0x24);


            displayline("Found one at %d , %d - Vendor ID: %x   Device ID:%x Header:%2\n",i,j,VendorID, DeviceID,Header);
            for (l=0; l<6; l++)
            {
              if (BAR[l]!=0)
              {
                DWORD temp=BAR[l];
                if (temp & 1)
                {
                  temp&=0xFFF8;
                  displayline("Port %x\n",temp);
                }
                else
                {
                  temp&=0xFFFFFFF0;
                  displayline("Address %8\n",temp);
                }

              }
            }
            //displayline("Command:   %x   Status:   %x \n", pciConfigReadWord(i,j,k,4), pciConfigReadWord(i,j,k,6));
            //displayline("%x - RevisionID:%x   ProgIF:   %x Subclass: %x  ClassCode: %x\n", pciConfigReadDWord(i,j,k,8), pciConfigReadByte(i,j,k,8), pciConfigReadByte(i,j,k,9), pciConfigReadByte(i,j,k,10), pciConfigReadByte(i,j,k,11));
            //displayline("BAR0: %8 BAR1: %8 BAR2: %8\n", pciConfigReadDWord(i,j,k,0x10),pciConfigReadDWord(i,j,k,0x14), pciConfigReadDWord(i,j,k,0x18));
            //displayline("BAR3: %8 BAR4: %8 BAR5: %8\n", pciConfigReadDWord(i,j,k,0x1C),pciConfigReadDWord(i,j,k,0x20), pciConfigReadDWord(i,j,k,0x24));
            //displayline("Int line: %d Int pin: %d\n", pciConfigReadByte(i,j,k,0x3c), pciConfigReadByte(i,j,k,0x3d));
          }
        }


      }


    }

  }

}
