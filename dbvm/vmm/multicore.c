/*
 * This sourcefile holds the code for setting up and querying other cpu cores
 * on the system
 */

#include "multicore.h"
#include "common.h"
#include "apic.h"
#include "mm.h"

volatile unsigned int cpucount;


typedef struct _MPFPS
{
        DWORD signature; // 0x5f504d5f : '_MP_'
        DWORD MPConfigPointer; //
        BYTE Length;
        BYTE Version;
        BYTE Checksum;
        BYTE MPFEATURE1;
        BYTE MPFEATURE2;
        BYTE MPFEATURE3;
        BYTE MPFEATURE4;
        BYTE MPFEATURE5;
} __attribute__((__packed__)) *PMPFPS;

typedef struct _MPCONFIG
{
        DWORD signature; // 0x504d4350 : 'PCMP'
        WORD BaseTableLength; //
        BYTE SpecRev;
        BYTE Checksum;
        char OEMID[8];
        char PRODUCTID[12];
        DWORD OEMTablePointer;
        WORD OEMTableSize;
        WORD EntryCount;
        DWORD AddressofLocalAPIC;
        WORD ExtendedTableLength;
        BYTE ExtendedTableChecksum;
        BYTE Filler;
} __attribute__((__packed__)) *PMPCONFIG;

typedef struct _MPCONFCPU
{
				BYTE EntryType;
        BYTE LocalAPICID;
        BYTE LocalAPICVersion;
        unsigned CPUEnabledBit : 1;
        unsigned CPUBootstrapProcessorBit: 1;
        unsigned reserved : 6;
        DWORD CPUSignature;
        DWORD CPUFeatureFlags;
} __attribute__((__packed__)) *PMPCONFCPU;

typedef struct _RSD_PTR
{
        char signature[8]; //'RSD PTR '
        unsigned char checksum;
        char OEMID[6];
        char reserved;
        DWORD rsdtaddress;
} __attribute__((__packed__)) *PRSD_PTR;

typedef struct _RSDT
{
        DWORD signature; //RSDT
        DWORD length;
        char revision;
        unsigned char checksum;
        char OEMID[6];
        char OEM_TABLE_ID[8];
        char OEM_REVISION[4];
        char Creator_ID[4];
        DWORD Creator_revision;
        DWORD Entries[];
} __attribute__((__packed__)) RSDT, *PRSDT;

typedef struct _APICDT
{
        DWORD signature; //APIC
        DWORD length;
        char revision;
        unsigned char checksum;
        char OEMID[6];
        char OEM_TABLE_ID[8];
        char OEM_REVISION[4];
        char Creator_ID[4];
        DWORD Creator_revision;
        DWORD LocalAPIC;
        DWORD flags;
        BYTE APICStructure[];
} __attribute__((__packed__)) APICDT, *PAPICDT;

int checkcrc(UINT64 PhysicalAddress, int size)
{
  //everything counted up must be 0 as 1 byte
  unsigned char *x=(unsigned char *)mapPhysicalMemory(PhysicalAddress, size);
  unsigned char total=0;
  int i;

  for (i=0; i<size; i++)
    total=total+x[i];

  unmapPhysicalMemory(x, size);

  return (total==0);

}

void *find_RSDP_inrange(BYTE *address,int size)
{
  int i;

  for (i=0; i<size; i++, address++)
  {
    if (*(QWORD*)address==0x2052545020445352ULL)
    {
      displayline("POSSIBLE FOUND: %8\n",(QWORD)address);

      if (checkcrc((UINT64)address,20))
        return address;
      else
        displayline("Invalid crc\n");


    }
  }
  return NULL;
}

void *find_MP_inrange(BYTE *address,int size)
{
	int i;
	PMPFPS mp;
	PMPCONFIG mpc;

	for (i=0; i<size; i++, address++)
	{
		if (*(DWORD*)address==0x5f504d5f)
		{
			//displayline("POSSIBLE FOUND: %8\n",(QWORD)address);
			mp=(PMPFPS)address;
			mpc=(PMPCONFIG)((QWORD)mp->MPConfigPointer);

			if ((QWORD)mpc<0x400000)
			{
				if (mpc->signature==0x504d4350)
				{
					return mp; //it's valid
				}
			}

		}
	}
	return NULL;
}

unsigned int initAPcpus(DWORD entrypage)
{
	int found=0;
	int hasHT;

	int isAMD;

	copymem((void *)(QWORD)entrypage, APbootcode, (QWORD)APbootcodeEnd-(QWORD)APbootcode);

	{
	  UINT64 a,b,c,d;
    a=0;
    _cpuid(&a,&b,&c,&d);
    isAMD=((b==0x68747541) && (d==0x69746e65) && (c==0x444d4163));



	  if (!isAMD)
	  {
	    a=1;
	    _cpuid(&a,&b,&c,&d);

      displayline("Feature flag=%x\n",d);
      hasHT=(d >> 28) & 1;
      if (hasHT)
        displayline("hasHT==1\n");
	  }
	  else
	    hasHT=0;
	}

	displayline("Find EBDA:");

	WORD EDBAsegment=*(WORD*)0x40e;
	QWORD EDBAlocation=EDBAsegment<<4;
	displayline("%8\n",EDBAlocation);

	displayline("Checking RSDP tables\n");
	PRSD_PTR rsd_ptr=NULL;


	rsd_ptr=find_RSDP_inrange((void*)EDBAlocation,1024);
	if (!rsd_ptr)
	  rsd_ptr=find_RSDP_inrange((void*)0x9fc00,1024);

	if (!rsd_ptr)
	  rsd_ptr=find_RSDP_inrange((void*)0xf0000,65536);

	if (rsd_ptr)
	{
	  PRSDT rsdt=NULL;

	  displayline("RSDP is located at %x\n",rsd_ptr->rsdtaddress);

	  rsdt=(PRSDT)mapPhysicalMemory(rsd_ptr->rsdtaddress,sizeof(RSDT));

	  if (rsdt->signature==0x54445352)
	  {

	    DWORD rsdtlength;

	    //remap with the actual length
	    rsdtlength=rsdt->length;
	    unmapPhysicalMemory(rsdt,sizeof(RSDT));

	    rsdt=(PRSDT)mapPhysicalMemory(rsd_ptr->rsdtaddress,rsdtlength);

      if (checkcrc(rsd_ptr->rsdtaddress, rsdt->length))
      {

        {
          int i;
          int entrycount=(rsdt->length-36)/4;
          displayline("Entrycount=%d\n",entrycount);

          for (i=0; i<entrycount; i++)
          {
            PAPICDT apicdt=NULL;
            displayline("Entry %d: %x\n",i,rsdt->Entries[i]);

            apicdt=(PAPICDT)mapPhysicalMemory(rsdt->Entries[i],sizeof(APICDT));

            if (apicdt->signature==0x43495041)
            {
              int j=0;
              int APICStructure_size=apicdt->length-44;
              displayline("This one holds APIC info\n");

              //remap
              DWORD apicdtlength=apicdt->length;
              unmapPhysicalMemory(apicdt, sizeof(APICDT));
              apicdt=(PAPICDT)mapPhysicalMemory(rsdt->Entries[i],apicdtlength);

              while (j<APICStructure_size)
              {
                //parse the structures
                switch (apicdt->APICStructure[j])
                {
                  case 0:
                  {
                    //Processor Local APIC structure (only one i'm interested in)
                    displayline("Processor Local APIC structure\n");
                    found++;

                    if (found==2) //multi or threaded, launch them
                    {
                      void *LAPIC=mapPhysicalMemory(apicdt->LocalAPIC, 4096);
                      SetPageToWriteThrough(LAPIC);
                      initcpus((QWORD)LAPIC, entrypage);

                      unmapPhysicalMemory(LAPIC, 4096);
                    }


                    break;
                  }

                  case 1:
                  {
                    //IO APIC structure
                    displayline("IO APIC structure\n");

                    break;
                  }

                  case 2:
                  {
                    displayline("Interrupt Source Override\n");

                    break;
                  }

                  case 3:
                  {
                    displayline("Non-maskable Interrupt Source\n");

                    break;
                  }

                  case 4:
                  {
                    displayline("Local APIC NMI Structure\n");
                    break;
                  }

                  default:
                  {
                    displayline("Unknown Structure\n");
                    break;
                  }

                }

                j+=apicdt->APICStructure[j+1]; //Length

              }


              if (apicdt)
              {
                unmapPhysicalMemory(apicdt, apicdtlength);
                apicdt=NULL;
              }
            }
            if (apicdt)
            {
              unmapPhysicalMemory(apicdt, sizeof(APICDT));
              apicdt=NULL;
            }
          }
        }
      }
      else
        displayline("rsdt is invalid\n");

      if (rsdt)
      {
        unmapPhysicalMemory(rsdt,rsdtlength);
        rsdt=NULL;
      }
    }
	  if (rsdt)
	  {
	    unmapPhysicalMemory(rsdt,sizeof(RSDT));
	    rsdt=NULL;
	  }
	}
	else
	  displayline("No RSD_PTR found\n");



	if (found>0)
	  return found;

	//still here
	displayline("Falling back to MP table");
	PMPFPS mp;
	PMPCONFIG mpc;
	PMPCONFCPU mpcpu;


	//check first KB of EDBA
	mp=find_MP_inrange((void*)EDBAlocation,1024);
	if (mp!=NULL)
	{
		displayline("EBDA: FOUND: %8\n",mp);
	}
	else
	{
		displayline("EBDA: NOT FOUND\n");
		displayline("Last KB Base (0x9fc00->0x9ffff) : Scanning...");
		mp=find_MP_inrange((void*)0x9fc00,1024);
		if (mp!=NULL)
		{
			displayline("FOUND: %8\n",mp);
		}
		else
		{
			displayline("NOT FOUND\n");
			displayline("ROM (0xf0000->0xfffff) : Scanning...");
			mp=find_MP_inrange((void*)0xf0000,65536);
			if (mp!=NULL)
			{
				displayline("FOUND: %8\n",mp);
			}
			else
			{
				displayline("NOT FOUND\n");
				displayline("ALL (0x0->0xfffff) : Scanning...");
				mp=find_MP_inrange((void*)0,1024*1024);
				if (mp!=NULL)
				{
					displayline("FOUND: %8\n",mp);
				}
				else
				{
					displayline("NOT FOUND\n");
					displayline("NO MULTIPROCESS SUPPORT\n");
				}
			}
		}
	}

	if (mp!=NULL)
	{
		int i;
		BYTE *p;
		mpc=(PMPCONFIG)((QWORD)mp->MPConfigPointer);
		displayline("Configuration table at %8\n",mpc);
		displayline("Base table length=%d\n",mpc->BaseTableLength);
		displayline("Specification=1.%d\n",mpc->SpecRev);
		displayline("OEM ID=");
		for (i=0; i<8; i++)
			displayline("%c",mpc->OEMID[i]);
		displayline("\n");

		displayline("Product ID=");
		for (i=0; i<12; i++)
			displayline("%c",mpc->PRODUCTID[i]);
		displayline("\n");

		displayline("OEM Table Pointer=%8\n",mpc->OEMTablePointer);
		displayline("OEM Table Size=%d\n",mpc->OEMTableSize);

		displayline("Entry Count=%d\n",mpc->EntryCount);
		displayline("Address of Local APIC=%8\n",mpc->AddressofLocalAPIC);


		displayline("Extended Table Length=%d\n",mpc->ExtendedTableLength);
		displayline("Extended Table Checksum=%2\n",mpc->ExtendedTableChecksum);

		displayline("sizeof(struct _MPCONFIG)=%d\n",sizeof(struct _MPCONFIG));

		i=0;
		p=(BYTE *)((QWORD)mpc+sizeof(struct _MPCONFIG));
		while (i<(mpc->EntryCount))
		{
		  //displayline("i=%d Checking %8\n",i,p);

		  switch (p[0])
		  {
		    case 0:
		      mpcpu=(PMPCONFCPU)p;
		      displayline("CPU entry at %8\n",p);
		      displayline("Local APICID=%d\n",mpcpu->LocalAPICID);
		      displayline("Local APICID version=%d\n",mpcpu->LocalAPICVersion);
		      displayline("CPU Enabled=%d\n",mpcpu->CPUEnabledBit);
		      displayline("CPU Bootstrap Processor=%d\n",mpcpu->CPUBootstrapProcessorBit);
		      displayline("CPU Signature=%8\n",mpcpu->CPUSignature);
		      displayline("CPU Feature Flags=%8\n",mpcpu->CPUFeatureFlags);

		      if (mpcpu->CPUEnabledBit)
		      {
		        found++;
		        if (hasHT) //quick assumption, could be wrong but it's the closest I can guess without acpi
		          found++;
		      }

		      p+=20;
		      break;

		    case 1:
		      //displayline("BUS entry at %8\n",p);
		      p+=8;
		      break;

		    case 2:
		      //displayline("IO APIC entry at %8\n",p);
		      p+=8;
		      break;

		    case 3:
		      //displayline("IO interrupt assignment entry at %8\n",p);
		      p+=8;
		      break;

		    case 4:
		      //displayline("Local interrupt assignment entry at %8\n",p);
		      p+=8;
		      break;

		    default:
		      //displayline("error\n");
		      p+=8;
		      break;
		  }

		  i++;
		}
	  displayline("before initcpus. found=%d\n\r", found);

	  if (found>1)
	  {
      void *LAPIC=mapPhysicalMemory(mpc->AddressofLocalAPIC, 4096);

      SetPageToWriteThrough(LAPIC);
      asm volatile ("": : :"memory");
      initcpus((QWORD)LAPIC, entrypage);

      asm volatile ("": : :"memory");

	    unmapPhysicalMemory(LAPIC, 4096);
/*
      int i;
	    while (1)
	    {
	      i++;
	      if (i%29==0)
	        sendstringf("i=%d\n",i);

	    }*/
	  }




	}

	displayline("before return. found=%d\n\r", found);
	return (found?found:1);
}


