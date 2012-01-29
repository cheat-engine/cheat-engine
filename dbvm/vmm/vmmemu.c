#include "vmmemu.h"
#include "common.h"
#include "main.h"

void handleMODRM(unsigned char *realinstruction,int start, VMRegisters *registers, int addresssizeoverride, int used_segment, int *reg1,int *reg2, unsigned int *Address)
{
/* returns 0 if address, returns 1 if reg(2) */
	char rm=realinstruction[start] & 7;
	char reg=(realinstruction[start] >> 3) & 7;
	char mod=(realinstruction[start] >> 6) & 3;

	*reg1=reg;
	*reg2=0;
	*Address=0;

	if (addresssizeoverride==0)
	{
	  switch (mod)
		{
			case 0:
				switch (rm)
				{
					case 0: //bx+si
						*Address=(registers->ebx+registers->esi) & 0xffff;
						break;

					case 1: //bx+di
						*Address=(registers->ebx+registers->edi) & 0xffff;
						break;					

					case 2: //bp+si
						*Address=(registers->ebp+registers->esi) & 0xffff;
						if (used_segment==0)
							used_segment=3; //it just is for bp
						break;	

					case 3: //bp+di
						*Address=(registers->ebp+registers->edi) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;	

					case 4: //si
						*Address=(registers->esi) & 0xffff;
						break;

					case 5: //di
						*Address=(registers->edi) & 0xffff;
						break;

					case 6: //disp16
						*Address=*Address+*(short int *)(&realinstruction[start+1]);
						break;

					case 7: //bx
						*Address=(registers->ebx) & 0xffff;
						break;
				}
			  break;

			case 1:
				switch (rm)
				{
					case 0: //bx+si
						*Address=(registers->ebx+registers->esi) & 0xffff;
						break;

					case 1: //bx+di
						*Address=(registers->ebx+registers->edi) & 0xffff;
						break;					

					case 2: //bp+si
						*Address=(registers->ebp+registers->esi) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;	

					case 3: //bp+di
						*Address=(registers->ebp+registers->edi) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;	

					case 4: //si
						*Address=(registers->esi) & 0xffff;
						break;

					case 5: //di
						*Address=(registers->edi) & 0xffff;
						break;

					case 6: //bp
						*Address=(registers->ebp) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;

					case 7: //bx
						*Address=(registers->ebx) & 0xffff;
						break;
				}
				*Address+=*(char *)(&realinstruction[start+1]);
			  break;

			case 2:
				switch (rm)
				{
					case 0: //bx+si
						*Address=(registers->ebx+registers->esi) & 0xffff;
						break;

					case 1: //bx+di
						*Address=(registers->ebx+registers->edi) & 0xffff;
						break;					

					case 2: //bp+si
						*Address=(registers->ebp+registers->esi) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;	

					case 3: //bp+di
						*Address=(registers->ebp+registers->edi) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;	

					case 4: //si
						*Address=(registers->esi) & 0xffff;
						break;

					case 5: //di
						*Address=(registers->edi) & 0xffff;
						break;

					case 6: //bp
						*Address=(registers->ebp) & 0xffff;
						if (used_segment==0)
							used_segment=3;
						break;

					case 7: //bx
						*Address=(registers->ebx) & 0xffff;
						break;
				}
				*Address+=*(short int *)(&realinstruction[start+1]);
			  break;

			case 3:
				*reg2=rm;
			  break;

		}
	}
	else
	{
		//32-bit
		sendstring("Failure: Address size override used in emulated realmode! Not yet implemented!\n");
		return;
	}

	switch (used_segment)
	{
		case 0:
			*Address+=vmread32(0x680c); //default ds
			break;

		case 1:
			*Address+=vmread32(0x6808); //add cs base
			break;

		case 2:
			*Address+=vmread32(0x6806); //add es base
			break;

		case 3:
			*Address+=vmread32(0x680a); //add ss base
			break;

		case 4:
			*Address+=vmread32(0x680c); //add ds base (forced)
			break;

		case 5:
			*Address+=vmread32(0x680e); //add fs base
			break;

		case 6:
			*Address+=vmread32(0x6810); //add gs base
			break;
	}
}

int emulatevm86(unsigned char *instructionpointer,VMRegisters *registers)
{
	int used_segment=0; //default ds
	int operandsizeoverride=0;
	int addresssizeoverride=0;
	int ip=0;

	int reg1,reg2;
	ULONG address;



	//prefix scan:
	while (1)
	{
		ip++;
		switch (instructionpointer[ip])
		{
			case 0x2e:
				used_segment=1; //cs
				continue;

			case 0x26:
				used_segment=2; //es
				continue;

			case 0x36:
				used_segment=3; //ss
				continue;

			case 0x3e:
				used_segment=4; //ds
				continue;

			case 0x64:
				used_segment=5; //fs
				continue;

			case 0x65:
				used_segment=6; //gs
				continue;

			case 0x66:
				operandsizeoverride=1;								
				continue;

			case 0x67:
				addresssizeoverride=1;
				continue;

			case 0xf0:								
				continue;

/*
			case 0xf2:
				continue;

			case 0xf3:
				continue;
*/

			default : 
				ip--;
				break; //no prefix	
		}
	}

	switch (instructionpointer[ip])
	{
		case 0x0f:
		  switch (instructionpointer[ip+1])
			{
				case 0x01:
				{
					handleMODRM(instructionpointer, ip+2, registers, addresssizeoverride, used_segment,&reg1,&reg2,&address);
					sendstringf("handleMODRM result:\n\rreg1=%d reg2=%d address=%8\n\r",reg1,reg2,address);
					switch (reg1) //  /r param (reg1)
					{
						case 2:
  						sendstringf("lgdt\n\r");
							{
								//lgdt
								WORD limit;
								DWORD base;
								instructionpointer=(unsigned char *)address;
								limit=*(WORD *)instructionpointer;
								base=*(DWORD *)instructionpointer;
								if (operandsizeoverride==0)
									base=base & 0xFFFFFF; //only the first 24 bits of the base are used

								vmwrite32(0x6816,(ULONG)base);				
								vmwrite32(0x4810,(ULONG)limit);

								
							}							
  						sendstringf("lgdt\n\r");
              break;

						case 3:
							//lidt
							sendstringf("lidt\n\r");
							{
								WORD limit;
								DWORD base;
								instructionpointer=(unsigned char *)address;
								limit=*(WORD *)instructionpointer;
								base=*(DWORD *)instructionpointer;
								if (operandsizeoverride==0)
									base=base & 0xFFFFFF; //only the first 24 bits of the base are used

								vmwrite32(0x6818,(ULONG)base);				
								vmwrite32(0x4812,(ULONG)limit);
							}
							
              break;
					}
					break;
				}

				case 0x06: //CLTS
				{
					sendstringf("clts\n\r");
					vmwrite32(0x6800,vmread32(0x6800) & 0xFFFFFFF7); //disable bit3  (TS)
				
				}
				break;

				case 0x08: //invd
				{
					sendstringf("invd\n\r");
					__asm("invd\n");					
				}
				break;

				case 0x20: //mov to cr
				{
					sendstringf("Move to control register\n\r");

				}
				break;

				case 0x22: //mov from cr
				{
					sendstringf("Move from control register\n\r");
				}
				break;

				default:
					sendstringf("Unknown instruction\n\r");
			}
	}
	return 0;
}

