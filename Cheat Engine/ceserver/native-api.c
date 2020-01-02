#include "api.h"
#include "porthelp.h"
#include "ceserver.h"
#include "threads.h"
#include "symbols.h"
#include "context.h"
#include "native-api.h"

DWORD AOBScan(HANDLE hProcess, const char* pattern, const char* mask, uint64_t start, uint64_t end, int inc, int protection,uint64_t * match_addr) {

	RegionInfo rinfo;
	uint64_t tmp = start;
	uint64_t tmp2 = tmp;

	char* MemoryBuff = (char*)malloc(4096);
	int patternLength = (int)strlen(mask);

	int result_count = 0;

	while (tmp < end) {
		VirtualQueryEx(hProcess, (void*)tmp, &rinfo, NULL);
		if (rinfo.size == 0) {
			return -1;
		}
		if((rinfo.protection & protection) != 0)
		{
			tmp2 = tmp;
			while (tmp2 < tmp + rinfo.size)
			{
				if (!ReadProcessMemory(hProcess, (void*)tmp2, MemoryBuff, 4096)) {
					break;
				}

				for (int i = 0; i < 4096; i += inc)
				{ 
					for (int k = 0; k < patternLength; k++)
					{

						if (!(mask[k] == '?' || pattern[k] == (MemoryBuff[k])))
						{
							goto label;

						}
					}
					match_addr[result_count] = tmp2;
					result_count++;
					if (result_count >= MAX_HIT_COUNT)return result_count;
					
				label:
					tmp2 += inc; MemoryBuff += inc;
				}
				MemoryBuff -= 4096;
			}
		}
		tmp += rinfo.size;
	}

return result_count;
}
