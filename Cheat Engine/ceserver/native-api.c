
#include <stdio.h>
#include <pthread.h>

#include <sys/mman.h>


#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <stdint.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>

#include <strings.h>

#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/syscall.h>
#include <signal.h>

#ifndef __x86_64__
#include <asm/signal.h>
#endif


#include <sys/eventfd.h>

#include <errno.h>

#include <semaphore.h>
#include <sys/queue.h>
#include <limits.h>

#include <sys/ptrace.h>

#ifndef __x86_64__
#include <linux/elf.h>
#include <linux/uio.h>
#endif


#ifdef __arm__
#ifndef __ANDROID__
#include <linux/user.h>
#endif
#endif


#ifndef __ANDROID__
#if defined(__i386__) || defined(__x86_64__)
#include <sys/user.h>
#endif
#endif

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
