
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <sys/select.h>

#include <zlib.h>

#include <errno.h>
#include <elf.h>
#include <signal.h>
#include <sys/prctl.h>

#include <unistd.h>
#include <errno.h>
#include <dlfcn.h>

#include <sys/mman.h>
#include <libgen.h>
#include <dirent.h>
#include <fcntl.h>





#include "ceserver.h"
#include "porthelp.h"
#include "api.h"
#include "ceservertest.h"
#include "symbols.h"
#include "extensionfunctions.h"
#include "native-api.h"
#include "extensionloader.h"
#include "options.h"

pthread_t pth;
pthread_t identifierthread;
volatile int done;
int PORT;

int ALLOC_WITHOUT_EXTENSION=0; //in case extension loading fails

__thread int isDebuggerThread; //0 when not, else it contains the processhandle
__thread int debugfd;

__thread char* threadname;

#define CESERVERVERSION 6 //6 because modulelist got changed


char versionstring[]="CHEATENGINE Network 2.3";
char *CESERVERPATH;

volatile int connections=0;
pthread_mutex_t connectionsCS;


void initCESERVERPATH()
{
  int l;
  CESERVERPATH=malloc(512);
  CESERVERPATH[0]=0;

  l=readlink("/proc/self/exe", CESERVERPATH, 256);

  //basename and basedir bahave different in android, so just do this:
  while (l)
  {
    if (CESERVERPATH[l]=='/')
      return;
    else
    {
      CESERVERPATH[l]=0;
      l--;
    }


  }
  strcpy(CESERVERPATH,"./");
}

ssize_t recvall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  // enter recvall
  flags=flags | MSG_WAITALL;

  while (sizeleft>0)
  {
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      if (threadname)
        debug_log("%s: Error: recv returned 0\n", threadname);
      else
        debug_log("Error: recv returned 0\n");
      return i;
    }

    if (i==-1)
    {
      debug_log("recv returned -1\n");
      if (errno==EINTR)
      {
        debug_log("errno = EINTR\n");
        i=0;
      }
      else
      {
        debug_log("Error during recvall: %d. errno=%d\n",(int)i, errno);
        return i; //read error, or disconnected
      }

    }

    totalreceived+=i;
    sizeleft-=i;
  }

  // leave recvall
  return totalreceived;
}

ssize_t sendall (int s, void *buf, size_t size, int flags)
{
  ssize_t totalsent=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  while (sizeleft>0)
  {
    ssize_t i=send(s, &buffer[totalsent], sizeleft, flags);

    if (i==0)
    {
      return i;
    }

    if (i==-1)
    {
      if (errno==EINTR)
        i=0;
      else
      {
        debug_log("Error during sendall: %d. error=%s\n",(int)i, strerror(errno));
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i;
  }

  return totalsent;
}

ssize_t sendstring16(int s, char *str, int flags)
{
  uint16_t l;
  if (str)
    l=strlen(str);
  else
    l=0;

  sendall(s, &l,sizeof(l),l?MSG_MORE:flags);
  if (l)
    sendall(s, str, l,flags);

  return l;
}

int sendinteger(int s, int val, int flags)
{
  return sendall(s, &val,sizeof(val),flags);
}


char* receivestring16(int s)
/* Receives a string that is preceded by a 16 bit length identifier (Allocates a string. Clean it up yourself)
 * returns NULL if the length is 0 bytes
 */

{
  char *str;
  uint16_t l;
  recvall(s, &l, sizeof(l),0);

  if (l)
  {
    str=malloc(l+1);
    recvall(s, str, l,0);
    str[l]=0;
    return str;
  }
  else
    return NULL;
}


int DispatchCommand(int currentsocket, unsigned char command)
{
  int r;

  switch (command)
  {
    case CMD_GETVERSION:
    {
      PCeVersion v;
      //debug_log("version request");
      fflush(stdout);
      int versionsize=strlen(versionstring);
#ifdef SHARED_LIBRARY
      versionsize+=3;
#endif
      v=(PCeVersion)malloc(sizeof(CeVersion)+versionsize);
      v->stringsize=versionsize;
      v->version=CESERVERVERSION;

#ifdef SHARED_LIBRARY
      memcpy((char *)v+sizeof(CeVersion),"lib",3);//tell ce it's the lib version
      memcpy((char *)v+sizeof(CeVersion)+3, versionstring, versionsize);

#else
      memcpy((char *)v+sizeof(CeVersion), versionstring, versionsize);
#endif

      //version request
      sendall(currentsocket, v, sizeof(CeVersion)+versionsize, 0);

      free(v);

      break;
    }

    case CMD_SET_CONNECTION_NAME:
    {
      debug_log("CMD_SET_CONNECTION_NAME\n");
      uint32_t namelength;


      if (recvall(currentsocket, &namelength, sizeof(namelength), MSG_WAITALL)>0)
      {
        char name[namelength+1];

        recvall(currentsocket, name, namelength, MSG_WAITALL);
        name[namelength]=0;

        if (threadname)
        {
          free(threadname);
          threadname=NULL;
        }
        threadname=strdup(name);

        debug_log("This thread is called %s\n", name);
      }

      fflush(stdout);

      break;
    }

    case CMD_GETABI:
    {
#ifdef WINDOWS
      unsigned char abi=0;
#else
      unsigned char abi=1;
#endif
      sendall(currentsocket, &abi, sizeof(abi), 0);
      break;
    }

    case CMD_GETARCHITECTURE:
    {
      unsigned char arch;
      HANDLE h;
      //ce 7.4.1+ : Added the processhandle

      debug_log("CMD_GETARCHITECTURE\n");

      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        //intel i386=0
        //intel x86_64=1
        //arm 32 = 2
        //arm 64 = 3
        debug_log("(%d)",h);
        arch=getArchitecture(h);
      }

      if(SPECIFIED_ARCH != 9)
      {
        arch = SPECIFIED_ARCH;
      }
      debug_log("=%d\n", arch);
      sendall(currentsocket, &arch, sizeof(arch), 0);
      break;
    }

    case CMD_CLOSECONNECTION:
    {
      debug_log("Connection %d closed properly\n", currentsocket);
      fflush(stdout);
      close(currentsocket);

      return 0;
    }

    case CMD_TERMINATESERVER:
    {
      debug_log("Command to terminate the server received\n");
      fflush(stdout);
      close(currentsocket);
      exit(0);
    }

    case CMD_STARTDEBUG:
    {
      HANDLE h;
      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        int r;
        debug_log("Calling StartDebug(%d)\n", h);
        r=StartDebug(h);
        sendall(currentsocket, &r, sizeof(r), 0);

        if (r)
        {
          isDebuggerThread=h;
          debugfd=GetDebugPort(h);
        }
      }
      break;
    }

    case CMD_WAITFORDEBUGEVENT:
    {
      struct
      {
        HANDLE pHandle;
        int timeout;
      } wfd;

      if (recvall(currentsocket, &wfd, sizeof(wfd), MSG_WAITALL)>0)
      {
        int r;
        DebugEvent event;
        memset(&event, 0, sizeof(event));

        r=WaitForDebugEvent(wfd.pHandle, &event, wfd.timeout);
        sendall(currentsocket, &r, sizeof(r), r?MSG_MORE:0);

        if (r)
        {
          if (event.debugevent==SIGTRAP)
          {
            debug_log("!!!SIGTRAP!!!\n");
            debug_log("event.address=%llx\n", event.address);
          }

          sendall(currentsocket, &event, sizeof(event),0);
        }
      }
      break;
    }

    case CMD_CONTINUEFROMDEBUGEVENT:
    {
      struct
      {
        HANDLE pHandle;
        int tid;
        int ignore;
      } cfd;

      if (recvall(currentsocket, &cfd, sizeof(cfd), MSG_WAITALL)>0)
      {
        int r;
        // Calling ContinueFromDebugEvent
        r=ContinueFromDebugEvent(cfd.pHandle, cfd.tid, cfd.ignore);
        // Returned from ContinueFromDebugEvent
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_SETBREAKPOINT:
    {
      CeSetBreapointInput sb;

      debug_log("CMD_SETBREAKPOINT. sizeof(sb)=%d\n", sizeof(sb));

      if (recvall(currentsocket, &sb, sizeof(sb), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling SetBreakpoint\n");
        r=SetBreakpoint(sb.hProcess, sb.tid, sb.debugreg, (void *)sb.Address, sb.bptype, sb.bpsize);
        debug_log("SetBreakpoint returned %d\n",r);
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_REMOVEBREAKPOINT:
    {
      CeRemoveBreapointInput rb;

      if (recvall(currentsocket, &rb, sizeof(rb), MSG_WAITALL)>0)
      {
        int r;



        debug_log("%s: Calling RemoveBreakpoint\n", threadname);
        r=RemoveBreakpoint(rb.hProcess, rb.tid, rb.debugreg, rb.wasWatchpoint);
        debug_log("RemoveBreakpoint returned: %d\n", r);
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_GETTHREADCONTEXT:
    {
#pragma pack(1)
      struct
      {
        HANDLE hProcess;
        uint32_t tid;
      } gtc;
#pragma pack()

      CONTEXT Context;
      uint32_t result;

      debug_log("CMD_GETTHREADCONTEXT:\n");

      recvall(currentsocket, &gtc, sizeof(gtc), MSG_WAITALL);

      debug_log("Going to call GetThreadContext(%d, %d, %p)\n", gtc.hProcess, gtc.tid, &Context);
      memset(&Context, 0, sizeof(Context));

      result=GetThreadContext(gtc.hProcess, gtc.tid, &Context);

      debug_log("result=%d\n", result);

      if (result)
      {
        debug_log("Context.structsize=%d\n", Context.structsize);
        uint32_t structsize=Context.structsize;
        sendall(currentsocket, &result, sizeof(result), MSG_MORE);
        sendall(currentsocket, &structsize, sizeof(structsize), MSG_MORE);
        sendall(currentsocket, &Context, structsize, 0); //and context
      }
      else
        sendall(currentsocket, &result, sizeof(result), 0);

      break;

    }

case CMD_SETTHREADCONTEXT:
    {
#pragma pack(1)
      struct
      {
        HANDLE hProcess;
        uint32_t tid;
        uint32_t structsize;
      } stc;
#pragma pack()

      uint32_t result;

      PCONTEXT c;

      debug_log("CMD_SETTHREADCONTEXT:\n");

      recvall(currentsocket, &stc, sizeof(stc), MSG_WAITALL);
      debug_log("hProcess=%d tid=%d structsize=%d\n", stc.hProcess, stc.tid, stc.structsize);

      c=(PCONTEXT)malloc(stc.structsize);
      recvall(currentsocket, c, stc.structsize, MSG_WAITALL);

      debug_log("received a context with data: structsize=%d type=%d\n", c->structsize, c->type);

      debug_log("Going to call SetThreadContext(%d, %d, %p)\n", stc.hProcess, stc.tid, c);

      result=SetThreadContext(stc.hProcess, stc.tid, c);
      free(c);

      debug_log("result=%d\n", result);

      sendall(currentsocket, &result, sizeof(result), 0);


      break;

    }

    case CMD_SUSPENDTHREAD:
    {
      CeSuspendThreadInput st;

      if (recvall(currentsocket, &st, sizeof(st), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling SuspendThread\n");
        r=SuspendThread(st.hProcess, st.tid);
        debug_log("SuspendThread returned\n");
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_RESUMETHREAD:
    {
      CeResumeThreadInput rt;

      if (recvall(currentsocket, &rt, sizeof(rt), MSG_WAITALL)>0)
      {
        int r;

        debug_log("Calling ResumeThread\n");
        r=ResumeThread(rt.hProcess, rt.tid);
        debug_log("ResumeThread returned\n");
        sendall(currentsocket, &r, sizeof(r), 0);
      }
      break;
    }

    case CMD_CLOSEHANDLE:
    {
      HANDLE h;

      if (recvall(currentsocket, &h, sizeof(h), MSG_WAITALL)>0)
      {
        CloseHandle(h);
        int r=1;
        sendall(currentsocket, &r, sizeof(r), 0); //stupid naggle

      }
      else
      {
        debug_log("Error during read for CMD_CLOSEHANDLE\n");
        close(currentsocket);
        fflush(stdout);
        return 0;
      }
      break;
    }


    case CMD_CREATETOOLHELP32SNAPSHOTEX:
    {
      CeCreateToolhelp32Snapshot params;
      //debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX\n");

      if (recvall(currentsocket, &params, sizeof(CeCreateToolhelp32Snapshot), MSG_WAITALL) > 0)
      {
        HANDLE r=CreateToolhelp32Snapshot(params.dwFlags, params.th32ProcessID);

        if ((params.dwFlags & TH32CS_SNAPTHREAD)==TH32CS_SNAPTHREAD)
        {
          //send the list of threadid's

          if (r)
          {
            PThreadList tl=(PThreadList)GetPointerFromHandle(r);
            sendall(currentsocket, &tl->threadCount, sizeof(int), MSG_MORE);
            sendall(currentsocket, &tl->threadList[0], tl->threadCount*sizeof(int),0);

            CloseHandle(r);
          }
          else
          {
            int n=0;
            sendall(currentsocket, &n, sizeof(int), 0);
          }
        }
        else
        if ((params.dwFlags & TH32CS_SNAPMODULE)==TH32CS_SNAPMODULE)
        {
          ModuleListEntry me;

          char *outputstream;
          int pos=0;

         // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX with TH32CS_SNAPMODULE\n");

          outputstream=malloc(65536);
          memset(outputstream,0,65536);

          if (r && (Module32First(r, &me))) do
          {
            int namelen=strlen(me.moduleName);
            PCeModuleEntry m;


            if ((pos+sizeof(CeModuleEntry)+namelen) > 65536)
            {
              //flush the stream
             // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList flush in loop\n");
              sendall(currentsocket, outputstream, pos, 0);
              pos=0;
            }

            m=(PCeModuleEntry)&outputstream[pos];
            m->modulebase=me.baseAddress;
            m->modulesize=me.moduleSize;
            m->modulefileoffset=me.fileOffset;
            m->modulenamesize=namelen;
            m->modulepart=me.part;
            m->result=1;

            // Sending %s size %x\n, me.moduleName, r->modulesize
            memcpy((char *)m+sizeof(CeModuleEntry), me.moduleName, namelen);

            pos+=sizeof(CeModuleEntry)+namelen;


          } while (Module32Next(r, &me));

          if (pos) //flush the stream
          {
           // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList flush after loop\n");
            sendall(currentsocket, outputstream, pos, 0);
          }

          //send the end of list module
         // debug_log("CMD_CREATETOOLHELP32SNAPSHOTEX: ModuleList end of list\n");

          CeModuleEntry eol;
          eol.result=0;
          eol.modulenamesize=0;
          sendall(currentsocket, &eol, sizeof(eol), 0);

          free(outputstream);

          if (r)
            CloseHandle(r);

        }
        else
        {
          sendall(currentsocket, &r, sizeof(HANDLE), 0); //the others are not yet implemented
        }
      }
      else
      {
        debug_log("Error during read for CMD_CREATETOOLHELP32SNAPSHOTEX\n");
        fflush(stdout);
        close(currentsocket);
        return 0;
      }
      break;
    }

    case CMD_CREATETOOLHELP32SNAPSHOT:
    {
      CeCreateToolhelp32Snapshot params;
      HANDLE result;

      //debug_log("CMD_CREATETOOLHELP32SNAPSHOT\n");

      if (recvall(currentsocket, &params, sizeof(CeCreateToolhelp32Snapshot), MSG_WAITALL) > 0)
      {
        //debug_log("Calling CreateToolhelp32Snapshot\n");
        result=CreateToolhelp32Snapshot(params.dwFlags, params.th32ProcessID);
       // debug_log("result of CreateToolhelp32Snapshot=%d\n", result);

       // fflush(stdout);





        sendall(currentsocket, &result, sizeof(HANDLE), 0);

      }
      else
      {
        debug_log("Error during read for CMD_CREATETOOLHELP32SNAPSHOT\n");
        fflush(stdout);
        close(currentsocket);
        return 0;
      }

      break;
    }

    case CMD_MODULE32FIRST: //slightly obsolete now
    case CMD_MODULE32NEXT:
    {
      HANDLE toolhelpsnapshot;
      if (recvall(currentsocket, &toolhelpsnapshot, sizeof(toolhelpsnapshot), MSG_WAITALL) >0)
      {
        BOOL result;
        ModuleListEntry me;
        CeModuleEntry *r;
        int size;

        if (command==CMD_MODULE32FIRST)
          result=Module32First(toolhelpsnapshot, &me);
        else
          result=Module32Next(toolhelpsnapshot, &me);

        if (result)
        {
          size=sizeof(CeModuleEntry)+ strlen(me.moduleName);
          r=(PCeModuleEntry)malloc(size);
          r->modulebase=me.baseAddress;
          r->modulesize=me.moduleSize;
          r->modulenamesize=strlen(me.moduleName);
          r->modulefileoffset=me.fileOffset;
          r->modulepart=me.part;


          // Sending %s size %x\n, me.moduleName, r->modulesize
          memcpy((char *)r+sizeof(CeModuleEntry), me.moduleName, r->modulenamesize);
        }
        else
        {
          size=sizeof(CeModuleEntry);
          r=(PCeModuleEntry)malloc(size);
          r->modulebase=0;
          r->modulesize=0;
          r->modulenamesize=0;
          r->modulepart=0;
        }

        r->result=result;
/*
        if (result)
        {
          debug_log("CMD_MODULE32 returning %s : base=%x size=%x part=%d (me.part=%d)\n", me.moduleName, r->modulebase, r->modulesize, r->modulepart, me.part);

        }
        else
          debug_log("CMD_MODULE32 returning <nomodule> : base=%x size=%x part=%d\n", r->modulebase, r->modulesize, r->modulepart);*/

        sendall(currentsocket, r, size, 0);

        free(r);
      }
      break;
    }

    case CMD_PROCESS32FIRST: //obsolete
    case CMD_PROCESS32NEXT:
    {
      HANDLE toolhelpsnapshot;
      if (recvall(currentsocket, &toolhelpsnapshot, sizeof(toolhelpsnapshot), MSG_WAITALL) >0)
      {
        ProcessListEntry pe;
        BOOL result;
        CeProcessEntry *r;
        int size;

        if (command==CMD_PROCESS32FIRST)
          result=Process32First(toolhelpsnapshot, &pe);
        else
          result=Process32Next(toolhelpsnapshot, &pe);

        //  debug_log("result=%d\n", result);

        if (result)
        {
          size=sizeof(CeProcessEntry)+ strlen(pe.ProcessName);
          r=(PCeProcessEntry)malloc(size);
          r->processnamesize=strlen(pe.ProcessName);
          r->pid=pe.PID;
          memcpy((char *)r+sizeof(CeProcessEntry), pe.ProcessName, r->processnamesize);
        }
        else
        {
          size=sizeof(CeProcessEntry);
          r=(PCeProcessEntry)malloc(size);
          r->processnamesize=0;
          r->pid=0;
        }

        r->result=result;

        sendall(currentsocket, r, size, 0);

        free(r);

      }
      break;
    }

    case CMD_READPROCESSMEMORY:
    {
      CeReadProcessMemoryInput c;

      r=recvall(currentsocket, &c, sizeof(c), MSG_WAITALL);
      if (r>0)
      {
        PCeReadProcessMemoryOutput o=NULL;
        o=(PCeReadProcessMemoryOutput)malloc(sizeof(CeReadProcessMemoryOutput)+c.size);

        o->read=ReadProcessMemory(c.handle, (void *)(uintptr_t)c.address, &o[1], c.size);

        if (c.compress)
        {
          //compress the output
#define COMPRESS_BLOCKSIZE (64*1024)
          int i;
          unsigned char *uncompressed=(unsigned char *)&o[1];
          uint32_t uncompressedSize=o->read;
          uint32_t compressedSize=0;
          int maxBlocks=1+(c.size / COMPRESS_BLOCKSIZE);

          unsigned char **compressedBlocks=malloc(maxBlocks*sizeof(unsigned char *) ); //send in blocks of 64kb and reallocate the pointerblock if there's not enough space
          int currentBlock=0;

          z_stream strm;
          strm.zalloc = Z_NULL;
          strm.zfree = Z_NULL;
          strm.opaque = Z_NULL;
          deflateInit(&strm, c.compress);

          compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
          strm.avail_out=COMPRESS_BLOCKSIZE;
          strm.next_out=compressedBlocks[currentBlock];

          strm.next_in=uncompressed;
          strm.avail_in=uncompressedSize;

          while (strm.avail_in)
          {
            r=deflate(&strm, Z_NO_FLUSH);
            if (r!=Z_OK)
            {
              if (r==Z_STREAM_END)
                break;
              else
              {
                debug_log("Error while compressing\n");
                break;
              }
            }

            if (strm.avail_out==0)
            {
              //new output block
              currentBlock++;
              if (currentBlock>=maxBlocks)
              {
                //list was too short, reallocate
                debug_log("Need to realloc the pointerlist (p1)\n");

                maxBlocks*=2;
                compressedBlocks=realloc(compressedBlocks, maxBlocks*sizeof(unsigned char*));
              }
              compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
              strm.avail_out=COMPRESS_BLOCKSIZE;
              strm.next_out=compressedBlocks[currentBlock];
            }
          }
          // finishing compressiong
          while (1)
          {

            r=deflate(&strm, Z_FINISH);

            if (r==Z_STREAM_END)
              break; //done

            if (r!=Z_OK)
            {
              debug_log("Failure while finishing compression:%d\n", r);
              break;
            }

            if (strm.avail_out==0)
            {
              //new output block
              currentBlock++;
              if (currentBlock>=maxBlocks)
              {
                //list was too short, reallocate
                debug_log("Need to realloc the pointerlist (p2)\n");
                maxBlocks*=2;
                compressedBlocks=realloc(compressedBlocks, maxBlocks*sizeof(unsigned char*));
              }
              compressedBlocks[currentBlock]=malloc(COMPRESS_BLOCKSIZE);
              strm.avail_out=COMPRESS_BLOCKSIZE;
              strm.next_out=compressedBlocks[currentBlock];
            }
          }
          deflateEnd(&strm);

          compressedSize=strm.total_out;
          // Sending compressed data
          sendall(currentsocket, &uncompressedSize, sizeof(uncompressedSize), MSG_MORE); //followed by the compressed size
          sendall(currentsocket, &compressedSize, sizeof(compressedSize), MSG_MORE); //the compressed data follows
          for (i=0; i<=currentBlock; i++)
          {
            if (i!=currentBlock)
              sendall(currentsocket, compressedBlocks[i], COMPRESS_BLOCKSIZE, MSG_MORE);
            else
              sendall(currentsocket, compressedBlocks[i], COMPRESS_BLOCKSIZE-strm.avail_out, 0); //last one, flush

            free(compressedBlocks[i]);
          }
          free(compressedBlocks);
        }
        else
          sendall(currentsocket, o, sizeof(CeReadProcessMemoryOutput)+o->read, 0);

        if (o)
          free(o);
      }
      break;
    }

    case CMD_WRITEPROCESSMEMORY:
    {
      CeWriteProcessMemoryInput c;

      debug_log("CMD_WRITEPROCESSMEMORY:\n");

      r=recvall(currentsocket, &c, sizeof(c), MSG_WAITALL);
      if (r>0)
      {
        CeWriteProcessMemoryOutput o;
        unsigned char *buf;

        debug_log("recv returned %d bytes\n", r);
        debug_log("c.size=%d\n", c.size);

        if (c.size)
        {
          buf=(unsigned char *)malloc(c.size);

          r=recvall(currentsocket, buf, c.size, MSG_WAITALL);
          if (r>0)
          {
            debug_log("received %d bytes for the buffer. Wanted %d\n", r, c.size);
            o.written=WriteProcessMemory(c.handle, (void *)(uintptr_t)c.address, buf, c.size);

            r=sendall(currentsocket, &o, sizeof(CeWriteProcessMemoryOutput), 0);
            debug_log("wpm: returned %d bytes to caller\n", r);

          }
          else
            debug_log("wpm recv error while reading the data\n");

          free(buf);
        }
        else
        {
          debug_log("wpm with a size of 0 bytes");
          o.written=0;
          r=sendall(currentsocket, &o, sizeof(CeWriteProcessMemoryOutput), 0);
          debug_log("wpm: returned %d bytes to caller\n", r);
        }
      }
      else
      {
        debug_log("RPM: recv failed\n");
      }
      break;
    }

    case CMD_VIRTUALQUERYEXFULL:
    {
      CeVirtualQueryExFullInput c;

      r=recvall(currentsocket, &c, sizeof(c), MSG_WAITALL);
      if (r>0)
      {
        RegionInfo *rinfo=NULL;
        uint32_t count=0;
        if (VirtualQueryExFull(c.handle, c.flags, &rinfo, &count))
        {
          int i;

          sendall(currentsocket, &count, sizeof(count),0);

          for (i=0; i<count; i++)
            sendall(currentsocket, &rinfo[i], sizeof(RegionInfo),0);

          if (rinfo)
            free(rinfo);
        }
      }
      break;
    }
    case CMD_GETREGIONINFO:
    case CMD_VIRTUALQUERYEX:
    {
      CeVirtualQueryExInput c;
      r=recvall(currentsocket, &c, sizeof(c), MSG_WAITALL);
      if (r>0)
      {
        RegionInfo rinfo;
        CeVirtualQueryExOutput o;

        if (sizeof(uintptr_t)==4)
        {
          if (c.baseaddress>0xFFFFFFFF)
          {
            o.result=0;
            sendall(currentsocket, &o, sizeof(o), 0);
            break;
          }
        }

        char mapsline[200];

        if (command==CMD_VIRTUALQUERYEX)
          o.result=VirtualQueryEx(c.handle, (void *)(uintptr_t)c.baseaddress, &rinfo, NULL);
        else
        if (command==CMD_GETREGIONINFO)
          o.result=VirtualQueryEx(c.handle, (void *)(uintptr_t)c.baseaddress, &rinfo, mapsline);

        o.protection=rinfo.protection;
        o.baseaddress=rinfo.baseaddress;
        o.type=rinfo.type;
        o.size=rinfo.size;

        if (command==CMD_VIRTUALQUERYEX)
          sendall(currentsocket, &o, sizeof(o), 0);
        else
        if (command==CMD_GETREGIONINFO)
        {
          sendall(currentsocket, &o, sizeof(o), MSG_MORE);
          {
            uint8_t size=strlen(mapsline);
            sendall(currentsocket, &size, sizeof(size), MSG_MORE);
            sendall(currentsocket, mapsline, size, 0);
          }
        }
      }
      break;
    }
    case CMD_OPENPROCESS:
    {
      int pid=0;

      r=recvall(currentsocket, &pid, sizeof(int), MSG_WAITALL);
      if (r>0)
      {
        int processhandle;

        debug_log("OpenProcess(%d)\n", pid);
        processhandle=OpenProcess(pid);

        debug_log("processhandle=%d\n", processhandle);
        sendall(currentsocket, &processhandle, sizeof(int), 0);
      }
      else
      {
        debug_log("Error\n");
        fflush(stdout);
        close(currentsocket);
        return 0;
      }
      break;
    }

    case CMD_GETSYMBOLLISTFROMFILE:
    {
      //get the list and send it to the client
      //zip it first
      struct {
        uint32_t fileoffset;
        uint32_t symbolpathsize;
      } input;

      if (recvall(currentsocket, &input, sizeof(input), MSG_WAITALL)>0)
      {
        if (input.fileoffset)
          debug_log("CMD_GETSYMBOLLISTFROMFILE with fileoffset=%x\n",input.fileoffset);

        char *symbolpath=(char *)malloc(input.symbolpathsize+1);
        symbolpath[input.symbolpathsize]='\0';

        if (recvall(currentsocket, symbolpath, input.symbolpathsize, MSG_WAITALL)>0)
        {
          unsigned char *output=NULL;

          if (input.fileoffset)
            debug_log("symbolpath=%s\n", symbolpath);

          if (memcmp("/dev/", symbolpath, 5)!=0) //don't even bother if it's a /dev/ file
            GetSymbolListFromFile(symbolpath, input.fileoffset, &output);

          if (output)
          {
            if (input.fileoffset)
            {
              debug_log("output is not NULL (%p)\n", output);
              debug_log("Sending %d bytes\n", *(uint32_t *)&output[4]);
              fflush(stdout);
            }
            sendall(currentsocket, output, *(uint32_t *)&output[4], 0); //the output buffer contains the size itself
            free(output);
          }
          else
          {
            if (input.fileoffset)
              debug_log("Sending 8 bytes (fail)\n");

            uint64_t fail=0;
            sendall(currentsocket, &fail, sizeof(fail), 0); //just write 0
          }
        }
        else
        {
          //debug_log("Failure getting symbol path\n");
          close(currentsocket);
        }
        free(symbolpath);
      }
      break;
    }

    case CMD_LOADEXTENSION:
    {
      //Load the extension if it isn't loaded yet
      //LOADEXTENSION(handle)
      uint32_t handle;
      if (recvall(currentsocket, &handle, sizeof(handle),0)>0)
      {
        int result=loadCEServerExtension(handle);

        sendall(currentsocket, &result, sizeof(result),0);
      }

      break;
    }

    case CMD_ALLOC:
    {
      //ALLOC(processhandle, preferedbase, size)
      CeAllocInput c;
      debug_log("CESERVER: CMD_ALLOC\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        uint64_t address;
        debug_log("c.hProcess=%d\n", c.hProcess);
        debug_log("c.preferedBase=%llx\n", c.preferedBase);
        debug_log("c.size=%d\n", c.size);
        debug_log("c.windowsprotection=%x\n", c.windowsprotection);

        if (ALLOC_WITHOUT_EXTENSION)
        {
          debug_log("ALLOC_WITHOUT_EXTENSION==1\n");
          fflush(stdout);
          address=allocWithoutExtension(c.hProcess, (void*)c.preferedBase, c.size, windowsProtectionToLinux(c.windowsprotection));
        }
        else
        {
          debug_log("ALLOC_WITHOUT_EXTENSION==0\n");
          fflush(stdout);

          address=ext_alloc(c.hProcess, c.preferedBase, c.size, windowsProtectionToLinux(c.windowsprotection));
        }

        sendall(currentsocket, &address, sizeof(address),0);
      }

      break;
    }

    case CMD_FREE:
    {
      CeFreeInput c;
      debug_log("CESERVER: CMD_FREE\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        uint32_t r;
        r=ext_free(c.hProcess, c.address, c.size);

        sendall(currentsocket, &r, sizeof(r),0);
      }

      break;
    }

    case CMD_CREATETHREAD:
    {
      CeCreateThreadInput c;
      debug_log("CESERVER: CMD_CREATETHREAD\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        uint64_t th;
        HANDLE h;
        th=ext_createThread(c.hProcess, c.startaddress, c.startaddress);

        debug_log("returned from ext_createthread\n");

        if (th) //create a handle for this object
        {
          uint64_t *threadobject=(uint64_t *)malloc(sizeof(uint64_t));

          *threadobject=th;
          h=CreateHandleFromPointer(threadobject, htNativeThreadHandle);
        }
        else
          h=0;

        sendall(currentsocket, &h, sizeof(h),0);
      }
      break;
    }

    case CMD_LOADMODULEEX:
    {
      CeLoadModuleInputEx c;

      debug_log("CESERVER: CMD_LOADMODULEEX\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        char modulepath[c.modulepathlength+1];

        if (recvall(currentsocket, &modulepath, c.modulepathlength,0)>0)
        {
          uint64_t result;
          modulepath[c.modulepathlength]=0;

          result=ext_loadModuleEx(c.hProcess, c.dlopenaddress, modulepath);



          sendall(currentsocket, &result, sizeof(result),0);
        }
      }
      break;
    }

    case CMD_LOADMODULE:
    {
      CeLoadModuleInput c;

      debug_log("CESERVER: CMD_LOADMODULE\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        char modulepath[c.modulepathlength+1];

        if (recvall(currentsocket, &modulepath, c.modulepathlength,0)>0)
        {
          uint64_t result;
          modulepath[c.modulepathlength]=0;

          result=ext_loadModule(c.hProcess, modulepath);


          debug_log("ext_loadModule returned %llx\n", result);

          sendall(currentsocket, &result, sizeof(result),0);
        }
      }
      break;
    }

    case CMD_SPEEDHACK_SETSPEED:
    {
      CeSpeedhackSetSpeedInput c;
      debug_log("CESERVER: CMD_SPEEDHACK_SETSPEED\n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        uint32_t r;
        r=ext_speedhack_setSpeed(c.hProcess, c.speed);

        sendall(currentsocket, &r, sizeof(r),0);
      }

      break;
    }

    case CMD_CHANGEMEMORYPROTECTION:
    {
      CeChangeMemoryProtection c;
      debug_log("CESERVER: CMD_CHANGEMEMORYPROTECTION \n");
      if (recvall(currentsocket, &c, sizeof(c),0)>0)
      {
        RegionInfo ri;
        uint32_t r;
        uint32_t oldprotection;
        uint32_t newprotection;

        if (VirtualQueryEx(c.hProcess, (void*)c.address, &ri, NULL))
        {
          oldprotection=ri.protection;


          //convert the given protection to a linux protection
          newprotection=windowsProtectionToLinux(c.windowsprotection);


          r=ext_changememoryprotection(c.hProcess, c.address, c.size, newprotection);
        }
        else
          debug_log("Failure getting the old protection");

        sendall(currentsocket, &r, sizeof(r),MSG_MORE);
        sendall(currentsocket, &oldprotection, sizeof(oldprotection),0);
      }

      break;
    }

    case CMD_GETOPTIONS:
    {
      handleGetOptions(currentsocket);
      break;
    }

    case CMD_GETOPTIONVALUE:
    {
      handleGetOption(currentsocket);
      break;
    }

    case CMD_SETOPTIONVALUE:
    {
      handleSetOption(currentsocket);
      break;
    }

    case CMD_OPENNAMEDPIPE:
    {
      HANDLE pipehandle=0;
      uint32_t timeout;
      debug_log("CMD_OPENNAMEDPIPE\n");
      char *pipename=receivestring16(currentsocket);
      recvall(currentsocket, &timeout, sizeof(timeout),0);
      if (pipename)
      {
        debug_log("pipename=%s\n", pipename);
        pipehandle=OpenPipe(pipename, timeout);
        free(pipename);
      }

      debug_log("sending pipehandle %d to caller\n",pipehandle);

      sendall(currentsocket, &pipehandle, sizeof(HANDLE),0 );
      break;
    }

    case CMD_PIPEREAD:
    {
      CeReadPipe c;
      int32_t count=0;
      recvall(currentsocket, &c, sizeof(c),0);

      debug_log("CMD_PIPEREAD: %d bytes\n",c.size);
      if (c.size)
      {
        void *buf=malloc(c.size);
        count=ReadPipe(c.hPipe, buf, c.size, c.timeout);

        sendall(currentsocket, &count, sizeof(count), count>0?MSG_MORE:0); //can be negative
        if (count>0)
          sendall(currentsocket, buf, count,0);
      }
      else
        sendall(currentsocket, &count, sizeof(count), 0);


      break;
    }

    case CMD_PIPEWRITE:
    {
      CeWritePipe c;
      uint32_t count=0;
      c.size=0;
      recvall(currentsocket, &c, sizeof(c),0);

     // debug_log("CMD_PIPEWRITE:hPipe=%d count=%d  timeout:%d\n",c.hPipe, c.size, c.timeout);

      if (c.size)
      {
       // debug_log("valid size");
        void *buf=malloc(c.size);
        if (buf)
        {
         // debug_log("allocated memory. Calling recvall\n");
          count=recvall(currentsocket, buf, c.size,0);
          if (count>0)
          {
            //debug_log("recvall returned %d\n", count);
            //debug_log("Sending this to the pipe\n", count);

           // fflush(stdout);
            count=WritePipe(c.hPipe,buf, count, c.timeout);

            //debug_log("WritePipe returned %d\n", count);
          }

          free(buf);
        }
        else
          debug_log("CMD_PIPEWRITE: failed to allocate %d bytes\n", c.size);
      }

      sendall(currentsocket, &count, sizeof(count), 0);
      break;
    }

    case CMD_GETCESERVERPATH:
    {
      sendstring16(currentsocket, CESERVERPATH, 0);
      break;
    }

    case CMD_ISANDROID:
    {
      unsigned char r;
#ifdef __ANDROID__
      r=1;
#else
      r=0;
#endif
      sendall(currentsocket, &r,sizeof(r),0);

      break;
    }

    case CMD_SETCURRENTPATH:
    {
      char *path=receivestring16(currentsocket);
      char r=chdir(path)==0;
      sendall(currentsocket, &r,1,0);

      free(path);
      break;
    }

    case CMD_GETCURRENTPATH:
    {
      char *path=(char*)malloc(PATH_MAX);
      char *p=getcwd(path, PATH_MAX-1);
      sendstring16(currentsocket, p,0);
      free(path);
      break;
    }

    case CMD_ENUMFILES:
    {
      char *path=receivestring16(currentsocket);
      DIR *d=opendir(path);
      if (d)
      {
        struct dirent* dc;

        while (dc=readdir(d))
        {
          if ((dc->d_name) && (strlen(dc->d_name)))
          {
            sendstring16(currentsocket, dc->d_name, MSG_MORE);
            sendall(currentsocket, &dc->d_type,1,MSG_MORE);
          }
        }

        closedir(d);
      }
      sendstring16(currentsocket, NULL,0);

      free(path);


      break;
    }

    case CMD_GETFILEPERMISSIONS:
    {
      char r;
      char *path=receivestring16(currentsocket);
      struct stat s;
      if (lstat(path, &s)==0)
      {
        uint32_t mode=s.st_mode & 0xfff;
        r=1;
        sendall(currentsocket, &r,1,MSG_MORE);
        sendall(currentsocket, &mode, sizeof(uint32_t),0);

      }
      else
      {
        r=0;
        sendall(currentsocket, &r,1,0);
      }

      free(path);
      break;
    }

    case CMD_SETFILEPERMISSIONS:
    {
      char *path=receivestring16(currentsocket);
      uint32_t mode;
      char r;
      recvall(currentsocket, &mode, sizeof(mode),0);

      r=chmod(path, mode)==0;


      sendall(currentsocket, &r, 1,0);
      free(path);
      break;
    }

    case CMD_GETFILE:
    {
      char *path=receivestring16(currentsocket);
      int f=open(path,O_RDONLY);

      if (f!=-1)
      {
        uint32_t filesize=lseek(f,0, SEEK_END);

        lseek(f,0, SEEK_SET);

        if (filesize)
        {
          char *contents=malloc(filesize);
          filesize=read(f, contents, filesize);

          if (filesize!=0xffffffff)
          {
            sendall(currentsocket, &filesize, sizeof(filesize),MSG_MORE);
            sendall(currentsocket, contents, filesize,0);
          }
          else
            sendall(currentsocket, &filesize, sizeof(filesize),0); //read error

          free(contents);
        }
        else
          sendall(currentsocket, &filesize, sizeof(filesize),0); //filesize of 0
      }
      else
      {
        uint32_t invalid=0xffffffff;
        sendall(currentsocket, &invalid,sizeof(uint32_t),0); //open file error
      }

      free(path);
      break;
    }

    case CMD_PUTFILE:
    {
      char *path=receivestring16(currentsocket);
      uint32_t filesize;
      char *contents;
      int f,r;
      r=0;
      recvall(currentsocket, &filesize,sizeof(filesize),0);

      contents=malloc(filesize);
      recvall(currentsocket, contents, filesize,0);

      f=creat(path,0777);
      if (f!=-1)
      {
        if (write(f,contents, filesize)!=-1)
          r=1; //success
      }


      sendall(currentsocket, &r,1,0);

      free(path);
      free(contents);
      break;
    }

    case CMD_CREATEDIR:
    {
      debug_log("CMD_CREATEDIR\n");
      char *path=receivestring16(currentsocket);

      debug_log("Creating %s\n", path);
      int r=mkdir(path,0777)==0;

      if (!r)
      {
        debug_log("mkdir error: %s\n", strerror(errno));
      }
      else
        debug_log("success\n");


      sendall(currentsocket, &r,1,0);

      free(path);
      break;
    }

    case CMD_DELETEFILE:
    {
      char *path=receivestring16(currentsocket);
      int r=unlink(path)==0;

      if (!r)
      {
        if (errno==EISDIR)
        {
          r=rmdir(path)==0;
        }
      }


      sendall(currentsocket, &r,1,0);

      free(path);
      break;
    }

	case CMD_AOBSCAN:
	{
		CeAobScanInput c;
		debug_log("CESERVER: CMD_AOBSCAN\n");
		if (recvall(currentsocket, &c, sizeof(c), 0) > 0)
		{
	
			int n = c.scansize;
			char* data = (char*)malloc(n*2);
			uint64_t* match_addr = (uint64_t*)malloc(sizeof(uint64_t) * MAX_HIT_COUNT);

			if (recvall(currentsocket, data, n*2, 0)>0)
			{
				char* pattern = (char*)malloc(n);
				char* mask = (char*)malloc(n);

				memcpy(pattern, data, n);
				memcpy(mask, &data[n], n);
				int ret = AOBScan(c.hProcess, pattern, mask, c.start, c.end, c.inc,c.protection, match_addr);
				debug_log("HIT_COUNT:%d\n", ret);
				free(pattern);
				free(mask);
				sendall(currentsocket, &ret, 4, 0);
				sendall(currentsocket, match_addr, sizeof(uint64_t)* ret, 0);
			}
			free(data);
			free(match_addr);
		}

		break;
	}

	debug_log("Unknown command received\n");
  fflush(stdout);
  close(currentsocket);

  return 0;


  }

  return 10000; //got to here
}

int CheckForAndDispatchCommand(int currentsocket)
{
  int r;
  unsigned char command;

  r=recv(currentsocket, &command, 1, MSG_DONTWAIT);
  if (r==1)
  {
    DispatchCommand(currentsocket, command);
    return 1;
  }

  return 0;
}



void threadStartedEvent(int socket)
{
  pthread_mutex_lock(&connectionsCS); //so there's no ptrace_attach busy when attaching after opening and reading memory
  connections++;
  pthread_mutex_unlock(&connectionsCS);
}

void threadClosedEvent(int socket)
{
  pthread_mutex_lock(&connectionsCS);
  connections--;

  if (connections==0)
  {
    debug_log("All connections gone. Closing all pipes (if any)\n");
    CloseAllPipes();
  }

  if (connections<0)
  {
    debug_log("Connection counter is fucked!\n");
    CloseAllPipes();

    connections=0;
  }
  pthread_mutex_unlock(&connectionsCS);

}


void *newconnection(void *arg)
{
  int s=(uintptr_t)arg;
  unsigned char command;

  int currentsocket=s;

  threadStartedEvent(currentsocket);

  threadname=NULL;
  isDebuggerThread=0;
  debugfd=-1;
  //printf("new connection. Using socket %d\n", s);
  while (done==0)
  {
    int r;
    //check if this is a debugger thread
    if (isDebuggerThread && (debugfd!=-1))
    {
      //wait for s and debugfd
      fd_set readfds;
      int maxfd=s;
      int sret;

      FD_ZERO(&readfds);
      FD_SET(s, &readfds);
      FD_SET(debugfd, &readfds);

      if (debugfd>maxfd)
        maxfd=debugfd;
      //  Waiting for multiple sockets
      sret=select(maxfd+1, &readfds, NULL, NULL,NULL );
      //  Wait done
      if (sret==-1)
      {
        if (errno==EINTR)
        {
          debug_log("Interrupted by signal. Checking again\n");
          continue;
        }
        else
        {
          debug_log("WTF?: %d\n", errno);
          while (1) sleep(60);
        }
      }

      if (FD_ISSET(debugfd, &readfds) && FD_ISSET(s, &readfds))
      {
        //alternate between s and debugfd if both are constantly readable
        if (currentsocket==debugfd)
          currentsocket=s;
        else
          currentsocket=debugfd;
      }
      else //not both readable
      if (FD_ISSET(debugfd, &readfds))
        currentsocket=debugfd;
      else
      if (FD_ISSET(s, &readfds))
        currentsocket=s;
      else //none readable (shouldn't really happen, but whatever...), wait again
        continue;
    }
    else
      currentsocket=s;

    r=recvall(currentsocket, &command, 1, MSG_WAITALL);

    if (r>0)
    {
      DispatchCommand(currentsocket, command);
    }
    else
    if (r==-1)
    {
      debug_log("read error on socket %d (%d)\n", s, errno);
      fflush(stdout);

      threadClosedEvent(currentsocket);
      close(currentsocket);

      return NULL;
    }
    else
    if (r==0)
    {
      if (threadname)
      {
        debug_log("%s has disconnected\n", threadname);

        if (isDebuggerThread)
        {
          debug_log("This was a debugger thread\n");
          StopDebug(isDebuggerThread);
          //find the process that this debugger belongs to

        }
        //

        //if p->debuggerThreadID
      }
      else
        debug_log("Peer has disconnected\n");
      fflush(stdout);

      threadClosedEvent(currentsocket);
      close(currentsocket);
      return NULL;
    }
  }

  threadClosedEvent(s);
  close(s);

  return NULL;
}

void *IdentifierThread(void *arg)
{
  int i;
  int s;
  int v=1;
#pragma pack(1)
  struct
  {
    uint32_t checksum;
    uint16_t port;
  } packet;
#pragma pack()

  socklen_t clisize;
  struct sockaddr_in addr, addr_client;

  debug_log("IdentifierThread active\n");

  fflush(stdout);

  s=socket(PF_INET, SOCK_DGRAM, 0);
  v=1;
  i=setsockopt(s, SOL_SOCKET, SO_BROADCAST, &v, sizeof(v));

  debug_log("IdentifierThread: setting SO_BROADCAST returned %d\n", i);

  memset(&addr, 0, sizeof(addr));

  addr.sin_family=PF_INET;
  addr.sin_addr.s_addr=INADDR_ANY;
  addr.sin_port=htons(3296);
  i=bind(s, (struct sockaddr *)&addr, sizeof(addr));

  if (i>=0)
  {
    while (1)
    {
      memset(&addr_client, 0, sizeof(addr_client));
      addr_client.sin_family=PF_INET;
      addr_client.sin_addr.s_addr=INADDR_BROADCAST;
      addr_client.sin_port=htons(3296);

      clisize=sizeof(addr_client);

      debug_log("IdentifierThread: Calling recvfrom size %d\n",sizeof(packet));
      fflush(stdout);
      i=recvfrom(s, &packet, sizeof(packet), 0, (struct sockaddr *)&addr_client, &clisize);

      debug_log("IdentifierThread: recvfrom returned %d\n", i);
      fflush(stdout);

      //i=recv(s, &v, sizeof(v), 0);
      if (i>=0)
      {

        debug_log("Identifier thread received a message :%d\n",v);
        debug_log("sizeof(packet)=%ld\n", sizeof(packet));

        debug_log("packet.checksum=%x\n", packet.checksum);
        packet.checksum*=0xce;
        packet.port=PORT;
        debug_log("packet.checksum=%x\n", packet.checksum);

        // packet.checksum=00AE98E7 - y=8C7F09E2

        fflush(stdout);

        i=sendto(s, &packet, sizeof(packet), 0, (struct sockaddr *)&addr_client, clisize);
        debug_log("sendto returned %d\n",i);
      }
      else
        debug_log("recvfrom failed\n");

      fflush(stdout);
    }
  }
  else
    debug_log("bind failed\n");

  debug_log("IdentifierThread exit\n");

  return 0;
}

#ifdef traptest
//test succes. This can be used as a vehdebug interface if ceserver is functioning as a injected .so
struct sigaction traphandler, oldtraphandler;


void mytraphandler(int signr, siginfo_t *info, struct ucontext_t *uap)
{
  //uap->uc_mcontext.gregs[REG_RIP]=0;
  printf("hello\n");

 // uap->uc_mcontext.gregs[16]=0;

}
#endif

#ifdef SHARED_LIBRARY
int ceserver()
#else
int main(int argc, char *argv[])
#endif
{
  int s;
  int b;
  int l;
  int a;

  pthread_mutex_init(&connectionsCS, NULL);

  initAPI();

  socklen_t clisize;
  struct sockaddr_in addr, addr_client;

  PORT=52736;

//        process_vm_readv(p->pid, NULL, 0,NULL,0,0);
  //(pid_t __pid, const struct iovec* __local_iov, unsigned long __local_iov_count, const struct iovec* __remote_iov, unsigned long __remote_iov_count, unsigned long __flags)

  #ifndef SHARED_LIBRARY
  int TEST_MODE = 0;
  int TEST_PID = 0;
  int opt;

  opterr = 0;

  int argv_attach_pid;
  int argv_search_option;
  int argv_port;
  int argv_arch;
  int argv_pid;
  while((opt = getopt(argc, argv, "a:m:p:s:t:")) != -1) 
  {
    switch(opt)
    {
      case 'a':
          errno = 0;
          argv_attach_pid = strtol(optarg,NULL,10);
          if(errno != ERANGE && errno != EINVAL)
            ATTACH_PID = argv_attach_pid;
          break;
      case 'm':
          errno = 0;
          argv_search_option = strtol(optarg,NULL,10);
          if(errno != ERANGE && errno != EINVAL)
            MEMORY_SEARCH_OPTION = argv_search_option;
          break;
      case 'p':
          errno = 0;
          argv_port = strtol(optarg,NULL,10);
          if(errno != ERANGE && errno != EINVAL && argv_port != 0)
            PORT = argv_port;
          break;
      case 's':
          errno = 0;
          argv_arch = strtol(optarg,NULL,10);
          if(errno != ERANGE && errno != EINVAL)
             SPECIFIED_ARCH = argv_arch;
          break;
      case 't':
          errno = 0;
          TEST_MODE = 1;
          argv_pid = strtol(optarg,NULL,10);
          if(errno != ERANGE && errno != EINVAL)
            TEST_PID = argv_pid;
          break;


      default:
          debug_log("Usage: %s [-a <attach_pid>] [-m <search_option>] [-p <port>] [-t <pid>] arg1 ...\n", argv[0]);
          break;
    }
  }
  #endif

  debug_log("listening on port %d\n",PORT);
  debug_log("---\n");

  done=0;

  debug_log("&s=%p\n", &s);
  #ifdef SHARED_LIBRARY
    debug_log("ceserver=%p\n",ceserver);
  #else
    debug_log("main=%p\n", main);
  #endif


  debug_log("sizeof(off_t)=%d\n",sizeof(off_t));
  debug_log("sizeof(off64_t)=%d\n",sizeof(off64_t));
  debug_log("sizeof(uintptr_t)=%d\n",sizeof(uintptr_t));
  debug_log("sizeof(long)=%d\n",sizeof(long));

  debug_log("---\n");
  initCESERVERPATH();
  debug_log("CESERVERPATH=%s\n", CESERVERPATH);


  fflush(stdout);
  debug_log("MEMORY_SEARCH_OPTION=%d\n", MEMORY_SEARCH_OPTION);
  fflush(stdout);
  debug_log("ATTACH_TO_ACCESS_MEMORY=%d\n", ATTACH_TO_ACCESS_MEMORY);
  fflush(stdout);
  debug_log("ATTACH_TO_WRITE_MEMORY=%d\n", ATTACH_TO_WRITE_MEMORY);
  fflush(stdout);


  if ((MEMORY_SEARCH_OPTION == 2) && (process_vm_readv==NULL)) //user explicitly wants to use process_vm_readv but it's not available
  {
    debug_log("process_vm_readv==NULL, so MEMORY_SEARCH_OPTION can not be 2. Setting it to 0\n");
    MEMORY_SEARCH_OPTION=0; //fallback to 0
  }



  debug_log("MEMORY_SEARCH_OPTION=%d\n", MEMORY_SEARCH_OPTION);


  debug_log("CEServer. Waiting for client connection\n");

  //if (broadcast)
  pthread_create(&identifierthread, NULL, IdentifierThread, NULL);

  s=socket(AF_INET, SOCK_STREAM, 0);
  debug_log("socket=%d\n", s);

  memset(&addr, 0, sizeof(addr));
  addr.sin_family=AF_INET;
  addr.sin_port=htons(PORT);
  addr.sin_addr.s_addr=INADDR_ANY;

  int optval = 1;
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof (optval));

  b=bind(s, (struct sockaddr *)&addr, sizeof(addr));
  if (b==0)
    debug_log("successfully bound socket\n");
  else
    debug_log("bind=%d (error)\n", b);

  if (b!=-1)
  {
    l=listen(s, 32);

    if (l==0)
      debug_log("Listening success\n");
    else
      debug_log("listen=%d (error)\n", l);

    clisize=sizeof(addr_client);
    memset(&addr_client, 0, sizeof(addr_client));

    #ifndef SHARED_LIBRARY
    sigaction(SIGPIPE, &(struct sigaction){SIG_IGN}, NULL);


    if (TEST_MODE == 1)
    {
      debug_log("TESTMODE\n");
      pthread_create(&pth, NULL, (void *)CESERVERTEST, (void*)(size_t)TEST_PID);
    }
#ifdef traptest

    {
      //struct sigcontext x;
      void **p;
      p=&traphandler.__sigaction_handler;

      *p=mytraphandler;

      //traphandler.__sigaction_handler=(void *
      traphandler.sa_flags=SA_SIGINFO;
      sigemptyset(&traphandler.sa_mask);

      sigaction(SIGTRAP, &traphandler, &oldtraphandler);




      asm __volatile__ (".byte 0xcc");

      printf("after the trap");
      return 1000;
    }
#endif
    #endif

    fflush(stdout);

    while (done==0)
    {
      int b=1;
      a=accept(s, (struct sockaddr *)&addr_client, &clisize);

      debug_log("accept=%d\n", a);

      fflush(stdout);



      if (a != -1)
      {
        int sor=setsockopt(a, IPPROTO_TCP, TCP_NODELAY, &b, sizeof(b));
        if (sor)
          debug_log("setsockopt TCP_NODELAY = 1 returned %d (%d)\n", sor, errno);
        pthread_create(&pth, NULL, (void *)newconnection, (void *)(uintptr_t)a);
      }
    }
  }

  debug_log("Terminate server\n");

  close(s);

  return 0;
}

#ifdef SHARED_LIBRARY
__attribute__((constructor))
int fork_process()
{
    debug_log("main process pid: %d\n", getpid());

    prctl(PR_SET_DUMPABLE, 1, 0, 0, 0);

    pid_t pid = fork();
    if (pid < 0) 
    {
        debug_log("fork");
    } 
    else if (pid == 0) 
    {
        debug_log("child process pid: %d\n", getpid());
        ceserver();
    }

}
#endif
