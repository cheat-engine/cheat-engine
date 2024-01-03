#ifndef CEPIPELIB_H
#define CEPIPELIB_H

#include <celog.h>
#include <cesocket.h>
#include <stdarg.h>
#include <stddef.h>
#include <errno.h>
#include <stdint.h>

#ifdef ANDROID
//alternatively, registersymbol('_errno','__errno')
int *__cdecl __errno(void);
#undef errno
#define errno (*__errno())
#endif

#ifdef __APPLE__
#undef errno
extern int errno;

#endif

#ifdef _WIN32
#include <windowslite.h>

    
#else
typedef int HANDLE;
#endif

#ifndef _WIN32
int unlink(const char *_Filename); 
#endif


char *strerror(int errnum);

int sprintf(char *str, const char *format, ...);
int snprintf(char *str, int max, const char *format, ...);

char *strcpy(char *dest, const char *src);
size_t strlen(const char *s);

void *malloc(size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);
char *strdup(const char *s);
void *memcpy(void *dest, const void *src, size_t n);

#ifndef _WIN32
ssize_t recv(int sockfd, void *buf, size_t len, int flags);
ssize_t send(int sockfd, const void *buf, size_t len, int flags);

int connect(int, void *, socklen_t);

#endif


char *defaultpipename="unnamed pipe";


typedef struct
{
  unsigned char *buf;
  int size;
  int pos;  
} MemoryStream, *PMemoryStream;

typedef struct
{
  char *pipename;
  char *debugname;  
  HANDLE handle;
} PipeConnection, *PPipeConnection;

typedef PPipeConnection PPipeServer;
typedef PPipeConnection PPipeClient;


void ms_read(PMemoryStream this, void* buf, int count)
{
  if (this->pos+count>this->size)
    count=this->size-this->pos;
  
  memcpy(buf, &this->buf[this->pos], count);  
}

void ms_write(PMemoryStream this, void* buf, int count)
{
  if ((buf==NULL) || (count==0))
    return;
  
  while (this->pos+count>this->size)
  {
    
    debug_log("Realloc memorystream: Old size=%d", this->size);
    
    if (this->size<1048576)
      this->size*=2;
    else
      this->size+=1048576;
    
    debug_log("Realloc memorystream: New size=%d", this->size);
    
    this->buf=(unsigned char*)realloc(this->buf, this->size);
  }
  
  memcpy(&this->buf[this->pos], buf, count);
  
  this->pos+=count;
}

uint8_t ms_readByte(PMemoryStream this)
{
  uint8_t r;
  ms_read(this, (void*)&r,1);
  return r;
}

uint16_t ms_readWord(PMemoryStream this)
{
  uint16_t r;
  ms_read(this, (void*)&r,2);
  return r;
}

uint32_t ms_readDword(PMemoryStream this)
{
  uint32_t r;
  ms_read(this, (void*)&r,4);
  return r;
}

uint64_t ms_readQword(PMemoryStream this)
{
  uint64_t r;
  ms_read(this, (void*)&r,8);
  return r;
}

void ms_writeByte(PMemoryStream this, uint8_t v)
{
  ms_write(this, (void*)&v,1);
}

void ms_writeWord(PMemoryStream this, uint16_t v)
{
  ms_write(this, (void*)&v,2);
}

void ms_writeDword(PMemoryStream this, uint32_t v)
{
  ms_write(this, (void*)&v,4);
}

void ms_writeQword(PMemoryStream this, uint64_t v)
{
  ms_write(this, (void*)&v,8);
}

void ms_writeString(PMemoryStream this, char *str)
{
  int l=0;
  if (str)
    l=strlen(str);
  
  ms_writeWord(this, l);
  ms_write(this, str,l);
}

PMemoryStream ms_create(int initialsize)
{
  PMemoryStream r=(PMemoryStream)malloc(sizeof(MemoryStream));
  if (initialsize==0)
    initialsize=32;
  
  r->buf=(unsigned char *)malloc(initialsize);
  r->size=initialsize;
  r->pos=0;
  
  return r;  
}

void ms_destroy(PMemoryStream this)
{
  if (this->buf)
  {
    free(this->buf);
    this->buf=NULL;
  }
  
  free(this);
  
}

//-----PipeConnection----

void ps_setDebugName(PPipeConnection this, char *name)
{
  if (this->debugname)
    free(this->debugname);
  
  this->debugname=strdup(name);
}

ssize_t recvall (HANDLE s, void *buf, size_t size, int flags)
{
  ssize_t totalreceived=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;  
  
 
  
  while (sizeleft>0)
  {
#ifdef _WIN32
   
    DWORD br=0;       

    if (ReadFile(s, &buffer[totalreceived], sizeleft, &br, NULL) == FALSE)
    {
      //debug_log("recvall: ReadFile returned FALSE.  br=%d", br);
      if (br==0)
        return totalreceived;
    }
    else
    {
     // debug_log("successfully read %d bytes", br);
    }

    totalreceived += br;
    sizeleft-= br;
#else
    ssize_t i=recv(s, &buffer[totalreceived], sizeleft, flags);

    if (i==0)
    {
      debug_log("recv returned 0\n");
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
        debug_log("Error during recvall ( %s ) ", strerror(errno));
        return i; //read error, or disconnected
      }

    }

    totalreceived+=i;
    sizeleft-=i;
#endif    
  }

  //printf("leave recvall\n");
  return totalreceived;
}

ssize_t sendall (HANDLE s, void *buf, size_t size, int flags)
{
  ssize_t totalsent=0;
  ssize_t sizeleft=size;
  unsigned char *buffer=(unsigned char*)buf;

  while (sizeleft>0)
  {
#ifdef _WIN32
    DWORD bw;
    if (WriteFile(s, &buffer[totalsent], sizeleft, &bw, NULL) == FALSE)
    {
      debug_log("sendall: WriteFile returned FALSE.  bw=%d (size was %d)", bw, size);
      if (bw==0)
        return totalsent;
    }

    totalsent += bw;
    sizeleft-= bw;
#else    
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
        debug_log((char *)"Error during sendall ( %s )\n", strerror(errno));
        return i;
      }
    }

    totalsent+=i;
    sizeleft-=i;
#endif    
  }

  return totalsent;
}


void ps_read(PPipeConnection this, void* buf, int count)
{
  
  if (this->handle)
  {
    int r=recvall(this->handle, buf,count,0);
    if (r!=count)
    {
      debug_log("ps_read: r(%d)!=count(%d)", r,count);
#ifdef _WIN32   
      CloseHandle(this->handle);
#else
      close(this->handle);
#endif      
      debug_log("closed the handle");   
      this->handle=0;
    }
  }
}

void ps_write(PPipeConnection this, void* buf, int count)
{
  if (this->handle)
  {
    char d[100];
    int i;
    int max=count;
    unsigned char *cbuf;
    if (max>20)
      max=20;
    
    cbuf=(unsigned char *)buf;

    debug_log("ps_write:");
    for (i=0; i<max; i++)
      debug_log("buf[%d]=%.2x", i, cbuf[i]);



    int r=sendall(this->handle, buf,count,0);
    if (r!=count)
    { 
      debug_log("ps_write: r!=count");      
#ifdef _WIN32         
      CloseHandle(this->handle);
#else
      close(this->handle);
#endif
      debug_log("closed the handle");    
      this->handle=0;
    }
  }
}



uint8_t ps_readByte(PPipeConnection this)
{
  uint8_t r;
  ps_read(this, (void*)&r,1);
  return r;
}

uint16_t ps_readWord(PPipeConnection this)
{
  uint16_t r;
  ps_read(this, (void*)&r,2);
  return r;
}

uint32_t ps_readDword(PPipeConnection this)
{
  uint32_t r;
  ps_read(this, (void*)&r,4);
  return r;
}

uint64_t ps_readQword(PPipeConnection this)
{
  uint64_t r;
  ps_read(this, (void*)&r,8);
  return r;
}

void ps_writeByte(PPipeConnection this, uint8_t v)
{
  ps_write(this, (void*)&v,1);
}

void ps_writeWord(PPipeConnection this, uint16_t v)
{
  ps_write(this, (void*)&v,2);
}

void ps_writeDword(PPipeConnection this, uint32_t v)
{
  ps_write(this, (void*)&v,4);
}

void ps_writeQword(PPipeConnection this, uint64_t v)
{
  ps_write(this, (void*)&v,8);
}

void ps_writeMemStream(PPipeConnection this, PMemoryStream ms)
{
  ps_writeDword(this, ms->pos);
  ps_write(this, ms->buf, ms->pos);      
}

void ps_writeMemStreamRaw(PPipeConnection this, PMemoryStream ms)
{
    debug_log("ps_writeMemStreamRaw: Writing %d bytes", ms->pos);
    ps_write(this, ms->buf, ms->pos);      
}

int ps_isvalid(PPipeConnection this)
{
  return this->handle!=0;  
}

void ps_destroy(PPipeConnection this)
{  
  debug_log("ps_destroy called");
  if (this)
  {
  
    if (this->pipename)
    {
      debug_log("Freeing pipename %s", this->pipename);
      free(this->pipename);
    }
    
    if (this->debugname)
    {
      debug_log("Freeing debugname");
      free(this->debugname);
    }
    else
      debug_log("no debugname to free");
    

    if (this->handle)
    {
      
      
#ifdef _WIN32  
      DisconnectNamedPipe(this->handle);
      CloseHandle(this->handle);      
#else
      close(this->handle);
#endif
    }
    else
      debug_log("no handle to close");
    
    
    debug_log("Freeing this");
    free(this);  
  }
  
  debug_log("returning from ps_destroy");
}




//correct: 1D 01 2F 74 6D 70 2F 63 65 70 69 70 65 5F 43 45 4C 55 41 53 45 52 56 45 52 34 30 33 30
//mine:    1D 01 2F 74 6D 70 2F 63 65 70 69 70 65 5F 43 45 4C 55 41 53 45 52 56 45 52 34 30 33 30

  
PipeConnection *CreateClientPipeConnection(char *name)
{
    HANDLE pipehandle;
    debug_log("CreateClientPipeConnection(\"%s\")", name);
  
    char pipename[300];
    struct sockaddr_un address;
      
#ifdef _WIN32
    snprintf(pipename, 300, "%s%s", "\\\\.\\pipe\\", name);  
#else
    #ifdef __APPLE__ 
    snprintf(pipename, 300, "/tmp/cepipe_%s", name);
    #else
    snprintf(pipename, 300, "%s", name);
    #endif
#endif
  
  debug_log("Connecting to pipe %s", pipename);
    
#ifdef _WIN32       
  pipehandle=CreateFileA(pipename,GENERIC_READ | GENERIC_WRITE, 0, NULL,  OPEN_EXISTING, 0, 0);   
#else
  {
      debug_log("using named sockets as pipe");
      int s;


    
      #ifdef __APPLE__
      memset(&address, 0, sizeof(address));
      strcpy(&address.sun_path[0], pipename);
      address.sun_path[strlen(pipename)]=0;
      
      #else
      strcpy(&address.sun_path[1], pipename);
      address.sun_path[0]=0; //abstract pipe
      
      #endif
      address.sun_family=AF_UNIX;
    
      
      debug_log("address.sun_path=%s",address.sun_path);

    int al=(int)SUN_LEN(&address);
    debug_log("SUN_LEN=%d", al);
#ifdef __APPLE__
    address.sun_len=al;
#endif
    
    s=socket(AF_UNIX, SOCK_STREAM,0);
    if (s)
    {
      if (connect(s,&address, al)==0)
      {
        debug_log("Succesful connection");
        pipehandle=s;
      }
      else
      {
          int le=errno;
        debug_log("Failure to connect to %s  (error=%d: %s)", pipename,le, strerror(le));
        pipehandle=0;
        close(s);
      }
    }
    else
      debug_log("failed creating socket");
      
        
  }
#endif

  debug_log("CreateFileA returned %x", pipehandle);

  PPipeConnection t=malloc(sizeof(PipeConnection));                      
  t->pipename=strdup(name);
  t->debugname=NULL;
  t->handle=pipehandle;
  return t;
  
}

#define CreatePipeServer CreatePipeConnection
PipeConnection *CreatePipeConnection(char *name)
{
  //single connect system
  int s;
  debug_log("CreatePipeConnection(\"%s\")\n",name);
  
#ifdef _WIN32
  //windows:
  HANDLE pipehandle;
  
  char pipename[300];
  snprintf(pipename, 300, "%s%s", "\\\\.\\pipe\\", name);

  
  debug_log("Calling CreateNamedPipeA with name %s", pipename);
  pipehandle = CreateNamedPipeA(pipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1, 256 * 1024, 16, INFINITE, NULL);
  debug_log("pipehandle=%p", pipehandle);
  
  if ((pipehandle==NULL) || (pipehandle==INVALID_HANDLE_VALUE)) 
  {
    //todo: UWP shit with duplicatehandle etc...
    return (void*)0;
  }
  
  debug_log("Calling ConnectNamedPipe");
  ConnectNamedPipe(pipehandle, NULL);
  debug_log("ConnectNamedPipe returned. So there was a connection");

  PPipeConnection t=malloc(sizeof(PipeConnection));                      
  t->pipename=strdup(name);
  t->debugname=NULL;
  t->handle=pipehandle;
  return t;
#else
  //not windows:
  s=socket(AF_UNIX, SOCK_STREAM,0);
  if (s)
  {
      char *path=malloc(100);

#ifdef __APPLE__
      snprintf(path,100,"/tmp/cepipe_%s", name);
      unlink(path);
#else
      snprintf(path,100," %s", name);
#endif

      struct sockaddr_un address;
      address.sun_family=AF_UNIX;
      strcpy(address.sun_path, path);

      int al=(int)SUN_LEN(&address);
      
#ifndef __APPLE__      
      debug_log("al=%d\n",al);
#else
      address.sun_path[0]=0;
#endif

      int optval=1;
      setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &optval, optval);

      int i;
      i=bind(s,(struct sockaddr *)&address, al);
      if (i==0)
      {
          debug_log((char*)"Starting to listen\n");
          i=listen(s,32);

          debug_log("Listen returned %d\n",i);
          if (i==0)
          {
              struct sockaddr_un addr_client;
              socklen_t clisize=sizeof(addr_client);
              int a=-1;


              while (a==-1)
              {
                debug_log("Calling accept (clisize=%d)",clisize);
                a=accept(s, (struct sockaddr *)&addr_client, &clisize);

                if (a!=-1)
                {
                    debug_log("Connection accepted");
                    debug_log("Closing the listener");
                    close(s); //stop listening
                    
                    PPipeConnection t=malloc(sizeof(PipeConnection));                      
                    t->pipename=strdup(name);
                    t->debugname=NULL;
                    t->handle=a;
                    return t;
                }
                else
                {
                  debug_log("accept returned %d (%d - %s )", a, errno, strerror(errno));                    
                  close(s);
                  break;
                }
              }

          }
          else
              debug_log((char*)"Listen failed");

      }
      else
        debug_log((char*)"Bind failed\n");
  }
  else
      debug_log((char*)"Failure creating socket\n");

  return (void*)0;
#endif
}


#endif
