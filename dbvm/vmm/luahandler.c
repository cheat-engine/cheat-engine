/*
 * luahandler.c
 *
 *  Created on: Feb 28, 2019
 *      Author: eric
 */

#include "common.h"
#include "mm.h"

#include "luahandler.h"
#include "lua/src/lua.h"
#include "lua/src/lauxlib.h"
#include "lua/src/lualib.h"
#include "vmcall.h"
#include "main.h"

lua_State *LuaVM;

int lua_print(lua_State *L)
{
  int paramcount=lua_gettop(L);
  int i;
  for (i=1;i<=paramcount;i++)
  {
    sendstring((char *)lua_tostring(L,i));
    sendstring("\n");
  }

  return 0;
}

int lua_cpuid(lua_State *L)
{
  unsigned long long rax,rbx,rcx,rdx;

  if (lua_gettop(L)>=1)
  {
    rax=lua_tointeger(L,1);

    if (lua_gettop(L)>=2)
    {
      rcx=lua_tointeger(L,2);
    }
  }

  _cpuid(&rax,&rbx,&rcx,&rdx);

  lua_pushinteger(L,rax);
  lua_pushinteger(L,rbx);
  lua_pushinteger(L,rcx);
  lua_pushinteger(L,rdx);

  return 4;
}

int lua_readMSR(lua_State *L)
{
  if (lua_gettop(L)>=1)
  {
    int msr=lua_tointeger(L,1);
    lua_pushinteger(L, readMSR(msr));
    return 1;
  }

  return 0;
}

int lua_writeMSR(lua_State *L)
{
  if (lua_gettop(L)>=2)
  {
    int msr=lua_tointeger(L,1);
    QWORD value=lua_tointeger(L,2);
    writeMSRSafe(msr, value);
  }

  return 0;
}

int lua_inportb(lua_State *L)
{
  if (lua_gettop(L)>=1)
  {
    int port=lua_tointeger(L,1);
    lua_pushinteger(L, inportb(port));
    return 1;
  }

  return 0;
}

int lua_inportd(lua_State *L)
{
  if (lua_gettop(L)>=1)
  {
    int port=lua_tointeger(L,1);
    lua_pushinteger(L, inportd(port));
    return 1;
  }

  return 0;
}

int lua_outportb(lua_State *L)
{
  if (lua_gettop(L)>=2)
  {
    int port=lua_tointeger(L,1);
    int value=lua_tointeger(L,2);
    outportb(port, value);
  }

  return 0;
}

int lua_outportd(lua_State *L)
{
  if (lua_gettop(L)>=1)
  {
    int port=lua_tointeger(L,1);
    int value=lua_tointeger(L,2);
    outportd(port, value);
  }

  return 0;
}

int lua_readBytes(lua_State *L)
{
  if (lua_gettop(L)>=2)
  {
    lua_Integer address=lua_tointeger(L,1);
    int size=lua_tointeger(L,2);

    //sendstringf("lua_readBytes(%6,%d)",address,size);

    lua_newtable(L);   //4
    int i;
    unsigned char *b=(unsigned char *)address;
    for (i=0; i<size; i++)
    {
      lua_pushinteger(L,i+1);
      lua_pushinteger(L, b[i]);
      lua_settable(L,-3);
    }

    return 1;
  }

  return 0;
}

int lua_writeBytes(lua_State *L)
{
  if (lua_gettop(L)>=2)
  {
    if (!lua_istable(L,2))
    {
      sendstring("lua_writeBytes: Parameter 2 must be a table\n");
      return 0;
    }

    lua_Integer address=lua_tointeger(L,1);

    int i;
    unsigned char *b=(unsigned char *)address;
    while (1)
    {
      lua_pushinteger(L,i+1);
      lua_gettable(L,2);
      if (lua_isnil(L,-1))
        break;

      b[i]=lua_tointeger(L,-1);
      lua_pop(L,1);
      i++;
    }
  }

  return 0;
}

int lua_psod(lua_State *L UNUSED)
{
  psod();

  return 0;
}

LUALIB_API void luaL_openlibs (lua_State *L);


lua_State *initializeLua(void)
{
  if (LuaVM==NULL)
  {
    LuaVM=luaL_newstate();
    if (LuaVM)
    {
       sendstringf("LuaVM created at %p\n",LuaVM);

       luaopen_base(LuaVM);
       luaL_openlibs(LuaVM);

       lua_settop(LuaVM,0);



       lua_register(LuaVM,"print", lua_print);

       lua_register(LuaVM,"inportb",lua_inportb);
       lua_register(LuaVM,"inportd",lua_inportd);

       lua_register(LuaVM,"outportb", lua_outportb);
       lua_register(LuaVM,"outportd", lua_outportd);

       lua_register(LuaVM,"readBytes", lua_readBytes);
       lua_register(LuaVM,"writeBytes", lua_writeBytes);

       lua_register(LuaVM,"readMSR", lua_readMSR);
       lua_register(LuaVM,"writeMSR", lua_writeMSR);

       lua_register(LuaVM,"cpuid", lua_cpuid);



       lua_register(LuaVM,"psod", lua_psod);
    }
  }
  return LuaVM;
}


void enterLuaConsole(void)
{
  char *buffer;
  int bufferpos;
  int buffersize;

  char *script;
  int scriptsize;
  int c;
  int bi,i,lp;

  initializeLua();
  buffer=malloc(4096);
  buffersize=4096;
  bufferpos=0;


  sendstringf("Welcome to DBVM's Lua console. Enter your code here (Ctrl+X executes the code)\n");

  script=(char *)malloc(8);
  scriptsize=8;

  c=waitforchar();
  //getchar()
  i=0;



  while (c!=24) //ctrl+x
  {
    buffer[bufferpos]=c;
    bufferpos++;

    if (bufferpos>=buffersize)
    {
      buffersize*=2;
      buffer=realloc(buffer,buffersize);
    }

    for (lp=0; lp<1000; lp++)
    {
      c=getchar();
      if ((c) && (c!=24)) break;

      c=getchar();
      if ((c) && (c!=24)) break;

      c=getchar();
      if ((c) && (c!=24)) break;

      c=getchar();
      if ((c) && (c!=24)) break;

      c=getchar();
      if ((c) && (c!=24)) break;
    }

    if ((c) && (c!=24)) continue;



    //sendstring("|");

    //c==0
    bi=0;
    for (bi=0; bi<bufferpos; bi++)
    {
      c=buffer[bi];

      if (isspace(c) || isgraph(c) || (c==13))
      {
        sendchar(c);
        script=addCharToString(c, script, i++, &scriptsize);

        if (c==13)
        {
          sendchar(10);
          script=addCharToString(10, script, i++, &scriptsize);
        }

        if (c==10) //undo double linefeeds
        {
          if (script[i--]==10)
            i--;
        }
      }
      else
      {
        if (c==127) //backspace
        {
          i=i-1;
          sendchar(c);
        }
      }
    }

    bufferpos=0;
    c=waitforchar();
  }

  //c==24
  sendstring("\n");
  script[i]=0;

  sendstring("Given script:\n");
  sendstring("-----------------------\n");
  sendstring(script);
  sendstring("\n-----------------------\n");

  i=luaL_dostring(LuaVM, script);

  sendstringf("luaL_dostring returned %d\n",i);
  int rcount=lua_gettop(LuaVM);
  sendstringf("rcount=%d\n", rcount);

  for (i=1;i<=rcount; i++)
  {
    sendstringf("%d: ",i);
    if (lua_isnil(LuaVM,i))
      sendstringf("nil",i);
    else
    if (lua_isuserdata(LuaVM,i))
    {
      sendstringf("UD:%6",lua_tointeger(LuaVM,i));
    }
    else
    if (lua_iscfunction(LuaVM, i))
    {
      sendstring("Native Function");

    }
    else
    if (lua_isfunction(LuaVM, i))
    {
      sendstring("Function");
    }
    else
    if (lua_istable(LuaVM,i))
    {
      sendstring("Table");
      //todo: show info

    }
    else
    {
      char *s=(char *)lua_tostring(LuaVM, i);
      sendstring(s);
    }

    sendstring("\n");
  }

  lua_settop(LuaVM, 0);

  free(script);
  free(buffer);
}
