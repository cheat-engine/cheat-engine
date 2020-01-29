//Copyright Cheat Engine 2020
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace CESDK
{
    public class CESDKLua
    {
        private const int LUA_TNONE          = -1;
        private const int LUA_TNIL           = 0;
        private const int LUA_TBOOLEAN       = 1;
        private const int LUA_TLIGHTUSERDATA = 2;
        private const int LUA_TNUMBER        = 3;
        private const int LUA_TSTRING        = 4;
        private const int LUA_TTABLE         = 5;
        private const int LUA_TFUNCTION      = 6;
        private const int LUA_TUSERDATA      = 7;
        private const int LUA_TTHREAD        = 8;   

        [DllImport("kernel32", SetLastError = true, CharSet = CharSet.Ansi)]
        static extern IntPtr LoadLibraryA([MarshalAs(UnmanagedType.LPStr)]string lpFileName);

        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Ansi)]
        public static extern IntPtr GetProcAddress(IntPtr hModule, [MarshalAs(UnmanagedType.LPStr)]string procedureName);

        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern bool FreeLibrary(IntPtr hModule);


        private CESDK sdk;

        [ThreadStatic]
        private IntPtr lua_State; //unique per thread


        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int LuaCall(IntPtr lua_State);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int LuaCallSimplified();


        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_gettop(IntPtr state);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_settop(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushvalue(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_rotate(IntPtr state, int idx, int n);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_copy(IntPtr state, int fromidx, int toidx);





        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushcclosure(IntPtr state, LuaCall func, int n);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_setglobal(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string str);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_getglobal(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string str);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushnil(IntPtr state);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushinteger(IntPtr state, [MarshalAs(UnmanagedType.I8)]UInt64 i);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushnumber(IntPtr state, [MarshalAs(UnmanagedType.R8)]Double n);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushlstring(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string s, [MarshalAs(UnmanagedType.SysUInt)]IntPtr size);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushstring(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string s);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pushboolean(IntPtr state, [MarshalAs(UnmanagedType.Bool)]bool b);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_type(IntPtr state, int idx);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_isnumber(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_isinteger(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_isstring(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_iscfunction(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_isuserdata(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate double dlua_tonumberx(IntPtr state, int idx, [MarshalAs(UnmanagedType.I4)] ref int isnum);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_tointegerx(IntPtr state, int idx, [MarshalAs(UnmanagedType.I4)] ref int isnum);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_toboolean(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)][return: MarshalAs(UnmanagedType.LPStr)]
        private delegate string dlua_tolstring(IntPtr state, int idx, int count);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_rawlen(IntPtr state, int idx);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_callk(IntPtr state, int nargs, int nresults, IntPtr context, IntPtr k);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_pcallk(IntPtr state, int nargs, int nresults, IntPtr context, IntPtr k);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dluaL_loadstring(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string script);

        


        //ce plugin:
        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate IntPtr dGetLuaState();

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate IntPtr dLuaRegister(IntPtr state, [MarshalAs(UnmanagedType.LPStr)]string s, LuaCall func);


        //lua related sdk exports:
        private dGetLuaState _GetLuaState;
        private dLuaRegister LuaRegister;


        //local native lua functions
        private dlua_gettop lua_gettop;
        private dlua_settop lua_settop;
        private dlua_pushvalue lua_pushvalue;
        private dlua_rotate lua_rotate;
        private dlua_copy lua_copy;
        private dlua_pushcclosure lua_pushcclosure;
        private dlua_setglobal lua_setglobal;
        private dlua_getglobal lua_getglobal;        
        private dlua_pushnil lua_pushnil;
        private dlua_pushinteger lua_pushinteger;
        private dlua_pushnumber lua_pushnumber;
        private dlua_pushlstring lua_pushlstring;
        private dlua_pushstring lua_pushstring;
        private dlua_pushboolean lua_pushboolean;
        private dlua_type lua_type;
        private dlua_isnumber lua_isnumber;
        private dlua_isinteger lua_isinteger;
        private dlua_isstring lua_isstring;
        private dlua_iscfunction lua_iscfunction;
        private dlua_isuserdata lua_isuserdata;
        private dlua_tonumberx lua_tonumberx;
        private dlua_tointegerx lua_tointegerx;
        private dlua_toboolean lua_toboolean;
        private dlua_tolstring lua_tolstring;
        private dlua_rawlen lua_rawlen;
        private dlua_callk lua_callk;
        private dlua_pcallk lua_pcallk;

        private dluaL_loadstring luaL_loadstring;


        public IntPtr State { get { return GetLuaState(); } }

        private static List<LuaCall> luafunctions = new List<LuaCall>();

        //c# versions
        public int GetTop() { return lua_gettop(State); }
        public int GetTop(IntPtr L) { return lua_gettop(L); }
        public int SetTop(int idx) { return lua_settop(State, idx); }
        public int SetTop(IntPtr L, int idx) { return lua_settop(L, idx); }
        public int PushValue(int idx) { return lua_pushvalue(State, idx); }
        public int PushValue(IntPtr L, int idx) { return lua_pushvalue(L, idx); }
        public int Rotate(int idx, int n) { return lua_rotate(State, idx, n); }
        public int Rotate(IntPtr L, int idx, int n) { return lua_rotate(L, idx, n); }
        public int Copy(int idx, int fromidx, int toidx) { return lua_copy(State, fromidx, toidx); }
        public int Copy(IntPtr L, int fromidx, int toidx) { return lua_copy(L, fromidx, toidx); }


        public void Pop(IntPtr L, int n) { lua_settop(State, -n - 1); }
        public void Pop(int n) { Pop(State, n); }
        public void Remove(IntPtr L, int idx) { Rotate(L, idx, -1); Pop(L, 1); }
        public void Remove(int idx) { Remove(State, idx); }        
        public void Insert(IntPtr L, int idx) { lua_rotate(L, idx, 1); }
        public void Insert(int idx) { Insert(State, idx); }

        public void Replace(IntPtr L, int idx) { lua_copy(State, -1, idx); Pop(State, 1); }
        public void Replace(int idx) { Replace(State, idx); }

        public int Type(int idx) { return lua_type(State, idx); }
        public int Type(IntPtr L, int idx) { return lua_type(L, idx); }


        public int PushClosure(LuaCall func, int n) { return lua_pushcclosure(State, func, n); }
        public int PushFunction(LuaCall func) { return PushClosure(func, 0); }
        public int SetGlobal(string str) { return lua_setglobal(State, str); }
        public int GetGlobal(string str) { return lua_getglobal(State, str); }

        /// <summary>
        /// Registers a new function with CE's lua environment
        /// </summary>
        /// <param name="FuncName">new lua function name</param>
        /// <param name="func">pointer to the method to be called when the function is executed</param>
        public void Register(string FuncName, LuaCallSimplified func)
        {
            LuaCall z = delegate (IntPtr x) {
                int r;
                IntPtr bla = lua_State;
                lua_State = x;
                r = func();
                lua_State = bla;
                return r;
            };

            luafunctions.Add(z); //prevent it from getting garbage collected
            LuaRegister(State, FuncName, z);
        }

        public void Register(string FuncName, LuaCall func)
        {
            luafunctions.Add(func); //prevent it from getting garbage collected
            LuaRegister(State, FuncName, func);            
        }

        public void PushInteger(UInt64 i) { lua_pushinteger(State, i); }
        public void PushInteger(IntPtr L, UInt64 i) { lua_pushinteger(L, i); }

        public void PushNumber(Double n) { lua_pushnumber(State, n); }
        public void PushNumber(IntPtr L, Double n) { lua_pushnumber(L, n); }

        public void PushLString(string s, IntPtr size) { lua_pushlstring(State, s, size); }               
        public void PushLString(IntPtr L, string s, IntPtr size) { lua_pushlstring(L, s, size); }

        public void PushString(string s) { lua_pushstring(State, s); }
        public void PushString(IntPtr L, string s) { lua_pushstring(L, s); }

        public void PushBoolean(bool b) { lua_pushboolean(State, b); }
        public void PushBoolean(IntPtr L, bool b) { lua_pushboolean(L, b); }

        public Boolean IsFunction(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TFUNCTION; }
        public Boolean IsFunction(int idx) { return lua_type(State, idx) == LUA_TFUNCTION; }
        public Boolean IsTable(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TTABLE; }
        public Boolean IsTable(int idx) { return lua_type(State, idx) == LUA_TTABLE; }
        public Boolean IsLightUserdata(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TLIGHTUSERDATA; }
        public Boolean IsLightUserdata(int idx) { return lua_type(State, idx) == LUA_TLIGHTUSERDATA; }
        public Boolean IsHeavyUserdata(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TUSERDATA; }
        public Boolean IsHeavyUserdata(int idx) { return lua_type(State, idx) == LUA_TUSERDATA; }
        public Boolean IsNil(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TNIL; }
        public Boolean IsNil(int idx) { return lua_type(State, idx) == LUA_TNIL; }
        public Boolean IsBoolean(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TBOOLEAN; }
        public Boolean IsBoolean(int idx) { return lua_type(State, idx) == LUA_TBOOLEAN; }
        public Boolean IsThread(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TTHREAD; }
        public Boolean IsThread(int idx) { return lua_type(State, idx) == LUA_TTHREAD; }
        public Boolean IsNone(IntPtr L, int idx) { return lua_type(L, idx) == LUA_TNONE; }
        public Boolean IsNone(int idx) { return lua_type(State, idx) == LUA_TNONE; }
        public Boolean IsNoneOrNil(IntPtr L, int idx) { return lua_type(L, idx) <=0; }
        public Boolean IsNoneOrNil(int idx) { return lua_type(State, idx) <= 0; }

        public Boolean IsNumber(IntPtr L, int idx) { return lua_isnumber(L, idx); }
        public Boolean IsNumber(int idx) { return lua_isnumber(State, idx); }
        public Boolean IsInteger(IntPtr L, int idx) { return lua_isinteger(L, idx); }
        public Boolean IsInteger(int idx) { return lua_isinteger(State, idx); }
        public Boolean IsString(IntPtr L, int idx) { return lua_isstring(L, idx); }
        public Boolean IsString(int idx) { return lua_isstring(State, idx); }
        public Boolean IsCFunction(IntPtr L, int idx) { return lua_iscfunction(L, idx); }
        public Boolean IsCFunction(int idx) { return lua_iscfunction(State, idx); }
        public Boolean IsUserData(IntPtr L, int idx) { return lua_isuserdata(L, idx); }
        public Boolean IsUserData(int idx) { return lua_isuserdata(State, idx); }
        public double ToNumber(IntPtr L, int idx) { int isnumber = 0; return lua_tonumberx(L, idx, ref isnumber); }
        public double ToNumber(int idx) { return ToNumber(State, idx); }
        public int ToInteger(IntPtr L, int idx) {
            int isnumber = 0;
            int r=lua_tointegerx(L, idx, ref isnumber);

            if (isnumber==0)
            {
                r=(int)lua_tonumberx(L, idx, ref isnumber);
                if (isnumber == 0)
                    return 0;
            }

            return r;
        }
        public int ToInteger(int idx) { return ToInteger(State, idx); }

        public bool ToBoolean(IntPtr L, int idx) { return lua_toboolean(L, idx); }
        public bool ToBoolean(int idx) { return lua_toboolean(State, idx); }

        public string ToLString(IntPtr L, int idx, int count) { return lua_tolstring(L, idx, count); }
        public string ToLString(int idx, int count) { return ToLString(State, idx, count); }

        public int ObjLen(IntPtr L, int idx) { return lua_rawlen(L, idx); }
        public int ObjLen(int idx) { return lua_rawlen(State, idx); }

        public string ToString(IntPtr L, int idx)
        {
            int len = ObjLen(L, idx);
            return ToLString(L, idx, len);
        }
        public string ToString(int idx, int count) { return ToString(State, idx); }

        public int PCall(IntPtr L, int nargs, int nresults) { return lua_pcallk(L, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }
        public int PCall(int nargs, int nresults) { return lua_pcallk(State, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }

        
        public int Call(IntPtr L, int nargs, int nresults) { return lua_callk(L, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }
        public int Call(int nargs, int nresults) { return lua_callk(State, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }

        public int LoadString(IntPtr L, string script) { return luaL_loadstring(L, script); }
        public int LoadString(string script) { return luaL_loadstring(State, script); }

        public int DoString(string x)
        {
            LoadString(x);
            return PCall(0, -1);
        }

        private IntPtr GetLuaState()
        {
            if (lua_State==(IntPtr)0)
            {
                if (_GetLuaState == null)
                    _GetLuaState = Marshal.GetDelegateForFunctionPointer<dGetLuaState>(sdk.pluginexports.GetLuaState);

                lua_State = _GetLuaState();
            }           
            return lua_State;
        }

        public CESDKLua(CESDK sdk)
        {
            //init lua
            IntPtr hLibLua=LoadLibraryA("lua53-32.dll");

            if (hLibLua == IntPtr.Zero)
                hLibLua = LoadLibraryA("lua53-64.dll");

            if (hLibLua != IntPtr.Zero)
            {
                //load the most commonly used functions 
                lua_gettop = Marshal.GetDelegateForFunctionPointer<dlua_gettop>(GetProcAddress(hLibLua, "lua_gettop"));
                lua_settop = Marshal.GetDelegateForFunctionPointer<dlua_settop>(GetProcAddress(hLibLua, "lua_settop"));
                lua_pushvalue = Marshal.GetDelegateForFunctionPointer<dlua_pushvalue>(GetProcAddress(hLibLua, "lua_pushvalue"));
                lua_rotate = Marshal.GetDelegateForFunctionPointer<dlua_rotate>(GetProcAddress(hLibLua, "lua_rotate"));
                lua_copy = Marshal.GetDelegateForFunctionPointer<dlua_copy>(GetProcAddress(hLibLua, "lua_copy"));                
                lua_pushcclosure = Marshal.GetDelegateForFunctionPointer<dlua_pushcclosure>(GetProcAddress(hLibLua, "lua_pushcclosure"));
                lua_setglobal = Marshal.GetDelegateForFunctionPointer<dlua_setglobal>(GetProcAddress(hLibLua, "lua_setglobal"));
                lua_getglobal = Marshal.GetDelegateForFunctionPointer<dlua_getglobal>(GetProcAddress(hLibLua, "lua_getglobal"));
                lua_pushnil = Marshal.GetDelegateForFunctionPointer <dlua_pushnil>(GetProcAddress(hLibLua, "lua_pushnil"));
                lua_pushinteger = Marshal.GetDelegateForFunctionPointer<dlua_pushinteger>(GetProcAddress(hLibLua, "lua_pushinteger"));
                lua_pushnumber = Marshal.GetDelegateForFunctionPointer<dlua_pushnumber>(GetProcAddress(hLibLua, "lua_pushnumber"));
                lua_pushlstring = Marshal.GetDelegateForFunctionPointer<dlua_pushlstring>(GetProcAddress(hLibLua, "lua_pushlstring"));
                lua_pushstring = Marshal.GetDelegateForFunctionPointer<dlua_pushstring>(GetProcAddress(hLibLua, "lua_pushstring"));
                lua_pushboolean = Marshal.GetDelegateForFunctionPointer<dlua_pushboolean>(GetProcAddress(hLibLua, "lua_pushboolean"));

                lua_type = Marshal.GetDelegateForFunctionPointer<dlua_type>(GetProcAddress(hLibLua, "lua_type"));
                
                lua_isnumber = Marshal.GetDelegateForFunctionPointer<dlua_isnumber>(GetProcAddress(hLibLua, "lua_isnumber"));
                lua_isinteger = Marshal.GetDelegateForFunctionPointer<dlua_isinteger>(GetProcAddress(hLibLua, "lua_isinteger"));
                lua_isstring = Marshal.GetDelegateForFunctionPointer<dlua_isstring>(GetProcAddress(hLibLua, "lua_isstring"));
                lua_iscfunction = Marshal.GetDelegateForFunctionPointer<dlua_iscfunction>(GetProcAddress(hLibLua, "lua_iscfunction"));
                lua_isuserdata = Marshal.GetDelegateForFunctionPointer<dlua_isuserdata>(GetProcAddress(hLibLua, "lua_isuserdata"));

                lua_tonumberx = Marshal.GetDelegateForFunctionPointer<dlua_tonumberx>(GetProcAddress(hLibLua, "lua_tonumberx"));
                lua_tointegerx = Marshal.GetDelegateForFunctionPointer<dlua_tointegerx>(GetProcAddress(hLibLua, "lua_tointegerx"));

                lua_toboolean = Marshal.GetDelegateForFunctionPointer<dlua_toboolean>(GetProcAddress(hLibLua, "lua_toboolean"));
                lua_tolstring = Marshal.GetDelegateForFunctionPointer<dlua_tolstring>(GetProcAddress(hLibLua, "lua_tolstring"));
                lua_rawlen = Marshal.GetDelegateForFunctionPointer<dlua_rawlen>(GetProcAddress(hLibLua, "lua_rawlen"));


                lua_callk = Marshal.GetDelegateForFunctionPointer<dlua_callk>(GetProcAddress(hLibLua, "lua_callk"));
                lua_pcallk = Marshal.GetDelegateForFunctionPointer<dlua_pcallk>(GetProcAddress(hLibLua, "lua_pcallk"));

                luaL_loadstring = Marshal.GetDelegateForFunctionPointer<dluaL_loadstring>(GetProcAddress(hLibLua, "luaL_loadstring"));
            }

            LuaRegister = Marshal.GetDelegateForFunctionPointer<dLuaRegister>(sdk.pluginexports.LuaRegister);



            this.sdk = sdk;
        }

    }
}
