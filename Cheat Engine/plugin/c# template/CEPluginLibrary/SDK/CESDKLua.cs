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
        private delegate int dlua_pushinteger(IntPtr state, [MarshalAs(UnmanagedType.I8)]Int64 i);
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
        private delegate Int64 dlua_tointegerx(IntPtr state, int idx, [MarshalAs(UnmanagedType.I4)] ref int isnum);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate bool dlua_toboolean(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate IntPtr dlua_tolstring(IntPtr state, int idx, IntPtr sizeptr); 
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)][return: MarshalAs(UnmanagedType.SysInt)]
        private delegate IntPtr dlua_touserdata(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_rawlen(IntPtr state, int idx);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_gettable(IntPtr state, int idx);
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        private delegate int dlua_settable(IntPtr state, int idx);



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

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate void dLuaPushClassInstance(IntPtr state, IntPtr instance);
        


        //lua related sdk exports:
        private dGetLuaState _GetLuaState;
        private dLuaRegister LuaRegister;
        private dLuaPushClassInstance LuaPushClassInstance;


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
        private dlua_touserdata lua_touserdata;
        private dlua_rawlen lua_rawlen;
        private dlua_gettable lua_gettable;
        private dlua_settable lua_settable;
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


        public int PushClosure(LuaCall func, int n)
        {
            return lua_pushcclosure(State, func, n);
        }
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

                IntPtr oldOverride = lua_StateOverride;
                lua_StateOverride = x;
                r = func();
                lua_StateOverride = oldOverride;
                return r;
            };

            luafunctions.Add(z); //prevent it from getting garbage collected
            LuaRegister(State, FuncName, z);
        }

        public void Register(string FuncName, LuaCall func)
        {
            LuaCall z = delegate (IntPtr x)
             {
                 int r;
                 IntPtr oldOverride = lua_StateOverride;
                 lua_StateOverride = x;
                 r = func(x);
                 lua_StateOverride = oldOverride;
                 return r;
             };
            luafunctions.Add(z); //prevent it from getting garbage collected
            LuaRegister(State, FuncName, z);            
        }

        public void PushInteger(Int64 i) { lua_pushinteger(State, i); }
        public void PushInteger(IntPtr L, Int64 i) { lua_pushinteger(L, i); }

        public void PushNumber(Double n) { lua_pushnumber(State, n); }
        public void PushNumber(IntPtr L, Double n) { lua_pushnumber(L, n); }

        public void PushLString(string s, IntPtr size) { lua_pushlstring(State, s, size); }               
        public void PushLString(IntPtr L, string s, IntPtr size) { lua_pushlstring(L, s, size); }

        public void PushString(string s) { lua_pushstring(State, s); }
        public void PushString(IntPtr L, string s) { lua_pushstring(L, s); }

        public void PushBoolean(bool b) { lua_pushboolean(State, b); }
        public void PushBoolean(IntPtr L, bool b) { lua_pushboolean(L, b); }

        public void PushCEObject(IntPtr L, IntPtr ceobject) { LuaPushClassInstance(L, ceobject); }
        public void PushCEObject(IntPtr ceobject) { LuaPushClassInstance(State, ceobject); }


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
        public Boolean IsCEObject(IntPtr L, int idx) { return IsHeavyUserdata(L, idx); }
        public Boolean IsCEObject(int idx) { return IsHeavyUserdata(State, idx); }
        


        public double ToNumber(IntPtr L, int idx) { int isnumber = 0; return lua_tonumberx(L, idx, ref isnumber); }
        public double ToNumber(int idx) { return ToNumber(State, idx); }
        public Int64 ToInteger(IntPtr L, int idx) {
            int isnumber = 0;
            Int64 r =lua_tointegerx(L, idx, ref isnumber);

            if (isnumber==0)
            {
                r=(Int64)lua_tonumberx(L, idx, ref isnumber);
                if (isnumber == 0)
                    return 0;
            }

            return r;
        }
        public Int64 ToInteger(int idx) { return ToInteger(State, idx); }

        public bool ToBoolean(IntPtr L, int idx) { return lua_toboolean(L, idx); }
        public bool ToBoolean(int idx) { return lua_toboolean(State, idx); }

        public string ToLString(IntPtr L, int idx, ref IntPtr count)
        {
            IntPtr ps=lua_tolstring(L, idx, IntPtr.Zero);
            string s=Marshal.PtrToStringAnsi(ps);
            return s;
        }
        public string ToLString(int idx, ref IntPtr count) { return ToLString(State, idx, ref count); }

        public IntPtr ToCEObject(IntPtr L, int idx)
        {
            IntPtr p = lua_touserdata(L, idx);
            p=Marshal.ReadIntPtr(p);           

            return p;            
        }
        public IntPtr ToCEObject(int idx) { return ToCEObject(State, idx); }

        public int ObjLen(IntPtr L, int idx) { return lua_rawlen(L, idx); }
        public int ObjLen(int idx) { return lua_rawlen(State, idx); }
        public int GetTable(IntPtr L, int idx) { return lua_gettable(L, idx); }
        public int GetTable(int idx) { return lua_gettable(State, idx); }
        public int SetTable(IntPtr L, int idx) { return lua_settable(L, idx); }
        public int SetTable(int idx) { return lua_settable(State, idx); }

        public string ToString(IntPtr L, int idx)
        {
            IntPtr len = (IntPtr)ObjLen(L, idx);
            return ToLString(L, idx, ref len);
        }
        public string ToString(int idx) { return ToString(State, idx); }

        public int PCall(IntPtr L, int nargs, int nresults) {
            int pcr=lua_pcallk(L, nargs, nresults, IntPtr.Zero, IntPtr.Zero);
            if (pcr!=0)
                throw new System.ApplicationException("PCall failed with error " + pcr.ToString() + " (" + ToString(-1) + ")");

            return pcr;
        }
        public int PCall(int nargs, int nresults) { return PCall(State, nargs, nresults); }

        
        public int Call(IntPtr L, int nargs, int nresults) { return lua_callk(L, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }
        public int Call(int nargs, int nresults) { return lua_callk(State, nargs, nresults, IntPtr.Zero, IntPtr.Zero); }

        public int LoadString(IntPtr L, string script) { return luaL_loadstring(L, script); }
        public int LoadString(string script) { return luaL_loadstring(State, script); }

        public int DoString(string x)
        {
            LoadString(x);
            return PCall(0, -1);
        }

        [ThreadStatic]
        static IntPtr lua_StateOverride = (IntPtr)0;

        private IntPtr GetLuaState()
        {
            if (lua_StateOverride != IntPtr.Zero)
                return lua_StateOverride;

            if (_GetLuaState == null)
                _GetLuaState = Marshal.GetDelegateForFunctionPointer<dGetLuaState>(sdk.pluginexports.GetLuaState);

            /*
            if (lua_State==(IntPtr)0)
            {


                lua_State = _GetLuaState();
            }           
            */
            return _GetLuaState(); 
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
                lua_touserdata = Marshal.GetDelegateForFunctionPointer<dlua_touserdata>(GetProcAddress(hLibLua, "lua_touserdata"));

                lua_rawlen = Marshal.GetDelegateForFunctionPointer<dlua_rawlen>(GetProcAddress(hLibLua, "lua_rawlen"));

                lua_gettable = Marshal.GetDelegateForFunctionPointer<dlua_gettable>(GetProcAddress(hLibLua, "lua_gettable"));
                lua_settable = Marshal.GetDelegateForFunctionPointer<dlua_settable>(GetProcAddress(hLibLua, "lua_settable"));



                lua_callk = Marshal.GetDelegateForFunctionPointer<dlua_callk>(GetProcAddress(hLibLua, "lua_callk"));
                lua_pcallk = Marshal.GetDelegateForFunctionPointer<dlua_pcallk>(GetProcAddress(hLibLua, "lua_pcallk"));

                luaL_loadstring = Marshal.GetDelegateForFunctionPointer<dluaL_loadstring>(GetProcAddress(hLibLua, "luaL_loadstring"));
            }

            LuaRegister = Marshal.GetDelegateForFunctionPointer<dLuaRegister>(sdk.pluginexports.LuaRegister);

            LuaPushClassInstance = Marshal.GetDelegateForFunctionPointer<dLuaPushClassInstance>(sdk.pluginexports.LuaPushClassInstance);



            this.sdk = sdk;
        }

    }
}
