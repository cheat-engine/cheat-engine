using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;


namespace CEPluginLibrary
{
 
    public class PluginMainClass
    {
        private const string PLUGINNAME = "C# Plugin Template for Cheat Engine 7.1+";
        private const int PLUGINVERSION = 6; //CE SDK plugin version it expects to work with (needed in case newer ce versions change things)
        static IntPtr PluginNamePtr;

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern IntPtr LoadLibrary(string libname);


        [StructLayout(LayoutKind.Sequential)]
        private struct TExportedFunctions
        {
            public int sizeofExportedFunctions;
            public IntPtr GetLuaState;
            public IntPtr ProcessMessages;
            public IntPtr CheckSynchronize;
            public IntPtr MainThreadCall;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct TPluginVersion
        {
            public UInt32 version;
            public IntPtr name;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct TPluginInit
        {
            public IntPtr name;
            public IntPtr GetVersion;
            public IntPtr EnablePlugin;
            public IntPtr DisablePlugin;
            public int version;
        }

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate Boolean delegateGetVersion([MarshalAs(UnmanagedType.Struct)] ref TPluginVersion PluginVersion, int TPluginVersionSize);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate Boolean delegateEnablePlugin([MarshalAs(UnmanagedType.Struct)] ref TExportedFunctions ExportedFunctions, UInt32 pluginid);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate Boolean delegateDisablePlugin();

        private static PluginMainClass mainself; //prevents garbage collection

        private delegateGetVersion delGetVersion;
        private delegateEnablePlugin delEnablePlugin;
        private delegateDisablePlugin delDisablePlugin;

        public UInt32 pluginid;
        private TExportedFunctions pluginexports;

        private Boolean GetVersion([MarshalAs(UnmanagedType.Struct)] ref TPluginVersion PluginVersion, int TPluginVersionSize)
        {
            PluginVersion.name = PluginNamePtr;
            PluginVersion.version = PLUGINVERSION;
            return true;            
        }

        private Boolean EnablePlugin([MarshalAs(UnmanagedType.Struct)] ref TExportedFunctions ExportedFunctions, UInt32 pluginid)
        {            
            this.pluginid = pluginid; 
            pluginexports = ExportedFunctions;
            return true;
        }

        private Boolean DisablePlugin()
        {
            return true;
        }

        PluginMainClass()
        {
            delGetVersion = GetVersion;
            delEnablePlugin = EnablePlugin;
            delDisablePlugin = DisablePlugin;
            return;
        }

        public static int CEPluginInitialize(string parameters)
        {
            if (mainself == null)
                mainself = new PluginMainClass();

            if ((Int64)PluginNamePtr == 0)
                PluginNamePtr = Marshal.StringToHGlobalAnsi(PLUGINNAME);



            UInt64 a = UInt64.Parse(parameters);


            TPluginInit bla;  //= Marshal.PtrToStructure<TPluginInit>((IntPtr)a);
            bla.name = PluginNamePtr;
            bla.GetVersion = Marshal.GetFunctionPointerForDelegate(mainself.delGetVersion);
            bla.EnablePlugin = Marshal.GetFunctionPointerForDelegate(mainself.delEnablePlugin);
            bla.DisablePlugin = Marshal.GetFunctionPointerForDelegate(mainself.delDisablePlugin);
            bla.version = PLUGINVERSION;
            Marshal.StructureToPtr<TPluginInit>(bla, (IntPtr)a, false);

            return 1;
        }

    }   
}
