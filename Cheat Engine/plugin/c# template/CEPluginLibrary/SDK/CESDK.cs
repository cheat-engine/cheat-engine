using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;


//CE SDK wrapper.  You usually don't need to be here, so close your eyes and walk away

namespace CESDK
{
 
    public abstract class CESDKPluginClass
    {
        public CESDK sdk;
        public abstract String GetPluginName();
        public abstract Boolean EnablePlugin();
        public abstract Boolean DisablePlugin();
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct TExportedFunctions
    {
        public int sizeofExportedFunctions;
        public IntPtr GetLuaState;
        public IntPtr LuaRegister;
        public IntPtr LuaPushClassInstance;
        public IntPtr ProcessMessages;
        public IntPtr CheckSynchronize;
    }

    public class CESDK
    {
        public static CESDKPluginClass currentPlugin;
        public CESDKLua lua;
        private const int PLUGINVERSION = 6; //CE SDK plugin version it expects to work with (needed in case newer ce versions change things)
        static IntPtr PluginNamePtr;

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern IntPtr LoadLibrary(string libname);




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

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate void delegateProcessMessages();

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate bool delegateCheckSynchronize(int timeout);


        private static CESDK mainself; //prevents garbage collection

        private delegateGetVersion delGetVersion;
        private delegateEnablePlugin delEnablePlugin;
        private delegateDisablePlugin delDisablePlugin;
        private delegateProcessMessages delProcessMessages;
        private delegateCheckSynchronize delCheckSynchronize;

        public UInt32 pluginid;
        public TExportedFunctions pluginexports;

        /// <summary>
        /// Call this to handle all waiting window messages in the CE main thread. Handy when doing something slow inside the main thread and you don't want the user to think CE has frozen
        /// </summary>
        public void ProcessMessages()
        {
            delProcessMessages();
        }

        /// <summary>
        /// Call this when have the main thread frozen but may be waiting for a thread to handle it's synchronize event (handy when you have frozen the main thread and have a thread running that's first syncing with CE's gui)
        /// </summary>
        /// <param name="timeout">Timeout in milliseconds to wait</param>
        /// <returns></returns>

        public bool CheckSynchronize(int timeout)
        {
            return delCheckSynchronize(timeout);
        }

        
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

            //setup the delegates
            if (delProcessMessages == null)
                delProcessMessages = Marshal.GetDelegateForFunctionPointer<delegateProcessMessages>(pluginexports.ProcessMessages);

            if (delCheckSynchronize == null)
                delCheckSynchronize = Marshal.GetDelegateForFunctionPointer<delegateCheckSynchronize>(pluginexports.CheckSynchronize);

            if (lua==null)
                lua = new CESDKLua(this);

            currentPlugin.sdk = this;
            return currentPlugin.EnablePlugin();
        }

        private Boolean DisablePlugin()
        {
            return currentPlugin.DisablePlugin();            
        }

        CESDK()
        {
            delGetVersion = GetVersion;
            delEnablePlugin = EnablePlugin;
            delDisablePlugin = DisablePlugin;
            return;
        }

        public static int CEPluginInitialize(string parameters)
        {
            if (mainself == null)
                mainself = new CESDK();

            if ((Int64)PluginNamePtr == 0)
            {
                Type[] x=typeof(CESDKPluginClass).Assembly.GetTypes();                
           
                for (int i=0; i<x.Count(); i++)
                {
                    if (x[i].IsSubclassOf(typeof(CESDKPluginClass)))
                    {
                        currentPlugin = (CESDKPluginClass)Activator.CreateInstance(x[i]);
                        break;
                    }
                }

                if (currentPlugin == null)
                    return 0;

                PluginNamePtr = Marshal.StringToHGlobalAnsi(currentPlugin.GetPluginName());
            }



            UInt64 a = UInt64.Parse(parameters);


            TPluginInit bla;
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
