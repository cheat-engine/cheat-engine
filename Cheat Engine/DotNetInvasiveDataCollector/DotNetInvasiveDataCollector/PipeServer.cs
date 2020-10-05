// Copyright Cheat Engine. All Rights Reserved.

using System;
using System.Threading;
using System.Reflection;
using System.IO.Pipes;
using Microsoft.Win32.SafeHandles;
using System.Collections.Generic;
using System.Text;

namespace DotNetInterface
{   

    public class PipeServer
    {
        static class Commands
        {
            public const byte TEST = 0;
            public const byte INITMODULELIST = 1;
            public const byte GETMETHODENTRYPOINT = 2;

            public const byte EXIT = 255;
        }
        

        Thread ServerThread;
        SafePipeHandle sph;
        NamedPipeServerStream s;

        List<Module> ModuleList = new List<Module>();

        



        public void WriteWord(UInt16 v)
        {
            s.Write(BitConverter.GetBytes(v), 0, 2);
        }

        public UInt16 ReadWord()
        {
            byte[] val = new byte[2];
            s.Read(val, 0, 2);
            return BitConverter.ToUInt16(val,0);
        }

        public void WriteDword(UInt32 v)
        {
            s.Write(BitConverter.GetBytes(v), 0, 4);
        }
        public UInt32 ReadDword()
        {
            byte[] val = new byte[4];
            s.Read(val, 0, 4);
            return BitConverter.ToUInt32(val, 0);
        }

        public void WriteQword(UInt64 v)
        {
            s.Write(BitConverter.GetBytes(v), 0, 8);
        }

        public UInt64 ReadQword()
        {
            byte[] val = new byte[8];
            s.Read(val, 0, 8);
            return BitConverter.ToUInt64(val, 0);
        }

        public void WriteUTF8String(String str)
        {
            byte[] stringbytes=Encoding.UTF8.GetBytes(str);
            WriteDword((uint)stringbytes.Length);
            s.Write(stringbytes, 0, stringbytes.Length);
        }

        private void getMethodEntryPoint()
        {
            int moduleid = (int)ReadDword();
            int methoddef = (int)ReadDword();
            UInt64 a = 0;


            if ((moduleid >= 0) && (moduleid < ModuleList.Count))
            {
                Module m = ModuleList[moduleid];
                try
                {
                    MethodBase mb = m.ResolveMethod(methoddef);

                    System.Runtime.CompilerServices.RuntimeHelpers.PrepareMethod(mb.MethodHandle);


                    IntPtr p = mb.MethodHandle.GetFunctionPointer();
                    a = (UInt64)p.ToInt64();
                }
                catch
                {                
                }

                            
            }
    
            WriteQword(a);
        }

        private void initModuleList()
        {
            AppDomain cd=AppDomain.CurrentDomain;

            Assembly[] AssemblyList = cd.GetAssemblies();
            ModuleList.Clear();          
            
            int i;
            for (i=0; i<AssemblyList.Length; i++)
            {
                Module[] LocalModuleList = AssemblyList[i].GetModules();
                int j;
                for (j=0; j<LocalModuleList.Length; j++)
                    ModuleList.Add(LocalModuleList[j]);
            }

            //send the list, the order of this list is going to be used as ID's for modulespecific access           
            WriteDword((uint)ModuleList.Count); 
            for (i = 0; i < ModuleList.Count; i++)
                WriteUTF8String(ModuleList[i].Name);
        }

        private void PipeServerThread()
        {
            

            try
            {
                s = new NamedPipeServerStream(PipeDirection.InOut, false, false, sph);
                s.WaitForConnection();
            }
            catch
            {
                return;
            }

            try
            {
                while (1==1)
                {
                    int command = s.ReadByte();

                    switch (command)
                    {
                        case -1:
                        case Commands.EXIT: return; //pipe closed

                        case Commands.TEST:
                            s.WriteByte((byte)(s.ReadByte() ^ 0xce));
                            break;

                        case Commands.INITMODULELIST:
                            initModuleList();
                            break;

                        case Commands.GETMETHODENTRYPOINT:
                            getMethodEntryPoint();
                            break;

                            //case Commands.
                    }
                }                                    
            }
            catch
            {                
            }
            finally
            {
                s.Close();
            }

            

        }

        PipeServer(IntPtr PipeHandle)
        {
            try
            {
                sph = new SafePipeHandle(PipeHandle, true);
                ServerThread = new Thread(new ThreadStart(PipeServerThread));
                ServerThread.Start();

                
            }
            catch
            {

            }
        }

        public static int Init(string parameters)
        {
            //new instance
            PipeServer newServer = new PipeServer((IntPtr)UInt64.Parse(parameters));
            return 1;
        }
    }
}


