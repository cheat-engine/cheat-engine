// Copyright Cheat Engine. All Rights Reserved.

using System;
using System.Threading;
using System.Reflection;
using System.IO.Pipes;
using Microsoft.Win32.SafeHandles;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace DotNetInterface
{   

    public class PipeServer
    {
        static class Commands
        {
            public const byte TEST = 0;
            public const byte INITMODULELIST = 1;
            public const byte GETMETHODENTRYPOINT = 2;
            public const byte GETFIELDTYPENAME = 3;
            public const byte GETFIELDVALUE = 4;
            public const byte SETFIELDVALUE = 5;
            public const byte LOADMODULE = 6;
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

        public string ReadUTF8String()
        {
            int stringlength = (int)ReadDword();
            byte[] stringbytes = new byte[stringlength];
            s.Read(stringbytes, 0, stringlength);

            return Encoding.UTF8.GetString(stringbytes);
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

                    Type t = mb.GetType();
                    Marshal.PrelinkAll(t);
                    
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

        private void getFieldTypeName()
        {
            string fieldtypename="";
            int moduleid = (int)ReadDword();
            int fielddef = (int)ReadDword();
            if ((moduleid >= 0) && (moduleid < ModuleList.Count))
            {
                Module m = ModuleList[moduleid];
                try
                {
                    FieldInfo mb = m.ResolveField(fielddef);

                    fieldtypename = mb.FieldType.FullName; 
                }
                catch
                {
                }
            }

            WriteUTF8String(fieldtypename);
        }



        private void getFieldValue()
        {
            string value = "";
            int moduleid = (int)ReadDword();
            int fielddef = (int)ReadDword();
            IntPtr instanceptr = (IntPtr)ReadQword();
            object instance = null;

            GCHandle instanceGCH;

            try
            {

                if (instanceptr!=IntPtr.Zero)
                {
                    instanceGCH = GCHandle.Alloc(null);
                    Marshal.WriteIntPtr((IntPtr)instanceGCH, instanceptr);
                    instance = instanceGCH.Target;
                }

            
                if ((moduleid >= 0) && (moduleid < ModuleList.Count))
                {
                    Module m = ModuleList[moduleid];                    
                    FieldInfo mb = m.ResolveField(fielddef);
                    if (mb != null)
                    {
                        object o = mb.GetValue(instance);
                        if (o != null)
                        {
                            Type ot = o.GetType();
                            if ((ot.IsPrimitive) || (Type.GetTypeCode(ot) == TypeCode.String))
                                value = o.ToString();
                            else
                            {
                                value = o.GetType().Name;
                                GCHandle g = GCHandle.Alloc(o);
                                IntPtr addr = GCHandle.ToIntPtr(g);
                                IntPtr real = Marshal.ReadIntPtr(addr);
                                value = o.GetType().Name + " : " + real.ToString("X8");
                                g.Free();

                                System.GC.Collect(); //without this you can see the process memory grow a lot
                            }
                        }
                        else
                            value = "null";
                    }                   
                }
            }
            catch
            {

            }
            finally
            {
                WriteUTF8String(value);

                
                if (instanceptr != IntPtr.Zero)
                {
                    instance = null;
                    try
                    {
                        Marshal.WriteIntPtr((IntPtr)instanceGCH, IntPtr.Zero);
                        instanceGCH.Free();
                    }
                    catch
                    {

                    }                        
                }
            }

           
        }

        private void setFieldValue()
        {
            int moduleid = (int)ReadDword();
            int fielddef = (int)ReadDword();
            IntPtr instanceptr = (IntPtr)ReadQword();
            string value = ReadUTF8String();

            object instance = null;
            GCHandle instanceGCH;

            try
            {
                if (instanceptr != IntPtr.Zero)
                {
                    instanceGCH = GCHandle.Alloc(null);
                    Marshal.WriteIntPtr((IntPtr)instanceGCH, instanceptr);
                    instance = instanceGCH.Target;
                }


                if ((moduleid >= 0) && (moduleid < ModuleList.Count))
                {
                    Module m = ModuleList[moduleid];
                   
                    FieldInfo mb = m.ResolveField(fielddef);
                    TypeCode tc = Type.GetTypeCode(mb.FieldType);

          

                    switch (tc)
                    {
                        case TypeCode.Boolean:
                            mb.SetValue(instance, Boolean.Parse(value));
                            break;

                        case TypeCode.Char:
                            mb.SetValue(instance, Char.Parse(value));
                            break;

                        case TypeCode.SByte:
                            mb.SetValue(instance, SByte.Parse(value));
                            break;

                        case TypeCode.Byte:
                            mb.SetValue(instance, Byte.Parse(value));
                            break;

                        case TypeCode.Int16:
                            mb.SetValue(instance, Int16.Parse(value));
                            break;

                        case TypeCode.UInt16:
                            mb.SetValue(instance, UInt16.Parse(value));
                            break;

                        case TypeCode.Int32:
                            mb.SetValue(instance, Int32.Parse(value));
                            break;

                        case TypeCode.UInt32:
                            mb.SetValue(instance, UInt32.Parse(value));
                            break;

                        case TypeCode.Int64:
                            mb.SetValue(instance, Int32.Parse(value));
                            break;

                        case TypeCode.UInt64:
                            mb.SetValue(instance, UInt32.Parse(value));
                            break;

                        case TypeCode.Single:
                            mb.SetValue(instance, Single.Parse(value));
                            break;

                        case TypeCode.Double:
                            mb.SetValue(instance, Double.Parse(value));
                            break;

                        case TypeCode.Decimal:
                            mb.SetValue(instance, Decimal.Parse(value));
                            break;

                        case TypeCode.DateTime:
                            mb.SetValue(instance, DateTime.Parse(value));
                            break;

                        case TypeCode.String:
                            mb.SetValue(instance, value);
                            break;
                    }
                    
                }

                if (instanceptr != IntPtr.Zero)
                {
                    instance = null;
                    Marshal.WriteIntPtr((IntPtr)instanceGCH, IntPtr.Zero);
                    instanceGCH.Free();
                }
            }
            catch
            {

            }

        }

        private void loadModule()
        {
            string modulepath=ReadUTF8String();
            try
            {                
                Assembly a = Assembly.LoadFile(modulepath);
                WriteDword(1);
            }
            catch
            {
                WriteDword(0);
            }
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

                        case Commands.GETFIELDTYPENAME:
                            getFieldTypeName();
                            break;

                        case Commands.GETFIELDVALUE:
                            getFieldValue();
                            break;

                        case Commands.SETFIELDVALUE:
                            setFieldValue();
                            break;

                        case Commands.LOADMODULE:
                            loadModule();
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


