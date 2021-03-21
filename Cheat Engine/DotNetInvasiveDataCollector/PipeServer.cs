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
            public const byte GETMETHODPARAMETERS = 7;
            public const byte WRAPOBJECT = 8;
            public const byte UNWRAPOBJECT = 9;
            public const byte INVOKEMETHOD = 10;
            public const byte EXIT = 255;
        }
        

        Thread ServerThread;
        SafePipeHandle sph;
        NamedPipeServerStream s;

        List<Module> ModuleList = new List<Module>();



        public void WriteByte(byte v)
        {
            s.Write(BitConverter.GetBytes(v), 0, 1);
        }

        
        public byte ReadByte()
        {
            byte[] val = new byte[1];
            if (s.Read(val, 0, 1) == 1)
                return val[0];
            else
                throw new Exception("ReadByte returned incorrect amount");
                
        }

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

        private void getMethodParameters()
        {
            int moduleid = (int)ReadDword();
            int methoddef = (int)ReadDword();
            string returntypestring = "void";
            string parameterstring = "";
            List<TypeCode> parameters = new List<TypeCode>{ }; //first one is the return type, the rest are the parameters

            if ((moduleid >= 0) && (moduleid < ModuleList.Count))
            {
                Module m = ModuleList[moduleid];
                try
                {
                    MethodBase mb = m.ResolveMethod(methoddef);
                    if (mb is MethodInfo)
                    {
                        int i;
                        MethodInfo mi = (MethodInfo)mb;
                        returntypestring = mi.ReturnType.FullName;

                        parameters.Add(Type.GetTypeCode(mi.ReturnType));

                        ParameterInfo[] pi= mi.GetParameters();
                        for (i=0; i<pi.Length; i++)
                        {
                            string ps = "";
                            ParameterInfo p = pi[i];
                            ps = p.ParameterType.FullName + ' ' + p.Name;

                            if (i == 0)
                                parameterstring = ps;
                            else
                                parameterstring = parameterstring + ", " + ps;

                            parameters.Add(Type.GetTypeCode(p.ParameterType));                            
                        }

                        
                    }                    
                }
                catch
                {
                   
                }
            }

            parameterstring = "(" + parameterstring + ")";
            WriteUTF8String(returntypestring);
            WriteUTF8String(parameterstring);

            WriteByte((byte)parameters.Count);
            for (int i=0; i<parameters.Count; i++)            
                WriteByte((byte)parameters[i]);            

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
                    Marshal.PrelinkAll(mb.GetType());

                    System.Runtime.CompilerServices.RuntimeHelpers.PrepareMethod(mb.MethodHandle);

                    Type t = mb.GetType();                    
                    
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
            instanceGCH = GCHandle.Alloc(null);
            try
            {
                

                if (instanceptr!=IntPtr.Zero)
                {                    
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
                instanceGCH = GCHandle.Alloc(null);

                if (instanceptr != IntPtr.Zero)
                {
                    
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
                    instanceGCH = GCHandle.FromIntPtr(IntPtr.Zero);
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
                
                Module[] ml = a.GetModules();
                int i;
                for (i=0; i<ml.Length; i++)
                {                    
                    Type[] types = ml[i].GetTypes();
                    int j;
                    for (j = 0; j < types.Length; j++)
                    {
                        MethodInfo[] mil=types[j].GetMethods();
                        int k;
                        for (k = 0; k < mil.Length; k++)
                        {
                            System.Runtime.CompilerServices.RuntimeHelpers.PrepareMethod(mil[k].MethodHandle);
                            Marshal.Prelink(mil[k]);                            
                        }
                       
                        //Marshal.PrelinkAll(types[j]);
                    }
                }
                WriteDword(1);
            }
            catch
            {
                WriteDword(0);
            }
        }

        List<object> reflist = new List<object>();

        private void wrapObject()
        {
            UInt64 v=ReadQword(); //native address
            GCHandle h = GCHandle.Alloc(null);
            Marshal.WriteIntPtr((IntPtr)h, (IntPtr)v);

            
            object x = h.Target;

            reflist.Add(x);

            WriteQword((UInt64)GCHandle.ToIntPtr(h));
        }

        private void unwrapObject()
        {
            UInt64 v = ReadQword(); //gchandle wrapped address
            GCHandle h=GCHandle.FromIntPtr((IntPtr)v);
            object t = h.Target;

            h.Free();
            reflist.Remove(t);            
        }


        private void invokeMethod()
        {
            int i;
            int moduleid = (int)ReadDword();
            int methoddef = (int)ReadDword();
            GCHandle instancehandle = GCHandle.FromIntPtr((IntPtr)ReadQword()); //it's a gcobject
            object instance = instancehandle.Target;
            
            int paramcount = (int)ReadByte();
            object[] parameters = new object[paramcount];

            for (i=0; i<paramcount; i++)
            {
                byte[] v;
                object param = null;
                byte type = (byte)ReadByte();
                switch ((TypeCode)type)
                {
                    case TypeCode.Boolean:                                        
                        param = ReadByte()!=0;
                        break;  

                    case TypeCode.Char:
                        v = new byte[2];
                        s.Read(v, 0, 2);
                        param=BitConverter.ToChar(v, 0);                        
                        break;

                    case TypeCode.SByte:                        
                        param = (sbyte)ReadByte();
                        break;

                    case TypeCode.Byte:
                        param = ReadByte();
                        break;

                    case TypeCode.Int16:
                        param = (Int16)ReadWord();
                        break;

                    case TypeCode.UInt16:
                        param = ReadWord();
                        break;

                    case TypeCode.Int32:
                        param = (Int32)ReadDword();
                        break;

                    case TypeCode.UInt32:
                        param = ReadDword();
                        break;

                    case TypeCode.Int64:
                        param = (Int64)ReadQword();
                        break;

                    case TypeCode.UInt64:
                        param = ReadQword();
                        break;

                    case TypeCode.Single:
                        v = new byte[4];
                        s.Read(v, 0, 4);
                        param = BitConverter.ToSingle(v, 0);
                        break;

                    case TypeCode.Double:
                        v = new byte[8];
                        s.Read(v, 0, 8);
                        param = BitConverter.ToDouble(v, 0);
                        break;

                    case TypeCode.Decimal:                        
                        v = new byte[8];                        
                        s.Read(v, 0, 8);
                        param = (decimal)BitConverter.ToDouble(v, 0); //convert the double to decimal
                        break;

                    /*
                    v = new byte[16];
                    s.Read(v, 0, 16);
                    {
                        int[] bits = new int[4];
                        bits[0]=BitConverter.ToInt32(v, 0);
                        bits[1]=BitConverter.ToInt32(v, 4);
                        bits[2]=BitConverter.ToInt32(v, 8);
                        bits[3]=BitConverter.ToInt32(v, 12);

                        param = new decimal(bits);
                    }
* 
                    */
                    case TypeCode.DateTime:
                        param=DateTime.FromFileTime((long)ReadQword());
                        break;

                    case TypeCode.String:
                        param = ReadUTF8String();
                        break;

                    case TypeCode.Object:
                    default: //handle as a wrapped gc handle
                        {
                            IntPtr val;

                            v = new byte[8];
                            s.Read(v, 0, 8);
                            if (IntPtr.Size == 4)
                                val = (IntPtr)BitConverter.ToInt32(v, 0);
                            else
                                val = (IntPtr)BitConverter.ToInt64(v, 0);

                            if (val == IntPtr.Zero)
                                param = null;
                            else
                            {
                                GCHandle h = GCHandle.FromIntPtr(val);
                                param = h.Target;
                            }
                           
                            break;
                        }


                }


                parameters[i] = param;
            }
        

            if ((moduleid >= 0) && (moduleid < ModuleList.Count))
            {
                byte[] bytes;
                Module m = ModuleList[moduleid];
                MethodBase mb = m.ResolveMethod(methoddef);
                if (mb!=null)
                {
                    object result = mb.Invoke(instance, parameters);

                    if (result == null)
                        WriteByte((byte)TypeCode.Empty);
                    else
                    {
                        Type rtype = result.GetType();
                        TypeCode tc = Type.GetTypeCode(rtype);
                        WriteByte((byte)tc);
                        switch (tc)
                        {
                            case TypeCode.Boolean:
                                if ((Boolean)result)
                                    WriteByte(1);
                                else
                                    WriteByte(0);

                                break;

                            case TypeCode.Char:
                                bytes = BitConverter.GetBytes((Char)result);
                                s.Write(bytes, 0, 2);
                                break;

                            case TypeCode.SByte:
                            case TypeCode.Byte:
                                WriteByte((byte)result);
                                break;

                            case TypeCode.Int16:
                            case TypeCode.UInt16:
                                WriteWord((UInt16)result);
                                break;

                            case TypeCode.Int32:
                            case TypeCode.UInt32:
                                WriteDword((UInt32)result);
                                break;

                            case TypeCode.Int64:
                            case TypeCode.UInt64:
                                WriteQword((UInt64)result);
                                break;

                            case TypeCode.Single:
                                bytes = BitConverter.GetBytes((Single)result);
                                s.Write(bytes, 0, 4);
                                break;

                            case TypeCode.Double:
                                bytes = BitConverter.GetBytes((Double)result);
                                s.Write(bytes, 0, 8);
                                break;

                            case TypeCode.Decimal:
                                Double tempdouble = (Double)(Decimal)result;
                                bytes = BitConverter.GetBytes(tempdouble);
                                s.Write(bytes, 0, 8);
                                break;

                            case TypeCode.DateTime:
                                //conver the time to filetime
                                WriteQword((UInt64)((DateTime)result).ToFileTime());
                                break;

                            case TypeCode.String: //assuming utf8 string 
                                WriteUTF8String((string)result);
                                break;

                            case TypeCode.Object:
                            default: //return a gchandle
                                WriteQword((UInt64)GCHandle.ToIntPtr(GCHandle.Alloc(result)));
                                break;
                        }
                    }
                }
                else
                {
                    WriteByte(255);
                    WriteUTF8String("Invalid method");
                }

            }
  

        }




        private void PipeServerThread()
        {
            

            try
            {
                s = new NamedPipeServerStream(PipeDirection.InOut, false, false, sph);
                s.WaitForConnection();

                //sph.Close();                                
                sph = null;
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

                        case Commands.GETMETHODPARAMETERS:
                            getMethodParameters();
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

                        case Commands.WRAPOBJECT:
                            wrapObject();
                            break;

                        case Commands.UNWRAPOBJECT:
                            unwrapObject();
                            break;

                        case Commands.INVOKEMETHOD:
                            invokeMethod();
                            break;

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


