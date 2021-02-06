using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using Microsoft.CSharp;
using System.IO;
using System.CodeDom.Compiler;
using System.Runtime.InteropServices;

namespace CSCompiler
{
    public class Compiler
    {
        [UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        private delegate Boolean delegateCompile(string script, string outputpath, IntPtr userdata, delegateCompileError error);

        [UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        private delegate void delegateAddAssemblyReference(string path);

        [UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        private delegate void delegateSetCoreAssembly(string path);

        [UnmanagedFunctionPointer(CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        public delegate void delegateCompileError(IntPtr userdata,  string errormsg);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        private delegate void delegateRelease();

        
        CompilerParameters cp;

        string CoreAssembly;


        public void AddAssemblyReference(string path)
        {
            cp.ReferencedAssemblies.Add(path);            
        }

        public void SetCoreAssembly(string path)
        {
            CoreAssembly = path;
        }

        public Boolean CompileCode(string script, string outputpath, IntPtr userdata, delegateCompileError error)
        {
            if (outputpath=="")
            {
                error(userdata, "No output assembly path set");
                return false;
            }

            cp.OutputAssembly=outputpath;
            if (CoreAssembly!="")
                cp.CoreAssemblyFileName = CoreAssembly;

            CodeDomProvider provider = CodeDomProvider.CreateProvider("CSharp");
            CompilerResults r = provider.CompileAssemblyFromSource(cp, script);

            if (r.Errors.Count > 0)
            {
                int i;
                for (i = 0; i < r.Errors.Count; i++)
                    error(userdata, r.Errors[i].Line.ToString() + " - " + r.Errors[i].ErrorText);

                return false;
            }
            else
            {
                return true;
            }            
        }


        Compiler()
        {
            cp = new CompilerParameters();
            cp.GenerateExecutable = false;
            cp.GenerateInMemory = false;
        }

        GCHandle CurrentCompilerHandle;

        [StructLayout(LayoutKind.Sequential)]
        public struct CompilerFunctions
        {
            
            public IntPtr CompileCode;
            public IntPtr AddReference;
            public IntPtr SetCoreAssembly;
            public IntPtr Release;
        }

        void Release()
        {
            if (CurrentCompilerHandle.IsAllocated)
                CurrentCompilerHandle.Free();
        }



        public static int NewCompiler(string parameters)
        {
            //parameter is formatted as address,path
            UInt64 a;

            if (UInt64.TryParse(parameters, out a))
            {
                CompilerFunctions s = new CompilerFunctions();
                Compiler newCompiler = new Compiler();


                newCompiler.CurrentCompilerHandle = GCHandle.Alloc(newCompiler);

                s.CompileCode = Marshal.GetFunctionPointerForDelegate((delegateCompile)(newCompiler.CompileCode));
                s.AddReference = Marshal.GetFunctionPointerForDelegate((delegateAddAssemblyReference)(newCompiler.AddAssemblyReference));
                s.SetCoreAssembly = Marshal.GetFunctionPointerForDelegate((delegateAddAssemblyReference)(newCompiler.SetCoreAssembly));
                s.Release = Marshal.GetFunctionPointerForDelegate((delegateRelease)(newCompiler.Release));

                Marshal.StructureToPtr<CompilerFunctions>(s, (IntPtr)a, false);

                return 1;
            }
            else
                return 0;
        }
    }
}
