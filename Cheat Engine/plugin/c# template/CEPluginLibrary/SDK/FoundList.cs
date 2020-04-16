using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CESDK
{
    //Not much of an SDK but more an example of how to wrap the exposed classes by CE into C# classes. Learn from this and implement the other features you like


    class FoundList :CEObjectWrapper
    {
        public int Count { get { return GetCount(); } }

        int GetCount()
        {
            try
            {
                lua.PushCEObject(CEObject);
                lua.PushString("Count");
                lua.GetTable(-2);

                return (int)lua.ToInteger(-1);
            }
            finally
            {
                lua.SetTop(0);
            }                
        }

        public string GetAddress(int i)
        {
            
            try
            {
                lua.PushCEObject(CEObject);
                lua.PushString("Address");
                lua.GetTable(-2);

                if (lua.IsTable(-1))
                {
                    lua.PushInteger(i);
                    lua.GetTable(-2); //gets index i from the Address table  (pushInteger increased the stack by 1 so the -1 turned to -2, just in case you wanted to know...)
                    return lua.ToString(-1);
                }                
            }
            finally
            {
                lua.SetTop(0);
            }

            return "Error";
        }

        public string GetValue(int i)
        {

            try
            {
                lua.PushCEObject(CEObject);
                lua.PushString("Value");
                lua.GetTable(-2);

                if (lua.IsTable(-1))
                {
                    lua.PushInteger(i);
                    lua.GetTable(-2); //gets index i from the Address table  (pushInteger increased the stack by 1 so the -1 turned to -2, just in case you wanted to know...)
                    return lua.ToString(-1);
                }
            }
            finally
            {
                lua.SetTop(0);
            }

            return "Error";
        }

        public void Initialize()
        {
            try
            {
                lua.PushCEObject(CEObject);

                lua.PushString("initialize");
                lua.GetTable(-2);

                if (lua.IsFunction(-1) == false)
                    throw new System.ApplicationException("foundlist with no initialize method");

                lua.PCall(0, 0);
            }
            finally
            {
                lua.SetTop(0);
            }
        }

        public FoundList(MemScan ms)
        {
            try
            {
                lua.GetGlobal("createFoundList");
                if (lua.IsNil(-1))
                    throw new System.ApplicationException("You have no createFoundList (WTF)");

                lua.PushCEObject(ms.obj);
                int pcr = lua.PCall(1, 1);

                if (lua.IsCEObject(-1))
                    CEObject = lua.ToCEObject(-1);                        
                else
                    throw new System.ApplicationException("No idea what it returned");
            }
            finally
            {
                lua.SetTop(0);
            }

        }


    }
}
