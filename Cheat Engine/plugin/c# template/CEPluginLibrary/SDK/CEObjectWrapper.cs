using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CESDK
{
    /// <summary>
    /// Base class for implementing objects inherited from TObject  (just a destructor in this case)
    /// </summary>
    class CEObjectWrapper
    {        
        protected CESDKLua lua = CESDK.currentPlugin.sdk.lua;
        protected IntPtr CEObject;
        public IntPtr obj { get { return CEObject; } }



        ~CEObjectWrapper()
        {
            if (CEObject != IntPtr.Zero)
            {
                lua.PushCEObject(CEObject);
                lua.PushString("destroy");
                lua.GetTable(-2);

                if (lua.IsFunction(-1))
                {
                    lua.PCall(0, 0);
                }
                else
                    throw new System.ApplicationException("Object without a destroy method");
            }
        }
    }
}
