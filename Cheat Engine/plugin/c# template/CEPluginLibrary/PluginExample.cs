using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CESDK;

namespace CEPluginLibrary
{
    class PluginExample : CESDKPluginClass
    {
        public override string GetPluginName()
        {
            return "C# Plugin Template for Cheat Engine 7.1+";
        }

        public override bool DisablePlugin() //called when disabled
        {
            
            return true;
        }
        
        public override bool EnablePlugin() //called when enabled
        {
            //you can use sdk here
            //sdk.lua.dostring("print('I am alive')");
            sdk.lua.Register("test", MyFunction);
            sdk.lua.Register("test2", MyFunction2);

            return true;            
        }

        int MyFunction()
        {
            sdk.lua.PushString("WEEEE");
            
            return 1;
        }

        int MyFunction2(IntPtr L)
        {
            sdk.lua.PushString(L, "Works");
            sdk.lua.PushString("And this as well");

            sdk.lua.DoString("MainForm.Caption='Changed by test2()'");
            
            return 2;
        }

    }
}
