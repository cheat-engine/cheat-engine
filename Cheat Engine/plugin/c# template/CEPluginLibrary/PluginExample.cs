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
            var l = sdk.lua;
            l.PushString("WEEEE");                        
            if (l.GetTop() > 0)
            {
                if (l.IsInteger(1))
                {
                    int i = l.ToInteger(1);
                    l.PushInteger(i * 2);
                    return 2;
                }
            }

            return 1;
        }

        int MyFunction2(IntPtr L)
        {
            var l = sdk.lua;
            l.PushString(L, "Works");
            l.PushString("And this as well");

            l.DoString("MainForm.Caption='Changed by test2()'");
            
            return 2;
        }

    }
}
