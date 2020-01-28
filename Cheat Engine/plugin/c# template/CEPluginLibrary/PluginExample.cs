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
        static readonly PluginExample thisplugin = new PluginExample();

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
            //sdk.lua.dostring("print('I am allive')");
            
            return true;            
        }
    }
}
