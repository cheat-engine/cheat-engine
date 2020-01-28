using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CEPluginLibrary
{
    
    class PluginExample : CESDKPluginClass
    {        
        public override bool DisablePlugin() //called when disabled
        {
            return true;
        }

        public override bool EnablePlugin() //called when enabled
        {
            return true;            
        }
    }
}
