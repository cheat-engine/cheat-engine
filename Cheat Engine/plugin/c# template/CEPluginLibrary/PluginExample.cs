using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
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
            

            sdk.lua.Register("pluginexample1", MyFunction);
            sdk.lua.Register("pluginexample2", MyFunction2);
            sdk.lua.Register("pluginexample3", MyFunction3);
            sdk.lua.Register("pluginexample4", MyFunction4);
            sdk.lua.Register("formpymcformface", MyFunction4);

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

            l.DoString("MainForm.Caption='Changed by test2()'");

            l.PushString(L, "Works");
            l.PushString("And this as well");

           return 2;
        }

        public void NewThreadExample()
        {
            //return;
            sdk.lua.DoString("print('Running from a different thread. And showing this requires the synchronize capability of the main thread')"); //print is threadsafe

            //now running an arbitrary method in a different thread

        }
        int MyFunction3()
        {
            Thread thr = new Thread(NewThreadExample);
            int i = 0;
            thr.Start();

            while (thr.IsAlive)
            {                
                sdk.CheckSynchronize(10); //ce would freeze without this as print will call Synchronize to run it in the main thread               
                i = i + 1;
            }

            sdk.lua.PushInteger(i);
            
            return 1;
        }

        void NewGuiThread()
        {
            PluginExampleForm formpy = new PluginExampleForm();
            System.Windows.Forms.Application.Run(formpy);
            return;
        }

        int MyFunction4()
        {
            if (sdk.lua.ToBoolean(1))
            {
                //run in a thread
                Thread thr = new Thread(NewGuiThread);
                thr.Start();
            }
            else
            {
                //formpy.Show(); //or formpy.ShowDialog()
                //run in the current thread (kinda)
                NewGuiThread();                
            }

            sdk.lua.PushInteger(100);
            return 1;
        }

    }
}
