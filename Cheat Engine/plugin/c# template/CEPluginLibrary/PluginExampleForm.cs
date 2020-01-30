using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using CESDK;

namespace CEPluginLibrary
{
    public partial class PluginExampleForm : Form
    {
        public PluginExampleForm()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {           
            MessageBox.Show("WEEEEEEE");
            GC.Collect();
        }

        

        private void button2_Click(object sender, EventArgs e)
        {
            MemScan ms;
            try
            {
                ms = new MemScan();
            }
            catch(Exception ex)
            {
                Console.WriteLine(ex.Message);
            }



            ms = null;
        }
    }
}
