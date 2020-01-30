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

        MemScan ms;

        private void MemScanDone(object sender)
        {
            //called from CE's main UI thread. Problematic if the form was created using a new thread
            if (this.InvokeRequired)
            {                
                this.BeginInvoke(((MemScan)sender).OnScanDone,sender);
            }
            else
            {
                MessageBox.Show("Scan done. Todo: Create a foundlist");
                button2.Enabled = true;
            }

        }

        private void MemScanGuiUpdate(object sender, UInt64 TotalAddressesToScan, UInt64 CurrentlyScanned, UInt64 ResultsFound)
        {
            //called from CE's main UI thread. Problematic if the form was created using a new thread
            if (this.InvokeRequired)
            {
                this.BeginInvoke(((MemScan)sender).OnGuiUpdate, sender, TotalAddressesToScan, CurrentlyScanned, ResultsFound);
            }
            else
            {
                if (TotalAddressesToScan > 0)
                {
                    int percentage = (int)((double)(CurrentlyScanned/TotalAddressesToScan ) * 100);
                    progressBar1.Value = percentage;
                }
                else
                    progressBar1.Value = 0;
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            if (ms == null)
            {
                try
                {
                    ms = new MemScan();

                    ms.OnGuiUpdate = MemScanGuiUpdate;
                    ms.OnScanDone = MemScanDone;
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            ScanParameters scanParams = new ScanParameters();

            scanParams.Value = textBox1.Text;
            ms.Scan(scanParams);

            button2.Enabled = false;

            //ms.WaitTillDone();
        }

        private void PluginExampleForm_Load(object sender, EventArgs e)
        {
            comboBox1.SelectedIndex = 2;
            //listView1.VirtualListSize = 10;
        }

        private void listView1_RetrieveVirtualItem(object sender, RetrieveVirtualItemEventArgs e)
        {
            if (e.Item == null)
            {
                e.Item = new ListViewItem();
                e.Item.Text = "weee"+e.ItemIndex;
                e.Item.SubItems.Add("bla");
            }
           
        }
    }
}
