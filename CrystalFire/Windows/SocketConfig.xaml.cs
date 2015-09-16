using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using System.Xml;

using Microsoft.Win32;


namespace CrystalFire
{
    /// <summary>
    /// Interaction logic for CfgWindow.xaml
    /// </summary>
    public partial class SocketConfig : Window
    {
        private CfgSettings config, mainConfig;
        private String path;
        private SocketWindow _window;



        public SocketConfig(SocketWindow window)
            : base()
        {
            InitializeComponent();
            _window = window;


        }


       private void SaveCfgBtn_Click(object sender, RoutedEventArgs e)
       {
            OnSave();
            
            //System.Windows.MessageBox.Show("The configuration is saved!");
        }

        private void SaveExitCfgBtn_Click(object sender, RoutedEventArgs e)
        {
            OnSave();
            this.DialogResult = true;
            this.Close();
        }

        private void OnSave()
        {
            try
            {
                //config.HistPath = TextBox4.Text;
                config.DammagePath = TextBox1.Text;
                config.ObservationHost = TextBox2.Text;
                config.ObservationPort = Int32.Parse(TextBox3.Text);
                config.WriteToFile();
                _window.Config = config;
            }
            catch (Exception e2)
            {
                System.Windows.MessageBox.Show("Cannot save configuration:" + e2.Message);
            }
        }

        private void CgfCancelBtn_Click(object sender, RoutedEventArgs e)
        {
            this.DialogResult = false;
            this.Close();
        }

        private void iconOpenBtn_Click(object sender, RoutedEventArgs e)
        {
            Microsoft.Win32.OpenFileDialog openFileDialog1 = new Microsoft.Win32.OpenFileDialog();
            bool? userClickedOK = openFileDialog1.ShowDialog();

            if (userClickedOK == true)
            {
                TextBox1.Text = openFileDialog1.FileName;
            }

        }

        private void Window_Initialized(object sender, RoutedEventArgs e)
        {
            try
            {
                
                config = new CfgSettings();
                config.LoadFromFile();
                if (config!=null)
                {
                    TextBox1.Text = config.DammagePath;
                    TextBox2.Text = config.ObservationHost;
                    TextBox3.Text = config.ObservationPort.ToString();
                }
            }
            catch (Exception e1)
            {
                System.Windows.MessageBox.Show("Error parsing config.xml file: " + e1.Message);
                this.Close();
            }
        }


    }
}
