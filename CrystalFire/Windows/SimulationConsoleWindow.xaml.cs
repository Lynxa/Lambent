using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Net.Sockets;
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

namespace CrystalFire
{
    /// <summary>
    /// Interaction logic for SimulationConsoleWindow.xaml
    /// </summary>
    public partial class SimulationConsoleWindow : Window
    {
        StreamWriter sw;
        //private AdminWindow _replay = null;
        private SocketWindow _replay1 = null;
        private ObservableCollection<String> log;

        public SimulationConsoleWindow(StreamWriter sw)
        {
            log = new ObservableCollection<string>();
            InitializeComponent();
            ConsoleLog.DataContext = log;
            this.sw = sw;
            if (this.Title.StartsWith("Admin"))
            {
                log.Add("Connected to communication module.");
                log.Add("Please, enter a message:");
            }
            else
            {
                log.Add("Connected to simulation");
                log.Add("Please, enter a message:");
            }
        }
        private void OnEnter(object sender, RoutedEventArgs e)
        {
            try
            {
                String command = CommandBox.Text;
                if (command.Length < 2) return;
                if (!command.EndsWith(". "))
                {
                    if (command.EndsWith(".")) command += " ";
                    else command += ". ";
                }
                sw.Write(command);
                sw.Flush();
                log.Add(command);

                if (this.Title.StartsWith("Admin"))
                {
                    if (CommandBox.Text.StartsWith("shutdown"))
                    {
                        log.Add("Shutting down...");

                        //TODO: Raise shutdown event
                        
                        this.Close();
                    }
                    else
                    {
                        CommandBox.Text = "";
                    }
                }

            }
            catch (Exception e1)
            {
                MessageBox.Show("Connection error!" + e1.Message);
                this.Close();
            }
        }
    }
}
