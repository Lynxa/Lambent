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

namespace CrystalFire.Windows
{
    /// <summary>
    /// Interaction logic for InitialSelectionWindow.xaml
    /// </summary>
    public partial class InitialSelectionWindow : Window
    {
        
        public InitialSelectionWindow()
        {
            {
                InitializeComponent();
            }
            
        }

        private void OnReplayButton(object sender, RoutedEventArgs e)
        {
            OnClick(RunMode.Log);
        }

        private void OnNewSimulationButton(object sender, RoutedEventArgs e)
        {
            OnClick(RunMode.Full);

        }

        private void OnClick(RunMode runMode)
        {
            bool to_close;
            SocketWindow socketWindow = new SocketWindow(runMode, out to_close);
            if (!to_close)
            {
                socketWindow.Show();
            }

            //this.Close();
        }
    }
}
