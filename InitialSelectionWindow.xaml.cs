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

namespace AgentsRebuilt
{
    /// <summary>
    /// Interaction logic for InitialSelectionWindow.xaml
    /// </summary>
    public partial class InitialSelectionWindow : Window
    {
        public InitialSelectionWindow()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            bool to_close;
            ReplayWindow replayWindow = new ReplayWindow(out to_close);
            if (!to_close)
            {
                replayWindow.Show();
            }
           
         
        }

        private void Button_Click_1(object sender, RoutedEventArgs e)
        {
            bool to_close;
            SocketWindow socketWindow = new SocketWindow(out to_close);
            if (!to_close)
            {
                socketWindow.Show();
            }
           
            //this.Close();
        }
    }
}
