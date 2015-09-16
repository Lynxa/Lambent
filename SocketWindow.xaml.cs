using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Diagnostics.Eventing.Reader;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Net.Configuration;
using System.Net.Sockets;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Timers;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Windows.Threading;
using AgentsConfig;
using AgentsRebuilt.Annotations;
using Microsoft.Win32;
using System.Windows.Forms;
using Orientation = System.Windows.Controls.Orientation;

namespace AgentsRebuilt
{
    /// <summary>
    /// Interaction logic for ReplayWindow.xaml
    /// </summary>
    public partial class SocketWindow : Window, INotifyPropertyChanged
    {
        
        /*UI elements*/
        public static System.Timers.Timer _runTimer = new System.Timers.Timer(1500);
        StackPanel pausePanel = new StackPanel();
        Polygon runIcon = new Polygon();

        private int _timerPeriod = 1500;

        public CfgSettings Config = null;
        private LogProcessor _logProcessor = new LogProcessor();

        private ExecutionState execState = ExecutionState.Void;
        private ExecutionState _previousState = ExecutionState.Void;
        private int _move_to = 0;
        private Boolean isFirstLine = true;
        private bool stopwork;
        private EnvironmentState ast = null;
        private String _lockerLogProcessorIndex = "meta";
        private bool _is_actual = false;
        private String _currentAgent = "god";
        private DateTime _statusTime;
        private String _currentMessage;


        private AgentDataDictionary _agentDataDictionary;
        private double _statePeriod;
        private bool _stopSleeping=false;
        private StreamReader inputAdmin;
        private StreamWriter outputAdmin;
        
        private bool _hasAdministratorPrivilleges = false;

        private Dispatcher dsc;

        ObservableCollection<Agent> visualAgents;
        ObservableCollection<Agent> visualAgentsPane;
        ObservableCollection<Item> visualAuctions;
        ObservableCollection<Item> visualItemsPane;
        ObservableCollection<Item> visualCommons;
        ObservableCollection<String> tradeLog = null;


        private static StreamWriter _strWriter;

        private static bool Connect(String host, int port, out StreamWriter outStream, out StreamReader inStream)
        {
            try
            {
                TcpClient _client = new TcpClient();
                _client.Connect(host, port);
               NetworkStream stream = _client.GetStream();
                outStream = new StreamWriter(stream);
                inStream = new StreamReader(stream);
                 return true;
            }
            catch (Exception)
            {
                outStream = null;
                inStream = null;
                return false;
            }
        }

        public double RunTimerPeriod
        {
            get { return ((double)_timerPeriod / 1000); }
            set
            {
                _timerPeriod = (int) (value * 1000);
                OnPropertyChanged();
            }
        }

        public class NameMultiValueConverter : IMultiValueConverter
        {
            public object Convert(object[] values, Type targetType, object parameter,
                System.Globalization.CultureInfo culture)
            {
                return String.Format("{0} {1}", values[0], values[1]);
            }

            public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
            {
                string[] splitValues = ((string) value).Split(' ');
                return splitValues;
            }
        }

        public SocketWindow(out bool to_close)
        {
            to_close = false;
            InitializeComponent();
            while (Config == null)
            {
                while (Config == null)
                {
                    SocketConfig socketConfig = new SocketConfig(this);
                    bool? userClickedOK = socketConfig.ShowDialog();
                    if (userClickedOK == false)
                    {
                        to_close = true;
                        return;
                    }
                    else

                    if (Config == null)
                    {
                        System.Windows.MessageBox.Show("Invalid configuration. Please, change it.");
                    }

                }
            }
            if (!to_close)
            {
               // RunCommunication();
                Connect("localhost", 5100, out outputAdmin, out inputAdmin);

                // CfgSettings.WritePathToFile("path_to_config.txt", rawpath);
                //if (!_readCfg) System.Windows.MessageBox.Show("Invalid configuration file");
                //else
                {
                    ConfigList.DataContext = Config;
                    _agentDataDictionary = new AgentDataDictionary(Config);
                    _statePeriod = SpeedSlider.Value;
                }
                AuctionsLabel.Header = "{Special items}";
                LoadAndSetup(Config);
                SetBindings();
                SetUI();

                //RunRegistration();
                //RunSimulation();

                //AgentDataDictionary = new AgentDataDictionary(@"D:\#Recent_desktop\UNI\PES602\DMG\dammage 1.0\domains\english.pl");
                this.Title = "MultiAgentz Visualization - " + Config.HistPath;
            }
        }

        
      
        private void SetBindings()
        {
            this.AgentsList.DataContext = _logProcessor.CurrentState.Agents;
            this.AuctionsList.DataContext = _logProcessor.CurrentState.Auctions;
            this.CommonsList.DataContext = _logProcessor.CurrentState.CommonRights;
            this.AgentsPane.DataContext = _logProcessor.AllAgents;
            this.ItemsPane.DataContext = _logProcessor.AllItems;
            this.ClockList.DataContext = _logProcessor.CurrentState.Clock.TextList;
            this.ClockListNames.DataContext = _logProcessor.CurrentState.Clock.TextListNames;
            this.TradeChannel.DataContext = new List<String>();
            
            this.MainSlider.DataContext = _logProcessor;

            this.RunButton.Click += OnRun;
            //this.StepForward.Click += OnStepForward;
            //this.StepBackward.Click += OnStepBackward;
            //this.FirstStep.Click += OnFirst;
            //this.LastStep.Click += OnLast;
            //this.GoButton.Click += OnGo;
            //this.StopButton.Click += OnStop;
            

            this.IndexBlock.DataContext = _logProcessor;
            this.SpeedSlider.DataContext = this;
        }

        private void SetUI()
        {
            
            pausePanel.Orientation = Orientation.Horizontal;
            Polygon pause1 = new Polygon();
            pause1.Points = new PointCollection(new List<Point>() { new Point(0, 0), new Point(3, 0), new Point(3, 14), new Point(0, 14) });
            pause1.Fill = Brushes.Black;
            pause1.Stroke = Brushes.Black;
            Polygon pause2 = new Polygon();
            pause2.Points = new PointCollection(new List<Point>() { new Point(5, 0), new Point(8, 0), new Point(8, 14), new Point(5, 14) });
            pause2.Fill = Brushes.Black;
            pause2.Stroke = Brushes.Black;

            pausePanel.Children.Add(pause1);
            pausePanel.Children.Add(pause2);

            runIcon.Points = new PointCollection(new List<Point>() { new Point(0, 0), new Point(11, 7), new Point(0, 14) });
            runIcon.Fill = Brushes.Black;
            runIcon.Stroke = Brushes.Black;

        }

        private void OnStepBackward(object sender, RoutedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                _logProcessor.GetPreviousLine();
            });
        }

        private void LoadAndSetup(CfgSettings Config)
        {
            //NetworkReader nr = new NetworkReader();
            //nr.OnDataRevieved += NrOnOnDataRevieved;
            dsc = this.Dispatcher;

            if (_logProcessor.InitSocket(Config, _agentDataDictionary, dsc, _currentAgent))
            {
                try
                {

                    Item.cfgSettings = Config;
                    this.Title = "MultiAgentz Visualization - " + Config.Host + ":" + Config.Port;

                    StatusLabel.Foreground = new SolidColorBrush(Colors.Black);
                    StatusLabel.Content = _currentMessage;

                    
                    MainSlider.Maximum = 10;

                    LineNumberToCoordinateConverter.FieldCount = (int) MainSlider.Maximum;
                    LineNumberToCoordinateConverter.FieldDuration = 100;
                        
                    MainSlider.Value = 0;
                    int number = (int) MainSlider.Maximum;
                    dsc.Invoke(() =>
                    {
                        SliderMark10.Content = number;
                        {
                            SliderMark1.Content = (int) (number/10);
                            SliderMark2.Content = (int) (number*2/10);
                            SliderMark3.Content = (int) (number*3/10);
                            SliderMark4.Content = (int) (number*4/10);
                            SliderMark5.Content = (int) (number*5/10);
                            SliderMark6.Content = (int) (number*6/10);
                            SliderMark7.Content = (int) (number*7/10);
                            SliderMark8.Content = (int) (number*8/10);
                            SliderMark9.Content = (int) (number*9/10);
                        }

                    });


                    //  UpdateSliderMarks(dsc, (int)MainSlider.Maximum);
                    AuctionsLabel.Header = AgentDataDictionary.GetSpecialItemGroupName();

                    

                }
                catch
                {
                    System.Windows.MessageBox.Show("Something wrong with configuration file content");
                }
            }
            else
            {
                System.Windows.MessageBox.Show("Cannot coonnect to host: " + Config.Host + ":" + Config.Port);
            }
        
    }

        private void OnTimer(object sender, System.Timers.ElapsedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                _logProcessor.GetNextLine(ExecutionState.Running);
            });
        }




        #region Config

            public
            string ReadPathFromFile(string filename)
        {
            string result = string.Empty;
            if (File.Exists(filename))
            {
                String[] st = File.ReadAllLines(filename);
                if (st.Length>0) result = st[0];
            }
            return result;
        }



        #endregion

        #region ControlElements

        private void OnStepForward(object sender, RoutedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                _logProcessor.GetNextLine(ExecutionState.Running);
            });
        }

        private void OnRun(object sender, RoutedEventArgs e)
        {

            this.RunButton.Click -= OnRun;
            this.RunButton.Content = pausePanel;
            this.RunButton.Click += OnPause;

            execState = ExecutionState.Running;

            _runTimer.Elapsed += OnTimer;
            _runTimer.Start();

        }

        private void OnPause(object sender, RoutedEventArgs e)
        {
            this.RunButton.Click -= OnPause;
            execState = ExecutionState.Paused;
            this.RunButton.Content = runIcon;
            this.RunButton.Click += OnRun;
            _runTimer.Elapsed -= OnTimer;
            _runTimer.Stop();
        }


        private void OnFirst(object sender, RoutedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                String st;
                _logProcessor.GetFirstLine(out st);
            });
        }


        private void OnLast(object sender, RoutedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                String st;
                _logProcessor.GetLastLine(out st);
            });
        }

        private void OnRestart (object sender, RoutedEventArgs e)
        {
            this.RunButton.Click -= OnRestart;
            this.RunButton.Content = pausePanel;
            this.RunButton.Click += OnPause;
            execState = ExecutionState.Running;

            this.Dispatcher.Invoke(() =>
            {
                execState = ExecutionState.Running;
                String st;
                _logProcessor.GetFirstLine(out st);
            });
            
            _runTimer.Elapsed += OnTimer;
            _runTimer.Start();
            
           
        }
       
        private void OnStop(object sender, RoutedEventArgs e)
        {

            if (execState == ExecutionState.Running)
            {
                this.RunButton.Content = runIcon;
                this.RunButton.Click -= OnPause;
            }
            else if (execState == ExecutionState.Paused)
            {
                this.RunButton.Click -= OnRun;
            }
            this.RunButton.Click += OnRestart;

            if (execState == ExecutionState.Paused || execState == ExecutionState.Running)
            {
                if (execState == ExecutionState.Running)
                {_runTimer.Elapsed -= OnTimer;}

                execState = ExecutionState.Void;
                _runTimer.Stop();

                this.Dispatcher.Invoke(() =>
                {
                    String st;
                    _logProcessor.GetLastLine(out st);
                });
            }
        }

        private void OnConfig(object sender, RoutedEventArgs e)
        {
            Config.LoadFromFile();
            Window frm2 = new SocketConfig(this);
            frm2.Show();
        }

        private void OnGo(object sender, RoutedEventArgs e)
        {
            this.Dispatcher.Invoke(() =>
            {
                bool isChecked = StateRadio.IsChecked != null && (bool)StateRadio.IsChecked;
                if (!_logProcessor.GetStateOrTime(LineNumber.Text, isChecked))
                {
                    StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                    StatusLabel.Content = "Illegal step number!";
                    _statusTime = DateTime.Now;
                }

            });
        }

       
        private void Move(int Step)
        {
            this.Dispatcher.Invoke(() =>
            {
                if (!_logProcessor.GetSpecificLine(Step))
                {
                    StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                    StatusLabel.Content = "Error while moving to state "+Step +"!";
                    _statusTime = DateTime.Now;
                }
            });
        }

        private void OnExit(object sender, RoutedEventArgs e)
        {
            Close();
        }


        private void MainSlider_OnValueChangedSlider_ValueChanged(object sender,
            RoutedPropertyChangedEventArgs<double> e)
        {
            if (execState != ExecutionState.Stopped && (int)MainSlider.Value != _logProcessor.Index)
            {
                double d = MainSlider.Value;
                int tp = (int)d;
                if (tp < _logProcessor.GetAgentStartStep(_currentAgent))
                {
                    tp = _logProcessor.GetAgentStartStep(_currentAgent);
                    //MainSlider.Value = tp;
                }
                else if (tp > LogProcessor.GetAgentEndStep(_currentAgent))
                {
                    tp = LogProcessor.GetAgentEndStep(_currentAgent);
                    //MainSlider.Value = tp;
                }

                Task.Factory.StartNew(() =>
                {

                    lock (_lockerLogProcessorIndex)
                    {
                        if (_logProcessor.GetNumber < tp || tp < 0)
                        {
                            this.Dispatcher.Invoke(() =>
                            {
                                StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                                StatusLabel.Content = "Illegal step number!";
                                _statusTime = DateTime.Now;
                            });
                            return;
                        }
                        //ast = null;
                        isFirstLine = true;
                        _move_to = tp;
                        if (execState != ExecutionState.Moving)
                        {
                            _previousState = execState;
                            execState = ExecutionState.Moving;
                        }
                    }
                });
            }
        }

        private void Slider_ValueChanged(object sender, RoutedPropertyChangedEventArgs<double> e)
        {
            if (_runTimer != null){
                
                _runTimer.Interval = _timerPeriod;
            }
            _statePeriod = SpeedSlider.Value;
        }

        #endregion

        private void CheckBox1_Checked(object sender, RoutedEventArgs e)
        {
            _is_actual = true;
            _stopSleeping = true;
            SpeedSlider.IsEnabled = false;
        }

        private void CheckBox1_Unchecked(object sender, RoutedEventArgs e)
        {
            _is_actual = false;
            _stopSleeping = true;
            SpeedSlider.IsEnabled = true;
        }


        private void UIElement_OnMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            var item = (sender as Grid);
            Agent tp = (Agent)item.DataContext;
            if (Keyboard.Modifiers.ToString().Contains("Control"))
            {
                if (tp.ID == _currentAgent)
                {
                    this.Dispatcher.Invoke(() =>
                    {
                        StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                        StatusLabel.Content = "This is actually the current agent.";
                        _statusTime = DateTime.Now;
                    });
                }
                else if (_logProcessor.IsAgentLogAvailable(tp.ID))
                {
                    _currentAgent = tp.ID;
                    _logProcessor.CurrentAgent = _currentAgent;
                    _previousState = execState;
                    execState = ExecutionState.Switching;
                }
                else
                {
                    this.Dispatcher.Invoke(() =>
                    {
                        StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                        StatusLabel.Content = "Agent #" + tp.ID + " doesn't have the log file";
                        _statusTime = DateTime.Now;
                    });
                }
            }
            else
            {
                tp.IsExpanded = !tp.IsExpanded;
            }
        }


        private void KVPItem_Clicked(object sender, RoutedEventArgs e)
        {
            var item = sender as Grid;
            Item tp = (Item)item.DataContext;
            Window frm3 = new ItemWindow(tp, _logProcessor.Index-1, _logProcessor.GetTimeStampByState(_logProcessor.Index-1));
            frm3.Show();
            //MessageBox.Show("suspicious "+ tp.InstanceOf);
        }

        private void CommonRightsItem_Clicked(object sender, RoutedEventArgs e)
        {
            var item = sender as System.Windows.Controls.Button;
            var st = item.DataContext.GetType();
            if (st.Name.Equals("Item"))
            {
                Item tp = (Item) item.DataContext;
                Window frm3 = new ItemWindow(tp, _logProcessor.Index - 1, _logProcessor.GetTimeStampByState(_logProcessor.Index - 1));
                frm3.Show();
            }
            else if (st.Name.Equals("Agent"))
            {
                Agent tp = (Agent) item.DataContext;
                tp.IsPaneExpanded = !tp.IsPaneExpanded;
            }
        
        }

        private void MainSlider_DragLeave(object sender, System.Windows.DragEventArgs e)
        {
           
        }

        private void LineNumber_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (PlaceMarker == null) return;
            string text = LineNumber.Text;
            int tp;
            bool t = Int32.TryParse(text, out tp);

            bool isChecked = StateRadio.IsChecked != null && (bool) StateRadio.IsChecked;

            bool notTooBig = isChecked ? tp < _logProcessor.GetNumber : tp < _logProcessor.GetTimeByState(_logProcessor.GetNumber-1);
            if (!LineNumber.Text.Equals("0") && !LineNumber.Text.Equals("") && (tp >= 0 && notTooBig))
            {
                 {
                    this.Dispatcher.Invoke(() =>
                    {
                        PlaceMarker.Visibility = Visibility.Visible;
                    });
                }
            }
            else
            {
                this.Dispatcher.Invoke(() =>
                {
                    PlaceMarker.Visibility = Visibility.Collapsed;
                });
            }
        }

        private void AgentFirstStepButton_onClick(object sender, RoutedEventArgs e)
        {
            var item = sender as System.Windows.Controls.Button;
            var st = item.DataContext.GetType();
            if (st.Name.Equals("Agent"))
            {
                Agent tp = (Agent)item.DataContext;
                Move(tp.FirstStep);
            }
        }

        private void AgentLastStepButton_onClick(object sender, RoutedEventArgs e)
        {
            var item = sender as System.Windows.Controls.Button;
            var st = item.DataContext.GetType();
            if (st.Name.Equals("Agent"))
            {
                Agent tp = (Agent)item.DataContext;
                Move(tp.LastStep);
            }
        }

        public void Reload(CfgSettings cfg)
        {
            Config = cfg;
            _agentDataDictionary = new AgentDataDictionary(cfg);
            LoadAndSetup(cfg);
        }

        

        private void OnAgent(object sender, RoutedEventArgs e)
        {
            StreamWriter sst;
            StreamReader ssi;
            if (Connect("localhost", 7100, out sst, out ssi))
            {
                
                SimulationConsoleWindow console = new SimulationConsoleWindow(sst, this);
                console.Title = "Agent Console";
                console.Show();
            }
            else
            {
                System.Windows.Forms.MessageBox.Show("Cannot connect to the simulation! Check if simulation and proxy are running.");
            }
        }

        private void OnMainLoad(object sender, RoutedEventArgs e)
        {
            {       

            }
        }



        private void StateRadio_Checked(object sender, RoutedEventArgs e)
        {
            PlaceMarker.Fill = new SolidColorBrush(Colors.BlueViolet);
            LineNumber.Text = "0";
        }

        private void StateRadio_Unchecked(object sender, RoutedEventArgs e)
        {
            PlaceMarker.Fill = new SolidColorBrush(Colors.Red);
            LineNumber.Text = "0";
        }

        private void UIElement_OnMouseDown(object sender, MouseButtonEventArgs e)
        {
            var item = (sender as System.Windows.Controls.Grid);
            Item tp = (Item)item.DataContext;
            tp.IsExpanded = !tp.IsExpanded;
        }


        private void ButtonBase_OnClick(object sender, RoutedEventArgs e)
        {
            var item = (sender as System.Windows.Controls.Button);
            Item tp = (Item)item.DataContext;
            tp.IsPaneExpanded = !tp.IsPaneExpanded;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChangedEventHandler handler = PropertyChanged;
            if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));

        }
        private void OnAdmin(object sender, RoutedEventArgs e)
        {

            if (outputAdmin!=null)
            {
                //FIX IT FIT IT VIK new AdminWindow == this
                
                SimulationConsoleWindow console = new SimulationConsoleWindow(outputAdmin, this);
                console.Title = "Agent Console";
                console.Show();
            }
            else
            {
                System.Windows.Forms.MessageBox.Show("Cannot connect to the simulation! Check if simulation and proxy are running.");
            }
        }

        public void ShutdownAdmin()
        {
            AdminButton.IsEnabled = false;
            
        }

        private void RunCommunication()
        {
            String ss = System.IO.Directory.GetCurrentDirectory();
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
            startInfo.Arguments = @"-b control/communication -e communication(5100,5200,5300)";
            startInfo.WindowStyle = ProcessWindowStyle.Normal;

            Process processTemp = new Process();
            processTemp.StartInfo = startInfo;
            processTemp.EnableRaisingEvents = true;
            processTemp.Start();

        }


        private void RunRegistration()
        {

            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
            startInfo.Arguments = @"-b reason/simulation -e simulation(localhost,5400)";
            startInfo.WindowStyle = ProcessWindowStyle.Normal;

            Process processTemp = new Process();
            processTemp.StartInfo = startInfo;
            processTemp.EnableRaisingEvents = true;
            processTemp.Start();
        
        }

        private void RunSimulation()
        {

            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
            startInfo.Arguments = @"-b control/registration -e registration(localhost,5300,_,6000)";
            startInfo.WindowStyle = ProcessWindowStyle.Normal;

            Process processTemp = new Process();
            processTemp.StartInfo = startInfo;
            processTemp.EnableRaisingEvents = true;
            processTemp.Start();

        }
        private void NotifyPropertyChanged([CallerMemberName] String propertyName = "")
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }
    }
}