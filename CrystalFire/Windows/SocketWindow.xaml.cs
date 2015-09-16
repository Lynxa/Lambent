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
//using AgentsConfig;
using CrystalFire.Annotations;
using Microsoft.Win32;

using Orientation = System.Windows.Controls.Orientation;

namespace CrystalFire
{
    /// <summary>
    /// Interaction logic for ReplayWindow.xaml
    /// </summary>
    public partial class SocketWindow : INotifyPropertyChanged
    {
        
    //    /*UI elements*/
        /*UI elements*/
        public static System.Timers.Timer _runTimer = new System.Timers.Timer(1500);


        

        public CfgSettings Config = new CfgSettings();

        private StateEngine _logProcessor = new StateEngine();

        private String _currentMessage;
        
        //Redo this per runmode

        ExecutionState execState = ExecutionState.Void;
        private RunMode _runMode = RunMode.Log;
        private bool _is_actual;
        private bool _stopSleeping;

        public SocketWindow(RunMode runMode, out bool to_close)
        {
            to_close = false;
            InitializeComponent();
            Config.LoadFromFile();

            if (!to_close)
            {
                _runMode = runMode;
                LoadAndSetup(Config);
                SetBindings();
                SetUI();

                if (runMode == RunMode.Full)
                {
                    this.Title = "MultiAgentz Visualization - " + Config.ObservationHost + ":" + Config.ObservationPort;
                }
                if (runMode == RunMode.Log)
                {
                    this.Title = "MultiAgentz Visualization - " + Config.HistPath;
                }
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
            this.ConfigList.DataContext = Config;
            this.MainSlider.DataContext = _logProcessor;
            this.SpeedSlider.DataContext = _logProcessor;
            this.RunButton.DataContext = _logProcessor;

            this.AgentButton.Click += _logProcessor.OnAgent;
            this.AdminButton.Click += _logProcessor.OnAdmin;
            this.ConfigButton.Click += OnConfig;
            this.ExitButton.Click += OnExit;
            //TODO: Shutdown event handler

            this.RunButton.Click += _logProcessor.OnRun;
            this.StepForward.Click += _logProcessor.OnStepForward;
            this.StepBackward.Click += _logProcessor.OnStepBackward;
            this.FirstStep.Click += _logProcessor.OnFirst;
            this.LastStep.Click += _logProcessor.OnLast;
            //this.GoButton.Click += OnGo;
            //this.LineNumber.TextChanged += LineNumber_TextChanged;
            this.StopButton.Click += OnStop;
            
            this.SpeedSliderCheckBox.Checked += SpeedSliderCheckBox_Checked;
            this.SpeedSliderCheckBox.Checked += SpeedSliderCheckBox_Unchecked;
            
            this.IndexBlock.DataContext = _logProcessor;
            
        }

        private void SetUI()
        {
            if (_runMode == RunMode.Full || _runMode == RunMode.Agent)
            {
                SpeedSlider.Visibility = Visibility.Hidden;
                SpeedSliderTitle.Visibility = Visibility.Hidden;
                SpeedSliderMark1.Visibility = Visibility.Hidden;
                SpeedSliderMark2.Visibility = Visibility.Hidden;
                SpeedSliderMark3.Visibility = Visibility.Hidden;
                SpeedSliderMark4.Visibility = Visibility.Hidden;
                SpeedSliderMark5.Visibility = Visibility.Hidden;
                SpeedSliderMark6.Visibility = Visibility.Hidden;
                SpeedSliderCheckBox.Visibility = Visibility.Hidden;

            }
        
            //   
            AuctionsLabel.Header = "{Special items}";
            

           StatusLabel.Foreground = new SolidColorBrush(Colors.Black);

           //organize this per runmode
    
            LineNumberToCoordinateConverter.FieldCount = (int)MainSlider.Maximum;
    //        LineNumberToCoordinateConverter.FieldDuration = 100;

#region SliderMarks
            
            Binding myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "1";
            BindingOperations.SetBinding(SliderMark1, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "2";
            BindingOperations.SetBinding(SliderMark2, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "3";
            BindingOperations.SetBinding(SliderMark3, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "4";
            BindingOperations.SetBinding(SliderMark4, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "5";
            BindingOperations.SetBinding(SliderMark5, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "6";
            BindingOperations.SetBinding(SliderMark6, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "7";
            BindingOperations.SetBinding(SliderMark7, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "8";
            BindingOperations.SetBinding(SliderMark8, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "9";
            BindingOperations.SetBinding(SliderMark9, ContentProperty, myBinding);

            myBinding = new Binding();
            myBinding.Source = _logProcessor;
            myBinding.Path = new PropertyPath("Count");
            myBinding.Converter = new LineNumberToSliderConvertor();
            myBinding.UpdateSourceTrigger = UpdateSourceTrigger.PropertyChanged;
            myBinding.ConverterParameter = "10";
            BindingOperations.SetBinding(SliderMark10, ContentProperty, myBinding);

#endregion SliderMarks
            

            AuctionsLabel.Header = AgentDataDictionary.GetSpecialItemGroupName();
            
            //Redo this into binding / public property
            StatusLabel.Content = _currentMessage;

        }


        private void LoadAndSetup(CfgSettings Config)
        {
            var dsc = this.Dispatcher;
            _logProcessor.InitProcessor(Config, dsc, _runMode);
            
            try
            {
                Item.cfgSettings = Config;

            }
            catch
            {
                System.Windows.MessageBox.Show("Something wrong with configuration file content");
            }

        }



        private void OnRestart (object sender, RoutedEventArgs e)
        {
            this.RunButton.Click -= OnRestart;
            execState = ExecutionState.Running;

            this.Dispatcher.Invoke(() =>
            {
                execState = ExecutionState.Running;
                String st;

                //TODO:reimplement
                //_logProcessor.GetFirstLine(out st);
            });
            
           // _runTimer.Elapsed += OnTimer;
            _runTimer.Start();
        }

        private void OnStop(object sender, RoutedEventArgs e)
        {

            this.RunButton.Click += OnRestart;

            if (execState == ExecutionState.Paused || execState == ExecutionState.Running)
            {
                if (execState == ExecutionState.Running)
             //   { _runTimer.Elapsed -= OnTimer; }

                execState = ExecutionState.Void;
                _runTimer.Stop();

                this.Dispatcher.Invoke(() =>
                {
                    String st;

                    //TODO:reimplement
                    //_logProcessor.GetLastLine(out st);
                });
            }
        }

        private void OnConfig(object sender, RoutedEventArgs e)
        {
            Config.LoadFromFile();
            Window frm2 = new SocketConfig(this);
            frm2.ShowDialog();
        }


        private void OnExit(object sender, RoutedEventArgs e)
        {

            if (_logProcessor != null) _logProcessor.OnExit();
            Close();
        }


    //    #endregion

        private void SpeedSliderCheckBox_Checked(object sender, RoutedEventArgs e)
        {
            _is_actual = true;
            _stopSleeping = true;
            SpeedSlider.IsEnabled = false;
        }

        private void SpeedSliderCheckBox_Unchecked(object sender, RoutedEventArgs e)
        {
            _is_actual = false;
            _stopSleeping = true;
            SpeedSlider.IsEnabled = true;
        }

        //Clicked on agent header
        private void UIElement_OnMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            var item = (sender as Grid);
            Agent tp = (Agent)item.DataContext;
            if (Keyboard.Modifiers.ToString().Contains("Control"))
            {
                if (tp.ID == _logProcessor.CurrentAgent)
                {
                    this.Dispatcher.Invoke(() =>
                    {
                        StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                        StatusLabel.Content = "This is actually the current agent.";
                       // _statusTime = DateTime.Now;
                    });
                }
                //else if (_logProcessor.IsAgentLogAvailable(tp.ID))
                //{
                //    //_currentAgent = tp.ID;
                //    //_logProcessor.CurrentAgent = _currentAgent;
                //    //_previousState = execState;
                //    execState = ExecutionState.Switching;
                //}
                else
                {
                    //TODO: rearrange
                    this.Dispatcher.Invoke(() =>
                    {
                        StatusLabel.Foreground = new SolidColorBrush(Colors.Red);
                        StatusLabel.Content = "Agent #" + tp.ID + " doesn't have the log file";
                        //_statusTime = DateTime.Now;
                    });
                }
            }
            else
            {
                tp.IsExpanded = !tp.IsExpanded;
            }
        }


        private void CommonRightsItem_Clicked(object sender, RoutedEventArgs e)
        {
            var item = sender as System.Windows.Controls.Button;
            var st = item.DataContext.GetType();
            if (st.Name.Equals("Item"))
            {
                Item tp = (Item)item.DataContext;
                
                //TODO:do smth normal
                Window frm3 = new ItemWindow(tp, _logProcessor.Index - 1, _logProcessor.CurrentState.Clock.HappenedAt);
                frm3.Show();
            }
            else if (st.Name.Equals("Agent"))
            {
                Agent tp = (Agent)item.DataContext;
                tp.IsPaneExpanded = !tp.IsPaneExpanded;
            }

        }


        //private void LineNumber_TextChanged(object sender, TextChangedEventArgs e)
        //{
        //    if (PlaceMarker == null) return;
        //    string text = LineNumber.Text;
        //    int tp;
        //    bool t = Int32.TryParse(text, out tp);

        //    bool isChecked = StateRadio.IsChecked != null && (bool)StateRadio.IsChecked;

        //    bool notTooBig = isChecked ? tp < _logProcessor.GetNumber : tp < _logProcessor.GetTimeByState(_logProcessor.GetNumber - 1);
        //    if (!LineNumber.Text.Equals("0") && !LineNumber.Text.Equals("") && (tp >= 0 && notTooBig))
        //    {
        //        {
        //            this.Dispatcher.Invoke(() =>
        //            {
        //                PlaceMarker.Visibility = Visibility.Visible;
        //            });
        //        }
        //    }
        //    else
        //    {
        //        this.Dispatcher.Invoke(() =>
        //        {
        //            PlaceMarker.Visibility = Visibility.Collapsed;
        //        });
        //    }
        //}

    //    private void AgentFirstStepButton_onClick(object sender, RoutedEventArgs e)
    //    {
    //        var item = sender as System.Windows.Controls.Button;
    //        var st = item.DataContext.GetType();
    //        if (st.Name.Equals("Agent"))
    //        {
    //            Agent tp = (Agent)item.DataContext;
    //            Move(tp.FirstStep);
    //        }
    //    }

    //    private void AgentLastStepButton_onClick(object sender, RoutedEventArgs e)
    //    {
    //        var item = sender as System.Windows.Controls.Button;
    //        var st = item.DataContext.GetType();
    //        if (st.Name.Equals("Agent"))
    //        {
    //            Agent tp = (Agent)item.DataContext;
    //            Move(tp.LastStep);
    //        }
    //    }

    //    public void Reload(CfgSettings cfg)
    //    {
    //        Config = cfg;
    //        _agentDataDictionary = new AgentDataDictionary(cfg);
    //        LoadAndSetup(cfg);
    //    }





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

        //clicking on the item in agent's pane
        private void ButtonBase_OnClick(object sender, RoutedEventArgs e)
        {
            var item = (sender as System.Windows.Controls.Button);
            Item tp = (Item)item.DataContext;
            tp.IsPaneExpanded = !tp.IsPaneExpanded;
        }




    //    private void RunCommunication()
    //    {
    //        String ss = System.IO.Directory.GetCurrentDirectory();
    //        ProcessStartInfo startInfo = new ProcessStartInfo();
    //        startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
    //        startInfo.Arguments = @"-b control/communication -e communication(5100,5200,5300)";
    //        startInfo.WindowStyle = ProcessWindowStyle.Normal;

    //        Process processTemp = new Process();
    //        processTemp.StartInfo = startInfo;
    //        processTemp.EnableRaisingEvents = true;
    //        processTemp.Start();

    //    }


    //    private void RunRegistration()
    //    {

    //        ProcessStartInfo startInfo = new ProcessStartInfo();
    //        startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
    //        startInfo.Arguments = @"-b reason/simulation -e simulation(localhost,5400)";
    //        startInfo.WindowStyle = ProcessWindowStyle.Normal;

    //        Process processTemp = new Process();
    //        processTemp.StartInfo = startInfo;
    //        processTemp.EnableRaisingEvents = true;
    //        processTemp.Start();
        
    //    }

    //    private void RunSimulation()
    //    {

    //        ProcessStartInfo startInfo = new ProcessStartInfo();
    //        startInfo.FileName = @".\dammage 1.1\utils\eclipse.exe";
    //        startInfo.Arguments = @"-b control/registration -e registration(localhost,5300,_,6000)";
    //        startInfo.WindowStyle = ProcessWindowStyle.Normal;

    //        Process processTemp = new Process();
    //        processTemp.StartInfo = startInfo;
    //        processTemp.EnableRaisingEvents = true;
    //        processTemp.Start();

    //    }



        private void AgentFirstStepButton_onClick(object sender, RoutedEventArgs e)
        {
            throw new NotImplementedException();
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChangedEventHandler handler = PropertyChanged;
            if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));
        }

        private void AgentLastStepButton_onClick(object sender, RoutedEventArgs e)
        {
            throw new NotImplementedException();
        }


    }
}