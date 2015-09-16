using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net.Sockets;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media.TextFormatting;
using System.Windows.Shapes;
using System.Windows.Threading;
using CrystalFire.Annotations;

namespace CrystalFire
{
    internal class StateEngine : INotifyPropertyChanged
    {
        private AgentDataDictionary _agentDataDictionary;
        private Dispatcher _dispatcher;
        private StreamWriter myfile; 

        private StateDictionary states;
        private int _index;
        private EnvironmentState _currentState;
        private String _currentAgent;

        private OperationMode _operationMode;

        private CfgSettings cfgInfo;

        private ObservableCollection<Agent> _allAgents;
        private ObservableCollection<Item> _allItems;

        private NetworkReader nr;

        public static System.Timers.Timer _runTimer = new System.Timers.Timer(1500);

        private int _timerPeriod = 1500;

        private Process comm, registration, simulation;
        private StreamReader inputAdmin;
        private StreamWriter outputAdmin;

        private RunMode _runMode;
        //redo


        ExecutionState _execState = ExecutionState.Void;

        #region PublicProperties

        public ExecutionState ExecutionState
        {
            get { return _execState; }

            set
            {
                _execState = value;
                OnPropertyChanged();
            }


        }

        public int Index
        {
            get { return _index; }
            set
            {
                _index = value;
                OnPropertyChanged();
            }
        }

        public EnvironmentState CurrentState
        {
            get { return _currentState; }
            set
            {
                _currentState = value;
                OnPropertyChanged();
            }
        }

        public String CurrentAgent
        {
            get { return _currentAgent; }
            set
            {
                _currentAgent = value;
                OnPropertyChanged();
            }
        }

        public ObservableCollection<Agent> AllAgents
        {
            get { return _allAgents; }
            set
            {
                _allAgents = value;
                OnPropertyChanged();
            }
        }

        public ObservableCollection<Item> AllItems
        {
            get { return _allItems; }
            set
            {
                _allItems = value;
                OnPropertyChanged();
            }
        }

        public OperationMode OperationMode
        {
            get { return _operationMode; }
            set { _operationMode = value; }
        }


        public double RunTimerPeriod
        {
            get { return (_timerPeriod); }
            set
            {
                _timerPeriod = (int) value;
                _runTimer.Stop();
                _runTimer.Interval = _timerPeriod;
                _runTimer.Start();
            }
        }

        public int Count
        {
            get { return states == null ? 0 : states.Count; }
        }

        #endregion

        public void InitProcessor(CfgSettings config, Dispatcher dispatcher, RunMode runMode)
        {
            _agentDataDictionary = new AgentDataDictionary(config);
            _dispatcher = dispatcher;
            AllAgents = new ObservableCollection<Agent>();
            AllItems = new ObservableCollection<Item>();
            CurrentState = new EnvironmentState();
            CurrentState.Clock = new Clock();
            states = new StateDictionary();
            states.PropertyChanged += states_PropertyChanged;
            cfgInfo = config;
            CurrentAgent = "god";
            Index = 0;
            _runMode = runMode;
            
        }

        private void states_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            OnPropertyChanged("Count");
        }

        public bool Start()
        {
            try
            {
                
                _execState = ExecutionState.Running;
                if (_runMode == RunMode.Full)
                {
                 
                    using (comm = new Process())
                    {
                        comm.StartInfo = new ProcessStartInfo("1. communication.cmd");
                        comm.Start();
                        //comm.WaitForExit();
                    }
                
                    Connect(cfgInfo.AdminHost, cfgInfo.AdminPort, out outputAdmin, out inputAdmin);
                    nr = new NetworkReader();
                    nr.Connect(cfgInfo.ObservationHost, cfgInfo.ObservationPort);
                    nr.OnDataRevieved += NrOnOnDataRevieved;

                    OperationMode = OperationMode.Online;
                    myfile = new StreamWriter(@"D:\#Recent_desktop\UNI\PES602\DMG\dammage 1.1\myhist.db");
                
                    using (registration = new Process())
                    {
                        registration.StartInfo = new ProcessStartInfo("4. registration.cmd");
                        registration.Start();
                        // comm.WaitForExit();
                    }

                    using (simulation = new Process())
                    {
                        simulation.StartInfo = new ProcessStartInfo("5. simulation.cmd");
                        simulation.Start();
                        //comm.WaitForExit();
                    }

                }
                
                else if (_runMode == RunMode.Log)
                {

                    myfile = new StreamWriter(@"D:\#Recent_desktop\UNI\PES602\DMG\dammage 1.1\myhist2.db");
                    OperationMode = OperationMode.Offline;
                    var _log = System.IO.File.ReadAllLines(cfgInfo.HistPath).ToList<String>();
                    _log = RemoveEmpty(_log);
                    foreach (var line in _log)
                    {
                        var tLine = MapLine(line);
                        if (tLine!=null) states.Add(tLine.POV_agent, tLine);
                    }

                    _runTimer.Elapsed += OnTimer;
                    _runTimer.Start();

                    return true;

                }
                return false;
            }
            catch (Exception e1)
            {
                MessageBox.Show("Problem starting the simulation: " + e1.Message);
                return false;
            }
        }

        public void Stop()
        {
            myfile.Close();
            if (nr != null) nr.OnDataRevieved -= NrOnOnDataRevieved;
        }

        private void OnTimer(object sender, System.Timers.ElapsedEventArgs e)
        {
            _dispatcher.Invoke(NextState);
        }


        public void OnRun(object sender, RoutedEventArgs e)
        {
            if (ExecutionState == ExecutionState.Void)
            {
                Start();
            }

            if (ExecutionState == ExecutionState.Void || ExecutionState == ExecutionState.Paused)
            {
                if (OperationMode == OperationMode.Offline)
                {
                    _runTimer.Start();
                }
                ExecutionState = ExecutionState.Running;
            }

            else if (ExecutionState == ExecutionState.Running)
            {
                ExecutionState = ExecutionState.Paused;
                if (OperationMode == OperationMode.Offline)
                {
                    _runTimer.Stop();
                }
            }

        }

        public void OnStepForward(object sender, RoutedEventArgs e)
        {
            _dispatcher.Invoke(NextState);
        }

        public void OnStepBackward(object sender, RoutedEventArgs e)
        {
            _dispatcher.Invoke(() =>
            {
                    GetSpecificLine(Index - 1);
            });
        }


        public void OnFirst(object sender, RoutedEventArgs e)
        {
            _dispatcher.Invoke(() =>
            {
                int startStep = states.GetStartStep(CurrentAgent);
                if (Index != startStep)
                {
                    GetSpecificLine(startStep);
                    OperationMode = OperationMode.Offline;
                }
            });
        }

        public void OnLast(object sender, RoutedEventArgs e)
        {
            _dispatcher.Invoke(() =>
            {
                int endStep = states.GetEndStep(CurrentAgent);
                GetSpecificLine(endStep);
                if (_runMode == RunMode.Full || _runMode == RunMode.Agent)
                {
                    OperationMode = OperationMode.Online;
                }
            });
        }

        public bool GetSpecificLine(int number)
        {
            EnvironmentState sTate = new EnvironmentState();
            var sTate1 = states[_currentAgent, number - 1];
            if (sTate1 != null)
            {
                sTate1 = sTate1.GetFullCopy();
                var state2 = states[_currentAgent, number];
                if (state2 != null)
                {
                    state2 = state2.GetFullCopy();
                    StateObjectMapper.UpdateState(sTate, CurrentState, _agentDataDictionary, _dispatcher);
                    StateObjectMapper.UpdateState(sTate1, CurrentState, _agentDataDictionary, _dispatcher);
                    StateObjectMapper.UpdateState(state2, CurrentState, _agentDataDictionary, _dispatcher);
                    Index = number;
                }
            }
            else
            {
                var state2 = states[_currentAgent, number];
                if (state2 != null)
                {
                    state2 = state2.GetFullCopy();
                    StateObjectMapper.UpdateState(sTate, CurrentState, _agentDataDictionary, _dispatcher);
                    StateObjectMapper.UpdateState(state2, CurrentState, _agentDataDictionary, _dispatcher);
                    Index = number;
                }
                else return false;
            }

            return true;

        }

        internal void OnExit()
        {
            Stop();
            if (_runMode == RunMode.Full && outputAdmin != null)
            {
                outputAdmin.Write("shutdown. ");
                outputAdmin.Flush();
            }

        }

        internal void OnAgent(object sender, RoutedEventArgs e)
        {
            StreamWriter sst;
            StreamReader ssi;
            if (Connect("localhost", 7100, out sst, out ssi))
            {

                SimulationConsoleWindow console = new SimulationConsoleWindow(sst);
                console.Title = "Agent Console";
                console.Show();
            }
            else
            {
                System.Windows.MessageBox.Show("Cannot connect to the simulation! Check if simulation and proxy are running.");
            }
        }

        internal void OnAdmin(object sender, RoutedEventArgs e)
        {

            if (outputAdmin != null)
            {
                SimulationConsoleWindow console = new SimulationConsoleWindow(outputAdmin);
                //TODO: event wiring up
                console.Title = "Agent Console";
                console.Show();
            }
            else
            {
                MessageBox.Show("Cannot connect to the simulation! Check if simulation and proxy are running.");
            }
        }

        //public void GetFirstLine(out String errorMessage)
        //{
        //    GetSpecificLine(CurrentFirstStep);
        //    errorMessage = "";
        //}

        //public void GetLastLine(out String errorMessage)
        //{
        //    GetSpecificLine(CurrentLastStep);
        //    errorMessage = "";
        //}

        private void NrOnOnDataRevieved(string message)
        {
            myfile.WriteLine(message);
            myfile.Flush();

            var tState = MapLine(message);

            if (tState != null)
            {
                states.Add(tState.POV_agent, tState);

                if (tState.POV_agent.Equals(CurrentAgent) && OperationMode == OperationMode.Online)
                {
                    NextState();
                }
                //update All agents, 
                //update all items
                //update agents steps
            }
        
    
        }

        private EnvironmentState MapLine(String message)
        {
            String line = message.Substring(9);    //"msg(view(";
            String t_agent = line.Substring(0, line.IndexOf(','));
            line = line.Substring(line.IndexOf(',') + 1);
            line = line.Substring(0, line.LastIndexOf(',') - 1); //')';
            line += '.';

            KVP tResKvp = null;
            tResKvp = KVPGrinder.DecipherLine(line);
            EnvironmentState tst = null;

            if (tResKvp != null)
            {
            
                try
                {
                    tst = StateObjectMapper.MapState(tResKvp, _agentDataDictionary, _dispatcher);
                    tst.POV_agent = t_agent;
                }
                catch (Exception)
                {
                    //can write something somewhere in running log, but I seriously doubt this program will ever be that sophisticated.
                }
            }
            return tst;
        }

        public void NextState()
        {
            var st1 = states[CurrentAgent, Index + 1];
            if (st1 != null)
            {
                EnvironmentState sTate = st1.GetFullCopy();
                StateObjectMapper.UpdateState(sTate, CurrentState, _agentDataDictionary, _dispatcher);
                Index++;
            }
            else
            {
                if (_runMode == RunMode.Full || _runMode == RunMode.Full)
                {
                    OperationMode = OperationMode.Online;
                }
                else if (_runMode == RunMode.Log)
                {
                    //reload
                }
            }


        }

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

        protected static List<String> RemoveEmpty(List<String> lst)
        {
            List<String> result = new List<string>();
            foreach (var st in lst)
            {
                if (!st.Equals("")) result.Add(st);
            }
            return result;
        }

        public event PropertyChangedEventHandler PropertyChanged;

        [NotifyPropertyChangedInvocator]
        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChangedEventHandler handler = PropertyChanged;
            if (handler != null) handler(this, new PropertyChangedEventArgs(propertyName));
        }



    }
}
