using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using CrystalFire.Annotations;

namespace CrystalFire
{
    class StateDictionary : INotifyPropertyChanged
    {
        
            //          timestamp           agent           state
        Dictionary<String, Dictionary<String, EnvironmentState>> dict = new Dictionary<string, Dictionary<string, EnvironmentState>>();

            //     number   time
        Dictionary<int, String> _stateToTime = new Dictionary<int, string>();
        Dictionary<int, double> _stateToTimeSecsDictionary = new Dictionary<int, double>();
        Dictionary<string, int> _startSteps = new Dictionary<string, int>();
        Dictionary<string, int> _endSteps = new Dictionary<string, int>();

        public int Count
        {
            get { return dict.Count; }

        }
        
        private int stateCounter = 0;

        public void Add(String agentId, EnvironmentState state)
        {
            String timestamp = state.Clock.HappenedAt;
            if (dict.ContainsKey(timestamp) && dict[timestamp].ContainsKey(agentId))
            {
                state.Clock.HappenedAt += '0';
                timestamp = state.Clock.HappenedAt;
                var tDict = new Dictionary<string, EnvironmentState>();
                tDict.Add(agentId, state);
                dict.Add(timestamp,  tDict);
                _stateToTime.Add(stateCounter, timestamp);
                _stateToTimeSecsDictionary.Add(stateCounter, (int)Double.Parse(state.Clock.TimeStampH.Replace(".", ",")));
                stateCounter++;
                OnPropertyChanged();
            }
            else if (dict.ContainsKey(timestamp) && !dict[timestamp].ContainsKey(agentId))
            {
                dict[timestamp].Add(agentId, state);
            }
            else if (!dict.ContainsKey(timestamp))
            {
                timestamp = state.Clock.HappenedAt;
                var tDict = new Dictionary<string, EnvironmentState>();
                tDict.Add(agentId, state);
                dict.Add(timestamp, tDict);
                _stateToTime.Add(stateCounter, timestamp);
                _stateToTimeSecsDictionary.Add(stateCounter, (int)Double.Parse(state.Clock.TimeStampH.Replace(".", ",")));
                stateCounter++;
                OnPropertyChanged();
            }
            foreach (var agent in state.Agents)
            {
                if (!_startSteps.ContainsKey(agent.ID))
                {
                    _startSteps.Add(agent.ID, stateCounter-1);
                }
            }
            foreach (var agent in _startSteps.Keys)
            {
                if (!StateObjectMapper.ContainsAgent(state.Agents, agent) && !_endSteps.ContainsKey(agent))
                {
                    _endSteps.Add(agent, stateCounter-1);
                }
            }
        }

        public int GetStartStep(String agentId)
        {
            if (_startSteps.ContainsKey(agentId)) return _startSteps[agentId];
            return -1;
        }

        public int GetEndStep(String agentId)
        {
            if (_endSteps.ContainsKey(agentId)) return _endSteps[agentId];
            return dict.Count-1;
        }

        public EnvironmentState this[String agent, int i]
        {
            get
            {
                if (!_stateToTime.ContainsKey(i)) return null;
                String time = _stateToTime[i];
                EnvironmentState hState;
                if (dict[time].TryGetValue(agent, out hState)) return hState;
                return null;
            }
        }

        public EnvironmentState this[String agent, String st]
        {
            get
            {
                EnvironmentState hState;
                if (dict[st].TryGetValue(agent, out hState)) return hState;
                return null;
            }
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
