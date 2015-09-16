using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Win32;

namespace CrystalFire
{
    internal class EnvironmentState : INotifyPropertyChanged
    {
        public Clock Clock;
        public SystemEvent Event = null;
        public ObservableCollection <Agent> Agents = new ObservableCollection<Agent>();
        public ObservableCollection<Item> Auctions = new ObservableCollection<Item>();
        public ObservableCollection<Item> CommonRights = new ObservableCollection<Item>();
        public String POV_agent = "";
       

        public EnvironmentState GetFullCopy()
        {
            var result = new EnvironmentState();
            result.Clock = Clock.GetFullCopy();

            result.Event = Event == null ? null:Event.GetFullCopy();

            ObservableCollection<Agent> ti = new ObservableCollection<Agent>();
            foreach (var agent in Agents)
            {
                ti.Add(agent.GetFullCopy());
            }
            result.Agents = ti;

            ObservableCollection<Item> tii = new ObservableCollection<Item>();

            foreach (var allItem in Auctions)
            {
                tii.Add(allItem.GetFullCopy());
            }
            result.Auctions = tii;

            tii = new ObservableCollection<Item>();
            foreach (var allItem in CommonRights)
            {
                tii.Add(allItem.GetFullCopy());
            }
            result.CommonRights = tii;

            return result;
        }


        public event PropertyChangedEventHandler PropertyChanged;

        private void NotifyPropertyChanged([CallerMemberName] String propertyName = "")
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }
    }

    
}
