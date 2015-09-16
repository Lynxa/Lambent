using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;


namespace CrystalFire
{
    public class CfgSettings : INotifyPropertyChanged
    {
        private String _damagePath;
        private String _histPath;
        private String _observationHost;
        private int _observationPort;
        private String _adminHost;
        private int _adminPort;


        private const String ConfigPath = "config.xml";


        public int AdminPort
        {
            get { return _adminPort; }
            set { _adminPort = value; }
        }

        public string AdminHost
        {
            get { return _adminHost; }
            set { _adminHost = value; }
        }

        public String ObservationHost
        {
            get { return _observationHost; }
            set
            {
                _observationHost = value;
                NotifyPropertyChanged();
            }
        }

        public int ObservationPort
        {
            get { return _observationPort; }
            set
            {
                _observationPort = value;
                NotifyPropertyChanged();
            }
        }

        public String DammagePath {
            get { return _damagePath; }
            set
            {
                _damagePath = value;
                NotifyPropertyChanged();
            }
        }

        public String HistPath
        {
            get { return _histPath; }
            set
            {
                _histPath = value;
                NotifyPropertyChanged();
            }
        }
    
        
        public CfgSettings()
        {
        }

        public CfgSettings(String histPath, String dmgPath)
        {
            HistPath = histPath;
            DammagePath = dmgPath;
            
        }

        public bool LoadFromFile()
        {
            XmlDocument xmlDoc;
            
            try
            {
                xmlDoc = new XmlDocument();
                xmlDoc.Load(ConfigPath);

                HistPath = xmlDoc.GetElementById("HistPath").InnerText;
                DammagePath = xmlDoc.GetElementById("DmgPath").InnerText;

                ObservationHost = xmlDoc.GetElementById("ObservationHost").InnerText;
                ObservationPort = Int32.Parse(xmlDoc.GetElementById("ObservationPort").InnerText);

                AdminHost = xmlDoc.GetElementById("AdministrationHost").InnerText;
                AdminPort = Int32.Parse(xmlDoc.GetElementById("AdministrationPort").InnerText);

                return true;
            }
            catch(Exception)
            {
                return false;
            }
        }

        public void WriteToFile()
        {
            XmlDocument xmlDoc;
            try
            {
                xmlDoc = new XmlDocument();
                xmlDoc.Load(ConfigPath);
                xmlDoc.GetElementById("HistPath").InnerText = HistPath;
                xmlDoc.GetElementById("DmgPath").InnerText = DammagePath;
                xmlDoc.GetElementById("ObservationHost").InnerText = ObservationHost;
                xmlDoc.GetElementById("ObservationPort").InnerText = ObservationPort.ToString();
                xmlDoc.GetElementById("AdministrationHost").InnerText = AdminHost;
                xmlDoc.GetElementById("AdministrationPort").InnerText = AdminPort.ToString();

                xmlDoc.Save(ConfigPath);
            }
            catch (Exception exception)
            {
                throw new Exception(exception.Message);
            }
        }
        private void NotifyPropertyChanged([CallerMemberName] String propertyName = "")
        {
            if (PropertyChanged != null)
            {
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;


    }
}
