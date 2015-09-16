using System;
using System.Globalization;
using System.Net.Sockets;
using System.Runtime.Remoting.Messaging;
using System.Text;
using CrystalFire;

namespace CrystalFire
{
    public class NetworkReader
    {
        private const char TERMINATOR = '\r';
        private readonly TcpClient _client;
        private byte[] _buffer = new byte[10240];
        private String _data;

        public delegate void OnDataHandler(string message);
        public event OnDataHandler OnDataRevieved;
        private NetworkStream _stream;

        public NetworkReader()
        {
            _client = new TcpClient();
        }

        public bool Connect(string hostname, int port)
        {
            try
            {
                _client.Connect(hostname, port);
                _stream = new NetworkStream(_client.Client);
                _data = "";
                ReadNetworkData(_stream);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        private void ReadNetworkData(NetworkStream stream)
        {
            try
            {
                stream.BeginRead(_buffer, 0, _buffer.Length, ar =>
                {
                    ParseBuffer(stream.EndRead(ar));
                    ReadNetworkData(stream);
                }, stream);
            }
            catch
            {
            }
        }

        private void ParseBuffer(int numberOfBytesRead)
        {
            _data += Encoding.ASCII.GetString(_buffer, 0, numberOfBytesRead);


            String msg, tail;
            bool result = KVPGrinder.DecipherMsgLine(_data, out msg, out tail);
            if (result)
            {
                if (_data.StartsWith("msg(disconnec")) return;
                OnDataRevieved(msg);
                _data = tail;
            }

            //string[] strings = _data.Split(TERMINATOR);
            //if (strings.Length > 1)
            //{
            //    for (int i = 0; i < strings.Length - 1; i++)
            //    {
            //        OnDataRevieved(strings[i]);
            //    }
            //}
            //if (_data.EndsWith(TERMINATOR.ToString(CultureInfo.InvariantCulture)))
            //{
            //    OnDataRevieved(strings[strings.Length-1]);
            //    _data = "";
            //}
            //else
            //{
            //    _data = strings[strings.Length - 1];
            //}

        }

        


    }
}
