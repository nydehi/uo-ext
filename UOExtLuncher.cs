using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Sockets;
using UOExtDomain.Utilities;

namespace UOExtDomain
{
    class UOExtLuncher
    {
        /// <summary>
        /// Ограничивает доступ к сети только по локальной сети
        /// (полезно для отладки)
        /// </summary>
        public static bool Localhost = false;

        /// <summary>
        /// Сведения о локальном внутренем адресе машины
        /// </summary>
        private static IPHostEntry LocalEntry
        {
            get
            {
                if (m_LocalEntry == null)
                {
                    m_LocalEntry = new IPHostEntry();
                    m_LocalEntry.HostName = Localhost ? "localhost" : Dns.GetHostName();
                    List<IPAddress> addressList = new List<IPAddress>();
                    foreach (IPAddress ip in Dns.GetHostByName(m_LocalEntry.HostName).AddressList)
                        if (ip.AddressFamily == AddressFamily.InterNetwork)
                            addressList.Add(ip);
                    m_LocalEntry.AddressList = addressList.ToArray();
                } return m_LocalEntry;
            }
        }
        public static IPHostEntry m_LocalEntry = null;

        /// <summary>
        /// Порт на котором будет ввестись прослушивание
        /// </summary>
        private static int Port = Localhost ? 2585 : 2595; 

        /// <summary>
		/// Точка входа в приложение
		/// </summary>
		[STAThread]
		static void Main() 
		{
            var _address = IniFile.Default.ReadString("network", "ip", LocalEntry.AddressList[0].ToString());
            var _port    = IniFile.Default.ReadInt("network", "port", Port);

            var valid_address = String.Equals(_address, "127.0.0.1");
            foreach (var address in LocalEntry.AddressList)
                if (String.Equals(address.ToString(), _address)) {
                    valid_address = true;
                    break;
                }
            if (!valid_address)
                throw new Exception(String.Format("Указан не разрешенный аддресс \"{0}\"", _address));

            var endPoint = new IPEndPoint(IPAddress.Parse(_address), _port);
            UOExtServer.StartServer(endPoint);
            Console.WriteLine("[uoexts] Listening: {0}:{1}", endPoint.Address, endPoint.Port);
			
			// Ждём... =) 
			Console.ReadLine();
            Console.WriteLine("[uoexts] Closing.");
            //UOExtServer.SloseServer();
		}
    }
}
