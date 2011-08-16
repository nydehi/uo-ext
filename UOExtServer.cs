using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Net.NetworkInformation;
using System.Net.Sockets;
using UOExtDomain.Network;
using UOExtDomain.Utilities;
using System.Runtime.InteropServices;

namespace UOExtDomain
{
    public class UOExtServer
    {
        private static SocketServer m_SocketServer;
        private static String m_PluginsPath;
        private static String m_SearchMask;

        public static void StartServer(IPEndPoint endPoint, string pluginsPath = @"UOExtPlugins", string searchMask = @"Plugin-0x??.dll")
        {
            m_SearchMask  = searchMask;
            m_PluginsPath = pluginsPath;
            Console.WriteLine("new SendPluginsPacket");
            new SendPluginsPacket(m_PluginsPath, m_SearchMask);
            Console.WriteLine("new SocketServer");
            m_SocketServer = new SocketServer();
            Console.WriteLine("Start SocketServer");
            m_SocketServer.Start(endPoint.Address.ToString(), endPoint.Port, 0x0400, null, 
			                    new MessageHandler(MessageHandlerServer),
			                    new SocketServer.AcceptHandler(AcceptHandler),
			                    new CloseHandler(CloseHandler),
			                    new ErrorHandler(ErrorHandler));
        }

        public static void AcceptHandler(SocketClient pSocket)
        {
            Console.WriteLine("{0} : Accept Handler", pSocket.IpAddress);
        }

        static public void MessageHandlerServer(SocketBase socket, int iNumberOfBytes)
        {
            Console.WriteLine("{0} : Message Handler", socket.IpAddress);
        }

        public static void ErrorHandler(SocketBase socket, Exception pException)
        {
            Console.WriteLine(pException.Message);
        }

        public static void CloseHandler(SocketBase socket)
        {
            Console.WriteLine("{0} : Close Handler", socket.IpAddress);
        }
         
        

        #if !LIBRARY

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
                if (m_LocalEntry == null) {
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
 
        #if !TESTCENTER
        #region Application UOExtDomain (exe)
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
            StartServer(endPoint);
            Console.WriteLine("[uoexts] Listening: {0}:{1}", endPoint.Address, endPoint.Port);
			
			// Ждём... =) 
			Console.ReadLine();
            Console.WriteLine("[uoexts] Closing.");
			m_SocketServer.Dispose();
		}
        #endregion Application UOExtDomain (exe)
        #else
        #region Application UOExtTestClient (exe)   
        static public void MessageHandlerClient(SocketBase socket, int iNumberOfBytes)
        {
            Console.WriteLine("{0} : Message Handler", socket.IpAddress);
        }

		/// <summary>
		/// Точка входа в приложение
		/// </summary>
		[STAThread]
		static void Main() 
		{
            SocketClient pSocketClient = new SocketClient(
                    new MessageHandler(MessageHandlerClient),
                    new CloseHandler(CloseHandler),
                    new ErrorHandler(ErrorHandler), 10240);

            Localhost = true;
            pSocketClient.Connect(LocalEntry.AddressList[0].ToString(), Port);

            try
            {
                //byte[] messge_0x01 = new byte[] { 0x01, 0x0F, 0x11, 0x21, 0x31, 0x41, 0x51, 0x61, 0x71, 0x81, 0x91, 0xA1, 0xB1, 0xC1, 0xD1, 0xE1, 0xF1 };
                //byte[] messge_0x02 = new byte[] { 0x02, 0x09, 0x12, 0x22, 0x32, 0x42, 0x52, 0x62, 0x72, 0x82, 0x92 };
                //byte[] messge_0x03 = new byte[] { 0x03, 0x00 };
                //byte[] messge_0x04 = new byte[] { 0x04, 0x05, 0x14, 0x24, 0x34, 0x44, 0x54 };
                //pSocketClient.Send(messge_0x01);
                //pSocketClient.Send(messge_0x02);
                //pSocketClient.Send(messge_0x03);
                //pSocketClient.Send(messge_0x04);

                var packet1  = new ConnectionPacket(0x12345678);
                //pSocketClient.Send(packet1);
                
                var packet2 = new ConnectionPacket(ConnectionPacket.CurrentProtocolVer);
                pSocketClient.Send(packet2);
                
                pSocketClient.Send(new TestStaticPacket(1));
                pSocketClient.Send(new TestDinamicPacket("hellow server"));
                
                pSocketClient.Send(new TestDinamicPacket("test string"));
                pSocketClient.Send(new TestStaticPacket(1000000)); 

                
            } catch (Exception e) {
                Console.WriteLine(e.Message);
            }

            // Ждём... =) 
            Console.ReadLine();
            pSocketClient.Disconnect();
            pSocketClient.Dispose();
		}
        #endregion Application UOExtTestClient (exe)
#endif
#endif
    }
}
