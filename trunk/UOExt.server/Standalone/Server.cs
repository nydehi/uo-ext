using System;
using System.Collections;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using UOExt.Network;

namespace UOExt.Standalone
{
	/// <summary> 
	/// Релизиция модели сокетного сервера
	/// </summary>
	public class Server: IDisposable
	{
		private TcpListener m_listener;
		private Thread m_thread;
		private ArrayList m_peerList = new ArrayList();
        private bool m_disposed;
        private PacketHandler m_handler;

        /// <summary>
        /// List of connected peers.
        /// </summary>
        public ArrayList PeerList { get { return m_peerList; } }
	
		/// <summary>
		/// Constructor
		/// </summary>
		public Server()
		{
            m_disposed = false;
            m_handler = PacketHandler.Instatinate();
            m_thread = new Thread(new ThreadStart(AcceptThread));
            m_thread.Name = "UOExt server thread";
            m_thread.Start();
		}

		/// <summary>
		/// Destructor
		/// </summary>
		~Server()
		{
			if (!m_disposed)
				Stop();
		}

		/// <summary>
		/// IDisposable. Stop thread if any
		/// </summary>
		public void Dispose()
		{
			try {
				m_disposed = true;  
				if (m_thread != null)
					Stop();
			} catch { }
		}

        /// <summary>
		/// Main accept thread proc
		/// </summary>
		private void AcceptThread()
		{
            Socket client = null;
            try
			{
Console.WriteLine("AcceptThread Start");
                m_listener = new TcpListener(Dns.Resolve(Config.IP).AddressList[0], Config.Port);
                m_listener.Start();
				while (true)
				{
Console.WriteLine("AcceptThread DONE");
					client = m_listener.AcceptSocket();
Console.WriteLine("AcceptThread AcceptSocket");
					if (client.Connected)
					{
						string Addr = client.RemoteEndPoint.ToString();
						int index = Addr.IndexOf(':');
						Addr = Addr.Substring(0, index );

                        Peer peer = new Peer(client, this, m_handler);
                        m_peerList.Add(peer);
					}
				}
			} catch (System.Net.Sockets.SocketException e) {
Console.WriteLine("AcceptThread SocketException");
                if (e.ErrorCode != 10004) {         // Ошибка не связана с остановкой объекта TCPListener
                    if (client != null)       // Закрытие сокета
						if (client.Connected)
							client.Close();
				}
			} catch {
Console.WriteLine("AcceptThread Exception");
                if (client != null)       // Закрытие сокета
                    if (client.Connected)
                        client.Close();
            }
		}

		/// <summary> 
		/// Server stop
		/// </summary>
		public void Stop()
		{
			if (m_thread != null) {
                m_listener.Stop();
                m_thread.Join();
                m_thread = null;
			}

            lock (m_peerList)
            {
                for (int i = 0; i < m_peerList.Count; ++i)
                {
                    ((Peer)m_peerList[i]).Dispose();
                }
                m_peerList.Clear();
            }
      
			GC.Collect();
			GC.WaitForPendingFinalizers();
		}

        public void RemovePeer(Peer peer)
        {
            lock (m_peerList)
            {
                m_peerList.Remove(peer);
            }

        }

        public void WaitJoin()
        {
            m_thread.Join();
        }
	}
}