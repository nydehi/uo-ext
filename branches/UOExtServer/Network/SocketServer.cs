using System;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Collections;

namespace UOExtDomain.Network
{
	/// <summary> 
	/// Релизиция модели сокетного сервера
	/// </summary>
	public class SocketServer : SocketBase
	{
		/// <summary> 
        /// Вызывается при установлении соединения
		/// </summary>
		public delegate void AcceptHandler(SocketClient socket);

		/// <summary>
        /// Ссылка на метод, который будет вызываться при установлении соединения
		/// </summary>
		private AcceptHandler acceptHandler;

		/// <summary> 
		/// Объект TcpListener для реализации работы с сетью
		/// </summary>
		private TcpListener tcpListener;

		/// <summary>
		/// Фоновый поток для прослушивания порта на взодящие соединения
		/// </summary>
		private Thread acceptThread;

		/// <summary>
		/// Массив экземпляров SocketClient
		/// </summary>
		private ArrayList socketClientList = new ArrayList();
		public ArrayList SocketClientList
		{
			get{return socketClientList;}
		}
	
		/// <summary>
		/// Конструктор по умолчанию
		/// </summary>
		public SocketServer()
		{
			this.disposed = false;
		}

		/// <summary>
		/// Деструктор
		/// </summary>
		~SocketServer()
		{
			if (!this.disposed)
				Stop();
		}

		/// <summary>
		/// Освобождение всех ресурсов, используемых объектом
		/// </summary>
		public override void Dispose()
		{
			try {
				this.disposed = true;  
				if (this.acceptThread != null)
					Stop();
			} catch { }
			base.Dispose ();
		}


		
		/// <summary>
		/// Главный поток обработки входящих подключений
		/// </summary>
		private void AcceptThread()
		{
			Socket clientSocket = null;
			try
			{
Console.WriteLine("AcceptThread Start");
				this.tcpListener = new TcpListener(Dns.Resolve(this.IpAddress).AddressList[0],this.Port);
				this.tcpListener.Start();
				while (true)
				{
Console.WriteLine("AcceptThread DONE");
					clientSocket = this.tcpListener.AcceptSocket();
Console.WriteLine("AcceptThread AcceptSocket");
					if (clientSocket.Connected)
					{
						string Addr = clientSocket.RemoteEndPoint.ToString();
						int index = Addr.IndexOf(':');
						Addr = Addr.Substring(0, index );

						SocketClient socket = AcceptedSocketClient(this, clientSocket, Addr, this.Port, this.SizeOfRawBuffer, this.UserArg,                              
                            new MessageHandler(this.messageHandler), new CloseHandler(this.closeHandler), new ErrorHandler(this.errorHandler));       

						socketClientList.Add( socket );
						this.acceptHandler( socket );
					}
				}
			} catch (System.Net.Sockets.SocketException e) {
Console.WriteLine("AcceptThread SocketException");
                if (e.ErrorCode != 10004) {         // Ошибка не связана с остановкой объекта TCPListener
					this.errorHandler(null, e);
                    if (clientSocket != null)       // Закрытие сокета
						if (clientSocket.Connected)
							clientSocket.Close();
				}
			} catch (Exception e) {
Console.WriteLine("AcceptThread Exception");
				this.errorHandler(null, e);
                if (clientSocket != null)           // Закрытие сокета
					if (clientSocket.Connected)
						clientSocket.Close();
			}
		}

        protected virtual SocketClient AcceptedSocketClient( SocketServer socketServer, Socket clientSocket, string ipAddress, int port, 
			    int sizeOfRawBuffer, object userArg, MessageHandler messageHandler, CloseHandler closeHandler, ErrorHandler errorHandler)
		{
            return new SocketClient(socketServer, clientSocket, ipAddress, port, messageHandler, closeHandler, errorHandler, sizeOfRawBuffer, userArg);
		}


		public void RemoveSocket(SocketClient socketClient)
		{
			Monitor.Enter(socketClientList);
			try {
				foreach( SocketClient socket in socketClientList) {
					if(socket == socketClient) {
						socketClientList.Remove( socketClient );
						break;
					}
				}
			} catch { }
			Monitor.Exit(socketClientList);
		}

		/// <summary> 
		/// Запуск сервера 
		/// </summary>
		/// <param name="ipAddress">IP адресс для прослушивания</param>
		/// <param name="port">Порт для прослушивания</param>
		/// <param name="sizeOfRawBuffer">Размер буфера данных</param>
		/// <param name="userArg">Пользовательский объект</param>
        /// <param name="messageHandler">Ссылка на метод, который будет вызываться при получении сообщения</param>
        /// <param name="acceptHandler">Ссылка на метод, который будет вызываться при установлении соединения</param>
        /// <param name="closeHandler">Ссылка на метод, который будет вызываться при закрытии соединения</param>
        /// <param name="errorHandler">Ссылка на метод, который будет вызываться при возникновении ошибки</param>
		public void Start(string ipAddress, int port, int sizeOfRawBuffer, object userArg,
			MessageHandler messageHandler, AcceptHandler acceptHandler, CloseHandler closeHandler, ErrorHandler errorHandler)
		{
			if (this.acceptThread == null)
			{
				this.IpAddress = ipAddress;
				this.Port = port;
        
				this.messageHandler = messageHandler;
				this.acceptHandler = acceptHandler;
				this.closeHandler = closeHandler;
				this.errorHandler = errorHandler;
        
				this.SizeOfRawBuffer = sizeOfRawBuffer;
				this.UserArg = userArg;
        
				ThreadStart tsThread = new ThreadStart(AcceptThread);
				this.acceptThread = new Thread(tsThread);
				this.acceptThread.Name = "Notification.Accept";
				this.acceptThread.Start();
			}
		}
    
		/// <summary> 
		/// Остановка сервера 
		/// </summary>
		public void Stop()
		{
			if (this.acceptThread != null) {
				this.tcpListener.Stop();
				this.acceptThread.Join();
				this.acceptThread = null;
			}
   
			for (int iSocket = 0; iSocket < this.socketClientList.Count; ++iSocket) {
				SocketClient socket = (SocketClient)socketClientList[iSocket];
				socketClientList.Remove( socket );
				socket.Dispose();
			}
      
			GC.Collect();
			GC.WaitForPendingFinalizers();
      
			this.messageHandler = null;
			this.acceptHandler  = null;
			this.closeHandler   = null;
			this.errorHandler   = null;
      
			this.sizeOfRawBuffer = 0;
			this.userArg = null;
		}


		/// <summary>
		/// Число подключенных клиентов
		/// </summary>
		public int ConnectedClientCount
		{
			get
			{
				if(this.socketClientList == null) return 0;
				else return this.socketClientList.Count;
			}
		}

		/// <summary>
		/// Отправить всем подключенным клиентам пакет
		/// </summary>
        /// <param name="packet"></param>
		public int SendConnectedClients(SocketPacket packet)
		{
			int count = 0;
			ArrayList ObjectsToRemove = null;
             
			for(int x = 0; x < this.socketClientList.Count; x++)
				try
				{
					SocketClient socket = (SocketClient)this.socketClientList[x];
					if(socket.ClientSocket.Connected == true && socket.Send(packet) == true)
						count++;
					else
                        (ObjectsToRemove ?? (ObjectsToRemove = new ArrayList())).Add(socket);
				} catch( Exception e )
				{
					Console.WriteLine("Error:SocketServer: While in NotifyConnectedClients" +
						e.Message);
					System.Diagnostics.Debugger.Break();
				}

			if(ObjectsToRemove != null)
				foreach(SocketClient socket in ObjectsToRemove) {
					socket.Disconnect();
					socketClientList.Remove( socket );
					socket.Dispose();
				}

			return count;
		}
	}
}