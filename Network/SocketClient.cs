using System;
using System.Net;
using System.Collections;
using System.Net.Sockets;
using System.Threading;
using System.Windows.Forms;
using UOExtDomain.Utilities;

namespace UOExtDomain.Network
{
	/// <summary> 
    /// Релизиция модели сокетного клиента
	/// </summary>
	public class SocketClient : SocketBase
	{
        /// <summary>
        /// Подключен ли основной объект SocketClient к удаленному серверу.
        /// </summary>
        public bool Connected
        { get { return m_Connected ? ((tcpClient != null) ? tcpClient.Connected : m_Connected) : false; } }
		private bool m_Connected = false;
        private Queue packetQueue = new Queue();
        private byte[] theRest;

		#region Private Properties

		/// <summary>
        /// Объект NetworkStream
		/// </summary>
		private NetworkStream networkStream;

		/// <summary>
        /// Объект TcpClient для реализации работы с сетью
		/// </summary>
		private TcpClient tcpClient;

		/// <summary>
		/// Callback объект для чтения данных из сокета
		/// </summary>
		private AsyncCallback callbackReadMethod;

		/// <summary>
        /// Callback объект для записи данных в сокет
		/// </summary>
		private AsyncCallback callbackWriteMethod;

		/// <summary>
        /// Размера буфера чтения. По умолчанию равен 0x00100000
		/// </summary>
        private int receiveBufferSize = 0x00100000;

		/// <summary>
        /// Размера буфера записи. По умолчанию равен 0x00100000
		/// </summary>
        private int sendBufferSize = 0x00100000;

        /// <summary>
        /// Ссылка на объект SocketServer для этого клиента
        /// </summary>
		private SocketServer socketServer;

		/// <summary>
        /// Объект Socket для соединения клиента
		/// </summary>
		private Socket clientSocket;

		#endregion Private Properties

		#region Constructor & Destructor

		/// <summary>Конструктор используемый для серверной части в SocketServer</summary>
		/// <param name="socketServer">Ссылка на родительский объект SocketServer</param>
		/// <param name="clientSocket">Экземпляр объекта Socket для инкапсуляции</param>
        /// <param name="ipAddress">IP аддрес удалённого сервера</param>
        /// <param name="port">Порт удалённого сервера</param>
        /// <param name="messageHandler">Ссылка на метод, который будет вызываться при получении сообщения</param>
        /// <param name="closeHandler">Ссылка на метод, который будет вызываться при закрытии соединения</param>
        /// <param name="errorHandler">Ссылка на метод, который будет вызываться при возникновении ошибки</param>
        /// <param name="sizeOfRawBuffer">Размер буфер для чтения данных из сокета</param>
        /// <param name="userArg">Ссылка на пользовательский объект</param>
		public SocketClient(SocketServer socketServer, Socket clientSocket, string ipAddress, int port, 
            MessageHandler messageHandler, 	CloseHandler closeHandler, ErrorHandler errorHandler, int sizeOfRawBuffer, object userArg = null)
			:this(messageHandler,closeHandler,errorHandler, sizeOfRawBuffer, userArg)
		{
			this.socketServer = socketServer;
			this.clientSocket = clientSocket;
			this.ipAddress = ipAddress;
			this.port = port;

			this.networkStream = new NetworkStream(this.clientSocket);

			this.clientSocket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReceiveBuffer, this.receiveBufferSize);
			this.clientSocket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.SendBuffer,    this.sendBufferSize);
			this.clientSocket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.DontLinger,    1);
			this.clientSocket.SetSocketOption(SocketOptionLevel.Tcp,    SocketOptionName.NoDelay,       1);
			
            Receive();  // Получение данных
		}

		/// <summary> 
        /// Конструктор используемый для клиентской части в SocketClient
		/// </summary>
        /// <param name="messageHandler">Ссылка на метод, который будет вызываться при получении сообщения</param>
        /// <param name="closeHandler">Ссылка на метод, который будет вызываться при закрытии соединения</param>
        /// <param name="errorHandler">Ссылка на метод, который будет вызываться при возникновении ошибки</param>
        /// <param name="sizeOfRawBuffer">Размер буфер для чтения данных из сокета</param>
        /// <param name="userArg">Ссылка на пользовательский объект</param>
        /// <param name="sendBufferSize">Размера буфера записи. По умолчанию равен 0x00100000</param>
        /// <param name="receiveBufferSize">Размера буфера чтения. По умолчанию равен 0x00100000</param>
        public SocketClient(MessageHandler messageHandler, CloseHandler closeHandler, ErrorHandler errorHandler, 
            int sizeOfRawBuffer, object userArg = null, int sendBufferSize = 0x00100000, int receiveBufferSize = 0x00100000)
		{
            this.sendBufferSize = sendBufferSize;
            this.receiveBufferSize = receiveBufferSize;
			this.SizeOfRawBuffer = sizeOfRawBuffer;
			this.RawBuffer  = new Byte[this.SizeOfRawBuffer];

			this.userArg = userArg;
			this.messageHandler = messageHandler;
			this.closeHandler = closeHandler;
			this.errorHandler = errorHandler;

			this.callbackReadMethod  = new AsyncCallback(ReceiveComplete);
			this.callbackWriteMethod = new AsyncCallback(SendComplete);

			this.m_Connected = true;
			this.disposed = false;
		}

		/// <summary> 
		/// Деструктор 
		/// </summary>	
		~SocketClient()
		{
			if (!this.disposed)
				Dispose();
		}

		public override void Dispose()
		{
			try {
				this.disposed = true;
				this.Disconnect();
			} catch { }
			if (this.socketServer != null)
				this.socketServer.RemoveSocket(this);
			base.Dispose ();
		}

		#endregion Constructor & Destructor

        /// <summary>
        /// Объект Socket для соединения клиента
        /// </summary>
		public Socket ClientSocket
		{ 
			get { return this.clientSocket; } 
			set { this.clientSocket = value; } 
		}

        /// <summary>
        /// Ссылка на объект SocketServer для этого клиента
        /// </summary>
        public SocketServer SocketServer
        { get { return socketServer; } }

		/// <summary> 
		/// Подключение к серверу
		/// </summary>
		/// <param name="strIpAddress">IP аддрес сервера</param>
		/// <param name="iPort">Порт сервера</param>
		public void Connect(String ipAddress, int port)
		{
			try
			{
				if (this.networkStream == null) {
					this.IpAddress = ipAddress;
					this.Port = port;

					this.tcpClient = new TcpClient(this.IpAddress, this.Port);
					this.networkStream = this.tcpClient.GetStream();

					this.tcpClient.ReceiveBufferSize = this.receiveBufferSize;
					this.tcpClient.SendBufferSize = this.sendBufferSize;
					this.tcpClient.NoDelay = true;
					this.tcpClient.LingerState = new LingerOption(false,0);
					
					this.m_Connected = true;

                    Receive();  // Получение данных
				}
			}
			catch (System.Net.Sockets.SocketException e) {
                Console.WriteLine("{0} Error: Can't сonnect to server.", base.IpAddress);
                #if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
                #endif
                this.Disconnect();

				throw new Exception(e.Message, e.InnerException);
			}
		}

        /// <summary>
        /// Переподключение к северу
        /// </summary>
        public void Reconnect()
		{
            if (Connected)
                this.Disconnect();

            if (!String.IsNullOrEmpty(ipAddress) && port > 0)
                this.Connect(ipAddress, port);
		}

		/// <summary> 
		/// Отсоединение от сервера
		/// </summary>
		public virtual void Disconnect()
		{
            Console.ForegroundColor = ConsoleColor.DarkRed;
            Console.WriteLine("CALL Disconnect [Connected={0}] [m_Connected={1}]", Connected, m_Connected);
            Console.ResetColor();

			if(m_Connected) {
                this.closeHandler(this);

				if (this.networkStream != null )
					this.networkStream.Close();
                if (this.tcpClient != null && this.tcpClient.Connected)
					this.tcpClient.Close();
                if (this.clientSocket != null && this.clientSocket.Connected)
					this.clientSocket.Close();
			
				this.networkStream = null;
				this.tcpClient     = null;
				this.clientSocket  = null;

				this.m_Connected = false;
			}
		}

        // ================================================================

        /// <summary>
        /// Отправление пакета по сети
        /// </summary>
        /// <param name="packet">Пакет для отправки</param>
        /// <returns>Результат операции, т.е. true в если успех, в обратном случае false.</returns>
        public bool Send(SocketPacket packet)
        {
            return packet.Send(this);
        }

		/// <summary> 
		/// Отправление масива байт через сокет по сети
		/// </summary>
		/// <param name="rawBuffer">Массив не форматированных байтов для отправки</param>
        /// <returns>Результат операции, т.е. true в если успех, в обратном случае false.</returns>
		public bool Send(Byte[] rawBuffer)
		{
            if (!this.Connected && this.disposed)
                return false; // Соединение закрыто, прерываем процесс чтения данных

			if ((this.networkStream != null) && (this.networkStream.CanWrite))
				//&& (clientSocket != null) && (this.clientSocket.Connected == true))
			{                
                // Ансинхронная запись                                              вызов  SendComplete()
				this.networkStream.BeginWrite(rawBuffer, 0, rawBuffer.GetLength(0), this.callbackWriteMethod, null);
				return true;
			} else
				return false;
		}

		/// <summary> 
        /// Срабатывает по записи данных
		/// </summary>
        /// <param name="ar">Состояние асинхронной операции</param>
		private void SendComplete(IAsyncResult ar)
		{
            if (!this.Connected && this.disposed)
                return; // Соединение закрыто, прерываем процесс чтения данных

			try {
                ar.AsyncWaitHandle.WaitOne();
                //NetworkStream myNetworkStream = (NetworkStream)ar.AsyncState;
                //myNetworkStream.EndWrite(ar);

				if ((this.networkStream != null) )//&& (this.networkStream.CanWrite))
					this.networkStream.EndWrite(ar);
                else Console.WriteLine("Note: in SendComplete() networkStream is null");

                ar.AsyncWaitHandle.Close();
			} catch (Exception e) {
                Console.WriteLine("{0} Error: Can't write socket data.", base.IpAddress);
                #if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
                #endif
                this.Disconnect();
				try { System.Diagnostics.Debugger.Break(); } catch { ; }
			}
		}


		/// <summary> 
		/// Ожидание поступления новвых данных
        /// (не преостанавливает выполняемый поток)
		/// </summary>
		public void Receive()
		{
            try
			{
Console.WriteLine("Receive ");
                // Обработка полученных пакетов
			    while(packetQueue.Count != 0) {
                    var packet = packetQueue.Dequeue() as SocketPacket;
                    packet.Receive(this);
				    //this.messageHandler(this, RawBuffer.Length);
			    }

                if (!this.Connected && this.disposed)
                    return; // Соединение закрыто, прерываем процесс чтения данных

                // Асинхроное чтение
                if ( this.networkStream != null && this.networkStream.CanRead) {      
				    this.RawBuffer  = new Byte[this.SizeOfRawBuffer];
                    //if (/*&&*/ (this.networkStream.DataAvailable))
Console.WriteLine("BeginRead ");
                    // Ансинхронное чтение                                                вызов  ReceiveComplete()                  
				    this.networkStream.BeginRead(this.RawBuffer, 0, this.SizeOfRawBuffer, this.callbackReadMethod, null);
			    } else {      // Cокет не доступен для чтения
                    throw new Exception(String.Format("{0} Error: Socket not available for reading.", base.IpAddress));
                    this.Disconnect();
                }
            } catch(Exception e) {
                //throw new Exception("ОШИБКа ОШИБКА!!!");
                //throw e;

                Console.WriteLine("{0} Error: Can't receive socket data.", base.IpAddress);
                #if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
                #endif
                this.Disconnect();
				try { System.Diagnostics.Debugger.Break(); } catch { ; }
			}
		}

        /// <summary> 
		/// Срабатывает по получении данных
		/// </summary>
		/// <param name="ar">Состояние асинхронной операции</param>
		private void ReceiveComplete(IAsyncResult ar)
		{
Console.WriteLine("ReceiveComplete ");
            if (!this.Connected && this.disposed)
                return; // Соединение закрыто, прерываем процесс чтения данных

			try
			{
                ar.AsyncWaitHandle.WaitOne();
				if (this.networkStream.CanRead)
				{
					int bytesRecieved = this.networkStream.EndRead(ar);
Console.WriteLine("bytesRecieved {0}", bytesRecieved);
					if (bytesRecieved > 0) {
						try
						{
							if(theRest != null) {
								int i;
								byte[] tmp = new byte[bytesRecieved + theRest.Length];
								for(i = 0; i<theRest.Length; i++)
									tmp[i] = theRest[i];
								for(int j = 0; j< bytesRecieved; j++)
									tmp[i++] = RawBuffer[j];
								RawBuffer = tmp;
								bytesRecieved = bytesRecieved + theRest.Length;
								theRest = null;
							}

                            // Преобразование потока в пакеты
							ParsePacket(bytesRecieved);

                            if (packetQueue.Count > 0) {
                                var packet = packetQueue.Dequeue() as SocketPacket;
                                packet.Receive(this);
								//this.messageHandler(this, RawBuffer.Length);
							}
						}
						catch(Exception e) {
							throw e;
						}

						// Ожидание новвых данных
						if(this.m_Connected == true)
							Receive();
                        ar.AsyncWaitHandle.Close();
					}

				} //else Console.WriteLine("Note: in SendComplete() networkStream is null");
			}
			catch (Exception e)
			{
                Console.WriteLine("{0} Error: Can't read socket data.", base.IpAddress);
                #if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
                #endif
                this.Disconnect();
				try { System.Diagnostics.Debugger.Break(); } catch { ; }
			}
		}

        private void SaveTheRestofTheStream(byte[] src, int ptr, int TotalLen)
		{
			theRest = new byte[TotalLen - ptr];
			for(int i = 0; ptr < TotalLen; i++)
				theRest[i] = RawBuffer[ptr++];
			return;
		}

		private void ParsePacket(int TotalLength)
		{
            int ptr = 0;

            try
            {
                while (ptr < TotalLength)
                {
                    /*
                    if ((TotalLength - ptr) <= 1) {
                        return;
                    }
                    */

                    int headerSize = 4, packetSize = 0; 
                    byte primaryId, packetId;
                    if ((TotalLength - ptr) < 4) {
                        SaveTheRestofTheStream(RawBuffer, ptr, TotalLength);
                        return;
                    } else {
                        var bytes = new byte[2];
                        Array.Copy(RawBuffer, ptr, bytes, 0, 2);
                        packetSize = BitConverter.ToUInt16(bytes, 0) - headerSize;
                        primaryId = RawBuffer[ptr+2];
                        packetId  = RawBuffer[ptr+3];

                        if (!SocketPacket.PacketTypes.ContainsKey(packetId)) {
                            this.Send(new ReturnError_Packet0xF0(packetId, ReturnError_Packet0xF0.ErrorCode.UnknownPacket | ReturnError_Packet0xF0.ErrorCode.ClosingSocket));
                            return;
                        }
                    }
                        
                    /*
                    var packetId = RawBuffer[ptr];
                    if (!SocketPacket.PacketTypes.ContainsKey(packetId)) {
                        this.Send(new ErrorPacket(packetId, ErrorPacket.ErrorCode.UnknownPacket | ErrorPacket.ErrorCode.ClosingSocket));
                        return;
                    }

                    var packetSize = SocketPacket.PacketSizes[packetId];
                    int headerSize = packetSize >= 0 ? 1 : 5;
                    if (packetSize < 0) 
                        if((TotalLength - ptr) <= 5) {
                            SaveTheRestofTheStream(RawBuffer, ptr, TotalLength);
                            return;
                        } else {
                            var bytes = new byte[4];
                            Array.Copy(RawBuffer, ptr + 1, bytes, 0, 4);
                            //if (BitConverter.IsLittleEndian)
                            //    Array.Reverse(bytes);
                            packetSize = BitConverter.ToInt32(bytes, 0);
                        }
                    */

                    if (headerSize + packetSize > (TotalLength - ptr)) {
                        SaveTheRestofTheStream(RawBuffer, ptr, TotalLength);
                        return;
                    }

                    ptr += headerSize;
                    var rawData = new byte[packetSize];
                    for (int i = 0; i < packetSize; i++)
                        rawData[i] = RawBuffer[ptr++];

                    var packetType = SocketPacket.PacketTypes[packetId];
                    var packet = DExecutor.CreateInstance(packetType, rawData);
                    //var packet = DExecutor.CreateInstance<packetType>(rawData);
                    packetQueue.Enqueue(packet);
                }
            } catch (Exception e) {     // Данные не удалось преобразовать в пакеты
                Console.WriteLine("{0} Error: Can't parse recived data from socket to packet.", base.IpAddress);
                #if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
                #endif
                this.Disconnect();
            }

		}

	}
}

