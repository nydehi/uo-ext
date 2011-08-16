using System;

namespace UOExtDomain.Network
{
	/// <summary> 
	/// Вызывается при получении сообщения
	/// </summary>
	public delegate void MessageHandler(SocketBase socket, int iNumberOfBytes);
    
	/// <summary> 
	/// Вызывается при закрытии соединения
	///  </summary>
	public delegate void CloseHandler(SocketBase socket);
    
	/// <summary>
	/// Вызывается при возникновении ошибки
	///  </summary>
	public delegate void ErrorHandler(SocketBase socket, Exception exception);

	public abstract class SocketBase : IDisposable
	{
		#region Variables

		/// <summary>
		/// Ссылка на пользовательский объект
		/// </summary>
		protected internal Object userArg;

		/// <summary>
		/// Ссылка на метод, который будет вызываться при получении сообщения
		/// </summary>
		protected internal MessageHandler messageHandler;

		/// <summary>
        /// Ссылка на метод, который будет вызываться при закрытии соединения
		/// </summary>
		protected internal CloseHandler closeHandler;

		/// <summary>
        /// Ссылка на метод, который будет вызываться при возникновении ошибки
		/// </summary>
		protected internal ErrorHandler errorHandler;

		/// <summary>
		/// Флаг определяющий были ли освобождены и удаленны все ресурсы объекта
		/// </summary>
		protected internal bool disposed;

		/// <summary>
		/// IP аддрес к которому подключается клиент
		/// </summary>
		protected internal string ipAddress;

		/// <summary>
		/// Порт для соединения или прослушивания
		/// </summary>
		protected internal int port;

		/// <summary>
        /// Буфер для чтения данных из сокета
		/// </summary>
		protected internal byte[] rawBuffer;

		/// <summary>
        /// Размер буфера rawBuffer
		/// </summary>
		protected internal int sizeOfRawBuffer;

		#endregion
		
        #region Public Properties

		/// <summary> 
        /// IP аддрес к которому подключается клиент
		/// </summary>
		public string IpAddress 
		{ 
			get 
			{ 
				return this.ipAddress; 
			}
			set
			{ 
				this.ipAddress = value; 
			} 
		}

		/// <summary>
        /// Порт для соединения или прослушивания
		/// </summary>
		public int Port 
		{ 
			get 
			{ 
				return this.port; 
			} 
			set
			{
				this.port = value; 
			}
		}

		/// <summary>
        /// Ссылка на пользовательский объект
		/// </summary>
		public Object UserArg 
		{
			get 
			{
				return this.userArg; 
			} 
			set 
			{ 
				this.userArg = value; 
			} 
		}

		/// <summary>
        /// Буфер для чтения данных из сокета
		/// </summary>
		public byte[] RawBuffer 
		{ 
			get 
			{ 
				return this.rawBuffer; 
			} 
			set 
			{ 
				this.rawBuffer = value; 
			} 
		}

		/// <summary>
        /// Размер буфера RawBuffer
		/// </summary>
		public int SizeOfRawBuffer
		{ 
			get 
			{ 
				return this.sizeOfRawBuffer; 
			} 
			set 
			{ 
				this.sizeOfRawBuffer = value; 
			} 
		}


		#endregion Public Properties

		#region IDisposable Members

		public virtual void Dispose()
		{
		}

		#endregion
	}
}
