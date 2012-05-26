using System;
using System.Net.Sockets;
using UOExt.Network;

namespace UOExt.Standalone
{
    /// <summary> 
    /// Релизиция модели сокетного клиента
    /// </summary>
    public class Peer : IDisposable, IClientPeer
    {
        private bool m_connected;
        private byte[] m_buffer;
        private int m_bufferPointer;
        private Socket m_socket;
        private Server m_server;
        private byte[] m_sendBuffer;
        private int m_sendBufferPos;
        private bool m_disposed;
        private PacketHandler m_handler;

        private AsyncCallback m_AsyncSend;
        private bool m_sending;

        private AsyncCallback m_AsyncRecive;

        /// <summary>
        /// Подключен ли основной объект SocketClient к удаленному серверу.
        /// </summary>
        public bool Connected { get { return m_connected; } }

        #region Constructor & Destructor

        /// <summary>Constructor</summary>
        /// <param name="client">Client socket with connected client</param>
        /// <param name="ip">Client ip</param>
        /// <param name="port">Server port</param>
        public Peer(Socket client, Server server, PacketHandler handler)
        {
            m_disposed = false;
            m_socket = client;
            m_server = server;
            m_buffer = new byte[65536 * 2];
            m_sendBuffer = new byte[65536 * 2];
            m_bufferPointer = 0;
            m_handler = handler;
            m_connected = true;
            m_AsyncSend = new AsyncCallback(OnSend);
            m_AsyncRecive = new AsyncCallback(OnRecive);
            m_sending = false;

            m_socket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReceiveBuffer, 65536 * 2); // this should be enough for every packet
            m_socket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.SendBuffer, 65536 * 2);
            m_socket.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.DontLinger, 1);
            m_socket.SetSocketOption(SocketOptionLevel.Tcp, SocketOptionName.NoDelay, 1);

            BeginReceive();
        }

        /// <summary> 
        /// Descructor
        /// </summary>	
        ~Peer()
        {
            if (!m_disposed)
                Dispose();
        }

        public void Dispose()
        {
            try
            {
                m_disposed = true;
                Disconnect();
            }
            catch { }
            if (m_server != null)
                m_server.RemovePeer(this);
        }

        #endregion Constructor & Destructor

        /// <summary>
        /// Client socket
        /// </summary>
        public Socket Client { get { return m_socket; } set { m_socket = value; } }

        /// <summary>
        /// Server
        /// </summary>
        public Server Server { get { return m_server; } }

        /// <summary> 
        /// Отсоединение от сервера
        /// </summary>
        public virtual void Disconnect()
        {
            Console.ForegroundColor = ConsoleColor.DarkRed;
            Console.WriteLine("CALL Disconnect [Connected={0}] [m_Connected={1}]", Connected, m_connected);
            Console.ResetColor();

            if (m_connected)
            {
                if (m_socket != null && m_socket.Connected)
                    m_socket.Close();

                m_socket = null;
                m_connected = false;
            }
        }

        // ================================================================

        #region IClientPeer
        public void Send(Packet p)
        {
            p.Send(this);
        }

        /// <summary> 
        /// Send data to client
        /// </summary>
        /// <param name="bufferr">Data to send</param>
        public void Send(byte[] buffer)
        {
            if (!m_connected || m_disposed) return;
            lock (m_sendBuffer)
            {
                if (!m_sending)
                {
                    byte[] tmp = new byte[buffer.Length];
                    Array.Copy(buffer, tmp, buffer.Length);
                    m_socket.BeginSend(buffer, 0, buffer.Length, SocketFlags.None, m_AsyncSend, m_socket);
                    m_sending = true;
                }
                else
                {
                    Array.Copy(buffer, 0, m_sendBuffer, m_sendBufferPos, buffer.Length);
                    m_sendBufferPos += buffer.Length;
                }
            }
        }

        public void Close()
        {
            Dispose();
        }
        #endregion

        private void OnSend(IAsyncResult a)
        {
            if (!m_connected || m_disposed)
            {
                m_sending = false;
                return;
            }
            Socket s = (Socket)a.AsyncState;
            if (!s.Connected)
            {
                m_sending = false;
                Dispose();
                return;
            }
            try
            {
                int bytes = s.EndSend(a);
                if (bytes <= 0)
                {
                    m_sending = false;
                    Dispose();
                    return;
                }
                byte[] tmp;
                lock (m_sendBuffer)
                {
                    if (m_sendBufferPos > 0)
                    {
                        tmp = new byte[m_sendBufferPos];
                        Array.Copy(m_sendBuffer, 0, tmp, 0, m_sendBufferPos);
                        m_sendBufferPos = 0;
                    }
                    else
                    {
                        m_sending = false;
                        return;
                    }
                }
                m_socket.BeginSend(tmp, 0, tmp.Length, SocketFlags.None, m_AsyncSend, s);
            }
            catch (SocketException se)
            {
                if (se.ErrorCode == 10054)
                {
                    m_sending = false;
                    Dispose();
                    return;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("{0} Error: Can't read socket data.", m_socket.RemoteEndPoint.ToString());
#if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
#endif
                this.Disconnect();
                try { System.Diagnostics.Debugger.Break(); }
                catch { ; }
            }

        }

        public void BeginReceive()
        {
            try
            {
                Console.WriteLine("BeginReceive");
                if (!m_connected && m_disposed) return;

                m_socket.BeginReceive(m_buffer, m_bufferPointer, 65536 * 2 - m_bufferPointer, SocketFlags.None, m_AsyncRecive, m_socket);
            }
            catch (SocketException se)
            {
                if (se.ErrorCode == 10054)
                {
                    Dispose();
                    return;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("{0} Error: Can't receive socket data.", m_socket.RemoteEndPoint.ToString());
#if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
#endif
                Dispose();
                try { System.Diagnostics.Debugger.Break(); }
                catch { ; }
            }
        }

        private void OnRecive(IAsyncResult ar)
        {
            Console.WriteLine("ReceiveComplete ");
            if (!m_connected && m_disposed) return;

            Socket s = (Socket)ar.AsyncState;

            if (!s.Connected)
            {
                Dispose();
                return;
            }

            try
            {
                int m_recived = s.EndReceive(ar);
                m_bufferPointer += m_recived;
                if (m_bufferPointer > 3)
                {
                    int m_bufferPosition = 0;
                    do
                    {
                        if ((m_bufferPosition + 3) > m_bufferPointer) break;

                        int size = (m_buffer[m_bufferPosition] << 8) + m_buffer[m_bufferPosition + 1];
                        if ((m_bufferPointer + size) < m_bufferPosition) break;

                        byte header = m_buffer[m_bufferPosition + 2];

                        Console.WriteLine("C -> S: New packet with size: {0}", size);
                        Packet.DumpArray(Console.Out, m_buffer, m_bufferPosition, size);
                        m_handler.ProcessBuffer(this, header, m_buffer, m_bufferPosition + 3, (short)(size - 3));
                        m_bufferPosition += size;
                    } while (m_bufferPosition < m_bufferPointer);
                    if (m_bufferPosition > m_bufferPointer)
                    {
                        byte[] tmpBuffer = new byte[m_bufferPointer - m_bufferPosition];
                        Array.Copy(m_buffer, m_bufferPosition, tmpBuffer, 0, m_bufferPointer - m_bufferPosition);
                        Array.Copy(tmpBuffer, m_buffer, tmpBuffer.Length);
                    }
                    m_bufferPosition -= m_bufferPointer;
                }

                s.BeginReceive(m_buffer, m_bufferPointer, 65536 * 2 - m_bufferPointer, SocketFlags.None, m_AsyncRecive, s);
            }
            catch (SocketException se)
            {
                if (se.ErrorCode == 10054)
                {
                    Dispose();
                    return;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("{0} Error: Can't read socket data.", m_socket.RemoteEndPoint.ToString());
#if DEBUG
                Console.WriteLine("Description: {1}{0}StackTrace:{0}{2}{0}{0}", Environment.NewLine, e.Message, e.StackTrace);
#endif
                this.Disconnect();
                try { System.Diagnostics.Debugger.Break(); }
                catch { ; }
            }
        }
    }
}

