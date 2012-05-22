using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.IO;
using System.Collections;
using UOExtDomain;
using UOExtDomain.Utilities;

namespace UOExtDomain.Network
{
    [FlagsAttribute]
    public enum PacketType : byte
    {
        /// <summary>
        /// Не используемый пакет 
        /// </summary>
        None        = 0x00,

        /// <summary>
        /// Входящий пакет (от клиента к серверу)
        /// </summary>
        Incoming    = 0x01,

        /// <summary>
        /// Исходящий пакет (от сервера к клиенту)
        /// </summary>
        Outgoing    = 0x02,
        
        /// <summary>
        /// Многоцелевой пакет
        /// </summary>
        All_mode    = 0x03
    }

    public abstract class SocketPacket : IDisposable
    {
        /// <summary>
        /// Тип пакета
        /// </summary>
        public virtual PacketType Type 
        { get { return PacketType.None; } }

        protected readonly PacketType Direction;

        /// <summary>
        /// Размер данных в статическом пакете 
        /// (если размер динамический то значение должно быть меньше нуля)
        /// </summary>
        public virtual Int32 DataSize
        { get { return -1; } }

        /// <summary>
        /// Основной ID пакета (а шо он значит-то?)
        /// </summary>
        public virtual Byte PrimaryID  
        { get { return 0x00; } }

        /// <summary>
        /// ID пакета UOExt
        /// </summary>
        public virtual Byte PacketID
        { get { return 0x00; } }

        private Stream RawDataStream = new MemoryStream();

        /// <summary>
        /// Объект для чтения из бинарного потока данных пакета
        /// </summary>
        public BinaryReader DataReader
        { get { return m_DataReader ?? (m_DataReader = new BinaryReader(RawDataStream)); } }
        private BinaryReader m_DataReader = null;

        /// <summary>
        /// Объект для записи в бинарный поток данных пакета
        /// </summary>
        public BinaryWriter DataWriter
        { get { return m_DataWriter ?? (m_DataWriter = new BinaryWriter(RawDataStream)); } }
        private BinaryWriter m_DataWriter = null;

        public SocketPacket()
        {
            Direction = PacketType.Outgoing;
        }

        public SocketPacket(byte[] rawdata)
        {
            Direction = PacketType.Incoming;

            #if !TESTCENTER
            if ((Type & PacketType.Incoming) != PacketType.Incoming)
                throw new Exception(String.Format("Получение не разрешенного пакета {0}", StringID));
            #endif

            DataWriter.Write((byte[])rawdata);
            DataWriter.Flush();
            DataReader.BaseStream.Position = 0;
        }

        public bool Send(SocketClient client) 
        {
            #if !TESTCENTER
            if ((this.Type & PacketType.Outgoing) != PacketType.Outgoing)
                throw new Exception(String.Format("Попытка отправки не разрешенного пакета {0}", StringID));
            #endif

            this.DataWriter.Flush();
            uint _datasize = (this.DataSize < 0 ? (uint)this.DataReader.BaseStream.Length : (uint)this.DataSize) + 4;
            var rawData = new BinaryWriter(new MemoryStream((int)_datasize));

            //if (this.DataSize < 0)
            rawData.Write((ushort)_datasize);
            rawData.Write((byte)PrimaryID);
            rawData.Write((byte)PacketID);

            this.DataReader.BaseStream.Position = 0;
            var bytes = DataReader.ReadBytes((int)_datasize);
            rawData.Write((byte[])bytes);
            
            rawData.Flush();
            rawData.BaseStream.Position = 0;
            var binData = new BinaryReader(rawData.BaseStream);
            _datasize = (uint)binData.BaseStream.Length;
            bytes = binData.ReadBytes((int)_datasize);
            var result = client.Send(bytes);
            /*
            while (binData.BaseStream.Position != binData.BaseStream.Length) 
            {
                var bytes  = binData.ReadBytes(Math.Min((int)binData.BaseStream.Length, (int)512));
                var result = client.Send(bytes);
                if (!result)
                    return false;
            }
            */
            this.OnSend(client);

            rawData.Close();
            //rawData.Dispose();
            return result;
        }

        public void Receive(SocketClient client)
        {
            this.OnReceive(client);
        }

        public virtual void OnSend(SocketClient client)
        {
            #if DEBUG
            FormatBuffer(Console.Out, this.RawDataStream, "<<==   ", String.Format("PID ${0:X2} Size: {1:D}", this.PacketID, this.RawDataStream.Length), 0, 0xFF);
            Console.WriteLine();
            #endif

            #if !TESTCENTER
            if ((Type & PacketType.Outgoing) != PacketType.Outgoing)
                throw new Exception(String.Format("Отправка не разрешенного пакета {0}", StringID));
            #endif
            // ...
        }

        public virtual void OnReceive(SocketClient client)
        {
            #if DEBUG
            FormatBuffer(Console.Out, this.RawDataStream, "==>>   ", String.Format("PID ${0:X2} Size: {1:D}", this.PacketID, this.RawDataStream.Length), 0, 0xFF);
            Console.WriteLine();
            #endif

            #if !TESTCENTER
            if ((Type & PacketType.Incoming) != PacketType.Incoming)
                throw new Exception(String.Format("Получение не разрешенного пакета {0}", StringID));
            #endif

            this.RawDataStream.Position = 0;
            // ...
        } 

        public void Dispose()
        {
        }

        public static  Dictionary<byte, Type> PacketTypes 
        {
            get {
                if (m_PacketTypes == null)
                    BuildPacketDictionary();
                return m_PacketTypes;
            }
        }
        private static Dictionary<byte, Type> m_PacketTypes = null;

        public static  Dictionary<byte, Int32> PacketSizes 
        {
            get {
                if (m_PacketSizes == null)
                    BuildPacketDictionary();
                return m_PacketSizes;
            }
        }
        private static Dictionary<byte, Int32> m_PacketSizes = null;

        private static void BuildPacketDictionary()
        {
            var types = DExecutor.GetAssemblyTypes(typeof(SocketPacket), true);
            m_PacketSizes = new Dictionary<byte, Int32>(types.Count); 
            m_PacketTypes = new Dictionary<byte, Type>(types.Count);
            foreach (var type in types) {
                var packet = DExecutor.CreateInstance(type);
                var id     = DExecutor.GetProperty<Byte>(packet, "PacketID");
                var size   = DExecutor.GetProperty<Int32>(packet, "DataSize");
                m_PacketTypes.Add(id, type);
                m_PacketSizes.Add(id, size);
            }
        }

        /// <summary>
        /// PrimaryID и SecondaryID в строковом виде
        /// </summary>
        public String StringID
        { get { return String.Format("0x{0:X2}{1}", PrimaryID, this is ISocketPacketExtension ? String.Format("-0x{0:X4}", (this as ISocketPacketExtension).SecondaryID) : String.Empty); } }

        private static void FormatBuffer(TextWriter output, Stream input, string header = null, string args = null, int position = 0, int length = -1)
		{
            input.Position = (long)Math.Min(Math.Max(0, position), input.Length);
            length = length < 0 ? (int)input.Length : Math.Min(length, (int)input.Length);

			output.WriteLine("{0} 0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F | {1}", String.IsNullOrEmpty(header) ? "       " : header, args ?? String.Empty);
            output.WriteLine("       -- -- -- -- -- -- -- --  -- -- -- -- -- -- -- --");

			int byteIndex = 0;

			int whole = length >> 4;
			int rem = length & 0xF;

			for ( int i = 0; i < whole; ++i, byteIndex += 16 )
			{
				StringBuilder bytes = new StringBuilder( 49 );
				StringBuilder chars = new StringBuilder( 16 );
                
				for ( int j = 0; j < 16; ++j ) {
					int c = input.ReadByte();
					bytes.Append( c.ToString( "X2" ) );

					if ( j != 7 ) {
						bytes.Append( ' ' );
					} else {
						bytes.Append( "  " );
					}

					if ( c >= 0x20 && c < 0x80 ) {
						chars.Append( (char)c );
					} else {
						chars.Append( '.' );
					}
				}

				output.Write( byteIndex.ToString( "X4" ) );
				output.Write( "   " );
				output.Write( bytes.ToString() );
				output.Write( "  " );
				output.WriteLine( chars.ToString() );
			}

			if ( rem != 0 )
			{
				StringBuilder bytes = new StringBuilder( 49 );
				StringBuilder chars = new StringBuilder( rem );

				for ( int j = 0; j < 16; ++j ) {
					if ( j < rem ) {
						int c = input.ReadByte();

						bytes.Append( c.ToString( "X2" ) );

						if ( j != 7 ) {
							bytes.Append( ' ' );
						} else {
							bytes.Append( "  " );
						}

						if ( c >= 0x20 && c < 0x80 ) {
							chars.Append( (char)c );
						} else {
							chars.Append( '.' );
						}
					} else {
						bytes.Append( "   " );
					}
				}

				output.Write( byteIndex.ToString( "X4" ) );
				output.Write( "   " );
				output.Write( bytes.ToString() );
				output.Write( "  " );
				output.WriteLine( chars.ToString() );
			}
		}
    }


    public interface ISocketPacketExtension
    {
        UInt16 SecondaryID { get; }
    }

    public interface ISocketPacketRunUo
    {
        Object NetState { get; }
    }
}
