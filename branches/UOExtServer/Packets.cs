using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using ICSharpCode.SharpZipLib.Checksums;
using UOExtDomain.Utilities;

namespace UOExtDomain.Network
{
    public sealed class ConnectionPacket : SocketPacket
    {
        public override PacketType Type
        { get { return PacketType.Incoming; } }

        public override Byte PrimaryID
        { get { return 0x01; } }

        public override Int32 DataSize
        { get { return 4; } }

        public  UInt32 ProtocolVer
        { get { return m_ProtocolVer; } }
        private UInt32 m_ProtocolVer;

        /// <summary>
        /// Текущая Версия протокола, нет поддержки старых версий
        /// </summary>
        public const UInt32 CurrentProtocolVer = 1;

        public ConnectionPacket() : base()
        {
        }

        public ConnectionPacket(uint protocolVer = CurrentProtocolVer) : base()
        {
            m_ProtocolVer = protocolVer;
            this.DataWriter.Write((uint)m_ProtocolVer);
        }

        public ConnectionPacket(byte[] rawdata) : base(rawdata)
        {
            m_ProtocolVer = this.DataReader.ReadUInt32();
        }

        public override void OnReceive(SocketClient client)
        {
 	        base.OnReceive(client);
            #if !DEBUG
            Console.WriteLine("[uoexts] {0} recived Packet: 0x{1:X2} <Ver = {2}>", client.IpAddress, PrimaryID, ProtocolVer);
            #endif

            if (ProtocolVer != CurrentProtocolVer) {
                client.Send(new ErrorPacket(PrimaryID, ErrorPacket.ErrorCode.BadPacketsVer | ErrorPacket.ErrorCode.ClosingSocket));
                client.Disconnect();
            } else {
                var packet = new SendPluginsPacket("UOExtPlugins");
                //client.Send(new ErrorPacket(PrimaryID, ErrorPacket.ErrorCode.BadPacketsVer | ErrorPacket.ErrorCode.ClosingSocket));
                client.Send(SendPluginsPacket.Instance);
            }
        }
    }

    public sealed class SendPluginsPacket : SocketPacket
    {
        public override PacketType Type
        { get { return PacketType.Outgoing; } }

        public override Byte PrimaryID
        { get { return 0x02; } }

        public override Int32 DataSize
        { get { return -1; } }
         
        /// <summary>
        /// Используемый компрессор для сжатия данных
        /// </summary>
        private static readonly ICompressor Compressor = new Compressor(0x00);

        public SendPluginsPacket() : base()
        {
        }

        public SendPluginsPacket(string folder = "UOExtPlugins", string searchMask = @"Plugin-0x??.dll"): 
            this(Directory.GetFiles(Path.IsPathRooted(folder) ? folder : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, folder), searchMask))
        {            
        }

        private readonly byte DllAmount;

        public SendPluginsPacket(string[] files, string folder = null) : base()
        {
            DllAmount = (byte)files.Length;
            var data = new List<byte[]>(files.Length);
            if (files.Length > Byte.MaxValue)
                throw new ArgumentOutOfRangeException(String.Format("Пакет позваляет передать не более {0} плагинов.", Byte.MaxValue));
            this.DataWriter.Write((byte)Compressor.TypeId);          // 1 Byte - Packet Flags (Compression Type)
            this.DataWriter.Write((byte)files.Length);               // 1 Byte - Dll Amount
            foreach (var _file in files)                             // LOOP <Dll Amount> Times
            {
                var file = String.IsNullOrEmpty(folder) ? _file : Path.Combine(folder, _file);
                if (!File.Exists(file))
                    throw new Exception(String.Format("Файл: \"{0}\" не существует.", file));
                
                var reader = new BinaryReader(new FileStream(file, FileMode.Open));
                var bytes  = (byte[])reader.ReadBytes((int)reader.BaseStream.Length);
                reader.Close();
                reader.Dispose();

                var FileSize = (uint)bytes.Length;
                var crc32  = new Crc32();
                crc32.Update(bytes);
                bytes = Compressor.Compress(bytes, (uint)bytes.Length / 2);
                data.Add(bytes);

                this.DataWriter.Write((uint)bytes.Length);          // 4 Bytes Data Size
                this.DataWriter.Write((uint)FileSize);              // 4 Bytes Dll Size
                this.DataWriter.Write((uint)crc32.Value);           // 4 Bytes Dll CRC32
            }
            foreach (var bytes in data) {                           // LOOP <Dll Amount> Times
                this.DataWriter.Write((byte[])bytes);               // Bytes Dll content
            }

            m_Instance = this;
        }

        public SendPluginsPacket(byte[] rawdata) : base(rawdata)
        {
        }

        public override void OnSend(SocketClient client)
        {
            base.OnSend(client);
            #if !DEBUG
            Console.WriteLine("[uoexts] {0} sended Packet: 0x{1:X2} <{2} plugins, flags = 0x{3:X2}>", client.IpAddress, PrimaryID, DllAmount, Compressor.TypeId);
            #endif
        }

        public override void OnReceive(SocketClient client)
        {
            #if TESTCENTER
 	        base.OnReceive(client);
            var compressorId = this.DataReader.ReadByte();
            ICompressor compressor = new Compressor(compressorId);
            var dllAmount = this.DataReader.ReadByte();
            var dataSize  = new uint[dllAmount];
            var dllSize   = new uint[dllAmount];
            var dllCrc32  = new uint[dllAmount];
            for(var i = 0; i < dllAmount; ++i) {
                dataSize[i] = this.DataReader.ReadUInt32();
                dllSize[i]  = this.DataReader.ReadUInt32();
                dllCrc32[i] = this.DataReader.ReadUInt32();
            }
            for(var i = 0; i < dllAmount; ++i) {
                var bytes = this.DataReader.ReadBytes((int)dataSize[i]);
                bytes = compressor.Decompress(bytes, dllSize[i]);
                var crc32  = new Crc32();
                crc32.Update(bytes);
                if ((uint)crc32.Value != dllCrc32[i])
                    throw new Exception("Контрольная сумма CRC32 не совпадает!");
                if ((uint)bytes.Length != dllSize[i])
                    throw new Exception("Размер файла не совпадает!");
                var file = File.Create(Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, @"UOExtPlugins-new", String.Format(@"Plugin-0x{0:X2}.dll", i)));
                file.Write(bytes, 0, bytes.Length);
                file.Flush();
                file.Close();
            }
            #endif
        }

        public static SendPluginsPacket Instance
        { get { return m_Instance ?? new SendPluginsPacket("UOExtPlugins"); } }
        private static SendPluginsPacket m_Instance = null;
    }

    public sealed class ErrorPacket : SocketPacket
    {
        public override PacketType Type
        { get { return PacketType.Outgoing; } }

        public override Byte PrimaryID
        { get { return 0xF0; } }

        public override Int32 DataSize
        { get { return 8; } }

        public ErrorCode ErrorID
        { get { return m_ErrorID; } }
        private ErrorCode m_ErrorID;

        public Byte ErrorInPrimaryID
        { get { return m_ErrorInPrimaryID; } }
        private Byte m_ErrorInPrimaryID;

        public Boolean IsExtensionPacket
        { get { return m_IsExtensionPacket; } }
        private Boolean m_IsExtensionPacket;

        public UInt16 ErrorInSecondaryID
        { get { return m_ErrorInSecondaryID; } }
        private UInt16 m_ErrorInSecondaryID;
       
        public ErrorPacket() : base()
        {
        }

        public ErrorPacket(Byte primaryID, ErrorCode error) : this(primaryID, false, 0, error)
        {
        }

        public ErrorPacket(Byte primaryID, UInt16 secondaryID, ErrorCode error) : this(primaryID, true, secondaryID, error)
        {
        }

        private ErrorPacket(Byte primaryID, Boolean isExtensionPacket, UInt16 secondaryID, ErrorCode error)
        {
            #if TESTCENTER
            m_ErrorInPrimaryID   = primaryID;
            m_IsExtensionPacket  = isExtensionPacket;
            m_ErrorInSecondaryID = secondaryID;
            m_ErrorID            = error;
            this.DataWriter.Write((uint)error);               // 4 Bytes - Error Codes
            this.DataWriter.Write((byte)primaryID);           // 1 Byte  - Packet PrimaryID
            this.DataWriter.Write((bool)isExtensionPacket);   // 1 Byte  - is Extension Packet
            this.DataWriter.Write((ushort)secondaryID);       // 2 Byte  - Packet SecondaryID
            #endif
        }

        public override void OnSend(SocketClient client)
        {
            base.OnSend(client);
            #if !DEBUG
            Console.WriteLine("[uoexts] {0} sended Packet: 0x{1:X2} <ErrorCode = 0x{2:X8} in packet = 0x{3:X2}>", client.IpAddress, PrimaryID, ErrorID, ErrorInPrimaryID);
            #endif

            if (ErrorID.HasFlag(ErrorCode.ClosingSocket))
                client.Disconnect();
        }

        public override void OnReceive(SocketClient client)
        {
            base.OnReceive(client);
            #if TESTCENTER
            if (ErrorID.HasFlag(ErrorCode.ClosingSocket))
                client.Disconnect();
            #endif
        }

        [FlagsAttribute]
        public enum ErrorCode : uint
        {
            None            = 0x00000000,

            /// <summary>
            /// Был получен не известный пакет
            /// (Также возможно ошибка парсинга)
            /// </summary>
            UnknownPacket   = 0x00000001,

            /// <summary>
            /// Был получен не поддерживаемые пакет
            /// (Ошибка протокола, получен не поддерживаемый пакет)
            /// </summary>
            PacketTypeErr   = 0x00000002,

            /// <summary>
            /// Не поддерживаемая версия протокола
            /// (Конфликт версии клиент/сервера)
            /// </summary>
            BadPacketsVer   = 0x00000004,

            /// <summary>
            /// Уведомление о закрытии сокета (т.е. соединения)
            /// </summary>
            ClosingSocket   = 0xF0000000,
        }
    }

    #if DEBUG

    public sealed class TestStaticPacket : SocketPacket
    {
        public override PacketType Type
        { get { return PacketType.All_mode; } }

        public override Byte PrimaryID
        { get { return 0xE1; } }

        public override Int32 DataSize
        { get { return 4; } }

        public Int32 Value
        { get { return m_Value; } }
        private Int32 m_Value;

        public TestStaticPacket() : base()
        {
        }

        public TestStaticPacket(Int32 value) : base()
        {
            m_Value = value;
            this.DataWriter.Write((int)m_Value);
        }

        public TestStaticPacket(byte[] rawdata) : base(rawdata)
        {
            m_Value = this.DataReader.ReadInt32();
        }

        public override void OnReceive(SocketClient client)
        {
            base.OnReceive(client);
            var value = m_Value;
            #if !IGNOREPACKETTYPE
                value = -value;
            #else
                value += value < 0 ? -1 : +1;
            #endif

            if (Math.Abs(value) < 1000000 ? (Math.Abs(value) >= 10) : Math.Abs(value) - 1000000 >= 10) {
                Console.ForegroundColor = ConsoleColor.DarkRed;
                Console.WriteLine("VALUE Riched limit -> break");
                Console.ResetColor();
                return;
            }

            client.Send(new TestStaticPacket(value));
            Console.ForegroundColor = ConsoleColor.DarkGreen;
            Console.WriteLine("VALUE \t Received: {0} \t Sended: {1}", m_Value, value);
            Console.ResetColor();
        }
    }

    public sealed class TestDinamicPacket : SocketPacket
    {
        public override PacketType Type
        { get { return PacketType.All_mode; } }

        public override Byte PrimaryID
        { get { return 0xE2; } }

        public override Int32 DataSize
        { get { return -1; } }

        public String Value
        { get { return m_Value; } }
        private String m_Value;

        public TestDinamicPacket() : base()
        {
        }

        public TestDinamicPacket(String value) : base()
        { 
            m_Value = value;
            var bytes = Encoding.GetEncoding(1251).GetBytes(m_Value);
            this.DataWriter.Write((byte[])bytes);
        }

        public TestDinamicPacket(byte[] rawdata) : base(rawdata)
        {
            var bytes = this.DataReader.ReadBytes((int)this.DataReader.BaseStream.Length);
            m_Value = Encoding.GetEncoding(1251).GetString(bytes);
        }

        public override void OnReceive(SocketClient client)
        {
            base.OnReceive(client);
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine("STRING \t Received: \"{0}\"", m_Value);
            Console.ResetColor();
        }
    }

    #endif
}
