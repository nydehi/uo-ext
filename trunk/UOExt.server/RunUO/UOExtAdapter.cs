using System;
using System.Net;
using Server;
using Server.Network;
using UOExt;

namespace Scripts.UOExtAdapter
{
    public class Adapter
    {
        // Game server wants encrypted protocol?
        private static bool m_Encrypted = false;

        private static Server.Network.PacketHandler m_OldEFHandler;
        private static Server.Network.Packet m_UOExtSupport;

        private static UOExt.PacketHandler m_handler;

        public static void Initialize()
        {
            m_OldEFHandler = PacketHandlers.GetHandler(0xEF);
            PacketHandlers.Register(0xEF, 21, false, new Server.Network.OnPacketReceive(LoginServerSeed));
            PacketHandlers.Register(Config.EncapsulationHeader, 0, false, new Server.Network.OnPacketReceive(UOExtPacket));

            uint ip = (uint)(BitConverter.ToInt32(IPAddress.Parse(Config.IP).GetAddressBytes(), 0));

            m_UOExtSupport = new UOExtSupport(Config.EncapsulationHeader, ip, (short)Config.Port, GetServerFlags());
            m_UOExtSupport.SetStatic();

            m_handler = UOExt.PacketHandler.Instatinate();

            Console.WriteLine("UOExt: Adapter started.");
        }

        public static void Configure()
        {
            Config.IsUnix = Server.Core.Unix;
            Config.Is64Bit = Server.Core.Is64Bit;
        }

        private static byte GetServerFlags()
        {
            byte flag = 0;

            if (m_Encrypted)
            {
                flag |= 0x01;
            }
            if (Config.ExternalServerInRunUO)
            {
                flag |= 0x02;
            }

            return flag;
        }

        public static void LoginServerSeed(NetState state, PacketReader pvSrc)
        {
            int seed = pvSrc.ReadInt32(); // Seed

            int clientMaj = pvSrc.ReadInt32();
            int clientMin = pvSrc.ReadInt32();
            int clientRev = pvSrc.ReadInt32();
            int clientPat = pvSrc.ReadInt32();

            if ((seed == 0) && (clientMaj == 0) && (clientMin == 0) && (clientPat == 0) && (clientRev == 0))
            {
                state.SentFirstPacket = true;
                // This is UOExt. Cheers!
                state.Send(m_UOExtSupport);
                state.Flush();
                return;
            }

            // Enroute to old EF handler
            pvSrc.Seek(0, System.IO.SeekOrigin.Begin);
            m_OldEFHandler.OnReceive(state, pvSrc);

        }

        public static void UOExtPacket(NetState state, PacketReader pvSrc)
        {
            byte header = pvSrc.ReadByte();
            int position = pvSrc.Seek(0, System.IO.SeekOrigin.Current);
            m_handler.ProcessBuffer(new NetStateAdapter(state), header, pvSrc.Buffer, position, (short)(pvSrc.Size - 4));
        }

        private sealed class UOExtSupport : Server.Network.Packet
        {
            public UOExtSupport(byte encapsulation, uint ip, short port, byte flags)
                : base(0x00)
            {
                int size = 5;
                if (Config.ExternalServerInRunUO)
                {
                    size += 6;
                }
                EnsureCapacity(size);
                m_Stream.Write((byte)flags);
                m_Stream.Write((byte)encapsulation);
                if (Config.ExternalServerInRunUO)
                {
                    m_Stream.Write((uint)ip);
                    m_Stream.Write((short)port);
                    
                }
            }
        }
    }
}
