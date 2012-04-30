using System;
using Server.Network;

namespace Scripts.Warstone.UOExt
{
    public class UOExt
    {
        // Protocol header that will contain UOExt protocol
        private static byte m_EncapsulationHeader = 0xFF;

        // IP for update and technical data server. 0 - same as game server
        private static uint m_UpdateIP = 0;

        // Port for update and technical data server. 0 - same as game server
        private static short m_UpdatePort = 0;

        // If update server located somwhere else - keep connected
        private static bool m_PersistentConnection = false;

        // Game server wants encrypted protocol?
        private static bool m_Encrypted = false;

        // Crush client on any plugin load fail
        private static bool m_ForceCrushOnPluginError = true;


        private static PacketHandler m_OldEFHandler;
        private static Packet m_UOExtSupport;
        public static void Initialize() {
            m_OldEFHandler = PacketHandlers.GetHandler(0xEF);
            PacketHandlers.Register(0xEF, 21, false, new OnPacketReceive(LoginServerSeed));

            m_UOExtSupport = new UOExtSupport(m_EncapsulationHeader, m_UpdateIP, m_UpdatePort, GetServerFlags());
        }

        private static byte GetServerFlags() { 
            byte flag = 0;

            if (m_Encrypted) {
                flag |= 0x01;
            }
            if (m_PersistentConnection) {
                flag |= 0x02;
            }
            if (m_ForceCrushOnPluginError) {
                flag |= 0x04;
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
                // This is UOExt. Cheers!
                state.Send(m_UOExtSupport);
                state.Flush();
                return;
            }

            // Enroute to old EF handler
            pvSrc.Seek(0, System.IO.SeekOrigin.Begin);
            m_OldEFHandler.OnReceive(state, pvSrc);

        }

        private sealed class UOExtSupport : Packet
        {
            public UOExtSupport(byte encapsulation, uint ip, short port, byte flags)
                : base(0x00)
            {
                EnsureCapacity(11);
                m_Stream.Write((byte)encapsulation);
                m_Stream.Write((uint)ip);
                m_Stream.Write((short)port);
                m_Stream.Write((byte)flags);
            }
        }

    }
}
