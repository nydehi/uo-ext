using Server.Network;
using UOExt;
using UOExt.Network;

namespace Scripts.UOExtAdapter
{
    public class NetStateAdapter : IClientPeer
    {
        private sealed class UOExtEncapsulation : Server.Network.Packet
        {
            public UOExtEncapsulation(byte[] packet)
                : base(Config.EncapsulationHeader)
            {
                EnsureCapacity(packet.Length);
                m_Stream.Write(packet, 3, packet.Length - 3);
            }
        }

        private NetState m_ns;
        public NetStateAdapter(NetState ns)
        {
            m_ns = ns;

        }

        #region IClientPeer
        public void Send(UOExt.Network.Packet p)
        {
            p.Send(this);
        }

        public void Send(byte[] buffer)
        {
            m_ns.Send(new UOExtEncapsulation(buffer));
//            m_ns.Flush();
        }

        public void Close()
        {
            m_ns.Dispose(true);
        }
        #endregion
    }

}
