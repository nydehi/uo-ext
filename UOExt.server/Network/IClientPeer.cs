namespace UOExt.Network
{
    public interface IClientPeer
    {
        void Send(byte[] buffer);
        void Send(Packet p);
        void Close();
    }
}
