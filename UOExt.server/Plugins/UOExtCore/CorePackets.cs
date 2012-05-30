using System.Collections.Generic;
using UOExt.Network;

namespace UOExt.Plugins.UOExtCore
{
    public sealed class Handshake : Packet
    {
        /// <summary>
        /// UOExt handshake packet. Send "all ok"
        /// </summary>
        public Handshake(byte flags)
            : base(0x00, 2)
        {
            m_Writer.Write((byte)0x00);
            m_Writer.Write((byte)flags);
        }
    }

    public sealed class LibraryList : Packet
    {
        public LibraryList()
            : base(0x00)
        {
            EnsureCapacity((ushort)(Dll.Dlls.Length * 20 + 1));
            m_Writer.Write((byte)0x01);
            for (int i = 0; i < Dll.Dlls.Length; i++)
            {
                m_Writer.Write((uint)Dll.Dlls[i].Size);
                m_Writer.Write((byte[])Dll.Dlls[i].MD5, 0, 16);
            }
        }
    }

    public sealed class PluginsList : Packet
    {
        public PluginsList()
            : base(0x00)
        {
            EnsureCapacity((ushort)(Dll.LoadingOrder.Length * 4 + 1));
            m_Writer.Write((byte)0x02);
            
            for (int order = 0; order < Dll.LoadingOrder.Length; order++)
            {
                m_Writer.Write((ushort)Dll.LoadingOrder[order].Dll_Id);
                m_Writer.Write((byte)Dll.LoadingOrder[order].Id);
                m_Writer.Write((byte)Dll.LoadingOrder[order].PacketsAmount);
            }
        }
    }

    public sealed class DllHeader : Packet
    {
        public readonly ushort DllSize;

        public DllHeader(Dll dll, short pos)
            : base(0x00)
        {
            uint size = dll.Size;
            if (size > (65535 - 8)) size = 65535 - 8;
            DllSize = (ushort)size;

            EnsureCapacity((ushort)(DllSize + 3));

            m_Writer.Write((byte)0x03);
            m_Writer.Write((ushort)pos);
            m_Writer.Write((byte[])dll.Data, 0, DllSize);
        }
    }

    public sealed class SimpleDllHeader : Packet
    {
        public readonly ushort DllSize;

        public SimpleDllHeader(Dll dll)
            : base(0x00)
        {
            uint size = dll.Size;
            if (size > (65535 - 10)) size = 65535 - 10;
            DllSize = (ushort)size;

            EnsureCapacity((ushort)(DllSize + 5));

            m_Writer.Write((byte)0x05);
            m_Writer.Write((uint)dll.Size);
            m_Writer.Write((byte[])dll.Data, 0, DllSize);
        }
    }

    public sealed class DllContent : Packet
    {
        public readonly ushort DllChunkSize;
        public readonly uint DllOffset;

        public DllContent(Dll dll, uint offset)
            : base(0x00)
        {
            uint size = dll.Size - offset;
            if (size > (65535 - 6)) size = 65535 - 6;
            DllChunkSize = (ushort)size;
            DllOffset = offset;

            EnsureCapacity((ushort)(DllChunkSize + 1));

            m_Writer.Write((byte)0x04);
            m_Writer.Write((byte[])dll.Data, (int)DllOffset, DllChunkSize);
        }
    }

    public sealed class InitializationComplete : Packet
    {
        public InitializationComplete()
            : base(0x00, 1)
        {
            m_Writer.Write((byte)0xFF);
        }
    }

}
