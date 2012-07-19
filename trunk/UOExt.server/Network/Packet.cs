using System;
using System.IO;
using System.Text;

namespace UOExt.Network
{
    public abstract class Packet : IDisposable
    {
        private Stream RawDataStream = new MemoryStream();
        private byte[] m_Buffer;
        protected BinaryWriter m_Writer;
        private MemoryStream m_Stream;
        private byte m_Header;

        public Packet(byte header, ushort size)
        {
            m_Header = header;
            EnsureCapacity(size);
        }

        public Packet(byte header){
            m_Header = header;
        }

        public void EnsureCapacity(ushort size){
            if (Config.InRunUO) size++;
            size += 3;
            m_Buffer = new byte[size];
            m_Stream = new MemoryStream(m_Buffer);
            m_Writer = new BinaryWriter(m_Stream);
            if (Config.InRunUO) m_Writer.Write((byte)Config.EncapsulationHeader);
            m_Writer.Write((ushort)size);
            m_Writer.Write((byte)m_Header);
        }

        public void Send(IClientPeer p) 
        {
            m_Writer.Flush();
            p.Send(m_Buffer);
            this.OnSend(p);

        }

        public virtual void OnSend(IClientPeer p)
        {
            #if DEBUG
            Console.WriteLine("Sending to client packet with length {0}", m_Buffer.Length);
            DumpArray(Console.Out, m_Buffer);
            #endif
        }

        public void Dispose()
        {
        }

        public static void DumpArray(TextWriter output, byte[] buffer)
        {
            DumpArray(output, buffer, 0, 0);
        }
        public static void DumpArray(TextWriter output, byte[] buffer, int offset, int size)
        {
            output.WriteLine("00000000 | 00 01 02 03  04 05 06 07  08 09 0A 0B  0C 0D 0E 0F | 0123456789ABCDEF");
            int length = buffer.Length - offset;
            if (size == 0) size = buffer.Length - offset;
            size += offset;
            int locOffset = 0;
            do
            {
                if ((locOffset % 16) == 0)
                {
                    output.Write(locOffset.ToString("X8") + " |");
                }
                output.Write(" " + buffer[locOffset + offset].ToString("X2"));
                if (((offset + 1) % 4) == 0)
                {
                    output.Write(" ");
                }
                if ((offset % 16) == 15)
                {
                    output.WriteLine("| ");
                }
                locOffset++;
            } while ((offset + locOffset) < size);
            if ((locOffset % 16) != 15)
            {
                output.WriteLine();
            }
        }
    }
}
