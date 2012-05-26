using System;
using System.Collections;
using System.Reflection;
using UOExt.Network;
using UOExt.Plugins;

namespace UOExt
{
    public delegate void OnPacketRecive(IClientPeer peer, byte header, byte[] buffer, int offset, short length);
    public class PacketHandler
    {
        private struct PacketData
        {
            private OnPacketRecive m_handler;
            private short m_length;
            public OnPacketRecive Handler { get { return m_handler; } }
            public short Length { get { return m_length; } }

            public PacketData(OnPacketRecive handler, short length)
            {
                m_handler = handler;
                m_length = length;
            }
        }

        public static PacketHandler Instatinate(){
            PacketHandler hndlr = new PacketHandler();
            Assembly ass = Assembly.GetCallingAssembly();
            Type[] types = ass.GetTypes();
            for (int i = 0; i < types.Length; i++)
            {
                if (types[i].IsSubclassOf(typeof(CSharpServerPlugin)))
                {
                    CSharpServerPlugin plg = (CSharpServerPlugin)(types[i]).GetConstructor(Type.EmptyTypes).Invoke(null);
                    plg.UOExtInitialize(hndlr);
                    hndlr.m_Plugins.Add(hndlr);
                }
            }
            return hndlr;
        }

        private PacketData[] m_Handlers;
        private ArrayList m_Plugins;

        public PacketHandler()
        {
            m_Handlers = new PacketData[0x100];
            m_Plugins = new ArrayList();
        }
        public void RegisterPacketHandler(byte header, short length, OnPacketRecive onRecive)
        {
            m_Handlers[header] = new PacketData(onRecive, length);
        }

        public void ProcessBuffer(IClientPeer peer, byte header, byte[] buffer, int offset, short length)
        {
            short packetSize = GetPacketLength(header);
            if ((packetSize != length)&&(packetSize != 0))
            {
                // Big error here!
            }
            OnPacketRecive handler = GetPacketHandler(header);
            if (handler != null)
                handler(peer, header, buffer, offset, length);
        }

        public OnPacketRecive GetPacketHandler(byte header)
        {
            return m_Handlers[header].Handler;
        }

        public short GetPacketLength(byte header)
        {
            return m_Handlers[header].Length;
        }
    }
}
