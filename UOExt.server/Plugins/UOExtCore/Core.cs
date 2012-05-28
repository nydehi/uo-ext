using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using UOExt.Plugins;
using UOExt.Network;


namespace UOExt.Plugins.UOExtCore
{
    public sealed class Core : CSharpServerPlugin
    {
        private LibraryList m_libraryList;
        private PluginsList m_pluginsList;
        private InitializationComplete m_initComplete;


        public override void UOExtInitialize(PacketHandler hndlr)
        {
            m_libraryList = new LibraryList();
            m_pluginsList = new PluginsList();
            m_initComplete = new InitializationComplete();
            
            hndlr.RegisterPacketHandler(0x00, 0, new OnPacketRecive(UOExtPacket));

            for (short i = 0; i < Dll.Dlls.Length; i++)
            {
                Dll.Dlls[i].Header = new DllHeader(Dll.Dlls[i], i);
                uint offset = (uint)(Dll.Dlls[i].Header.DllSize);
                do { 
                    DllContent dc = new DllContent(Dll.Dlls[i], offset);
                    offset += (uint)dc.DllChunkSize;
                    Dll.Dlls[i].Content.Enqueue(dc);
                } while (offset < Dll.Dlls[i].Size);
            }
            Dll.UOExt = new Dll(Config.UOExtPath, true);
            Dll.UOExtGUI = new Dll(Config.UOExtGUIPath, true);
        }

        private static bool ByteArrayCompare(byte[] source, byte[] dest)
        {
            if (source.Length != dest.Length) return false;
            for (int i = 0; i < source.Length; i++) if (source[i] != dest[i]) return false;
            return true;
        }

        public void UOExtPacket(IClientPeer peer, byte header, byte[] buffer, int offset, short length)
        {
            MemoryStream ms = new MemoryStream(buffer, offset, length);
            BinaryReader br = new BinaryReader(ms);

            byte sequence = br.ReadByte();

            switch (sequence)
            {
                case (0x00):
                    byte version = br.ReadByte();
                    byte[] uoextmd5 = br.ReadBytes(16);
                    byte[] uoextguimd5 = br.ReadBytes(16);
                    if (version != 0)
                    {
                        peer.Close();
                    }
                    else
                    {

                        if (!ByteArrayCompare(uoextmd5, Dll.UOExt.MD5))
                        {
                            peer.Send(new Handshake(0x01));
                            peer.Send(Dll.UOExt.SimpleHeader);
                            foreach (DllContent dc in Dll.UOExt.Content)
                            {
                                peer.Send(dc);
                            }
                            return;
                        }
                        else if (!ByteArrayCompare(uoextguimd5, Dll.UOExtGUI.MD5))
                        {
                            peer.Send(new Handshake(0x02));
                            peer.Send(Dll.UOExtGUI.SimpleHeader);
                            foreach (DllContent dc in Dll.UOExtGUI.Content)
                            {
                                peer.Send(dc);
                            }
                        }
                        else
                        {
                            peer.Send(new Handshake(0));
                        }
                        peer.Send(m_libraryList);
                        peer.Send(m_pluginsList);
                    }
                    break;
                case (0x03):
                    for (short i = 0; i < Dll.Dlls.Length; i++)
                    {
                        peer.Send(Dll.Dlls[i].Header);
                        foreach (DllContent dc in Dll.Dlls[i].Content)
                        {
                            peer.Send(dc);
                        }
                    }
                    peer.Send(m_initComplete);
                    break;
            }
        }
    }

    public struct Plugin
    {
        public readonly uint Id;
        public readonly byte PacketsAmount;
        public readonly string Name;

        public Plugin(uint id, byte amount, string name)
        {
            Id = id;
            PacketsAmount = amount;
            Name = name;
        }
    }

    public sealed class Dll
    {
        public static Dll[] Dlls;
        private static MD5CryptoServiceProvider m_md5Provider;
        private const uint PD_NAME = 0;
        private const uint PD_PACKETSAMOUNT = 1;
        private static Dll m_uoext;
        private static Dll m_uoextgui;

        public static Dll UOExt { get { return m_uoext; } set { m_uoext = value; } }
        public static Dll UOExtGUI { get { return m_uoextgui; } set { m_uoextgui = value; } }


        static Dll()
        {
            m_md5Provider = new MD5CryptoServiceProvider();

            if (!Directory.Exists(Config.ClientPluginsPath))
            {
                Dlls = new Dll[0];
                return;
            }
            string[] files = Directory.GetFiles(Config.ClientPluginsPath, "*.plg");

            Dlls = new Dll[files.Length];
            for (int i = 0; i < files.Length; i++)
            {
                Dlls[i] = new Dll(files[i]);
            }
        }

        public readonly byte[] Data;
        public readonly uint Size;
        public readonly byte[] MD5;
        public readonly string Name;
        public readonly Plugin[] Plugins;
        public DllHeader Header;
        public readonly SimpleDllHeader SimpleHeader;
        public Queue<DllContent> Content;

        [DllImport("kernel32.dll")]
        internal static extern IntPtr LoadLibrary(string dllname);

        [DllImport("kernel32.dll")]
        internal static extern IntPtr GetProcAddress(IntPtr hModule, string procName);

        [DllImport("kernel32.dll")]
        internal static extern bool FreeLibrary(IntPtr hModule);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate IntPtr DllInit();

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate void DllInitDone();

        public Dll(string path, bool simple = false)
        {
            if (!File.Exists(path))
                throw new FileNotFoundException();

            Data = File.ReadAllBytes(path);
            Size = (uint)Data.Length;
            Name = Path.GetFileNameWithoutExtension(path);
            MD5 = m_md5Provider.ComputeHash(Data);

            Content = new Queue<DllContent>();
            if (!simple)
            {
                Console.WriteLine("UOExt: Loading {0}", Path.GetFileNameWithoutExtension(path));
                IntPtr hModule = LoadLibrary(path);
                IntPtr ptrDllInit = GetProcAddress(hModule, "DllInit");
                IntPtr ptrDllInitDone = GetProcAddress(hModule, "DllInitDone");

                DllInit plgDllInit = (DllInit)Marshal.GetDelegateForFunctionPointer(ptrDllInit, typeof(DllInit));
                DllInitDone plgDllInitDone = (DllInitDone)Marshal.GetDelegateForFunctionPointer(ptrDllInitDone, typeof(DllInitDone));

                IntPtr plgs = plgDllInit();
                int plgs_count = Marshal.ReadInt32(plgs);
                Plugins = new Plugin[plgs_count];
                for (int i = 0; i < plgs_count; i++)
                {
                    IntPtr currentPlgInfo = new IntPtr(Marshal.ReadInt32(plgs, i * 4 + 4));
                    uint descriptors_count = (uint)(Marshal.ReadInt32(currentPlgInfo, 4));

                    byte packets = 0;
                    string name = null;
                    for (int j = 0; j < descriptors_count; j++)
                    {
                        uint key = (uint)(Marshal.ReadInt32(currentPlgInfo, j * 4 + 4));
                        uint value = (uint)(Marshal.ReadInt32(currentPlgInfo, j * 4 + 8));

                        switch (key)
                        {
                            case PD_NAME:
                                name = Marshal.PtrToStringAnsi(new IntPtr(value));
                                break;
                            case PD_PACKETSAMOUNT:
                                packets = (byte)value;
                                break;
                        }
                    }
                    Plugins[i] = new Plugin((uint)i, packets, name);
                    Console.WriteLine("UOExt:  {0}) Name: '{1}', Packets: {2}", i, name, packets);
                }


                plgDllInitDone();

                FreeLibrary(hModule);
            }else{
                SimpleHeader = new SimpleDllHeader(this);
                uint offset = (uint)SimpleHeader.DllSize;
                do { 
                    DllContent dc = new DllContent(this, offset);
                    offset += (uint)dc.DllChunkSize;
                    Content.Enqueue(dc);
                } while (offset < Size);
            }
        }
    }
}
