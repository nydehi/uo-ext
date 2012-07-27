using System;
using System.Xml;
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
        private EFAnswer m_efpacket;


        public override void UOExtInitialize(PacketHandler hndlr)
        {
            m_libraryList = new LibraryList();
            m_pluginsList = new PluginsList();
            m_initComplete = new InitializationComplete();
            m_efpacket = null;

            hndlr.RegisterPacketHandler(0x00, 0, new OnPacketRecive(UOExtPacket));

            for (short i = 0; i < Dll.Dlls.Length; i++)
            {
                Dll.Dlls[i].Header = new DllHeader(Dll.Dlls[i], i);
                uint offset = (uint)(Dll.Dlls[i].Header.DllSize);
                do
                {
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
                case (0xFE):
                    if(m_efpacket == null)
                    {
                        m_efpacket = new EFAnswer();
                    }
                    peer.Send(m_efpacket);
                    break;
            }
        }
    }

    public struct Plugin
    {
        public readonly byte Id;
        public readonly byte PacketsAmount;
        public readonly string Name;
        public readonly ushort Dll_Id;

        public Plugin(ushort dll_id, byte id, byte amount, string name)
        {
            Dll_Id = dll_id;
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
        private static Plugin[] m_loadingOrder;

        public static Dll UOExt { get { return m_uoext; } set { m_uoext = value; } }
        public static Dll UOExtGUI { get { return m_uoextgui; } set { m_uoextgui = value; } }
        public static Plugin[] LoadingOrder { get { return m_loadingOrder; } }


        static Dll()
        {
            m_md5Provider = new MD5CryptoServiceProvider();
            if (Config.Is64Bit || Config.IsUnix)
            {
                Console.WriteLine("UOExt: 64bit or Unix env. Plugins info will be loaded from XML.");
            }


            if (!Directory.Exists(Config.ClientPluginsPath))
            {
                Dlls = new Dll[0];
                m_loadingOrder = new Plugin[0];
                return;
            }
            if (!File.Exists(Config.PluginInitOrderFile))
            {
                ConsoleColor cc = Console.ForegroundColor;
                Console.ForegroundColor = ConsoleColor.Yellow;
                Console.WriteLine("UOExt: Warning: No plugin init order file found. Loading all plugins.");
                Console.WriteLine("UOExt:           Path '{0}'", Config.PluginInitOrderFile);
                Console.ForegroundColor = cc;

                string[] files = Directory.GetFiles(Config.ClientPluginsPath, "*.plg");

                Dlls = new Dll[files.Length];
                for (int i = 0; i < files.Length; i++)
                {
                    Dlls[i] = new Dll(files[i], (ushort)i);
                }

                Queue<Plugin> order = new Queue<Plugin>();
                for (int i = 0; i < Dlls.Length; i++)
                    for (int j = 0; j < Dlls[i].Plugins.Length; j++) order.Enqueue(Dlls[i].Plugins[j]);
                m_loadingOrder = order.ToArray();

            }
            else
            {
                string[] info = File.ReadAllLines(Config.PluginInitOrderFile);

                Dictionary<string, Dll> dlls = new Dictionary<string,Dll>();
                Queue<Plugin> order = new Queue<Plugin>();

                ushort dll_id = 0;

                char[] splitter = new char[1];
                splitter[0] = ',';
                for (int i = 0; i < info.Length; i++)
                {
                    string[] row = info[i].Split(splitter, 2);
                    row[0] = row[0].Trim().ToUpper();

                    if (row[0][0] == ';') continue;
                    if (row.Length < 2)
                    {
                        ConsoleColor cc = Console.ForegroundColor;
                        Console.ForegroundColor = ConsoleColor.Yellow;
                        Console.WriteLine("UOExt: Warning: Illegal string found in plugin init order file. Skipping.");
                        Console.WriteLine("UOExt:           String: '{0}'", info[i]);
                        Console.ForegroundColor = cc;
                        continue;
                    }

                    row[1] = row[1].Trim();
                    ushort current_id;
                    if (!dlls.ContainsKey(row[0]))
                        dlls[row[0]] = new Dll(row[0], dll_id++);

                    current_id = dlls[row[0]].Id;

                    int number;
                    bool is_number = int.TryParse(row[1], out number);
                    for (int j = 0; j < dlls[row[0]].Plugins.Length; j++)
                        if (((is_number) && (number == dlls[row[0]].Plugins[j].Id)) || ((!is_number) && (dlls[row[0]].Plugins[j].Name == row[1])))
                        {
                            order.Enqueue(dlls[row[0]].Plugins[j]);
                            break;
                        }
                        
                }

                Dlls = new Dll[dlls.Count];
                foreach (Dll dll in dlls.Values)
                    Dlls[dll.Id] = dll;

                m_loadingOrder = order.ToArray();
            }
        }

        private static Plugin[] ReadLoadingOrder()
        {
            if (!File.Exists(Config.PluginInitOrderFile)) return new Plugin[0];

            string[] info = File.ReadAllLines(Config.PluginInitOrderFile);
            int loaded = 0;
            Plugin[] order = new Plugin[info.Length];

            char[] splitter = new char[1];
            splitter[0] = ',';
            for (int i = 0; i < info.Length; i++)
            {
                string[] row = info[i].Split(splitter, 2);
                row[0] = row[0].Trim();
                row[1] = row[1].Trim();
                Dll c_dll = null;
                for (int j = 0; j < Dlls.Length; j++)
                {
                    if (Dlls[j].Name == row[0])
                    {
                        c_dll = Dlls[j];
                        break;
                    }
                }
                if (c_dll != null)
                {
                    int number;
                    bool is_number = int.TryParse(row[1], out number);
                    for (int j = 0; j < c_dll.Plugins.Length; j++)
                    {
                        if (((is_number) && (number == c_dll.Plugins[j].Id)) || ((!is_number) && (row[1] == c_dll.Plugins[j].Name)))
                        {
                            order[loaded++] = c_dll.Plugins[j];
                            break;
                        }
                    }
                }
            }
            if (loaded != order.Length)
            {
                Plugin[] tmp = new Plugin[loaded];
                Array.Copy(order, tmp, loaded);
                order = tmp;
            }
            return order;
        }

        public readonly byte[] Data;
        public readonly uint Size;
        public readonly byte[] MD5;
        public readonly string Name;
        public readonly Plugin[] Plugins;
        public readonly ushort Id;
        public DllHeader Header;
        public readonly SimpleDllHeader SimpleHeader;
        public Queue<DllContent> Content;

        [DllImport("kernel32.dll")]
        internal static extern IntPtr LoadLibrary(string dllname);

        [DllImport("kernel32.dll")]
        internal static extern IntPtr GetProcAddress(IntPtr hModule, string procName);

        [DllImport("kernel32.dll")]
        internal static extern bool FreeLibrary(IntPtr hModule);

        [DllImport("kernel32.dll")]
        internal static extern uint GetLastError();

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate IntPtr DllInit();

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate void DllInitDone();

        public Dll(string path)
        {
            if (!File.Exists(path))
                throw new FileNotFoundException();

            Data = File.ReadAllBytes(path);
            Size = (uint)Data.Length;
            Name = Path.GetFileNameWithoutExtension(path);
            MD5 = m_md5Provider.ComputeHash(Data);
            Content = new Queue<DllContent>();
        }

        public Dll(string path, bool simple)
            : this(path)
        {
            SimpleHeader = new SimpleDllHeader(this);
            uint offset = (uint)SimpleHeader.DllSize;
            do
            {
                DllContent dc = new DllContent(this, offset);
                offset += (uint)dc.DllChunkSize;
                Content.Enqueue(dc);
            } while (offset < Size);
        }

        public Dll(string path, ushort dll_id)
            : this(path)
        {
            Id = dll_id;
            Console.WriteLine("UOExt: Loading {0} ({1})", Path.GetFileNameWithoutExtension(path), path);
            if (Config.Is64Bit || Config.IsUnix)
            {
                if (!File.Exists(path + ".xml"))
                {
                    throw new FileNotFoundException("Can't find {0}", path + ".xml");
                }
                XmlTextReader xml = new XmlTextReader(path + ".xml");
                xml.WhitespaceHandling = WhitespaceHandling.None;
                string name = "";
                byte packets = 0;
                while (xml.Read())
                {
                    if (xml.NodeType == XmlNodeType.Element)
                    {
                        if (xml.Name == "Plugin")
                        {
                            uint id = UInt32.Parse(xml.GetAttribute("Id"));
                            uint amount = UInt32.Parse(xml.GetAttribute("Count"));
                            Plugins = new Plugin[amount];
                            while (xml.Read() && xml.Name == "Descriptor")
                            {
                                uint descr = UInt32.Parse(xml.GetAttribute("Id"));
                                string value = xml.GetAttribute("Value");
                                switch (descr)
                                {
                                    case PD_NAME:
                                        name = value;
                                        break;
                                    case PD_PACKETSAMOUNT:
                                        packets = Byte.Parse(value);
                                        break;

                                }
                            }
                            Plugins[id] = new Plugin(dll_id, (byte)id, packets, name);
                            Console.WriteLine("UOExt:  {0}) Name: '{1}', Packets: {2}", id, name, packets);
                        }
                    }
                }

            }
            else
            {
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
                    Plugins[i] = new Plugin(dll_id, (byte)i, packets, name);
                    Console.WriteLine("UOExt:  {0}) Name: '{1}', Packets: {2}", i, name, packets);
                }


                plgDllInitDone();

                FreeLibrary(hModule);
            }
        }
    }
}
