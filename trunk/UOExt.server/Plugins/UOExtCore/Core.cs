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
        private Handshake m_handshakepacket;
        private LibraryList m_libraryList;
        private PluginsList m_pluginsList;
        private InitializationComplete m_initComplete;


        public override void UOExtInitialize(PacketHandler hndlr)
        {
            m_handshakepacket = new Handshake();
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
                    if (version != 0)
                    {
                        peer.Close();
                    }
                    else
                    {
                        peer.Send(m_handshakepacket);
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

        public Plugin(uint id, byte amount)
        {
            Id = id;
            PacketsAmount = amount;
        }
    }

    public sealed class Dll
    {
        public static Dll[] Dlls;
        private static MD5CryptoServiceProvider m_md5Provider;
        private static uint PD_PACKETSAMOUNT = 1;
        static Dll()
        {
            if (!Directory.Exists(Config.ClientPluginsPath))
            {
                Dlls = new Dll[0];
                return;
            }
            string[] files = Directory.GetFiles(Config.ClientPluginsPath, "*.plg");

            m_md5Provider = new MD5CryptoServiceProvider();
            Dlls = new Dll[files.Length];
            for (int i = 0; i < files.Length; i++)
            Dlls[i] = new Dll(files[i]);
        }

        public readonly byte[] Data;
        public readonly uint Size;
        public readonly byte[] MD5;
        public readonly string Name;
        public readonly Plugin[] Plugins;
        public DllHeader Header;
        public Queue<DllContent> Content;

        public Dll(string path)
        {
            if (!File.Exists(path))
                throw new FileNotFoundException();

            Data = File.ReadAllBytes(path);
            Size = (uint)Data.Length;
            Name = Path.GetFileNameWithoutExtension(path);
            MD5 = m_md5Provider.ComputeHash(Data);

            IntPtr plgs = new IntPtr((Int32)CallFunction(Name, "DllInit", typeof(Int32), null));
            int plgs_count = Marshal.ReadInt32(plgs);
            Plugins = new Plugin[plgs_count];
            for (int i = 0; i < plgs_count; i++)
            {
                IntPtr currentPlgInfo = new IntPtr(Marshal.ReadInt32(plgs, i * 4));
                uint descriptors_count = (uint)(Marshal.ReadInt32(currentPlgInfo, 4));
                IntPtr descr = new IntPtr(Marshal.ReadInt32(plgs, i * 4 + 8));
                for (int j = 0; j < descriptors_count; j++)
                {
                    uint key = (uint)(Marshal.ReadInt32(descr));
                    uint value = (uint)(Marshal.ReadInt32(descr, 4));
                    if (key == PD_PACKETSAMOUNT)
                    {
                        Plugins[i] = new Plugin((uint)i, (byte)value);
                        break;
                    }
                }
            }
            CallFunction(Name, "DllInitDone", null, null);

            Content = new Queue<DllContent>();
        }

        private static object CallFunction(string dllName, string entryPoint, Type returnType, object[] args)
        {
            var myAssemblyName = new AssemblyName("TempAssembly");
            var myAssemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(myAssemblyName, AssemblyBuilderAccess.Run);
            var myModuleBuilder = myAssemblyBuilder.DefineDynamicModule("TempModule");

            var paramTypes = (args == null) ? null : Type.GetTypeArray(args);
            var piMethodBuilder = myModuleBuilder.DefinePInvokeMethod(
               entryPoint, dllName,
               MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
               CallingConventions.Standard,
               returnType, paramTypes,
               CallingConvention.Winapi, CharSet.Ansi);

            piMethodBuilder.SetImplementationFlags(piMethodBuilder.GetMethodImplementationFlags() | MethodImplAttributes.PreserveSig);
            myModuleBuilder.CreateGlobalFunctions();
            var pinvokeMethod = myModuleBuilder.GetMethod(entryPoint);
            return pinvokeMethod.Invoke(null, args);
        }
    
    }
}
