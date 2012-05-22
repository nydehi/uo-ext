using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using System.Text;

namespace UOExtDomain
{
    [StructLayout(LayoutKind.Sequential, Pack = 1, Size = 8)]
    public struct PluginDescriptor {	        //			Пока не поддерживается
	    public uint				    descript;
	    public uint				    value;
    };

    public struct PluginInfo {
	    public IntPtr			    dllmain;	//			Главная функция плагина
	//  public uint				    descount;	// = 0		Число дескрипторов
        public PluginDescriptor[]   descript;	// = null	Дескрипторы
    };

    //public struct DllPlugins {
    //    public uint count;		// = 1		Используем только 1 плагин в *.dll
    //    public PluginInfo[]  		plugins;
    //};


    public sealed class Plugin
    {
        public static List<Plugin> Plugins;
        static Plugin()
        {
            var files = Directory.GetFiles(Path.IsPathRooted(UOExtServer.m_PluginsPath) ? UOExtServer.m_PluginsPath
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, UOExtServer.m_PluginsPath), UOExtServer.m_SearchMask);

            _md5Provider = new MD5CryptoServiceProvider();
            Plugins = new List<Plugin>(files.Length);
            foreach (var file in files)
                Plugins.Add(new Plugin(file));
        }

        public int Id { get { return Plugins.IndexOf(this); } }
        public readonly string dllName;
        public PluginInfo[] Info;
        public readonly byte[] Data;
        public readonly byte[] Md5;
        public readonly uint Size;

        private static MD5CryptoServiceProvider _md5Provider;
        public Plugin(string dllName)
        {
            if (!File.Exists(dllName))
                throw new FileNotFoundException();

            Data = File.ReadAllBytes(dllName);
            Size = (uint)Data.Length;
            Md5 = _md5Provider.ComputeHash(Data);
            this.dllName = dllName;
            DllInit();
            DllInitDone();
        }

        private unsafe void DllInit()
        {
            int offset = 4;
            var ptr = new IntPtr((Int32)CallFunction(dllName, "DllInit", typeof(Int32), null));
            Info = new PluginInfo[Marshal.ReadInt32(ptr)];
            for (int i = 0; i < Info.Length; ++i) {
                Info[i].dllmain = new IntPtr(Marshal.ReadInt32(ptr, offset));
                Info[i].descript = new PluginDescriptor[Marshal.ReadInt32(ptr, offset+=4)];
                offset += 4;
                for (int c = 0; c < Info[i].descript.Length; ++c, offset+=sizeof(PluginDescriptor)) {
                    Info[i].descript[c] = (PluginDescriptor)Marshal.PtrToStructure(new IntPtr(ptr.ToInt64()+offset), typeof(PluginDescriptor));
                }
            }

            return;
        }

        private void DllInitDone()
        {
            CallFunction(dllName, "DllInitDone", null, null);
            return;
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
