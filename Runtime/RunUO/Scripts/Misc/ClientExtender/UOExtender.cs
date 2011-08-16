//#define NOTDEFAULTRUNUO    // Использовать конфигурацию задаваемую сервером (не работает в официальной RunUO)

using System;
using System.IO;
using System.Net;
using System.Net.NetworkInformation;
using UOExtDomain;

namespace Server.Misc.ClientExtender
{
	public static class UOExtender
    {
        /// <summary>
        /// Опция: Используемый аддрес сервером.
        /// </summary>
        public static string Addres 
        { get {
            #if !NOTDEFAULTRUNUO
            return "127.0.0.1"; 
            #else
            return Server.Configuration.LocalEntry.AddressList[0].ToString(); 
            #endif
        } } 
		
		/// <summary>
        /// Опция: Используемый порт сервером.
		/// </summary>
        public static int Port 
        { get { 
            #if !NOTDEFAULTRUNUO
            return 2595;
            #else
            return Server.Core.Localhost ? 2585 : 2595;
            #endif
        } }

        /// <summary>
        /// Опция: Путь к папке с плагинами
        /// </summary>
        public static string PluginsFolder
        { get { 
            #if !NOTDEFAULTRUNUO
            return Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, @"Data", @"UOExtPlugins"); 
            #else
            return Path.Combine(Server.Configuration.DataPath, @"UOExtPlugins");
            #endif
        } }

        /// <summary>
        /// Опция: Путь к папке с плагинами
        /// </summary>
        public static string PluginFileMask
        { get { 
            return @"Plugin-0x??.dll"; 
        } }
		

        [CallPriority(10)]
        public static void Initialize()
        {			
            #if (NET_4_0 || NETCF_4_0 || MONO_4_0)
            IPEndPoint endPoint = new IPEndPoint(IPAddress.Parse(Addres), Port);
            UOExtServer.StartServer(endPoint, PluginsFolder, PluginFileMask);
            #else
            Type COM_UOExtServerAdapterType = Type.GetTypeFromProgID("UOExtDomain.UOExtServerAdapter");
            object COM_UOExtServerAdapterInstance = Activator.CreateInstance(COM_UOExtServerAdapterType);
            IUOExtServerAdapter COM_UOExtServerAdapter = (IUOExtServerAdapter)COM_UOExtServerAdapterInstance;
            COM_UOExtServerAdapter.CLRVersion();
            COM_UOExtServerAdapter.Start(Addres, Port, PluginsFolder, PluginFileMask);
            #endif            
        }
	}
}