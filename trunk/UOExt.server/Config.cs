using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace UOExt
{
    public static class Config
    {
        /// <summary>
        /// Where current code is running. Autodetect, do not touch.
        /// </summary>
        private static bool m_inRunUO;

        /// <summary>
        /// If code inside RunUO and this is true, then UOExt will be routed to m_ip:m_port server as update server.
        /// </summary>
        private static bool m_externalServerInRunUO = false;

        /// <summary>
        /// If code inside RunUO - this is encapsulation for UOExt protocol.
        /// </summary>
        private static byte m_encapsulationHeader = 0xFF;

        /// <summary>
        /// Client plugins folder. It will be filled with absolute path, if needed.
        /// </summary>
        private static string m_clientPluginsPath = @"UOExt\Plugins\Client";

        /// <summary>
        /// File with plugins init order. File format is:
        /// [DllName], [PluginNumber | PluginName]
        /// Example:
        /// ; This is comment. You can set ';' char only at start of a line.
        /// HelloWorld.plg, 0
        /// MyCoolPlugin.plg, 0
        /// MyOtherCoolPlugin.plg, 0
        /// MyCoolPlugin.plg, 1
        /// </summary>
        private static string m_pluginsInitOrderFile = @"UOExt\Plugins\Client\Order.cfg";

        /// <summary>
        /// Server plugins folder. It will be filled with absolute path, if needed.
        /// </summary>
        private static string m_serverPluginsPath = @"UOExt\Plugins\Server";

        /// <summary>
        /// Where is UOExt.GUI.dll located. This dll will deliver to client before any other plugins.
        /// </summary>
        private static string m_UOExtGUIPath = @"UOExt\UOExt.GUI.dll";

        /// <summary>
        /// Whereis is UOExt.dll located.
        /// </summary>
        private static string m_UOExtPath = @"UOExt\UOExt.dll";

        /// <summary>
        /// If code is Standalone - this is IP for server to Listen. If code inside RunUO and ExternalServerInRunUO == true, than this is IP to route UOExt during support detection phase.
        /// </summary>
        private static string m_ip = "127.0.0.1";

        /// <summary>
        /// If code is Standalone - this is port for server to Listen. If code inside RunUO and ExternalServerInRunUO == true, than this is port to route UOExt during support detection phase.
        /// </summary>
        private static int m_port = 2594;

        /// <summary>
        /// Autodetect.
        /// </summary>
        private static bool m_Unix = false;

        /// <summary>
        /// Autodetect.
        /// </summary>
        private static bool m_64bit = false;

        public static bool InRunUO { get { return m_inRunUO; } }
        public static byte EncapsulationHeader { get { return m_encapsulationHeader; } }
        public static string ClientPluginsPath { get { return m_clientPluginsPath; } }
        public static string ServerPluginsPath { get { return m_serverPluginsPath; } }
        public static string PluginInitOrderFile { get { return m_pluginsInitOrderFile; } }
        public static string UOExtGUIPath { get { return m_UOExtGUIPath; } }
        public static string UOExtPath { get { return m_UOExtPath; } }
        public static string IP { get { return m_ip; } }
        public static int Port { get { return m_port; } }
        public static bool ExternalServerInRunUO { get { return m_externalServerInRunUO; } }
        public static bool IsUnix { get { return m_Unix; } set { m_Unix = value; } }
        public static bool Is64Bit { get { return m_64bit; } set { m_64bit = value; } }

        private static void ConfigurePaths()
        {
            m_clientPluginsPath = Path.IsPathRooted(m_clientPluginsPath) ? m_clientPluginsPath
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, m_clientPluginsPath);
            m_serverPluginsPath = Path.IsPathRooted(m_serverPluginsPath) ? m_serverPluginsPath
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, m_serverPluginsPath);
            m_pluginsInitOrderFile = Path.IsPathRooted(m_pluginsInitOrderFile) ? m_pluginsInitOrderFile
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, m_pluginsInitOrderFile);
            m_UOExtPath = Path.IsPathRooted(m_UOExtPath) ? m_UOExtPath
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, m_UOExtPath);
            m_UOExtGUIPath = Path.IsPathRooted(m_UOExtGUIPath) ? m_UOExtGUIPath
                      : Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, m_UOExtGUIPath);
        }

        public static void Configure()
        {
            m_inRunUO = true;
            ConfigurePaths();
        }

        public static void Standalone()
        {
            m_inRunUO = false;
            ConfigurePaths();
			int platform = (int)Environment.OSVersion.Platform;
            if (platform == 4 || platform == 128)
            {
                m_Unix = true;
            }
#if Framework_4_0
		    m_64bit = Environment.Is64BitProcess;
#else
		    m_64bit = (IntPtr.Size == 8);	//Returns the size for the current /process/
#endif
        }
    }
}
