/***************************************************************************
 *                                               Created by :        StaticZ
 *                   IniFile.cs                  UO Quintessense server team
 *              ____________________             url   :   http://uoquint.ru
 *              Version : 14/01/2011             email :   uoquint@gmail.com 
 *                                               ---------------------------
 * History :
 *   16/10/2009 Первый реализ
 *
 * Todo :
 *
 ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.Linq;
using System.Text;
using System.IO;

namespace UOExtDomain.Utilities
{
    internal sealed class IniFile
    {
        [DllImport("Kernel32", EntryPoint = "GetPrivateProfileInt", CharSet = CharSet.Unicode)]
        private static extern Int32 _GetPrivateProfileInt(string appName, string keyName, Int32 valDefault, string filePath);

        [DllImport("Kernel32", EntryPoint = "GetPrivateProfileString", CharSet = CharSet.Unicode)]
        private static extern UInt32 _GetPrivateProfileString(string appName, string keyName, string valDefault, string valReturn, UInt32 size, string filePath);

        [DllImport("kernel32.dll", EntryPoint = "WritePrivateProfileString", CharSet = CharSet.Unicode, SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool _WritePrivateProfileString(string lpAppName, string lpKeyName, string lpString, string lpFileName);

        public static IniFile Default
        { get { return m_Default ?? new IniFile(null); } }
        private static IniFile m_Default = null;

        public readonly string ConfigPath;

        public IniFile(string configPath = "Config.ini")
        {
            if(String.IsNullOrEmpty(configPath) || String.Equals(configPath, "Config.ini")){
                configPath = Path.Combine(AppDomain.CurrentDomain.SetupInformation.ApplicationBase, "Config.ini");
                if (m_Default == null)
                    m_Default = this;
            }
            ConfigPath = configPath;
            #if DEBUG
            if (!File.Exists(ConfigPath))
                throw new Exception(String.Format("File: \"{0}\" doesn't exists.", ConfigPath));
            #endif
        }

        public bool ReadBool(string appName, string keyName, bool valDefault = false, bool write = true)
        {
            int getint = _GetPrivateProfileInt(appName, keyName, valDefault ? 1 : 0, ConfigPath);
            bool result = !(getint <= 0);
            
            if (write && result == valDefault)
                WriteBool(appName, keyName, valDefault);
            
            return result;
        }

        public int ReadInt(string appName, string keyName, int valDefault=0, bool write = true)
        {
            int result = _GetPrivateProfileInt(appName, keyName, valDefault, ConfigPath);
            
            if (write && result == valDefault)
                WriteInt(appName, keyName, valDefault);
            
            return result;
        }

        public string ReadString(string appName, string keyName, string valDefault = "", bool write = true)
        {
            string getstr = new string('\0', 0x4000);
            _GetPrivateProfileString(appName, keyName, valDefault, getstr, 0x4000, ConfigPath);
            string result = getstr.TrimEnd('\0');
            if (String.IsNullOrEmpty(result))
                result = valDefault;

            if (write && result == valDefault)
                WriteString(appName, keyName, valDefault);

            return result;
        }

        public bool WriteBool(string appName, string keyName, bool val)
        {
            bool result = _WritePrivateProfileString(appName, keyName, val ? "1" : "0", ConfigPath);
            return result;
        }

        public bool WriteInt(string appName, string keyName, int val)
        {
            bool result = _WritePrivateProfileString(appName, keyName, String.Format("{0}", val), ConfigPath);
            return result;
        }

        public bool WriteString(string appName, string keyName, string val)
        {
            bool result = _WritePrivateProfileString(appName, keyName, String.Format("\"{0}\"", val), ConfigPath);
            return result;
        }
    }
}
