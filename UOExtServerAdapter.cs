using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Net;
using System.Runtime.InteropServices;

namespace UOExtDomain
{
    #region COM Interface

    [ComVisible(true)]
    [Guid("E36BBF07-591E-4959-97AE-D439CBA392FB")]
    public interface IUOExtServerAdapter
    {
        void CLRVersion();
        void Start(string ip, int port, string folder, string mask);
    }

    [ComVisible(true)]
    [Guid("A6574755-925A-4E41-A01B-B6A0EEF72DF0")]
    public class UOExtServerAdapter : IUOExtServerAdapter
    {
        //private UOExtServer _UOExtServer = new MyClass();

        public void CLRVersion()
        {
            Console.WriteLine("CLR version from DLL: {0}", Environment.Version);
        }

        public void Start(string ip, int port, string folder, string mask)
        {
            Console.WriteLine("CLR Starting UOEXT server");
            Console.WriteLine("UOEXT ip={0}|", ip);
            Console.WriteLine("UOEXT port={0}|", port);
            Console.WriteLine("UOEXT folder={0}|", folder);
            Console.WriteLine("UOEXT mask={0}|", mask);
            UOExtServer.StartServer(new IPEndPoint(IPAddress.Parse(ip), port), folder, mask);
            Console.WriteLine("[uoexts] Listening: {0}:{1}", ip, port);
        }
    }

    #endregion COM Interface
}
