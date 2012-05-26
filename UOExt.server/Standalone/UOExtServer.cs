using System;
using System.IO;
using System.Net;
using UOExt.Network;

namespace UOExt.Standalone
{
    public static class UOExtServer
    {
		[STAThread]
		static void Main(){
            Config.Standalone();
            (new Server()).WaitJoin();
        }
    }
}
