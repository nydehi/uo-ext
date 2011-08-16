using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace UOExtDomain
{
    [ComVisible(true)]
    [Guid("E36BBF07-591E-4959-97AE-D439CBA392FB")]
    public interface IUOExtServerAdapter
    {
        void CLRVersion();
        void Start(string ip, int port, string folder, string mask);
    }
}