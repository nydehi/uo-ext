using System;
using System.Collections.Generic;
using System.Text;
using UOExt.Network;

namespace UOExt.Plugins
{
    public abstract class ServerPlugin
    {
        public abstract void UOExtInitialize(PacketHandler hndlr);
    }

    public abstract class CSharpServerPlugin : ServerPlugin
    {
    }

    public sealed class DllServerPlugin : ServerPlugin
    {
        private string m_path;
        public DllServerPlugin(string path)
        {
            m_path = path;
        }

        public override void UOExtInitialize(PacketHandler hndlr)
        {
            throw new NotImplementedException();
        }
    }
}
