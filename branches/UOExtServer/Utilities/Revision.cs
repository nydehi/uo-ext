/***************************************************************************
 *                                               Created by :        StaticZ
 *                  Interface.cs                 UO Quintessense server team
 *              ____________________             url   :   http://uoquint.ru
 *              Version : 14/01/2011             email :    admin@uoquint.ru
 *                                               ---------------------------
 * History :
 *   16/10/2009 Первый реализ
 *
 * Todo :
 *
 ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;

namespace UOExtDomain.Utilities
{
    public class Revision
    {
        private int m_DayRevision;
        private int m_SecRevision;

        public int DayRevision { get { return m_DayRevision; } set { m_DayRevision = value; } }
        public int SecRevision { get { return m_SecRevision; } set { m_SecRevision = value; } }

        public string StrRevision { get { return String.Format("{0:D4}-{1:D5}", m_DayRevision, m_SecRevision); } set { this.Parse(value); } }

        private void Parse(string revision)
        {
            if (revision.Length < 10)
                return;

            string[] str = revision.Split(new char[] { '-', '.', '/', '\\', ' ' });
            if (str.Length < 2)
                return;

            int dayRevision = 0;
            int secRevision = 0;
            for (int i = str.Length - 1; i >= 0; --i)
                if (str[i].Length == 4)
                    Int32.TryParse(str[i], out dayRevision);
                else if (str[i].Length == 5)
                    Int32.TryParse(str[i], out secRevision);
                else
                    continue;

            if (dayRevision != 0 && dayRevision != 0)
            {
                m_DayRevision = dayRevision;
                m_SecRevision = secRevision;
            }
        }

        public Revision(int dayRevision, int secRevision)
        {
            m_DayRevision = dayRevision;
            m_SecRevision = secRevision;
        }

        public Revision(string revision)
        {
            Parse(revision);
        }

        public Revision(DateTime dateTime)
        {
            m_DayRevision = (int)Math.Floor((dateTime - new DateTime(2000, 1, 1, 0, 0, 0)).TotalDays);
            m_SecRevision = (int)Math.Floor((dateTime - new DateTime(dateTime.Year, dateTime.Month, dateTime.Day, 0, 0, 0)).TotalSeconds / 2);
        }

        public static Revision Now { get { return new Revision(DateTime.Now); } }

        /// <summary>
        /// Сравнивает экзэмпляр класса с объектом Version version
        /// </summary>
        /// <param name="version">объект Version с которым будет проводиться сравнивание</param>
        /// <returns>Возвращает 1 в случае если version старее; -1 в случаее если version новее; 0 в случаее если version равно;</returns>
        public int Compare(Revision version)
        {
            int dayRevDelta = m_DayRevision - version.DayRevision;
            int secRevDelta = m_SecRevision - version.SecRevision;

            if (dayRevDelta > 0)
                return 1;
            else if (dayRevDelta < 0)
                return -1;
            else if (secRevDelta > 0)
                return 1;
            else if (secRevDelta < 0)
                return -1;
            else
                return 0;
        }
    }
}
