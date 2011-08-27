/***************************************************************************
 *                                               Created by :        StaticZ
 *                  Compressor.cs                UO Quintessense server team
 *              ____________________             url   :   http://uoquint.ru
 *              Version : 14/01/2011             email :    admin@uoquint.ru 
 *                                               ---------------------------
 * History :
 *   16/10/2010 Первый реализ
 *
 * Todo :
 *
 ***************************************************************************/

using System;
using System.IO;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

namespace UOExtDomain.Utilities
{
    internal interface ICompressor
    {
        /// <summary>
        /// Версия библиотеки
        /// </summary>
        string Version { get; }

        /// <summary>
        /// Числовой индификатор компрессора
        /// </summary>
        byte TypeId { get; }

        /// <summary>
        /// Компрессия данных
        /// </summary>
        /// <param name="data">Не сжатые данные ввиде массива байт</param>
        /// <param name="size">Предполагаемый размер после компрессии</param>
        /// <returns>Сжатые данные ввиде масива байт</returns>
        byte[] Compress(byte[] data, UInt32 size);

        /// <summary>
        /// Декомпрессия данных
        /// </summary>
        /// <param name="data">Сжатые данные ввиде масива байт</param>
        /// <param name="size">Предполагаемый размер после декомпрессии</param>
        /// <returns>Не сжатые данные ввиде массива байт</returns>
        byte[] Decompress(byte[] data, UInt32 size);
    }

    /// <summary>
    /// Компресаратор (враппер комперосоров)
    /// </summary>
    internal class Compressor : ICompressor
    {
        private readonly ICompressor _Compressor;

        public byte TypeId { get { return _Compressor.TypeId; } }

        public string Version { get { return _Compressor.Version; } }

        public byte[] Compress(byte[] data, UInt32 size)
        {
            return _Compressor.Compress(data, size);
        }

        public byte[] Decompress(byte[] data, UInt32 size)
        {
            return _Compressor.Decompress(data, size);
        }

        public static ICompressor New(Type compressorType)
        {
            return DExecutor.CreateInstance(compressorType) as ICompressor;
        }

        public static ICompressor New(byte typeId)
        {
            ICompressor compressor = null;
            switch (typeId) {
                case 0x00: compressor = new NoneCompressor(); break;
                case 0x01: try { compressor = new ZipNativeCompressor(); } catch { /*compressor = new ZipManagedCompressor();*/ }   break;
                case 0x04: try { compressor = new BZ2NativeCompressor(); } catch { /*compressor = new BZ2ManagedCompressor();*/ }   break;
                default: throw new ArgumentOutOfRangeException("Unknown compressor typeId.");
            } return compressor;
        }

        private Compressor()
        {
        }

        public Compressor(byte typeId)
        {
            _Compressor = Compressor.New(typeId);
        }      
    }

    /// <summary>
    /// Не использует сжатия (быстро, но большой объём)
    /// </summary>
    internal class NoneCompressor : ICompressor
    {
        public byte TypeId { get { return 0x00; } }

        public NoneCompressor()
        {
            m_Version = "1.0.0, 01-Aug-2011";
        }

        public string Version { get{ return m_Version; } }
        private readonly string m_Version;

        public byte[] Compress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();
            return data;
        }

        public byte[] Decompress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();
            return data;
        }
    }

    /// <summary>
    /// Востроеный компресор SharpZipLib ( http://sharpdevelop.net/OpenSource/SharpZipLib/ ) не требует стороних библиотек и не использует нативный код
    /// </summary>
    internal class ZipManagedCompressor //: ICompressor
    {
        //public byte[] Compress(byte[] data, UInt32 size)
        //{

        //}

    }

    /// <summary>
    /// Внешний нативный компресор zLib ( http://zlib.net/ ), чуть быстрее но хуже качество сжатия
    /// </summary>
    internal class ZipNativeCompressor : ICompressor
    {
        public byte TypeId { get { return 0x01; } }

        public enum ZLibError : int
        {
            VersionError = -6,
            BufferError = -5,
            MemoryError = -4,
            DataError = -3,
            StreamError = -2,
            FileError = -1,

            Okay = 0,

            StreamEnd = 1,
            NeedDictionary = 2
        }

        public enum ZLibQuality : int
        {
            Default = -1,
            None = 0,
            Speed = 1,
            Size = 9
        }

        [DllImport("zlib(x86).dll", EntryPoint = "zlibVersion")]
        private static extern unsafe string _LibVersion86();

        [DllImport("zlib(x64).dll", EntryPoint = "zlibVersion")]
        private static extern unsafe string _LibVersion64();

        [DllImport("zlib(x86).dll", EntryPoint = "compress2")]
        private static extern unsafe ZLibError _Compress86(byte[] dest, ref Int32 destLenght, byte[] source, Int32 sourceLenght, ZLibQuality quality);

        [DllImport("zlib(x64).dll", EntryPoint = "compress2")]
        private static extern unsafe ZLibError _Compress64(byte[] dest, ref Int32 destLenght, byte[] source, Int32 sourceLenght, ZLibQuality quality);

        [DllImport("zlib(x86).dll", EntryPoint = "uncompress")]
        private static extern unsafe ZLibError _Decompress86(byte[] dest, ref Int32 destLenght, byte[] source, Int32 sourceLenght);

        [DllImport("zlib(x64).dll", EntryPoint = "uncompress")]
        private static extern unsafe ZLibError _Decompress64(byte[] dest, ref Int32 destLenght, byte[] source, Int32 sourceLenght);

        public ZipNativeCompressor()
        {
            string dllName = UOExtServer.Is64BitProcess ? "zlib(x64).dll" : "zlib(x86).dll";
            string dllPath = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), dllName);
            if (!File.Exists(dllPath))
                throw new DllNotFoundException(String.Format("Не найдена библиотека {0}.", dllName));

            m_Version = UOExtServer.Is64BitProcess ? _LibVersion64() : _LibVersion86();
            //if (Version != "1.2.5, 10-Dec-2007")
            //    throw new DllNotFoundException(String.Format("Версия \"{0}\" библиотеки \"{1}\" не поддерживается.", Version, dllName));
        }

        public string Version { get{ return m_Version; } }
        private readonly string m_Version;

        public byte[] Compress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();

            unsafe
            {
                Int32 length = (Int32)size;
                byte[] result = new byte[length];

                fixed (byte* p1 = &data[0], p2 = &result[0]) {
                    var status = UOExtServer.Is64BitProcess 
                        ? _Compress64(result, ref length, data, (Int32)data.Length, ZLibQuality.Default)
                        : _Compress86(result, ref length, data, (Int32)data.Length, ZLibQuality.Default);

                    if (status != ZLibError.Okay) {
                        throw new ApplicationException(String.Format("Error {0} ocurse in data compression.", Enum.GetName(typeof(ZLibError), status)));
                    }
                }

                byte[] compress = new byte[length];
                Array.Copy(result, compress, length);
                return compress;
            }
        }

        public byte[] Decompress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();

            unsafe
            {
                Int32 length = (Int32)size;
                byte[] result = new byte[length];

                fixed (byte* p1 = &data[0], p2 = &result[0]) {
                    var status = UOExtServer.Is64BitProcess
                                 ? _Decompress64(result, ref length, data, (Int32)data.Length)
                                 : _Decompress86(result, ref length, data, (Int32)data.Length);
                    if (status != ZLibError.Okay) {
                        throw new ApplicationException(String.Format("Error {0} ocurse in data decompression.", Enum.GetName(typeof(ZLibError), status)));
                    }
                }

                byte[] decompress = new byte[length];
                Array.Copy(result, decompress, length);
                return decompress;
            }
        }
    }

    /// <summary>
    /// Внешний нативный компресор bzip2 ( http://bzip.org/ ), чуть медленее но лучшее качество сжатия
    /// </summary>
    internal class BZ2NativeCompressor : ICompressor
    {
        public byte TypeId { get { return 0x04; } }

        [DllImport("bzip2(x86).dll", EntryPoint = "BZ2_bzlibVersion")]
        private static extern unsafe string _LibVersion86();

        [DllImport("bzip2(x64).dll", EntryPoint = "BZ2_bzlibVersion")]
        private static extern unsafe string _LibVersion64();

        [DllImport("bzip2(x86).dll", EntryPoint = "BZ2_bzBuffToBuffCompress")]
        private static extern unsafe Int32 _Compress86(byte[] dest, ref UInt32 destLenght, byte[] source, UInt32 sourceLenght, Int32 blockSize100K, Int32 verbosity, Int32 workFactor);

        [DllImport("bzip2(x64).dll", EntryPoint = "BZ2_bzBuffToBuffCompress")]
        private static extern unsafe Int32 _Compress64(byte[] dest, ref UInt32 destLenght, byte[] source, UInt32 sourceLenght, Int32 blockSize100K, Int32 verbosity, Int32 workFactor);

        [DllImport("bzip2(x86).dll", EntryPoint = "BZ2_bzBuffToBuffDecompress")]
        private static extern unsafe Int32 _Decompress86(byte[] dest, ref UInt32 destLenght, byte[] source, UInt32 sourceLenght, Int32 small, Int32 verbosity);

        [DllImport("bzip2(x64).dll", EntryPoint = "BZ2_bzBuffToBuffDecompress")]
        private static extern unsafe Int32 _Decompress64(byte[] dest, ref UInt32 destLenght, byte[] source, UInt32 sourceLenght, Int32 small, Int32 verbosity);

        public BZ2NativeCompressor()
        {
            string dllName = UOExtServer.Is64BitProcess ? "bzip2(x64).dll" : "bzip2(x86).dll";
            string dllPath = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), dllName);
            if (!File.Exists(dllPath))
                throw new DllNotFoundException(String.Format("Не найдена библиотека {0}.", dllName));

            m_Version = UOExtServer.Is64BitProcess ? _LibVersion64() : _LibVersion86();
            //if (Version != "1.0.6, 6-Sept-2010")
            //    throw new DllNotFoundException(String.Format("Версия \"{0}\" библиотеки \"{1}\" не поддерживается.", Version, dllName));
        }

        public string Version { get{ return m_Version; } }
        private readonly string m_Version;

        public byte[] Compress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();

            unsafe
            {
                UInt32 length = size;
                byte[] result = new byte[length];

                fixed (byte* p1 = &data[0], p2 = &result[0])
                {
                    Int32 status = Int32.MaxValue;
                    while (status != 0)
                    {
                        status = UOExtServer.Is64BitProcess
                                 ? _Compress64(result, ref length, data, (UInt32)data.Length, 9, 0, 0)
                                 : _Compress86(result, ref length, data, (UInt32)data.Length, 9, 0, 0);
                        if (status == -8)
                        {
                            length *= 2;
                            result = new byte[length];
                        }
                        else if (status != 0)
                        {
                            throw new ApplicationException("Unnespectible error ocurse in data compression.");
                        }
                    }
                }

                byte[] compress = new byte[length];
                Array.Copy(result, compress, length);
                return compress;
            }
        }

        public byte[] Decompress(byte[] data, UInt32 size)
        {
            if (data != null ? data.Length < 1 : true)
                throw new ArgumentNullException();
            if (size == 0)
                throw new ArgumentOutOfRangeException();

            unsafe
            {
                UInt32 length = size;
                byte[] result = new byte[length];

                fixed (byte* p1 = &data[0], p2 = &result[0])
                {
                    Int32 status = Int32.MaxValue;
                    while (status != 0)
                    {
                        status = UOExtServer.Is64BitProcess
                                 ? _Decompress64(result, ref length, data, (UInt32)data.Length, 0, 0)
                                 : _Decompress86(result, ref length, data, (UInt32)data.Length, 0, 0);
                        if (status == -8)
                        {
                            length *= 2;
                            result = new byte[length];
                        }
                        else if (status != 0)
                        {
                            throw new ApplicationException("Unnespectible error ocurse in data decompression.");
                        }
                    }
                }

                byte[] decompress = new byte[length];
                Array.Copy(result, decompress, length);
                return decompress;
            }
        }
    }

}
