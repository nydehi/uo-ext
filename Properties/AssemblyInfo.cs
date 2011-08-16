using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// Управление общими сведениями о сборке осуществляется с помощью 
// набора атрибутов. Измените значения этих атрибутов, чтобы изменить сведения,
// связанные со сборкой.

#if LIBRARY

#if DEBUG
[assembly: AssemblyTitle("AppDomain (dll) - DEBUG")]
#else
[assembly: AssemblyTitle("AppDomain (dll)")]
#endif
[assembly: AssemblyProduct("AppDomain (dll)")]

#elif TESTCENTER

#if DEBUG
[assembly: AssemblyTitle("UOExtTestClient (exe) - DEBUG")]
#else
[assembly: AssemblyTitle("UOExtTestClient (exe)")]
#endif
[assembly: AssemblyProduct("UOExtTestClient (exe)")]

#else

#if DEBUG
[assembly: AssemblyTitle("AppDomain (exe) - DEBUG")]
#else
[assembly: AssemblyTitle("AppDomain (exe)")]
#endif
[assembly: AssemblyProduct("AppDomain (exe)")]

#endif


[assembly: AssemblyDescription("Server for UO client and protocol extender")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("http://code.google.com/p/uo-ext/")]
[assembly: AssemblyCopyright("Copyright © 2011")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

// Параметр ComVisible со значением FALSE делает типы в сборке невидимыми 
// для COM-компонентов.  Если требуется обратиться к типу в этой сборке через 
// COM, задайте атрибуту ComVisible значение TRUE для этого типа.
[assembly: ComVisible(false)]

// Следующий GUID служит для идентификации библиотеки типов, если этот проект будет видимым для COM
[assembly: Guid("7ee24497-b669-4089-a570-0f61fb55d64b")]

// Сведения о версии сборки состоят из следующих четырех значений:
//
//      Основной номер версии
//      Дополнительный номер версии 
//      Номер построения
//      Редакция
//
// Можно задать все значения или принять номер построения и номер редакции по умолчанию, 
// используя "*", как показано ниже:
// [assembly: AssemblyVersion("1.0.*")]
[assembly: AssemblyVersion("0.46.*")]
[assembly: AssemblyFileVersion("0.46.*")]
