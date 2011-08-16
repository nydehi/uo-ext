/***************************************************************************
 *                                               Created by :        StaticZ
 *                   DExecutor.cs                UO Quintessense server team
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
using System.Windows.Forms;
using System.Threading;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

namespace UOExtDomain.Utilities
{
    internal static class DExecutor
    {
        public static T CreateInstance<T>()
        {
            return (T)Activator.CreateInstance(typeof(T));
        }

        public static object CreateInstance(Type type)
        {
            return Activator.CreateInstance(type);
        }

        public static T CreateInstance<T>(params object[] args)
        {
            return (T)Activator.CreateInstance(typeof(T), args);
        }

        public static object CreateInstance(Type type, params object[] args)
        {
            return Activator.CreateInstance(type, args);
        }


        public static void SetStaticProperty(Type obj_type, string property_name, object new_value)
        {
            if (obj_type == null)
                throw new ArgumentNullException("obj_type");
            PropertyInfo property = obj_type.GetProperty(property_name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
            if (property == null)
                throw new InvalidOperationException(string.Format("Property '{0}' not found in class '{1}.", property_name, obj_type));
            property.SetValue(null, new_value, null);
        }

        public static void SetProperty(Control control, object obj, string property_name, object new_value)
        {
            if (control.InvokeRequired)
            {
                control.Invoke((ThreadStart)delegate
                {
                    SetProperty(obj, property_name, new_value);
                });
            }
            else
            {
                SetProperty(obj, property_name, new_value);
            }
        }

        public static void SetProperty(object obj, string property_name, object new_value)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");
            Type obj_type = obj.GetType();
            PropertyInfo property = obj_type.GetProperty(property_name, BindingFlags.Public | BindingFlags.Instance);
            if (property == null)
                throw new InvalidOperationException(string.Format("Property '{0}' not found in class '{1}.", property_name, obj_type));
            property.SetValue(obj, new_value, null);
        }

        public static object GetStaticProperty(Type obj_type, string property_name)
        {
            if (obj_type == null)
                throw new ArgumentNullException("obj_type");
            PropertyInfo property = obj_type.GetProperty(property_name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
            if (property == null)
                throw new InvalidOperationException(string.Format("Property '{0}' not found in class '{1}.", property_name, obj_type));
            return property.GetValue(null, null);
        }

        public static object GetProperty(Control control, object obj, string property_name)
        {
            if (control.InvokeRequired)
            {
                object result = null;
                control.Invoke((ThreadStart)delegate
                {
                    result = GetProperty(obj, property_name);
                });
                return result;
            }
            else
            {
                return GetProperty(obj, property_name);
            }
        }

        public static T GetProperty<T>(Control control, object obj, string property_name)
        {
            if (control.InvokeRequired)
            {
                T result = default(T);
                control.Invoke((ThreadStart)delegate
                {
                    result = GetProperty<T>(obj, property_name);
                });
                return result;
            }
            else
            {
                return GetProperty<T>(obj, property_name);
            }
        }

        public static object GetProperty(object obj, string property_name)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");
            Type obj_type = obj.GetType();
            PropertyInfo property = obj_type.GetProperty(property_name, BindingFlags.Public | BindingFlags.Instance);
            if (property == null)
                throw new InvalidOperationException(string.Format("Property '{0}' not found in class '{1}.", property_name, obj_type));
            return property.GetValue(obj, null);
        }

        public static T GetProperty<T>(object obj, string property_name)
        {
            return (T)GetProperty(obj, property_name);
        }

        //         public static void InvokeMethod(Control control, object obj, string method_name, params object[] args)
        //         {
        //             if (control.InvokeRequired)
        //             {
        //                 control.Invoke((ThreadStart)delegate
        //                 {
        //                     InvokeMethod(obj, method_name, args);
        //                 });
        //             }
        //             else
        //             {
        //                 InvokeMethod(obj, method_name, args);
        //             }
        //         }
        // 
        //         public static void InvokeMethod(object obj, string method_name, params object[] args)
        //         {
        //             if (obj == null)
        //                 throw new ArgumentNullException("obj");
        //             Type obj_type = obj.GetType();
        //             Type[] arg_types;
        //             if (args != null)
        //                 arg_types = Array.ConvertAll<object, Type>(args, delegate(object arg)
        //                 {
        //                     return arg != null ? arg.GetType() : typeof(void);
        //                 });
        //             else
        //                 arg_types = Type.EmptyTypes;
        // 
        // 
        // 
        //             MethodInfo method = obj_type.GetMethod(method_name, BindingFlags.Public | BindingFlags.Instance, null, arg_types, null);
        //             if (method == null)
        //                 throw new InvalidOperationException(string.Format("Method '{0}' not found in class '{1}.", method_name, obj_type));
        //             method.Invoke(obj, args);
        //         }

        public static object InvokeStaticMethod(Type obj_type, string method_name)
        {
            return InvokeStaticMethod(obj_type, method_name, null);
        }

        public static object InvokeStaticMethod(Type obj_type, string method_name, params object[] args)
        {
            if (obj_type == null)
                throw new ArgumentNullException("obj_type");
            MethodInfo method = obj_type.GetMethod(method_name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
            if (method == null)
                throw new InvalidOperationException(string.Format("Method '{0}' not found in class '{1}.", method_name, obj_type));
            return method.Invoke(null, args);
        }

        public static object InvokeMethod(object obj, string method_name)
        {
            return InvokeMethod(obj, null, method_name, null);
        }
        
        public static object InvokeMethod(object obj, string method_name, params object[] args)
        {
            return InvokeMethod(obj, null, method_name, args);
        }

        public static object InvokeMethod(object obj, Type result, string method_name, params object[] args)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");
            Type obj_type = obj.GetType();

            object res = result != null ? CreateInstance(result) : new object();

            res = obj_type.InvokeMember(method_name, System.Reflection.BindingFlags.InvokeMethod, null, obj, args); 

            //             if(result != null)
            //                 return (result)res;
            //             else
            return res;
        }


        public static List<Type> GetAssemblyTypes(Type basetype, bool onlysealed = false)
        {
            return GetAssemblyTypes(Assembly.GetExecutingAssembly(), true, basetype, null, onlysealed);
        }

        public static List<Type> GetAssemblyTypes(Type[] interfaces, bool onlysealed = false)
        {
            return GetAssemblyTypes(Assembly.GetExecutingAssembly(), false, null, interfaces, onlysealed);
        }

        public static List<Type> GetAssemblyTypes(Type basetype, Type[] interfaces, bool onlysealed = false)
        {
            return GetAssemblyTypes(Assembly.GetExecutingAssembly(), true, basetype, interfaces, onlysealed);
        }  

        private static List<Type> GetAssemblyTypes(Assembly assembly, bool hasbasetype, Type basetype, Type[] interfaces, bool onlysealed)
        {
            var types = new List<Type>();
            Type[] asmTypes = assembly.GetTypes();
            foreach (Type t in asmTypes)
            {
                if (t.IsAbstract)
                    continue;
                if (onlysealed && !t.IsSealed)
                    continue;
                if (hasbasetype && t.BaseType != basetype)
                    continue;
                if (interfaces != null && interfaces.Length > 0)
                {
                    var _findAll = true;
                    var intTypes = t.GetInterfaces();
                    foreach (var _interface in interfaces) {
                        var _find = false;
                        foreach (var _intType in intTypes)
                            if (_intType == _interface) {
                                _find = true;
                                break;
                            }
                        if (!_find) {
                            _findAll = false;
                            break;
                        }
                    }
                    if (!_findAll)
                        continue;
                }
                types.Add(t);
            }
            return types;
        }   
    }
}
