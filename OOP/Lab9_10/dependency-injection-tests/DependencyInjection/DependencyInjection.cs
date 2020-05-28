using System;
using System.Collections;
using System.Collections.Generic;

namespace DependencyInjection
{
    public class DependencyConstrutor : Attribute
    {
    }  
    public class NotDependencyConstrutor : Attribute
    {
    }   

    public class SimpleContainer
    {
        enum RegisterMethod
        {
            MethodType,
            MethodInstance,
            MethodSingleton
        }

        private static Dictionary<Type, Type> map = new Dictionary<Type, Type>();
        private static Dictionary<Type, object> instances = new Dictionary<Type, object>();
        private static Dictionary<Type, RegisterMethod> methodMap = new Dictionary<Type, RegisterMethod>();

        private void RegisterSingleton(bool Singleton, Type type)
        {
            if (methodMap.ContainsKey(type))
            {
                methodMap[type] = RegisterMethod.MethodSingleton;
                if (instances.ContainsKey(type))
                {
                    instances[type] = null;
                }
            }
            else
            {
                methodMap.Add(type, RegisterMethod.MethodSingleton);
            }
        }
        public void RegisterType<T> (bool Singleton) where T : class
        {   
            if (Singleton)
            {
                RegisterSingleton(Singleton, typeof(T));
            }
        }
        public void RegisterType<From, To> (bool Singleton) where To : From
        {
            if (Singleton)
            {
                RegisterSingleton(Singleton, typeof(From));
            }
            else
            {
                if (methodMap.ContainsKey(typeof(From)))
                {
                    methodMap[typeof(From)] = RegisterMethod.MethodType;
                    if (instances.ContainsKey(typeof(From)))
                    {
                        instances[typeof(From)] = null;
                    }
                }
                else
                {
                    methodMap.Add(typeof(From), RegisterMethod.MethodType);
                }

                if (map.ContainsKey(typeof(From)))
                {
                    map[typeof(From)] = typeof(To);
                }
                else
                {
                    map.Add(typeof(From), typeof(To));
                }
            }
        }

        public void RegisterInstance<T> (T instance)
        {
            if (methodMap.ContainsKey(typeof(T)))
            {
                methodMap[typeof(T)] = RegisterMethod.MethodInstance;
                
                if (instances.ContainsKey(typeof(T)))
                {
                    instances[typeof(T)] = instance;
                }
                else
                {
                    instances.Add(typeof(T), instance);
                }
            }
            else
            {
                methodMap.Add(typeof(T), RegisterMethod.MethodInstance);
            }
        }

        public Type ResolveType(Type t)
        {
            if (methodMap.ContainsKey(t) && map.ContainsKey(t))
            {
                return map[t];
            }

            if (t.IsInterface)
            {
                throw new NotSupportedException("No matching type to interface registered.");
            }
            return t;
        }
        public T Resolve<T>()
        {
            bool singleton = false;
            if (methodMap.ContainsKey(typeof(T)))
            {
                RegisterMethod m = methodMap[typeof(T)];
                switch (m)
                {
                    case RegisterMethod.MethodType : 
                        break;
                    case RegisterMethod.MethodInstance :
                        return (T)instances[typeof(T)];
                    case RegisterMethod.MethodSingleton :
                        singleton = true;
                        break;
                    default :
                        break;
                }
            }   
            var resolve = ResolveType(typeof(T));
            if (instances.ContainsKey(typeof(T)))
            {
                if (singleton && instances[typeof(T)] != null)
                {
                    return (T)instances[typeof(T)];
                }
            }

            var constructors = resolve.GetConstructors();
            var constructor = constructors[0];
            int max = -1;
            bool dependency = false;

            foreach (var c in constructors)
            {
                bool dependencyConst = c.GetCustomAttributes(typeof(DependencyConstrutor), false).Length > 0;
                if (dependency && dependencyConst)
                {
                    throw new NotSupportedException("No matching type to interface registered.");
                }
                else if (dependencyConst)
                {
                    constructor = c; 
                    dependency = true;
                }
                else if (!dependency && c.GetParameters().Length > max)
                {
                    max = c.GetParameters().Length;
                    constructor = c; 
                }              
            }

            var parameters = constructor.GetParameters();

            List<object> resolvedParameters = new List<object>();

            foreach (var item in parameters)
            {
                Type t = item.ParameterType;
                var method = typeof(T).GetMethod("Resolve");
                if (method != null)
                {
                    var genericMethod = method.MakeGenericMethod(new Type[] {t});
                    resolvedParameters.Add(genericMethod.Invoke(this, new object[]{}));
                }
                else
                {
                    if (t.IsValueType)
                        resolvedParameters.Add(Activator.CreateInstance(t));
                }
            }

            T result = (T)constructor.Invoke(resolvedParameters.ToArray());
            if (singleton)
            {
                instances[typeof(T)] = result;
            }

            return (T)result;
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            // SimpleContainer c = new SimpleContainer();
            // IFoo fs = c.Resolve<Foo>();

            // c.RegisterType<IFoo, Foo>(true);
            // IFoo f1 = c.Resolve<IFoo>();
            // IFoo f2 = c.Resolve<IFoo>();
            // Console.WriteLine(f1==f2);
            // Console.WriteLine(fs==f2);
            // c.RegisterType<IFoo>( false );
            // IFoo f3 = c.Resolve<IFoo>();
            // Console.WriteLine(fs==f3);
            // Console.WriteLine(f2==f3);
        }
    }
}
