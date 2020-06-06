using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

namespace DependencyInjection
{

    [Serializable]
    public class DependencyInterfaceException : Exception
    {
        public DependencyInterfaceException()
        {
        }

        public DependencyInterfaceException(Type t)
            : base(String.Format("Cannot create instance of interface: {0}", t.ToString()))
        {
        }
    }

    [Serializable]
    public class DependencyTypeException : Exception
    {
        public DependencyTypeException()
        {
        }

        public DependencyTypeException(Type t)
            : base(String.Format("Cannot resolve instance of type: {0}", t.ToString()))
        {
        }
    }

    [Serializable]
    public class DependencyCycleException : Exception
    {
        public DependencyCycleException()
            : base("Cycle detected when resolving dependency.")
        {
        }
    }

    [Serializable]
    public class DependencyConstructorException : Exception
    {
        public DependencyConstructorException()
            : base("Too many constructors with DependencyConstrutor attribute.")
        {
        }
    }

    public class DependencyConstrutor : Attribute
    {
    }  
    public class NotDependencyConstrutor : Attribute
    {
    }   

    public class DependencyProperty : Attribute
    {
    }   
    public class DependencyMethod : Attribute
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

        private List<Type> pathToResolve = new List<Type>();

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
            }
            else
            {
                methodMap.Add(typeof(T), RegisterMethod.MethodInstance);
            }

            if (instances.ContainsKey(typeof(T)))
            {
                instances[typeof(T)] = instance;
            }
            else
            {
                instances.Add(typeof(T), instance);
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
                throw new DependencyInterfaceException(t);
            }
            return t;
        }

        // Return singleton instance, if there is no instance yet
        // create new and cache.
        public object GetSingleton(Type type)
        {   
            if (methodMap.ContainsKey(type))
            {
                methodMap[type] = RegisterMethod.MethodSingleton; 
            }
            else
            {
                methodMap.Add(type, RegisterMethod.MethodSingleton);
            }
    
            if (instances.ContainsKey(type) && instances[type] != null)
            {
                return instances[type];
            }
            else
            {
                instances.Add(type, ResolveConcType(type));
            }
            return instances[type];
        }

        // Function to get instance of type if there is
        // cached one.
        public object TryGetInstance(Type type)
        {
            if (methodMap.ContainsKey(type))
            {
                methodMap[type] = RegisterMethod.MethodInstance;
                RegisterMethod m = methodMap[type];
                if (m == RegisterMethod.MethodInstance)
                {
                    return instances[type];
                }
                else if(m == RegisterMethod.MethodSingleton)
                {
                    return GetSingleton(type);
                }
            }
            return null; 
        }

        private void FillParameterList(ref ParameterInfo[] parameters, ref List<object> resolvedParameters)
        {
            foreach (var item in parameters)
            {
                Type t = item.ParameterType;
                if (methodMap.ContainsKey(t))
                {
                    object obj = TryGetInstance(t);
                    if (null != obj)
                    {
                        resolvedParameters.Add(obj);
                        continue;
                    }
                }
                object resolved = ResolveConcType(t);
                if (null != resolved)
                {
                    resolvedParameters.Add(resolved);
                }
            }
        }

        private void ResolveDependencyProperties(ref object result, ref PropertyInfo[] properties)
        {
            foreach (var p in properties)
            {
                if (p.GetCustomAttributes(typeof(DependencyProperty), false).Length > 0)
                {
                    // Resolve only if property has set accessor and value is not null
                    MethodInfo setter = p.GetSetMethod();
                    if (null != setter && null == p.GetValue(result))
                    {
                        Type toResolve = p.PropertyType;
                        object resolved = ResolveConcType(toResolve);
                        p.SetValue(result, resolved);
                    }
                }
            }
        }

        private void ResolveDependencyMethods(ref object result, ref MethodInfo[] methods)
        {
            foreach (var m in methods)
            {
                if (m.GetCustomAttributes(typeof(DependencyMethod), false).Length > 0)
                { 
                    var parameters = m.GetParameters();
                    if (parameters.Length > 0)
                    {
                        List<object> resolvedParameters = new List<object>();
                        foreach (var p in parameters)
                        {
                            Type toResolve = p.ParameterType;
                            resolvedParameters.Add(ResolveConcType(toResolve)); 
                        }
                        m.Invoke(result, resolvedParameters.ToArray());
                    }
                }
            }
        }
        private void FindConstructor(ref ConstructorInfo[] constructors, ref ConstructorInfo constructor)
        {
            int max = -1;
            bool dependency = false;
            foreach (var c in constructors)
            {
                // If there is DependencyConstructor attribute pick this one
                // (also check if there are no more with this attribute)
                // otherwise pick constructor with longest parameter list.
                bool dependencyConst = c.GetCustomAttributes(typeof(DependencyConstrutor), false).Length > 0;
                if (dependency && dependencyConst)
                {
                    throw new DependencyConstructorException();
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
        }
    
        private object ResolveConcType(Type type)
        {
            bool singleton = false;

            // If there is type in list alredy, cycle is detecded.
            if (pathToResolve.Contains(type))
            {
                throw new DependencyCycleException();
            }

            pathToResolve.Add(type);

            if (methodMap.ContainsKey(type))
            {
                RegisterMethod m = methodMap[type];
                switch (m)
                {
                    case RegisterMethod.MethodType : 
                        break;
                    case RegisterMethod.MethodInstance :
                        pathToResolve.Remove(type);
                        return instances[type];
                    case RegisterMethod.MethodSingleton :
                        singleton = true;
                        break;
                    default :
                        break;
                }
            }
    
            var resolve = ResolveType(type);
            if (instances.ContainsKey(type))
            {
                if (singleton && instances[type] != null)
                {
                    pathToResolve.Remove(type);
                    return instances[type];
                }
            }

            var constructors = resolve.GetConstructors();
            if (constructors.Length < 1)
            {
                if (resolve.IsValueType)
                {
                    pathToResolve.Remove(type);
                    return Activator.CreateInstance(resolve);
                }
                else
                {
                    throw new DependencyTypeException(resolve);
                }
            }

            var constructor = constructors[0];
            FindConstructor(ref constructors, ref constructor);
            var parameters = constructor.GetParameters();


            List<object> resolvedParameters = new List<object>();
            FillParameterList(ref parameters, ref resolvedParameters);
            
            // Throw exception when not all parameters are resolved.
            if (parameters.Length != resolvedParameters.Count)
            {
                throw new DependencyTypeException(resolve);
            }

            object result = constructor.Invoke(resolvedParameters.ToArray());
            if (singleton)
            {
                instances[type] = result;
            }

            var properties = resolve.GetProperties();
            ResolveDependencyProperties(ref result, ref properties);

            var methods = resolve.GetMethods();
            ResolveDependencyMethods(ref result, ref methods);
        
            pathToResolve.Remove(type);
            return result;
        }
        public T Resolve<T>()
        {   
            pathToResolve.Clear();
            object result = ResolveConcType(typeof(T));
            return (T)result;
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            // Please check UnitTest1.cs
        }
    }
}
