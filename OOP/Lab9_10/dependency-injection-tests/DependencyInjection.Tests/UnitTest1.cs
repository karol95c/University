using System;
using Xunit;

namespace DependencyInjection.Tests
{

    public interface IFoo
    {

    }
    public class Foo : IFoo
    {
        public Foo()
        {
        }
    }

    public interface IBar
    {
        public int getA();
        public int getB();
    }
    public class Bar : IBar
    {
        public int a;
        public int b;
        public Bar()
        {
        }
        public Bar(int a)
        {
            this.a = -1;
            this.b = -1;
        }

        public Bar(int a, int b)
        {
            this.a = 1;
            this.b = 1;
        }

        public int getA()
        {
            return this.a;
        }

        public int getB()
        {
            return this.b;
        }
    }

    public class BarNew : IBar
    {
        public int a;
        public int b;

        public int getA()
        {
            return this.a;
        }

        public int getB()
        {
            return this.b;
        }
    }

    public interface IQux
    {
        public int getA();
        public int getB();
    }

    public class Qux : IQux
    {
        
        public int a;
        public int b;


        public Qux(int a, int b)
        {
            this.a = -1;
            this.b = -1;
        }

        [DependencyConstrutor]
        public Qux(int a)
        {
            this.a = 1;
            this.b = 999;
        }
        public Qux(string s)
        {
            this.a = 999;
            this.b = 999;
        }

        public int getA()
        {
            return this.a;
        }

        public int getB()
        {
            return this.b;
        }

    }

    public class UnitTest1
    {
        [Fact]
        public void ReturnSameType()
        {
            SimpleContainer c = new SimpleContainer();
            IFoo fs = c.Resolve<Foo>();
            Assert.Equal(typeof(Foo), fs.GetType());
            Assert.NotEqual(typeof(IFoo), fs.GetType());

            c.RegisterType<IFoo, Foo>(false);
            IFoo f1 = c.Resolve<IFoo>();
            Assert.Equal(typeof(Foo), fs.GetType());
            Assert.NotEqual(typeof(IFoo), fs.GetType());
        }

        [Fact]
        public void ReturnDiffrentInstances()
        {
            SimpleContainer c = new SimpleContainer();
            Foo f1 = c.Resolve<Foo>();
            Foo f2 = c.Resolve<Foo>();
            Assert.NotEqual(f1, f2);

            c.RegisterType<IBar, Bar>(false);
            IBar b1 = c.Resolve<IBar>();
            IBar b2 = c.Resolve<IBar>();
            Assert.NotEqual(b1, b2);
        }


        [Fact]
        public void ReturnSameInstanceWhenSingletonIsTrue()
        {
            SimpleContainer c = new SimpleContainer();
            c.RegisterType<Foo>(true);
            Foo f1 = c.Resolve<Foo>();
            Foo f2 = c.Resolve<Foo>();
            Assert.Equal(f1, f2);

            c.RegisterType<IBar, Bar>(true);
            IBar b1 = c.Resolve<IBar>();
            IBar b2 = c.Resolve<IBar>();
            Assert.Equal(b1, b2);
        }


        [Fact]
        public void ReturnOverwritedRegister()
        {
            SimpleContainer c = new SimpleContainer();
            c.RegisterType<IBar, Bar>(false);
            IBar b1 = c.Resolve<IBar>();
            Assert.Equal(typeof(Bar), b1.GetType());
            c.RegisterType<IBar, BarNew>(false);
            IBar b2 = c.Resolve<IBar>();
            Assert.Equal(typeof(BarNew), b2.GetType());
        }


        [Fact]
        public void NotCreateInsanceOfInterfeceIfNotRegistered()
        {
            SimpleContainer c = new SimpleContainer();
            Assert.Throws<NotSupportedException>(() => c.Resolve<IFoo>());

            c.RegisterType<IFoo, Foo>(false);
            Foo f1 = c.Resolve<Foo>();
            Assert.NotNull(f1);
        }

        [Fact]
        public void ReturnSameInstanceWhenInstanceRegistered()
        {
            SimpleContainer c = new SimpleContainer();
            IFoo foo1 = new Foo();
            c.RegisterInstance<IFoo>( foo1 );
            IFoo foo2 = c.Resolve<IFoo>();
            IFoo foo3 = c.Resolve<IFoo>();
            Assert.Equal(foo1, foo2);
            Assert.Equal(foo1, foo1);

            c.RegisterType<IBar, Bar>(false);
            IBar b1 = c.Resolve<IBar>();
            IBar b2 = new Bar();
            c.RegisterInstance<IBar>( b2 );
            IBar b3 = c.Resolve<IBar>();
            Assert.NotEqual(b1, b3);
            Assert.Equal(typeof(Bar), b1.GetType());

            c.RegisterType<IBar, BarNew>(false);
            IBar b4 = c.Resolve<IBar>();
            Assert.Equal(typeof(BarNew), b4.GetType());
        }

        [Fact]
        public void ChooseConstructorWithLongestParamatereList()
        {
            SimpleContainer c = new SimpleContainer();
            c.RegisterType<IBar, Bar>(false);
            IBar b1 = c.Resolve<IBar>();
            Assert.Equal(typeof(Bar), b1.GetType());
            Assert.Equal(1, b1.getA());
            Assert.Equal(1, b1.getB());
        }

        [Fact]
        public void ChooseConstructorWithAttribute()
        {
            SimpleContainer c = new SimpleContainer();
            c.RegisterType<IQux, Qux>(false);

            IQux qux = c.Resolve<IQux>();

            Assert.Equal(1, qux.getA());
            Assert.Equal(999, qux.getB());
        }
    }
}
