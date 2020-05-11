using System;
using System.Collections.Generic;
using System.ComponentModel;  
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing;
using System.Runtime.InteropServices;


using System.Diagnostics;

namespace MyApp
{

    public class Person
    {
        public string surname;
        public string name;
        public string birthDate;
        public string address;

        public Person(string s, string n, string b, string a)
        {
            this.surname = s;
            this.name = n;
            this.birthDate = b;
            this.address = a;
        }
    }

    public class TreeViewPersonClicked
    {
        public string typeString;

        public TreeViewPersonClicked()
        {

        }
        
    }

    public class TreeViewClicked
    {
        public string typeClicked;

        public TreeViewClicked(string t)
        {
            typeClicked = t;
        }
    }

    public class UserFileAddClicked
    {
        
    }

    public class UserFileEditClicked
    {
    }

    public class UserFileEditingFinished
    {

        public Person personEdited;

        public UserFileEditingFinished(Person p)
        {
            personEdited = p;
        }
    }

    public class UserFileAddingFinished
    {

        public Person personAdded;

        public UserFileAddingFinished(Person p)
        {
            personAdded = p;
        }
    }
    

    
    public interface ISubscriber<T>
    {
        void Handle( T Notification );
    }
    public interface IEventAggregator
    {
        void RegisterSubscriber<T>( ISubscriber<T> Subscriber );
        void RemoveSubscriber<T>( ISubscriber<T> Subscriber );
        void RaiseNotification<T>( T Event );
    }
    
    public class Controller :
        ISubscriber<TreeViewClicked>,
        ISubscriber<UserFileAddClicked>,
        ISubscriber<UserFileEditClicked>,
        ISubscriber<UserFileEditingFinished>,
        ISubscriber<UserFileAddingFinished>
    {
        public MyForm form;

        public Controller (MyForm f)
        {
            form = f;

        }

        public void Handle(TreeViewClicked notification)
        {
            Console.WriteLine(notification.typeClicked);
            if (notification.typeClicked == "Studenci")
            {
                form.ShowListView(0);
            }
            else if (notification.typeClicked == "Wykładowcy")
            {
                form.ShowListView(1);
            }
            else
            {
                string[] personData = notification.typeClicked.Split(" "); 
                form.ShowPersonView(personData[0], personData[1]);
            }
        }

        public void Handle(UserFileAddClicked notification)
        {
            form.AddPerson();

        }

        public void Handle(UserFileEditClicked notification)
        {
            form.EditPerson();
        }

        public void Handle(UserFileEditingFinished notification)
        {
            form.ApplyEdit(notification.personEdited);
            form.ShowActivePersonView();
            form.UpdateTreeView();
        }

        public void Handle(UserFileAddingFinished notification)
        {
            form.AddNew(notification.personAdded);
            form.UpdateTreeView();
            form.ShowActiveListView();
        }
    }
   public class EventAggregator : IEventAggregator
    {
        Dictionary<Type, List<object>> _subscribers =
        new Dictionary<Type, List<object>>();
        #region IEventAggregator Members
        public void RegisterSubscriber<T>( ISubscriber<T> Subscriber )
        {
            if (!_subscribers.ContainsKey( typeof( T ) ))
                _subscribers.Add( typeof( T ), new List<object>() );
                _subscribers[typeof( T )].Add( Subscriber );
        }
        public void RemoveSubscriber<T>( ISubscriber<T> Subscriber )
        {
            if (_subscribers.ContainsKey( typeof( T ) ))
                _subscribers[typeof( T )].Remove( Subscriber );
        }
         public void RaiseNotification<T>( T Event )
        {
            // Console.WriteLine("test debug");
            if (_subscribers.ContainsKey( typeof( T ) ))
                foreach (ISubscriber<T> subscriber in
                    _subscribers[typeof( T )].OfType<ISubscriber<T>>())
                    subscriber.Handle( Event );
        }
        #endregion
 }
}