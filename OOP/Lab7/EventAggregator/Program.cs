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
    class Field : FlowLayoutPanel
    {
        public Label label;
        public TextBox text_box;

        public Field(string label_text)
            : base()
        {
            AutoSize = true;
            // this.Size = new Size(100,100);

            label = new Label();
            label.Text = label_text;
            label.AutoSize = true;
            label.Anchor = AnchorStyles.Left;
            label.TextAlign = ContentAlignment.MiddleLeft;

            Controls.Add(label);

            text_box = new TextBox();

            Controls.Add(text_box);
        }
    }

    public class MyForm : Form
    {
        internal System.Windows.Forms.TreeView treeView1;
        internal System.Windows.Forms.ListView listView1;
        internal System.Windows.Forms.ListBox listBox1;
        internal System.Windows.Forms.Button buttonAdd;
        internal System.Windows.Forms.Button buttonEdit;
        internal System.Windows.Forms.FlowLayoutPanel panel1;
        internal System.Windows.Forms.FlowLayoutPanel editpanel;
        // public event System.Windows.Forms.TreeNodeMouseClickEventHandler NodeMouseClick;
        private List<Field> fields;
        private EventAggregator ea;
        private List<Person> students;
        private List<Person> teachers;
        private int activeIndex;
        private int activeType;
        public MyForm(EventAggregator e)
        {

            fields = new List<Field>();
            InitComponents();

            ea = e;
            students = new List<Person>();
            teachers = new List<Person>();
            students.Add(new Person( "Kochanowski", "Jan", "1901-01-01", "ad"));
            students.Add(new Person( "Makowski","Tomasz", "1902-02-02", "ad"));

            teachers.Add(new Person( "Silor","Krzysztof", "1901-01-01", "ad"));
            teachers.Add(new Person( "Tomaszewski","Jerzy", "1902-02-02", "ad"));
            UpdateTreeView();
        }

        public void treeView1_NodeMouseClick(object sender,  
        TreeNodeMouseClickEventArgs e)
        {
            ea.RaiseNotification(new TreeViewClicked(e.Node.Text));
        }

        public void listViewVisibility(bool visible)
        {
            Console.WriteLine("listViewVisibility");
            listView1.Visible = visible;
        }

        public void listBoxVisibility(bool visible)
        {
            listBox1.Visible = visible;
            Console.WriteLine("listBoxVisibility");
        }

        public void addListItem(Person p)
        {
            string[] row = {p.surname, p.name, p.birthDate, p.address};
            var listViewItem = new ListViewItem(row);
            this.listView1.Items.Add(listViewItem);
        }
        
        private void InitializeTreeView()
        {
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.treeView1.Location = new System.Drawing.Point(0, 0);
            this.treeView1.Size = new System.Drawing.Size(200, ClientSize.Height);

            this.treeView1.BorderStyle = BorderStyle.FixedSingle;
            this.treeView1.BackColor = this.BackColor;
            this.treeView1.Scrollable = true;

            // Set the HideSelection property to false to keep the 
            // selection highlighted when the user leaves the control. 
            // This helps it blend with form.
            this.treeView1.HideSelection = true;

            // Set the ShowRootLines and ShowLines properties to false to 
            // give the TreeView a list-like appearance.
            this.treeView1.ShowRootLines = true;
            this.treeView1.ShowLines = true;

            // Add the nodes.
                        TreeNode studentsNode = new TreeNode("Studenci");
            TreeNode teachersNode = new TreeNode("Wykładowcy");
            this.treeView1.Nodes.Add(studentsNode);
            this.treeView1.Nodes.Add(teachersNode);

            this.treeView1.TabIndex = 0;

            this.treeView1.NodeMouseClick += 
                new TreeNodeMouseClickEventHandler(treeView1_NodeMouseClick);
            this.Controls.Add(this.treeView1);
        }
        public void InitializeListView()
        {
            listView1 = new ListView();
            this.listView1.Location = new System.Drawing.Point(treeView1.Width , 0);
            this.listView1.Size = new System.Drawing.Size(ClientSize.Width - treeView1.Width, 300);
            // listView1.Bounds = new Rectangle(new Point(10,10), new Size(300,200));

            // Set the view to show details.
            listView1.View = View.Details;
            // Allow the user to edit item text.
            listView1.LabelEdit = true;
            // Allow the user to rearrange columns.
            listView1.AllowColumnReorder = false;
            // Display check boxes.
            listView1.CheckBoxes = false;
            // Select the item and subitems when selection is made.
            listView1.FullRowSelect = true;
            // Display grid lines.
            listView1.GridLines = true;
            // Sort the items in the list in ascending order.
            listView1.Sorting = SortOrder.Ascending;

            // Create columns for the items and subitems.
            // Width of -2 indicates auto-size.
            listView1.Columns.Add("Nazwisko", -2, HorizontalAlignment.Left);
            listView1.Columns.Add("Imię", -2, HorizontalAlignment.Left);
            listView1.Columns.Add("Data urodzenia", -2, HorizontalAlignment.Left);
            listView1.Columns.Add("Adres", -2, HorizontalAlignment.Center);

            this.Controls.Add(this.listView1);
        }

        public void InitializeListBox()
        {
            panel1 = new FlowLayoutPanel();
            panel1.FlowDirection = FlowDirection.TopDown;
            // panel1.Dock = DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(treeView1.Width , 0);
            this.panel1.Size = new System.Drawing.Size(300, 300);

            Field last_name = new Field("Nazwisko");
            fields.Add(last_name);
            panel1.Controls.Add(last_name);

            Field first_name = new Field("Imię");
            fields.Add(first_name);
            panel1.Controls.Add(first_name);

            Field birth_date = new Field("Data urodzenia");
            fields.Add(birth_date);
            panel1.Controls.Add(birth_date);

            Field address = new Field("Adres");
            fields.Add(address);
            panel1.Controls.Add(address);

            panel1.Visible = true;

            this.Controls.Add(this.panel1);


            // listBox1 = new ListBox();
            // // Set the size and location of the ListBox.

            // listBox1.MultiColumn = false;
            // // Set the selection mode to multiple and extended.
            // listBox1.SelectionMode = SelectionMode.MultiExtended;

            // this.Controls.Add(listBox1);
            // listBox1.Visible = false;
        }       

        public void InitializeButtons()
        {
            // Create and initialize a Button.
            buttonAdd = new Button();
            buttonAdd.Text = "Dodaj";
            buttonAdd.Click += buttonAdd_Click;

            buttonAdd.Location = new Point(250, 350);
            buttonAdd.Size = new Size(100, 50);
            // Set the button to return a value of OK when clicked.
            buttonAdd.DialogResult = DialogResult.OK;
            // Add the button to the form.
            Controls.Add(buttonAdd);
            buttonAdd.Visible = false;

            buttonEdit = new Button();
            buttonEdit.Click += buttonEdit_Click;
            buttonEdit.Text= "Edytuj";
            buttonEdit.Location = new Point(250, 350);
            buttonEdit.Size = new Size(100, 50);
            // Set the button to return a value of OK when clicked.
            buttonEdit.DialogResult = DialogResult.OK;
            // Edit the button to the form.
            Controls.Add(buttonEdit);
             Field address = new Field("Adres");
            fields.Add(address);
            this.Controls.Add(address);
        
        }

        public void UpdateTreeView()
        {
            this.treeView1.Nodes.Clear();
            TreeNode studentsNode = new TreeNode("Studenci");
            foreach (Person p in students)
            {
                 studentsNode.Nodes.Add(new TreeNode(p.name + " " + p.surname));
            }

            TreeNode teachersNode = new TreeNode("Wykładowcy");
            foreach (Person p in teachers)
            {
                 teachersNode.Nodes.Add(new TreeNode(p.name + " " + p.surname));
            }

            this.treeView1.Nodes.Add(studentsNode);
            this.treeView1.Nodes.Add(teachersNode);
        }


        public void ShowListView(int type)
        {
            this.panel1.Visible = false;
            this.buttonEdit.Visible = false;
            this.buttonAdd.Visible = true;
            this.listView1.Visible = true;
            this.listView1.Items.Clear();
            if (type == 0)
            {
                foreach (Person p in students)
                {
                    addListItem(p);
                }
            }
            else if (type == 1)
            {
                foreach (Person p in teachers)
                {
                    addListItem(p);
                }
            }
        }

        public void ShowActiveListView()
        {
            ShowListView(activeType);
        }

        public void ShowPersonView(string name, string surname)
        {
            this.Controls.Add(panel1);
            this.panel1.Location = new System.Drawing.Point(treeView1.Width , 0);
            this.listView1.Visible = false;
            this.buttonAdd.Visible = false;
            this.panel1.Visible = true;
            this.buttonEdit.Visible = true;

            for (int i = 0; i < teachers.Count; ++i)
            {
                if (teachers[i].name == name && teachers[i].surname == surname)
                {
                    AddToField(i, 1);
                    return;
                }
            }

            for (int i = 0; i < students.Count; ++i)
            {
                if (students[i].name == name && students[i].surname == surname)
                {
                    AddToField(i, 0);
                    return;
                }
            }
        }

        public void ShowActivePersonView()
        {
            this.Controls.Add(panel1);
            this.panel1.Location = new System.Drawing.Point(treeView1.Width , 0);
            this.listView1.Visible = false;
            this.buttonAdd.Visible = false;
            this.panel1.Visible = true;
            this.buttonEdit.Visible = true;

            AddToField(activeIndex, activeType);
        }
        
        public void AddPerson()
        {
            this.panel1.Visible = true;
            Form prompt = new Form()
            {
                Width = 300,
                Height = 400,
                FormBorderStyle = FormBorderStyle.FixedDialog,
                Text = "x",
                StartPosition = FormStartPosition.CenterScreen
            };
            prompt.Controls.Add(panel1);
            this.panel1.Location = new System.Drawing.Point(0 , 0);

            fields[0].text_box.Text = ""; 
            fields[1].text_box.Text = ""; 
            fields[2].text_box.Text = "";
            fields[3].text_box.Text = "";

            fields[0].text_box.ReadOnly = false; 
            fields[1].text_box.ReadOnly = false; 
            fields[2].text_box.ReadOnly = false;
            fields[3].text_box.ReadOnly = false;
            Button b = new Button();
            b.Click += buttonAdd_Finished;
            b.Text= "Zapisz";
            b.Location = new Point(200, 300);
            b.Size = new Size(50, 50);
            // Set the button to return a value of OK when clicked.
            b.DialogResult = DialogResult.OK;
            // Edit the button to the form.
            prompt.Controls.Add(b);
        
            prompt.ShowDialog();

            ShowActiveListView();
        }
        public void EditPerson()
        {
            Form prompt = new Form()
            {
                Width = 300,
                Height = 400,
                FormBorderStyle = FormBorderStyle.FixedDialog,
                Text = "x",
                StartPosition = FormStartPosition.CenterScreen
            };
            prompt.Controls.Add(panel1);
            this.panel1.Location = new System.Drawing.Point(0 , 0);

            fields[0].text_box.ReadOnly = false; 
            fields[1].text_box.ReadOnly = false; 
            fields[2].text_box.ReadOnly = false;
            fields[3].text_box.ReadOnly = false;
            Button b = new Button();
            b.Click += buttonEdit_Finished;
            b.Text= "Zapisz";
            b.Location = new Point(200, 300);
            b.Size = new Size(50, 50);
            // Set the button to return a value of OK when clicked.
            b.DialogResult = DialogResult.OK;
            // Edit the button to the form.
            prompt.Controls.Add(b);
        
            prompt.ShowDialog();

            ShowActivePersonView();
        }
        
        public void AddToField(int index, int type)
        {
            // listBox1.Items.Clear();
            activeIndex = index;
            activeType = type;
            if (type == 0)
            {
                fields[0].text_box.Text = students[index].surname;
                fields[1].text_box.Text = students[index].name;
                fields[2].text_box.Text = students[index].birthDate;
                fields[3].text_box.Text = students[index].address;

            }
            else if (type == 1)
            {
                fields[0].text_box.Text = teachers[index].surname;
                fields[1].text_box.Text = teachers[index].name;
                fields[2].text_box.Text = teachers[index].birthDate;
                fields[3].text_box.Text = teachers[index].address;

            }

            fields[0].text_box.ReadOnly = true; 
            fields[1].text_box.ReadOnly = true; 
            fields[2].text_box.ReadOnly = true;
            fields[3].text_box.ReadOnly = true;
        }

        public void buttonAdd_Click(object sender, System.EventArgs e)
        {
            ea.RaiseNotification(new UserFileAddClicked());
        }
        public void buttonEdit_Click(object sender, System.EventArgs e)
        {
            
            ea.RaiseNotification(new UserFileEditClicked());

        }

        public void buttonEdit_Finished(object sender, System.EventArgs e)
        {
            Person p = new Person(fields[0].text_box.Text, fields[1].text_box.Text, fields[2].text_box.Text, fields[3].text_box.Text);
            ea.RaiseNotification(new UserFileEditingFinished(p));
        }

        public void buttonAdd_Finished(object sender, System.EventArgs e)
        {
            Person p = new Person(fields[0].text_box.Text, fields[1].text_box.Text, fields[2].text_box.Text, fields[3].text_box.Text);
            ea.RaiseNotification(new UserFileAddingFinished(p));
        }

        public void ApplyEdit(Person p)
        {
            if (activeType == 0)
            {
                students[activeIndex] = p;
            }

            if (activeType == 1)
            {
                teachers[activeIndex] = p;
            }
        }

        public void AddNew(Person p)
        {
            if (activeType == 0)
            {
                students.Add(p);
            }

            if (activeType == 1)
            {
                teachers.Add(p);
            }
        }

        private void InitComponents()
        {
            Text = "Kartoteka";
            ClientSize = new Size(800, 450);

            InitializeTreeView();

            InitializeListView();

            InitializeListBox();
            InitializeButtons();

            CenterToScreen();

        }
        [DllImport( "kernel32.dll" )]
        static extern bool AttachConsole( int dwProcessId );
        private const int ATTACH_PARENT_PROCESS = -1;
        [STAThread]
        static void Main()
        {
            AttachConsole( ATTACH_PARENT_PROCESS );

            Application.SetHighDpiMode(HighDpiMode.SystemAware);
            Application.EnableVisualStyles();

            EventAggregator ea = new EventAggregator();
            MyForm form = new MyForm(ea);
            Controller controller = new Controller(form);
            ea.RegisterSubscriber<TreeViewClicked>(controller);
            ea.RegisterSubscriber<UserFileEditClicked>(controller);
            ea.RegisterSubscriber<UserFileEditingFinished>(controller);
            ea.RegisterSubscriber<UserFileAddClicked>(controller);
            ea.RegisterSubscriber<UserFileAddingFinished>(controller);

            Application.Run(form);
        }
    }

    
}
